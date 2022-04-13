/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Copyright (c) 2011-2021 The plumed team
   (see the PEOPLE file at the root of the distribution for a list of names)

   See http://www.plumed.org for more information.

   This file is part of plumed, version 2.

   plumed is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   plumed is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with plumed.  If not, see <http://www.gnu.org/licenses/>.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
#ifndef __PLUMED_core_Atoms_h
#define __PLUMED_core_Atoms_h

#include "tools/TypesafePtr.h"
#include "tools/Communicator.h"
#include "tools/Tensor.h"
#include "tools/Units.h"
#include "tools/Exception.h"
#include "tools/AtomNumber.h"
#include "tools/ForwardDecl.h"
#include <vector>
#include <map>
#include <string>
#include <memory>
#include <iosfwd>

namespace PLMD {

class MDAtomsBase;
class PlumedMain;
class ActionAtomistic;
class ActionWithVirtualAtom;
class Pbc;

/// Class containing atom related quantities from the MD code.
/// IT IS STILL UNDOCUMENTED. IT PROBABLY NEEDS A STRONG CLEANUP
class Atoms
{
  friend class ActionAtomistic;
  friend class ActionWithVirtualAtom;
  int natoms;
  bool unique_serial=false; // use unique in serial mode
  std::vector<AtomNumber> unique;
  std::vector<unsigned> uniq_index;
/// Map global indexes to local indexes
/// E.g. g2l[i] is the position of atom i in the array passed from the MD engine.
/// Called "global to local" since originally it was used to map global indexes to local
/// ones used in domain decomposition. However, it is now also used for the NAMD-like
/// interface, where only a small number of atoms is passed to plumed.
  std::vector<int> g2l;
  std::vector<Vector> positions;
  std::vector<Vector> forces;
  std::vector<double> masses;
  std::vector<double> charges;
  std::vector<ActionWithVirtualAtom*> virtualAtomsActions;
  Tensor box;
  ForwardDecl<Pbc> pbc_fwd;
  Pbc&   pbc=*pbc_fwd;
  Tensor virial;
// this is the energy set by each processor:
  double md_energy;
// this is the summed energy:
  double energy;

  bool   dataCanBeSet;
  bool   collectEnergy;
  bool   energyHasBeenSet;
  unsigned positionsHaveBeenSet;
  bool massesHaveBeenSet;
  bool chargesHaveBeenSet;
  bool boxHasBeenSet;
  unsigned forcesHaveBeenSet;
  bool virialHasBeenSet;
  bool massAndChargeOK;
  unsigned shuffledAtoms;

  std::map<std::string,std::vector<AtomNumber> > groups;

  void resizeVectors(unsigned);

  std::vector<int> fullList;

  std::unique_ptr<MDAtomsBase> mdatoms;

  PlumedMain & plumed;

  Units MDUnits;
  Units units;

  bool naturalUnits;
  bool MDnaturalUnits;

  double timestep;
  double forceOnEnergy;

/// if set to true, all the forces in the global array are zeroes
/// at every step. It should not be necessary in general, but it is
/// for actions accessing to modifyGlobalForce() (e.g. FIT_TO_TEMPLATE).
  bool zeroallforces;

  double kbT;

  std::vector<ActionAtomistic*> actions;
  std::vector<int>    gatindex;

  bool asyncSent;
  bool atomsNeeded;

  class DomainDecomposition:
    public Communicator
  {
  public:
    bool on;
    bool async;

    std::vector<Communicator::Request> mpi_request_positions;
    std::vector<Communicator::Request> mpi_request_index;

    std::vector<double> positionsToBeSent;
    std::vector<double> positionsToBeReceived;
    std::vector<int>    indexToBeSent;
    std::vector<int>    indexToBeReceived;
    operator bool() const {return on;}
    DomainDecomposition():
      on(false), async(false)
    {}
    void enable(Communicator& c);
  };

  template <class P,class T>
  /// Class holding the action cache.
  /// P should be the type of actions that are cached (e.g., ActionAtomistic).
  /// T should be the type of cached object (e.g., a vector that is expensive to build).
  /// I add it here but might be moved to Tools if needed.
  /// It is actually general enough to be used for other tasks.
  class ActiveActionCache {
  public:
    /// Constructor, with cache size
    ActiveActionCache(std::size_t max_size=10):
      max_size(max_size)
    {
      if(max_size>0) data.reserve(max_size);
    }
    typedef std::vector<const P*> key;
    /// Add an object to the cache
    void add(const key& k,const T& cache) {
      // cache is disabled in this case
      if(max_size==0) return;
      // find existing entries with the same k
      auto erase=std::remove_if(data.begin(),data.end(),[k](const Entry & e) {return e.k==k;});
      // remove them (note: there should be at most one! maybe this could be optimized)
      data.erase(erase,data.end());
      // if needed, make space (should erase only one element! maybe this could be optimized)
      while(data.size()+1>max_size) data.erase(data.begin());
      // add
      data.emplace_back(Entry{k,cache});
      // update statistics
      max_usage=std::max(max_usage,data.size());
    }
    /// Retrieve object from the cache
    /// If found: return address
    /// If not found: return nullptr
    const T* get(const key& k)const {
      // search for an entry with the requested key
      // comparison should be fast for non-matching keys since they are expected to have different length typically
      // however, the matching key will be compared as whole vector
      auto find=std::find_if(data.begin(),data.end(),[k](const Entry & e) {return e.k==k;});
      if(find!=data.end()) {
        // found
        hit++;
        return &find->cache;
      } else {
        // not found
        miss++;
        return nullptr;
      }
    }
    /// Remove object(s) from the cache.
    /// Remove all objects where key contains i.
    /// Note: this is expensive since it requires scanning all the whole keys
    void remove(const P* i) {
      // find all keys containing pointer i
      auto erase=std::remove_if(data.begin(),data.end(),[i](const Entry & e) {
        return std::find_if(e.k.begin(),e.k.end(),[i](const P* p) {return p==i;})!=e.k.end();
      });
      // erase them
      data.erase(erase,data.end());
    }
    /// Clear the whole cache
    void clear() {
      data.clear();
    }
    /// Write report
    void report(std::ostream & os) const {
      if(size()>0) os<<"Atom list cache - current: " <<size()<<" max: "<<max_usage<<" hits: "<<hit<<"/"<<(hit+miss)<<"\n";
    }
    /// Return current cache size
    std::size_t size() const {
      return data.size();
    }
    /// Return cache capacity.
    /// Zero capacity implies the cache is disabled.
    std::size_t capacity() const {
      return max_size;
    }
  private:
    /// local class
    struct Entry {
      key k;
      T cache;
    };
    /// maximum cache size, set in constructor
    const std::size_t max_size;
    /// maximum cache usage
    std::size_t max_usage=0;
    /// number of hits, could be mutated while accessing cache
    std::size_t mutable hit=0;
    /// number of misses, could be mutated while accessing cache
    std::size_t mutable miss=0;
    /// this is the real cache
    std::vector<Entry> data;
  };

  DomainDecomposition dd;
  ActiveActionCache<ActionAtomistic,std::vector<AtomNumber>> actionsCache;
  long int ddStep;  //last step in which dd happened

  void share(const std::vector<AtomNumber>&);

public:

  explicit Atoms(PlumedMain&plumed);
  ~Atoms();

  void init();

  void share();
  void shareAll();
  void wait();
  void updateForces();

  void setRealPrecision(int);
  int  getRealPrecision()const;

  void setTimeStep(const TypesafePtr &);
  double getTimeStep()const;

  void setKbT(const TypesafePtr &);
  double getKbT()const;

  void setNatoms(int);
  int getNatoms()const;
  int getNVirtualAtoms()const;

  const long int& getDdStep()const;
  const std::vector<int>& getGatindex()const;
  const Pbc& getPbc()const;
  void getLocalMasses(std::vector<double>&);
  void getLocalPositions(std::vector<Vector>&);
  void getLocalForces(std::vector<Vector>&);
  void getLocalMDForces(std::vector<Vector>&);
  const Tensor& getVirial()const;

  void setCollectEnergy(bool b) { collectEnergy=b; }

  void setDomainDecomposition(Communicator&);
  void setAtomsGatindex(const TypesafePtr &,bool);
  void setAtomsContiguous(int);
  void setAtomsNlocal(int);

  void startStep();
  void setEnergy(const TypesafePtr &);
  void setBox(const TypesafePtr &);
  void setVirial(const TypesafePtr &);
  void setPositions(const TypesafePtr &);
  void setPositions(const TypesafePtr &,int);
  void setForces(const TypesafePtr &);
  void setForces(const TypesafePtr &,int);
  void setMasses(const TypesafePtr &);
  void setCharges(const TypesafePtr &);
  bool chargesWereSet() const ;
  bool boxWasSet() const ;

  void MD2double(const TypesafePtr & m,double&d)const;
  void double2MD(const double&d,const TypesafePtr & m)const;

  void createFullList(const TypesafePtr &);
  void getFullList(const TypesafePtr &);
  void clearFullList();

  void add(ActionAtomistic*);
  void remove(ActionAtomistic*);

  void updateRequestedAtoms(ActionAtomistic*);

  double getEnergy()const {plumed_assert(collectEnergy && energyHasBeenSet); return energy;}

  bool isEnergyNeeded()const {return collectEnergy;}

  void setMDEnergyUnits(double d) {MDUnits.setEnergy(d);}
  void setMDLengthUnits(double d) {MDUnits.setLength(d);}
  void setMDTimeUnits(double d) {MDUnits.setTime(d);}
  void setMDChargeUnits(double d) {MDUnits.setCharge(d);}
  void setMDMassUnits(double d) {MDUnits.setMass(d);}
  const Units& getMDUnits() {return MDUnits;}
  void setUnits(const Units&u) {units=u;}
  const Units& getUnits() {return units;}
  void updateUnits();

  AtomNumber addVirtualAtom(ActionWithVirtualAtom*);
  void removeVirtualAtom(ActionWithVirtualAtom*);
  ActionWithVirtualAtom* getVirtualAtomsAction(AtomNumber)const;
  bool isVirtualAtom(AtomNumber)const;
  void insertGroup(const std::string&name,const std::vector<AtomNumber>&a);
  void removeGroup(const std::string&name);
  void writeBinary(std::ostream&)const;
  void readBinary(std::istream&);
  double getKBoltzmann()const;
  double getMDKBoltzmann()const;
  bool usingNaturalUnits()const;
  void setNaturalUnits(bool n) {naturalUnits=n;}
  void setMDNaturalUnits(bool n) {MDnaturalUnits=n;}

  void setExtraCV(const std::string &name,const TypesafePtr & p);
  void setExtraCVForce(const std::string &name,const TypesafePtr & p);
  double getExtraCV(const std::string &name);
  void updateExtraCVForce(const std::string &name,double f);
  std::string reportCache() const;
};

inline
int Atoms::getNatoms()const {
  return natoms;
}

inline
int Atoms::getNVirtualAtoms()const {
  return virtualAtomsActions.size();
}

inline
const long int& Atoms::getDdStep()const {
  return ddStep;
}

inline
const std::vector<int>& Atoms::getGatindex()const {
  return gatindex;
}

inline
const Pbc& Atoms::getPbc()const {
  return pbc;
}

inline
bool Atoms::isVirtualAtom(AtomNumber i)const {
  return i.index()>=(unsigned) getNatoms();
}

inline
ActionWithVirtualAtom* Atoms::getVirtualAtomsAction(AtomNumber i)const {
  return virtualAtomsActions[i.index()-getNatoms()];
}

inline
bool Atoms::usingNaturalUnits() const {
  return naturalUnits || MDnaturalUnits;
}

inline
bool Atoms::chargesWereSet() const {
  return chargesHaveBeenSet;
}

inline
bool Atoms::boxWasSet() const {
  return boxHasBeenSet;
}

inline
const Tensor& Atoms::getVirial()const {
  return virial;
}


}
#endif
