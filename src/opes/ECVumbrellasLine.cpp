/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Copyright (c) 2020 of Michele Invernizzi.

The opes module is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The opes module is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with plumed.  If not, see <http://www.gnu.org/licenses/>.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
#include "ExpansionCVs.h"
#include "core/ActionRegister.h"

namespace PLMD {
namespace opes {

//+PLUMEDOC EXPANSION_CV ECV_UMBRELLAS_LINE
/*
Place Gaussian umbrellas on a line.
The umbrellas can be multidimensional, but you should rescale the dimensions so that a single SIGMA can be used.
Can be used with any Colvar as ARG.

\par Examples

us: ECV_UMBRELLAS_LINE ARG=cv MIN_CV=-1 MAX_CV=1 SIMGA=0.1

*/
//+ENDPLUMEDOC

class ECVumbrellasLine :
  public ExpansionCVs
{
private:
  double sigma_;
  unsigned P0_contribution_;
  std::vector<std::vector <double> > center_; //FIXME is this efficient??
  std::vector<std::vector <double> > ECVs_;
  std::vector<std::vector <double> > derECVs_;
  void initECVs();

public:
  explicit ECVumbrellasLine(const ActionOptions&);
  static void registerKeywords(Keywords& keys);
  void calculateECVs(const double *) override;
  const double * getPntrToECVs(unsigned) override;
  const double * getPntrToDerECVs(unsigned) override;
  std::vector< std::vector<unsigned> > getIndex_k() const override;
  std::vector<std::string> getLambdas() const override;
  void initECVs_observ(const std::vector<double>&,const unsigned,const unsigned) override;
  void initECVs_restart(const std::vector<std::string>&) override;
};

PLUMED_REGISTER_ACTION(ECVumbrellasLine,"ECV_UMBRELLAS_LINE")

void ECVumbrellasLine::registerKeywords(Keywords& keys) {
  ExpansionCVs::registerKeywords(keys);
  keys.use("ARG");
  keys.add("compulsory","MIN_CV","the minimum of the CV range to be explored");
  keys.add("compulsory","MAX_CV","the maximum of the CV range to be explored");
  keys.add("compulsory","SIGMA","sigma of the umbrella Gaussians");
  keys.add("compulsory","SPACING","1","the distance between umbrellas, in units of SIGMA");
  keys.addFlag("ADD_P0",false,"add the unbiased Boltzmann distribution to the target distribution, to make sure to sample it");
}

ECVumbrellasLine::ECVumbrellasLine(const ActionOptions&ao):
  Action(ao),
  ExpansionCVs(ao)
{
//set P0_contribution_
  bool add_P0=false;
  parseFlag("ADD_P0",add_P0);
  if(add_P0)
    P0_contribution_=1;
  else
    P0_contribution_=0;
//set umbrellas
  parse("SIGMA",sigma_);
  std::vector<double> min_cv;
  std::vector<double> max_cv;
  parseVector("MIN_CV",min_cv);
  parseVector("MAX_CV",max_cv);
  plumed_massert(min_cv.size()==getNumberOfArguments(),"wrong number of MIN_CVs");
  plumed_massert(max_cv.size()==getNumberOfArguments(),"wrong number of MAX_CVs");
  double spacing;
  parse("SPACING",spacing);
  double length=0;
  for(unsigned j=0; j<getNumberOfArguments(); j++)
    length+=std::pow(max_cv[j]-min_cv[j],2);
  length=std::sqrt(length);
  const unsigned sizeUmbrellas=1+std::round(length/(sigma_*spacing));
  center_.resize(getNumberOfArguments()); //center_[cv][umbrellas]
  for(unsigned j=0; j<getNumberOfArguments(); j++)
  {
    center_[j].resize(sizeUmbrellas+P0_contribution_);
    if(P0_contribution_==1)
      center_[j][0]=std::numeric_limits<double>::quiet_NaN(); // fake center
    for(unsigned k=0; k<sizeUmbrellas; k++)
      center_[j][P0_contribution_+k]=min_cv[j]+k*(max_cv[j]-min_cv[j])/(sizeUmbrellas-1);
  }

  checkRead();

//set ECVs stuff
  totNumECVs_=sizeUmbrellas+P0_contribution_;
  ECVs_.resize(getNumberOfArguments(),std::vector<double>(totNumECVs_));
  derECVs_.resize(getNumberOfArguments(),std::vector<double>(totNumECVs_));

//printing some info
  log.printf("  Total number of umbrellas = %u\n",sizeUmbrellas);
  log.printf("    with SIGMA = %g\n",sigma_);
  log.printf("    and SPACING = %g\n",spacing);
  if(P0_contribution_==1)
    log.printf(" -- ADD_P0: the target includes also the unbiased probability itself\n");
}

void ECVumbrellasLine::calculateECVs(const double * cv) {
  for(unsigned j=0; j<getNumberOfArguments(); j++)
  {
    for(unsigned k=P0_contribution_; k<totNumECVs_; k++) //if ADD_P0, the first ECVs=0
    {
      const double dist_jk=difference(j,center_[j][k],cv[j])/sigma_; //PBC might be present
      ECVs_[j][k]=0.5*std::pow(dist_jk,2);
      derECVs_[j][k]=dist_jk/sigma_;
    }
  }
}

const double * ECVumbrellasLine::getPntrToECVs(unsigned j)
{
  plumed_massert(isReady_,"cannot access ECVs before initialization");
  plumed_massert(j<getNumberOfArguments(),"ECV_UMBRELLAS_LINE has fewer CVs");
  return &ECVs_[j][0];
}

const double * ECVumbrellasLine::getPntrToDerECVs(unsigned j)
{
  plumed_massert(isReady_,"cannot access ECVs before initialization");
  plumed_massert(j<getNumberOfArguments(),"ECV_UMBRELLAS_LINE has fewer CVs");
  return &derECVs_[j][0];
}

std::vector< std::vector<unsigned> > ECVumbrellasLine::getIndex_k() const
{
  std::vector< std::vector<unsigned> > index_k(totNumECVs_,std::vector<unsigned>(getNumberOfArguments()));
  for(unsigned k=0; k<totNumECVs_; k++)
    for(unsigned j=0; j<getNumberOfArguments(); j++)
      index_k[k][j]=k; //this is trivial, since each center has a unique set of CVs
  return index_k;
}

std::vector<std::string> ECVumbrellasLine::getLambdas() const
{
  std::vector<std::string> lambdas(totNumECVs_);
  for(unsigned k=0; k<totNumECVs_; k++)
  {
    std::ostringstream subs;
    subs<<center_[0][k];
    for(unsigned j=1; j<getNumberOfArguments(); j++)
      subs<<"_"<<center_[j][k];
    lambdas[k]=subs.str();
  }
  return lambdas;
}

void ECVumbrellasLine::initECVs()
{
  plumed_massert(!isReady_,"initialization should not be called twice");
  isReady_=true;
  log.printf("  *%4u windows for ECV_UMBRELLAS_LINE\n",totNumECVs_);
}

void ECVumbrellasLine::initECVs_observ(const std::vector<double>& all_obs_cvs,const unsigned ncv,const unsigned index_j)
{
  //this non-linear exansion never uses automatic initialization
  initECVs();
}

void ECVumbrellasLine::initECVs_restart(const std::vector<std::string>& lambdas)
{
  std::size_t pos=0;
  for(unsigned j=0; j<getNumberOfArguments()-1; j++)
    pos = lambdas[0].find("_", pos+1); //checking only lambdas[0] is hopefully enough
  plumed_massert(pos<lambdas[0].length(),"this should not happen, fewer '_' than expected in ECV_UMBRELLAS_LINE");
  pos = lambdas[0].find("_", pos+1);
  plumed_massert(pos>lambdas[0].length(),"this should not happen, more '_' than expected in ECV_UMBRELLAS_LINE");

  std::vector<std::string> myLambdas=getLambdas();
  plumed_massert(myLambdas.size()==lambdas.size(),"RESTART - mismatch in number of ECV_UMBRELLAS_LINE");
  plumed_massert(std::equal(myLambdas.begin(),myLambdas.end(),lambdas.begin()),"RESTART - mismatch in lambda values of ECV_UMBRELLAS_LINE");

  initECVs();
}

}
}
