/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Copyright (c) 2011-2023 The plumed team
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
#ifndef __PLUMED_core_WithCmd_h
#define __PLUMED_core_WithCmd_h

#include "tools/TypesafePtr.h"
#include <string>
#include <string_view>

namespace PLMD {

/// Base for classes with cmd() method.
/// This is an abstract base class for classes with
/// cmd() method.
class WithCmd {
  bool check_recursion=false;
public:
  /// This is the preferred method as it avoid allocations of temporaries
  virtual void cmd(std::string_view key,const TypesafePtr & val=nullptr) {
    plumed_assert(!check_recursion) << "recursion detected, you forgot to override cmd";
    check_recursion=true;
    // makes sure check_recursion is set to false in case of an exception
    auto deleter=[](auto f) {*f=false; };
    std::unique_ptr<bool,decltype(deleter)> scope_deleter(&check_recursion,deleter);
    cmd(std::string(key),val);
  }
  /// This is the method we used in older plumed versions, so it is still possible.
  virtual void cmd(const std::string& key,const TypesafePtr & val=nullptr) {
    plumed_assert(!check_recursion) << "recursion detected, you forgot to override cmd";
    check_recursion=true;
    // makes sure check_recursion is set to false in case of an exception
    auto deleter=[](auto f) {*f=false; };
    std::unique_ptr<bool,decltype(deleter)> scope_deleter(&check_recursion,deleter);
    cmd(std::string_view(key),val);
  }
  void cmd(const std::string& key,const TypesafePtr & val,const std::size_t* shape) {
    cmd(std::string_view(key),TypesafePtr::setNelemAndShape(val,0,shape));
  }
  void cmd(const std::string& key,const TypesafePtr & val,std::size_t nelem, const std::size_t* shape=nullptr) {
    cmd(std::string_view(key),TypesafePtr::setNelemAndShape(val,nelem,shape));
  }
  void cmd(std::string_view key,const TypesafePtr & val,const std::size_t* shape) {
    cmd(key,TypesafePtr::setNelemAndShape(val,0,shape));
  }
  void cmd(std::string_view key,const TypesafePtr & val,std::size_t nelem, const std::size_t* shape=nullptr) {
    cmd(key,TypesafePtr::setNelemAndShape(val,nelem,shape));
  }
  void cmd(const char* key,const TypesafePtr & val=nullptr) {
    cmd(std::string_view(key),val);
  }
  void cmd(const char* key,const TypesafePtr & val,const std::size_t* shape) {
    cmd(std::string_view(key),TypesafePtr::setNelemAndShape(val,0,shape));
  }
  void cmd(const char* key,const TypesafePtr & val,std::size_t nelem, const std::size_t* shape=nullptr) {
    cmd(std::string_view(key),TypesafePtr::setNelemAndShape(val,nelem,shape));
  }
  virtual ~WithCmd();
};

inline
WithCmd::~WithCmd() {
// do nothing
// here just to allow inheriting from this class properly
}

}

#endif
