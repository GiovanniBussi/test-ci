/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Copyright (c) 2018-2023 The plumed team
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
#include "PlumedHandle.h"
#include "core/PlumedMain.h"
#include "lepton/Exception.h"
#include <cstring>
#ifdef __PLUMED_HAS_DLOPEN
#include <dlfcn.h>
#endif

// Including Plumed.h in this manner allows to create a local
// implementation of the wrapper in an anonymous namespace.
// This allows to avoid recoding all the Plumed.h stuff here
// and at the same time avoids possible conflicts.
#define __PLUMED_WRAPPER_IMPLEMENTATION 1
#define __PLUMED_WRAPPER_EXTERN 0
#define __PLUMED_WRAPPER_CXX_ANONYMOUS_NAMESPACE 1
#define __PLUMED_WRAPPER_CXX_ANONYMOUS_NAMESPACE_PLMD_EXCEPTIONS 1
#include "../wrapper/Plumed.h"

namespace PLMD
{


PlumedHandle::PlumedHandle():
  local(std::make_unique<PlumedMain>())
{
}

PlumedHandle::PlumedHandle(const char* kernel)
#ifdef __PLUMED_HAS_DLOPEN
  :
  loaded(plumed_c2v(plumed_create_dlopen(kernel)))
{
  if(!plumed_valid(plumed_v2c(loaded.get()))) {
    // destructor of loaded will get rid of the corresponding object
    plumed_error() << "You are trying to dynamically load a kernel, but the path " << kernel <<" could not be opened";
  }
}
#else
{
  plumed_error() << "You are trying to dynamically load a kernel, but PLUMED was compiled without dlopen";
}
#endif

PlumedHandle PlumedHandle::dlopen(const char* path) {
  return PlumedHandle(path);
}

PlumedHandle::~PlumedHandle() noexcept = default;

PlumedHandle::PlumedHandle(PlumedHandle && other) noexcept = default;

PlumedHandle & PlumedHandle::operator=(PlumedHandle && other) noexcept = default;

void PlumedHandle::cmd(std::string_view key,const TypesafePtr & ptr) {
  if(local) {
    // there the string_view is directly passed to the PlumedMain object
    local->cmd(key,ptr);
  } else if(loaded) {
    // in this case we have to materialize a string, leading to an extra allocation
    auto key_string=std::string(key);
    // once we have a string, we can use the const char* version
    cmd(key_string.c_str(),ptr);
  } else plumed_error() << "should never arrive here (either one or the other should work)";
}

void PlumedHandle::cmd(const char* key,const TypesafePtr & ptr) {
  if(local) {
    // here the char* will be transformed to string_view for the PlumedMain object
    local->cmd(key,ptr);
  } else if(loaded) {
    // a const char* is already good for the C like interface
    plumed_safeptr safe;
    safe.ptr=ptr.getRaw();
    safe.nelem=ptr.getNelem();
    safe.shape=const_cast<std::size_t*>(ptr.getShape());
    safe.flags=ptr.getFlags();
    safe.opt=nullptr;
    plumed_cmd(plumed_v2c(loaded.get()),key,safe);
  } else plumed_error() << "should never arrive here (either one or the other should work)";
}

void PlumedHandle::cmd(const std::string & key,const TypesafePtr & ptr) {
  if(local) {
    // here the string will be transformed to char* and then to string_view for the PlumedMain object
    local->cmd(key,ptr);
  } else if(loaded) {
    // here we already have a string, so we can forward to the const char* version
    cmd(key.c_str(),ptr);
  } else plumed_error() << "should never arrive here (either one or the other should work)";
}

void PlumedHandle::LoadedDeleter::operator()(void* loaded) const noexcept {
  plumed_finalize(plumed_v2c(loaded));
}

}
