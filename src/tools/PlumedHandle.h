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
#ifndef __PLUMED_tools_PlumedHandle_h
#define __PLUMED_tools_PlumedHandle_h

#include "core/WithCmd.h"
#include "TypesafePtr.h"
#include <memory>

namespace PLMD
{

class PlumedMain;

/**
Tiny local class to load a PLUMED kernel.

Maps command to either a loaded PLUMED kernel or to the present one.
It provides a wrapper around the official interface located at wrapper/Plumed.h,
with some changes.
Differences are:
- It does not use the `PLUMED_KERNEL` env var. Indeed, it would not make sense to use it,
  since this class is meant to load different kernels.
- It does not implement interoperability with C/FORTRAN interfaces.
- It does not implement global versions (e.g. PLMD::Plumed::gcmd).
- It does not implement PLMD::Plumed::valid. If kernel cannot be loaded, its constructor throws.
  This will make sure that once constructed the object is usable.
- This class is not copyable, but move semantics is enabled.

In its implementation, this class uses the PLMD::Plumed class in wrapper/Plumed.h and thus
has the same capability to load PLUMED kernels of any version (in principle starting with PLUMED 2.0).
It can also be created without passing any kernel path. In that case it refers to the current one
(the one to which this class belongs).

The following syntax creates a handle referring to the current kernel
\verbatim
PlumedHandle p;
// Alternatively:
// auto p=PlumedHandle();
p.cmd("init");
\endverbatim

The following syntax instead creates a handle referring to a loaded kernel
\verbatim
PlumedHandle p(PlumedHandle::dlopen("/path/to/libplumedkernel.so");
// Alternatively:
// auto p=PlumedHandle::dlopen("/path/to/libplumedkernel.so");
p.cmd("init");
\endverbatim

Notice that if there are problems loading the kernel an exception is thrown.
Thus, once constructed the object is guaranteed to be functional.

Also notice that this class overrides all the three different flavors of the
cmd() function, accepting std::string, const char*, or std::string_view as a key.
This is done to avoid extra allocations, considering that this class
is used to perform benchmarks.
*/
class PlumedHandle :
  public WithCmd
{
/// Pointer to PlumedMain.
/// Used when using the current kernel in order to avoid unneeded indirections.
  std::unique_ptr<PlumedMain> local;
/// Custom structure to delete loaded Plumed object.
  struct LoadedDeleter{
    void operator()(void* loaded) const noexcept;
  };
/// Pointer to loaded Plumed object.
/// Destroyed by a LoadedDeleter, which takes care of calling
/// plumed_finalize, which then calls dlclose
  std::unique_ptr<void,LoadedDeleter> loaded;
/// Constructor using the path to a kernel.
/// I keep it private to avoid confusion wrt the
/// similar constructor of PLMD::Plumed that accepts a string (conversion from FORTRAN).
  explicit PlumedHandle(const char* path);
public:
/// Default constructor.
/// Maps following commands to the current kernel.
  PlumedHandle();
/// Construct a PlumedHandle given the path to a kernel.
/// It just uses the private constructor PlumedHandle(const char* path).
  static PlumedHandle dlopen(const char* path);
/// Destructor.
/// This is the default destructor, but we keep it in the cpp file where PlumedMain is included
  virtual ~PlumedHandle() noexcept;
/// Move constructor.
/// This is the default destructor, but we keep it in the cpp file where PlumedMain is included
  PlumedHandle(PlumedHandle &&) noexcept;
/// Move assignment.
/// This is the default move assignment, but we keep it in the cpp file where PlumedMain is included
  PlumedHandle & operator=(PlumedHandle &&) noexcept;
/// Deleted copy constructor.
/// In Plumed.h, the Plumed object can be copied using a reference counter (similar to a shared_ptr).
/// Here we keep things simpler and consider this handle as a movable and not copyable
/// object (similar to a unique_ptr).
  PlumedHandle(const PlumedHandle &) = delete;
/// Deleted copy assignment.
/// In Plumed.h, the Plumed object can be copied usingh a reference counter (similar to a shared_ptr).
/// Here we keep things simpler and consider this handle as a movable and not copyable
/// object (similar to a unique_ptr).
  PlumedHandle & operator=(const PlumedHandle &) = delete;
/// Execute cmd.
  void cmd(std::string_view key,const TypesafePtr & ptr=nullptr) override;
/// Execute cmd.
  void cmd(const char* key,const TypesafePtr & ptr=nullptr) override;
/// Execute cmd.
  void cmd(const std::string & key,const TypesafePtr & ptr=nullptr) override;
/// Bring in the possibility to pass shape/nelem
  using WithCmd::cmd;
};

}
#endif
