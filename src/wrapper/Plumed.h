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
#ifndef __PLUMED_wrapper_Plumed_h
#define __PLUMED_wrapper_Plumed_h

/*
  This header might be included more than once in order to provide
  the declarations and the definitions. The guard is thus closed before the end of the file
  (match this brace) {
  and a new guard is added for the definitions.
*/

/**
\page ReferencePlumedH Reference for interfacing MD codes with PLUMED

  Plumed.h and Plumed.c contain the external plumed interface, which is used to
  integrate it with MD engines. This interface is very general, and is expected
  not to change across plumed versions. Plumed.c also implements a dummy version
  of the interface, so as to allow a code to be fully linked even if the plumed
  library is not available yet. These files could be directly included in the official
  host MD distribution. In this manner, it will be sufficient to link the plumed
  library at link time (on all systems) or directly at runtime (on systems where
  dynamic loading is enabled) to include plumed features.

  Notice that in PLUMED 2.5 this interface has been rewritten in order to allow
  more debugging features and a better behavior in multithread environments.
  The interface is almost perfectly backward compatible, although it implements
  a few additional functions. See more details below.

  A further improvement has been made in PLUMED 2.7, where the C++ interface has
  been modified to allow dynamic type checking. See more details below.

  Why is Plumed.c written in C and not C++? The reason is that the resulting Plumed.o
  needs to be linked with the host MD code immediately (whereas the rest of plumed
  could be linked a posteriori). Imagine the MD code is written in FORTRAN: when we
  link the Plumed.o file we would like not to need any C++ library linked. In this
  manner, we do not need to know which C++ compiler will be used to compile plumed.
  The C++ library is only linked to the "rest" of plumed, which actually uses it.
  Anyway, Plumed.c is written in such a manner to allow its compilation also in C++
  (C++ is a bit stricter than C). This will
  allow e.g. MD codes written in C++ to just incorporate Plumed.c (maybe renamed into
  Plumed.cpp), without the need of configuring a plain C compiler.

  Plumed interface can be used from C, C++ and FORTRAN. Everything concerning plumed
  is hidden inside a single object type, which is described in C by a structure
  (struct \ref plumed), in C++ by a class (PLMD::Plumed) and in FORTRAN by a
  fixed-length string (CHARACTER(LEN=32)). Obviously C++ can use both struct
  and class interfaces, but the second should be preferred since it will automatically take
  care of objects constructions and destructions. The reference interface
  is the C one, whereas FORTRAN and C++ interfaces are implemented as wrappers
  around it.
  In the C++ interface, all the routines are implemented as methods of PLMD::Plumed.
  In the C and FORTRAN interfaces, all the routines are named plumed_*, to
  avoid potential name clashes. Notice that the entire plumed library
  is implemented in C++, and it is hidden inside the PLMD namespace.

  Handlers to the plumed object can be converted among different representations,
  to allow inter-operability among languages. In C, there are tools to convert
  to/from FORTRAN, whereas in C++ there are tools to convert to/from FORTRAN and C.

  These handlers only contain a pointer to the real structure, so that
  when a plumed object is brought from one language to another,
  it brings a reference to the same environment.

  Moreover, to simplify life in all cases where a single Plumed object is
  required for the entire simulation (which covers many of the practical
  applications with conventional MD codes) it is possible to take advantage
  of a global interface, which is implicitly referring to a unique global instance.
  The global object should still be initialized and finalized properly.
  This global object is obviously not usable in a multithread context. In addition,
  it is difficult to use it in an exception-safe manner, so its usage in C++ is
  allowed but discouraged.

  As of PLUMED 2.5, the interface contains a reference counter that allows
  for a better control of plumed initializations and deallocations.
  This is particularly useful for the C++ interface that now
  behaves similarly to a primitive shared pointer and can be thus copied.
  In other languages, to use the reference counter correctly it is sufficient to
  remember the following rule: for any `plumed_create*` call, there should be a corresponding
  `plumed_finalize` call. More examples can be found below.

  The basic method to send a message to plumed is
\verbatim
  (C) plumed_cmd
  (C++) PLMD::Plumed::cmd
  (FORTRAN)  PLUMED_F_CMD
\endverbatim

  To initialize a plumed object, use:
\verbatim
  (C)        plumed_create
  (C++)      (constructor of PLMD::Plumed)
  (FORTRAN)  PLUMED_F_CREATE
\endverbatim

  As of PLUMED 2.5, you can also initialize a plumed object using the following functions,
  that load a specific kernel. The function plumed_create_dlopen2 allows to specify options
  for dlopen. The C++ version accepts an optional argument to this aim.
\verbatim
  (C)        plumed_create_dlopen or plumed_create_dlopen2
  (C++)      PLMD::Plumed::dlopen
  (FORTRAN)  PLUMED_F_CREATE_DLOPEN
\endverbatim

  To finalize a plumed object, use
\verbatim
  (C)        plumed_finalize
  (C++)      (destructor of PLMD::Plumed)
  (FORTRAN)  PLUMED_F_FINALIZE
\endverbatim

  To access to the global-object, use
\verbatim
  (C)        plumed_gcreate, plumed_gfinalize, plumed_gcmd
  (C++)      PLMD::Plumed::gcreate, PLMD::Plumed::gfinalize, PLMD::Plumed::gcmd
  (FORTRAN)  PLUMED_F_GCREATE, PLUMED_F_GFINALIZE, PLUMED_F_GCMD
\endverbatim

  To check if the global object has been initialized, use
\verbatim
  (C)        plumed_ginitialized
  (C++)      PLMD::Plumed::ginitialized
  (FORTRAN)  PLUMED_F_GINITIALIZED
\endverbatim

  Notice that when using runtime binding the plumed library might be not available.
  In this case, plumed_create (and plumed_gcreate) will still succeed, but a subsequent
  call to plumed_cmd (or plumed_gcmd) would exit. In order to avoid this
  unpleasant situation you have two options.

  First, you can check if plumed library is available before actually creating an object
  using this function:
\verbatim
  (C)        plumed_installed
  (C++)      PLMD::Plumed::installed
  (FORTRAN)  PLUMED_F_INSTALLED
\endverbatim

  Alternatively, as of PLUMED 2.5, you can interrogate the just created plumed
  object using the following function:
\verbatim
  (C)        plumed_valid
  (C++)      PLMD::Plumed::valid
  (FORTRAN)  PLUMED_F_VALID
\endverbatim

  If you want to create on purpose an invalid Plumed object (useful in C++ to postpone
  the loading of the library) you can use `Plumed p(Plumed::makeInvalid());`.

  To know if the global object is valid instead you should use the following function:
\verbatim
  (C)        plumed_gvalid
  (C++)      PLMD::Plumed::gvalid
  (FORTRAN)  PLUMED_F_GVALID
\endverbatim

  To convert handlers between different languages, use
\verbatim
  (C)        plumed_c2f                 (C to FORTRAN)
  (C)        plumed_f2c                 (FORTRAN to C)
  (C++)      Plumed(plumed) constructor (C to C++)
  (C++)      operator plumed() cast     (C++ to C)
  (C++)      Plumed(char*)  constructor (FORTRAN to C++)
  (C++)      toFortran(char*)           (C++ to FORTRAN)
\endverbatim

  As of PLUMED 2.5, when using C or C++ we allow a user to explicitly store a plumed object as
  a void pointer (indeed: that's the only thing contained in a plumed object).
  This might be useful in case you do not want to include the Plumed.h header in some
  of your headers. In order to convert to/from void pointers you can use the following functions
\verbatim
  (C)        plumed_v2c                 (void* to C)
  (C)        plumed_c2v                 (C to void*)
  (C++)      Plumed(void*) constructor  (void* to C++)
  (C++)      toVoid()                   (C++ to void*)
\endverbatim
  Using the functions above is much safer than accessing directly the pointer contained in the \ref plumed struct
  since, when compiling with debug options, it will check if the void pointer actually points to a plumed object.

  As of PLUMED 2.5, we added a reference count. It is in practice possible
  to create multiple `plumed` objects that refer to the same environment.
  This is done using the following functions
\verbatim
  (C)        plumed_create_reference     (from a C object)
  (C)        plumed_create_reference_f   (from a FORTRAN object)
  (C)        plumed_create_reference_v   (from a void pointer)
  (FORTRAN)  plumed_f_create_reference   (from a FORTRAN object)
\endverbatim
  In C++ references are managed automatically by constructors and destructor.
  In addition, you can manually manage them (with care!) using incref() and decref().

  The interface of the FORTRAN functions is very similar to that of the C functions
  and is listed below:

\verbatim
  FORTRAN interface
    SUBROUTINE PLUMED_F_CREATE(p)
      CHARACTER(LEN=32), INTENT(OUT)   :: p
    SUBROUTINE PLUMED_F_CREATE_DLOPEN(p,path)
      CHARACTER(LEN=32), INTENT(OUT)   :: p
      CHARACTER(LEN=*),  INTENT(IN)    :: path
    SUBROUTINE PLUMED_F_CREATE_REFERENCE(p,r)
      CHARACTER(LEN=32), INTENT(OUT)   :: p
      CHARACTER(LEN=32), INTENT(IN)    :: r
    SUBROUTINE PLUMED_F_CREATE_INVALID(p)
      CHARACTER(LEN=32), INTENT(OUT)   :: p
    SUBROUTINE PLUMED_F_CMD(p,key,val)
      CHARACTER(LEN=32), INTENT(IN)    :: p
      CHARACTER(LEN=*),  INTENT(IN)    :: key
      UNSPECIFIED_TYPE,  INTENT(INOUT) :: val(*)
    SUBROUTINE PLUMED_F_FINALIZE(p)
      CHARACTER(LEN=32), INTENT(IN)    :: p
    SUBROUTINE PLUMED_F_INSTALLED(i)
      INTEGER,           INTENT(OUT)   :: i
    SUBROUTINE PLUMED_F_VALID(p,i)
      CHARACTER(LEN=32), INTENT(IN)    :: p
      INTEGER,           INTENT(OUT)   :: i
    SUBROUTINE PLUMED_F_USE_COUNT(p,i)
      CHARACTER(LEN=32), INTENT(IN)    :: p
      INTEGER,           INTENT(OUT)   :: i
    SUBROUTINE PLUMED_F_GLOBAL(p)
      CHARACTER(LEN=32), INTENT(OUT)   :: p
    SUBROUTINE PLUMED_F_GINITIALIZED(i)
      INTEGER,           INTENT(OUT)   :: i
    SUBROUTINE PLUMED_F_GCREATE()
    SUBROUTINE PLUMED_F_GCMD(key,val)
      CHARACTER(LEN=*), INTENT(IN)     :: key
      UNSPECIFIED_TYPE, INTENT(INOUT)  :: val(*)
    SUBROUTINE PLUMED_F_GFINALIZE()
    SUBROUTINE PLUMED_F_GVALID(i)
      INTEGER,           INTENT(OUT)   :: i
\endverbatim

  Almost all C functions have a corresponding FORTRAN function.
  As a simple mnemonic, if you know the name of the C function you can obtain the
  corresponding FORTRAN subroutine by adding `F_` after the `PLUMED_` prefix.
  In addition, all `plumed` objects are replaced by `CHARACTER(LEN=32)` objects
  holding the same information. These pointers basically contain a text representation
  of the stored pointer, that is suitable to be contained in a string.
  Finally, whenever a C function returns a value,
  the corresponding FORTRAN subroutine will have an additional `INTENT(OUT)` parameter
  passed as the its last argument.

  When you compile the FORTRAN interface, wrapper functions are added with several possible
  name manglings, so you should not experience problems linking the plumed library with a FORTRAN file.

\section ReferencePlumedH-exceptions Error handling

  In case an error is detected by PLUMED, either because of some user error, some internal bug,
  or some mistake in using the library, an exception will be thrown. The behavior is different depending if you use
  PLUMED from C/FORTRAN or from C++.

  First of all, notice that access to PLUMED goes through three functions:
  - plumed_create: this, as of PLUMED 2.5, is guaranteed not to throw any exception. If there is a problem, it will
    just return a plumed object containing a NULL pointer
  - plumed_cmd: this function might throw exceptions.
  - plumed_finalize: this is a destructor and is guaranteed not to throw any exception.

  The following discussion concerns all the exceptions thrown by plumed_cmd.

  If you use C/FORTRAN, you will basically have no way to intercept the exception and the program will just terminate.

  If you use C++ but you are calling the C interface (e.g. \ref plumed_cmd), then you might be
  able to catch the exceptions thrown by PLUMED. Notice that all the exceptions thrown by PLUMED inherit from std::exception,
  so you might want to catch it by reference. Notice however that there is a C layer between your C++ code and the PLUMED
  library. In principle, the stack unwinding performed during exception handling is undefined in C and might lead to problems
  that are system and compiler dependent. In addition to this, there might be troubles when combining different compilers
  or different standard libraries. E.g., if you MD code is linked against a given C++ library and PLUMED is linked against
  another one, the two std::exception types will differ and you won't be able to catch exceptions raised by PLUMED.

  If you use C++ and you are calling the C++ interface (e.g. \ref Plumed::cmd), as of PLUMED 2.5 we implemented a complete
  remapping of the exceptions thrown by PLUMED.  This solves both the problems mentioned above. In particular:
  - Instead of throwing an exception, PLUMED will return (using a \ref plumed_nothrow_handler) the details about the occurred error.
  - An equivalent exception will be thrown within the inline PLUMED interface compiled with your MD code.

  As a consequence, you will be able to combine different compilers and avoid stack unwinding in the C layer.

  Notice that, even if you use \ref Plumed::cmd, if you are loading a kernel <=2.4 any exception generated by PLUMED will
  leak through the C layer. This might lead to undefined behavior. If you are lucky (with some compiler it works!) and
  the exception arrives to C, PLUMED will catch it and rethrow it as it would do if you were using a kernel >=2.5.

  The remapping of exceptions takes care of all the standard C++ exceptions plus all the exceptions raised within
  PLUMED. Unexpected exceptions that are derived from std::exception will be rethrown as std::exception.
  Notice that this implies some loss of information, since the original exception might have been of a different type.
  However, it also implies that the virtual table of the original exception won't be needed anymore. This allows to
  completely decouple the MD code from the PLUMED library.

\section ReferencePlumedH-typesafe Typesafe interface

  Starting with PLUMED 2.8, the `cmd` function of the C++ interface, and the similar function `gcmd`, can be called
  with several interfaces and can perform a typechecking on the passed argument. In particular, the following
  forms are now possible:
\verbatim
  cmd("string",value);        // by value
  cmd("string",&value);       // by pointer
  cmd("string",&value,nelem); // by pointer, specifying the number of elements of the passed array
  cmd("string",&value,shape); // by pointer, specifying the shape of the passed array
\endverbati
  The `nelem` and `shape` arguments are used by PLUMED to check that the user
  provided enough elements. If nelem is provided, the check is done on the flatten array, whereas if shape
  is passed a more thorough check is performed controlling each of the dimensions of the array.
  In addition to this, the type of the pointer (or of the value) is checked at runtime.

  All these checks are only implemented if the PLUMED library is recent (>=2.8). However, it will still be
  possible to load at runtime an older PLUMED library (<=2.7). For this reason, it is still compulsory
  to pass the correct types to the `cmd` function, also when the argument is passed by value.
  Type conversions are only performed between pointers and only in ways compatible with
  what is allowed in C++ (e.g., `const void*` cannot be converted to `void*`, but `void*` can
  be converted to `const void*`).

  Type checkes can be disabled in two ways:
  - By compiling `Plumed.h` with `-D__PLUMED_WRAPPER_CXX_TYPESAFE=0`
  - By setting `export PLUMED_TYPESAFE_IGNORE=yes` at runtime.

\section ReferencePlumedH-2-5 New in PLUMED 2.5

  The wrappers in PLUMED 2.5 have been completely rewritten with several improvements.
  The interface is almost perfectly backward compatible, although the behavior of C++ constructors
  has been modified slightly.
  In addition, a few new functions are introduced (explicitly marked in the documentation).
  As a consequence, if your code uses some of the new functions, you will not be able
  to link it directly with an older PLUMED library (though you will still be able to load
  an older PLUMED library at runtime). In addition, the reference counter changes slightly
  the behavior of the C++ methods used to interoperate with C and FORTRAN.

  An important novelty is in the way the runtime loader is implemented.
  In particular, the loader works also if the symbols of the main executable are not exported.
  The proper functions from the kernel are indeed searched explicitly now using `dlsym`.

  Some additional features can be enabled using suitable environment variables. In particular:
  - `PLUMED_LOAD_DEBUG` can be set to report more information about the loading process.
  - `PLUMED_LOAD_NAMESPACE` can be set to `LOCAL` to load the PLUMED kernel in a separate
    namespace. The default is global namespace, which is the same behavior of PLUMED <=2.4,
    and is consistent with what happens when linking PLUMED as a shared library.
  - `PLUMED_LOAD_NODEEPBIND` can be set to load the PLUMED kernel in not-deepbind mode. Deepbind
    mode implies that the symbols defined in the library are preferred to other symbols with the same name.
    Only works on systems supporting `RTLD_DEEPBIND` and is mostly for debugging purposes.

  Another difference is that the implementation of the wrappers is now completely contained in the `Plumed.h`
  file. You can see that the `Plumed.c` is much simpler now and just includes `Plumed.h`. With a similar
  procedure you could compile the wrappers directly into your code making it unnecessary to link
  the libplumedWrapper.a library. The corresponding macros are still subject to change and are not documented here.

  As written above, the plumed object now implements a reference counter.  Consider the following example
\verbatim
  plumed p=plumed_create();
  plumed_cmd(p,"init",NULL);
  plumed q=plumed_create_reference(p);
  plumed_finalize(p);
// at this stage, object q still exists
  plumed_cmd(q,"whatever",NULL);
  plumed_finalize(q);
// now plumed has been really finalized
\endverbatim

  In other words, every \ref plumed_create, \ref plumed_create_dlopen, \ref plumed_create_reference,
  \ref plumed_create_reference_f, and \ref plumed_create_reference_v call must be matched by a \ref plumed_finalize.
  Notice that in C++ whenever an object goes out of scope the reference counter
  will be decreased. In addition, consider that conversion from C/FORTRAN/void* to C++ implies calling a C++ constructor, that
  is increases the number of references by one. Converting from C++ to C/FORTRAN/void* instead does not call any constructor,
  that is the number of references is unchanged.

  The change in the behavior of C++ constructors means that the following code will behave in a backward incompatible manner:
\verbatim
  plumed p=plumed_create();
  plumed_cmd(p,"init",NULL);
  Plumed q(p);
  plumed_finalize(p);
// at this stage, object q still exists with PLUMED 2.5
// on the other hand, with PLUMED 2.4 object q refers to an
// already finalized object
  q.cmd("whatever",NULL);
\endverbatim

  Another difference is that the value of the variable `PLUMED_KERNEL` is read every time a new
  plumed object is instantiated. So, you might even use it to load different plumed versions
  simultaneously, although the preferred way to do this is using the function \ref plumed_create_dlopen.
  Notice that if you want to load multiple versions simultaneously you should load them in a local namespace.
  \ref plumed_create_dlopen does it automatically, whereas loading through env var `PLUMED_KERNEL` only does it if
  you also set env var `PLUMED_NAMESPACE=LOCAL`.

  Finally, a few functions have been added, namely:
  - Functions to find if a plumed object is valid
    (\ref plumed_valid(), \ref plumed_gvalid(), \ref PLMD::Plumed::valid(), and \ref PLMD::Plumed::gvalid()).
  - Functions to create a plumed object based on the path of a specific kernel
    (\ref plumed_create_dlopen() and \ref PLMD::Plumed::dlopen()).
  - Functions to create a plumed object referencing to another one, implementing a reference counter
    (\ref plumed_create_reference(), \ref plumed_create_reference_v(), \ref plumed_create_reference_f().

*/

/* BEGINNING OF DECLARATIONS */

/* SETTING DEFAULT VALUES FOR CONTROL MACROS */

/*
  1: make the C wrapper functions extern (default)
  0: make the C wrapper functions static (C) or inline (C++)

  If set to zero, it disables all functions that only make sense as extern, such as
  Fortran wrappers, global objects, and plumed_kernel_register.

  It can be set to zero to include multiple copies of the wrapper implementation without worrying
  about duplicated symbols.

  Notice that C++ wrappers are always inline. What this function controls is if the C wrappers
  (called by the C++ wrappers) is inline or not. Also consider that if this header is compiled
  with C++ and inline C wrappers, the C wrappers will be actually compiled with C++ linkage
  in the root namespace.

  Used both in declarations (to know which functions to declare) and definitions (to know which functions to define).
*/

#ifndef __PLUMED_WRAPPER_EXTERN
#define __PLUMED_WRAPPER_EXTERN 1
#endif

/*
  1: emit global plumed object and related functions (default)
  0: do not emit global plumed object and related functions

  Used both in declarations (to know which functions to declare) and definitions (to know which functions to define).
*/

#ifndef __PLUMED_WRAPPER_GLOBAL
#define __PLUMED_WRAPPER_GLOBAL 1
#endif

/*
  1: enable C++ wrapper (default)
  0: disable C++ wrapper

  Only used in declarations, but affects the scope of the C interface also in definitions.
*/

#ifndef __PLUMED_WRAPPER_CXX
#define __PLUMED_WRAPPER_CXX 1
#endif

/*
  1: new headers such as cstdlib are included in C++ (default)
  0: old headers such as stdlib.h are included in C++

  Should only be set to zero when including the Plumed.h file in a file using the
  old (stdlib.h) convention.

  Used both in declarations and definitions.
*/

#ifndef __PLUMED_WRAPPER_CXX_STD
#define __PLUMED_WRAPPER_CXX_STD 1
#endif

/*
  1: place C++ wrappers in an anonymous namespace
  0: place C++ wrappers in the PLMD namespace (default)

  It will make PLMD::Plumed a different class (though with the same name)
  in each of the translation units in which `Plumed.h` is included.

  Can be used to completey separate C++ implementations. However, it will make
  it impossible to transfer Plumed objects between different translation units
  without converting to a void* or plumed object.

  Only used in declarations, but affects the scope of the C interface also in definitions.
*/

#ifndef __PLUMED_WRAPPER_CXX_ANONYMOUS_NAMESPACE
#define __PLUMED_WRAPPER_CXX_ANONYMOUS_NAMESPACE 0
#endif

/*
  1: make PLMD::Plumed class polymorphic (default)
  0: make PLMD::Plumed class non-polymorphic

  Only used in declarations.
*/

#ifndef __PLUMED_WRAPPER_CXX_POLYMORPHIC
#define __PLUMED_WRAPPER_CXX_POLYMORPHIC 1
#endif

/*
  1: Enable typesafe interface (default)
  0: Disable typesafe interface

  Only used in declarations.
*/

#ifndef __PLUMED_WRAPPER_CXX_TYPESAFE
#define __PLUMED_WRAPPER_CXX_TYPESAFE 1
#endif

/*
  1: Define macros plumed_cmd and plumed_gcmd to use the C++ interface.
  0: Don't define macros plumed_cmd and plumed_gcmd (temporarily, this is the default).

  Only used in declarations.
*/

#ifndef __PLUMED_WRAPPER_CXX_BIND_C
#define __PLUMED_WRAPPER_CXX_BIND_C 0
#endif

/*
  1: Enable passing long long int
  0: Disable passing long long int

  Only used in declarations. Default is 0 in C++<11 and 1 in C++>=11
*/

#ifndef __PLUMED_WRAPPER_CXX_LONGLONG
#if __cplusplus > 199711L
#define __PLUMED_WRAPPER_CXX_LONGLONG 1
#else
#define __PLUMED_WRAPPER_CXX_LONGLONG 0
#endif
#endif

/*
  1: make the default constructor create an invalid object
  0: make the default constructor create a valid object

  Only for internal usage.
*/
#ifndef __PLUMED_WRAPPER_CXX_DEFAULT_INVALID
#define __PLUMED_WRAPPER_CXX_DEFAULT_INVALID 0
#endif

/*
  Size of a buffer used to store message for exceptions with noexcept constructor.
  Should typically hold short messages. Anyway, as long as the stack size stays within the correct
  limits it does not seem to affect efficiency. Notice that there cannot be recursive calls of
  PLMD::Plumed::cmd, so that it should be in practice irrelevant.
*/
#ifndef __PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER
#define __PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER 512
#endif

/*
 By default, assume C++11 compliant library is not available.
*/

#ifndef __PLUMED_WRAPPER_LIBCXX11
#define __PLUMED_WRAPPER_LIBCXX11 0
#endif

/* The following macros are just to define shortcuts */

/* Simplify addition of extern "C" blocks.  */
#ifdef __cplusplus
#define __PLUMED_WRAPPER_EXTERN_C_BEGIN extern "C" {
#define __PLUMED_WRAPPER_EXTERN_C_END }
#else
#define __PLUMED_WRAPPER_EXTERN_C_BEGIN
#define __PLUMED_WRAPPER_EXTERN_C_END
#endif

/* Without C++, stdlib functions should not be prepended with ::std:: */
#ifndef __cplusplus
#undef __PLUMED_WRAPPER_CXX_STD
#define __PLUMED_WRAPPER_CXX_STD 0
#endif

/* Set prefix for stdlib functions */
#if __PLUMED_WRAPPER_CXX_STD
#define __PLUMED_WRAPPER_STD ::std::
#else
#define __PLUMED_WRAPPER_STD
#endif

/* Allow using noexcept, explicit, and override with C++11 compilers */
#if __cplusplus > 199711L
#define __PLUMED_WRAPPER_CXX_NOEXCEPT noexcept
#define __PLUMED_WRAPPER_CXX_EXPLICIT explicit
#define __PLUMED_WRAPPER_CXX_OVERRIDE override
#else
#define __PLUMED_WRAPPER_CXX_NOEXCEPT throw()
#define __PLUMED_WRAPPER_CXX_EXPLICIT
#define __PLUMED_WRAPPER_CXX_OVERRIDE
#endif

/* Macros for anonymous namespace */
#if __PLUMED_WRAPPER_CXX_ANONYMOUS_NAMESPACE && defined(__cplusplus) /*{*/
#define __PLUMED_WRAPPER_ANONYMOUS_BEGIN namespace {
#define __PLUMED_WRAPPER_ANONYMOUS_END }
#else
#define __PLUMED_WRAPPER_ANONYMOUS_BEGIN
#define __PLUMED_WRAPPER_ANONYMOUS_END
#endif /*}*/

#if __PLUMED_WRAPPER_EXTERN /*{*/

#define __PLUMED_WRAPPER_C_BEGIN __PLUMED_WRAPPER_EXTERN_C_BEGIN extern
#define __PLUMED_WRAPPER_C_END __PLUMED_WRAPPER_EXTERN_C_END
#define __PLUMED_WRAPPER_INTERNALS_BEGIN __PLUMED_WRAPPER_EXTERN_C_BEGIN static
#define __PLUMED_WRAPPER_INTERNALS_END __PLUMED_WRAPPER_EXTERN_C_END

#else

#ifdef __cplusplus
#define __PLUMED_WRAPPER_C_BEGIN  __PLUMED_WRAPPER_ANONYMOUS_BEGIN inline
#define __PLUMED_WRAPPER_C_END __PLUMED_WRAPPER_ANONYMOUS_END
#else
#define __PLUMED_WRAPPER_C_BEGIN static
#define __PLUMED_WRAPPER_C_END
#endif

#define __PLUMED_WRAPPER_INTERNALS_BEGIN __PLUMED_WRAPPER_C_BEGIN
#define __PLUMED_WRAPPER_INTERNALS_END __PLUMED_WRAPPER_C_END

/* with an not-external interface, it does not make sense to define global functions */
#undef __PLUMED_WRAPPER_GLOBAL
#define __PLUMED_WRAPPER_GLOBAL 0

#endif /*}*/

#if __PLUMED_WRAPPER_CXX_STD
#include <cstddef> /* size_t */
#include <cstring> /* memcpy */
#else
#include <stddef.h>
#include <string.h>
#endif

/**
  \brief Main plumed object

  This is an object containing a Plumed instance, which should be used in
  the MD engine. It should first be initialized with plumed_create(),
  then it communicates with the MD engine using plumed_cmd(). Finally,
  before the termination, it should be deallocated with plumed_finalize().
  Its interface is very simple and general, and is expected
  not to change across plumed versions. See \ref ReferencePlumedH.
*/
typedef struct {
  /**
    \private
    \brief Void pointer holding the real PlumedMain structure

    To maintain binary compatibility, we should not add members to this structure.
    As of PLUMED 2.5, in order to add new components we do not store the pointer
    to \ref PlumedMain here but rather a pointer to an intermediate private structure
    that contains all the details.
  */
  void*p;
} plumed;

typedef struct {
  void* ptr;
  void (*handler)(void*,int,const char*,const void*);
} plumed_nothrow_handler;

/** \relates plumed
    \brief Structure holding a typesafe pointer.
*/

typedef struct {
  /** Pointer to data */
  const void* ptr;
  /** Number of elements (in case pointing to an array) */
  __PLUMED_WRAPPER_STD size_t nelem;
  /** Shape (scanned up to a zero value is found) */
  __PLUMED_WRAPPER_STD size_t* shape;
  /**
    sum of:
    sizeof(pointed data), up to 0x10000 (65536). 0 means size not checked
    0x10000    * data type, up to 0xff (255)
               0 not typechecked
               1 void
               2 nullptr
               3 integral
               4 floating point
               5 FILE (size will not be computed as it might be incomplete)
               >5 not typechecked, reserved for future extensions
    0x1000000  * 1 for unsigned (ignored)
    0x2000000  * pointer/const type, up to 8
               0 not typechecked
               1 T
               2 T       *
               3 T const *
               4 T       *       *
               5 T       * const *
               6 T const *       *
               7 T const * const *
    0x10000000 and higher bits are ignored, reserved for future extensions
  */
  unsigned long int flags;
  /** Optional information, not used yet  */
  void* opt;
} plumed_safeptr;

/** \relates plumed
    \brief Constructor

    Constructs a plumed object.

    Notice that if you are linking against libplumedWrapper.a, if you are
    using a code patched in runtime mode, or if you are including the `Plumed.c`
    file directly in your code, this constructor might return an invalid plumed
    object. In particular, this could happen if the `PLUMED_KERNEL` environment
    variable is not set or set incorrectly. In order to detect an incorrect
    plumed object you might use \ref plumed_valid() on the resulting object.
    Alternatively, if you use \ref plumed_cmd() on an invalid plumed object the code will exit.
    Also notice that to avoid memory leaks you should call \ref plumed_finalize()
    to finalize a plumed object even if it is invalid:
\verbatim
  plumed p=plumed_create();
  if(!plumed_valid(p)) {
// this will happen if the PLUMED_KERNEL variable is not set correctly
    plumed_finalize(p);
    return whatever;
  }
\endverbatim

    \return The constructed plumed object
*/
__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create(void);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Constructor from path. Available as of PLUMED 2.5

    It tries to construct a plumed object loading the kernel located at path.
    Notice that it could leave the resulting object in an invalid state.
    In order to detect an invalid
    plumed object you might use \ref plumed_valid() on the resulting object.
    Alternatively, if you use \ref plumed_cmd() on an invalid plumed object the code will exit.

    Also notice that to avoid memory leaks you should call \ref plumed_finalize()
    to finalize a plumed object even if it is invalid.
\verbatim
  plumed p=plumed_create(path);
  if(!plumed_valid(p)) {
// this will happen if the path argument is not set correctly
    plumed_finalize(p);
    return whatever;
  }
\endverbatim

    \return The constructed plumed object
*/
__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_dlopen(const char*path);
__PLUMED_WRAPPER_C_END


/**
  \brief Constructor from path. Available as of PLUMED 2.5

  Same as \ref plumed_create_dlopen, but also allows to specify the mode for dlopen.

  \warning
  Use with care, since not all the possible modes work correctly with PLUMED.
*/
__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_dlopen2(const char*path,int mode);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    Create a new reference to an existing object, increasing its reference count. Available as of PLUMED 2.5

    Use it to increase by one the reference count of a plumed object.
    The resulting pointer might be identical to the one passed as an
    argument, but the reference count will be incremented by one.
    Notice that you should finalize the resulting object.
\verbatim
  plumed p1;
  plumed p2;
  p1=plumed_create();
  p2=plumed_create_reference(p1);
  plumed_finalize(p1);
// now you can still use p2
  plumed_cmd(p2,"init",NULL);
  plumed_finalize(p2);
// now the underlying object is destroyed.
\endverbatim

    If the `p` object is invalid, also the returned object will be invalid.

    \param p The plumed object that will be referenced to.
    \return The constructed plumed object
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference(plumed p);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Create a new reference to an existing object passed as a void pointer, increasing its reference count. Available as of PLUMED 2.5

  \return The constructed plumed object
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference_v(void*v);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Create a new reference to an existing object passed as a fortran string, increasing its reference count. Available as of PLUMED 2.5

  \return The constructed plumed object
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference_f(const char*f);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Constructor as invalid. Available as of PLUMED 2.5

   Can be used to create an object in the same state as if it was returned by
   plumed_create_dlopen with an incorrect path (or plumed_create using runtime binding
   and an incorrect PLUMED_KERNEL).

   Can be used to initialize a plumed object to a well-defined state without explicitly
   creating it. The resulting object can be checked later with \ref plumed_valid.
   Consider the following example
\verbatim
    plumed p;
    p=plumed_create_invalid();
// at this point p is initialized to a well-defined (invalid) state.
    setenv("PLUMED_KERNEL","/path/to/kernel/libplumedKernel.so",1);
    plumed_finalize(p);
    p=plumed_create();
\endverbatim

    \return The constructed plumed object
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_invalid();
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Tells p to execute a command.

    If the object is not valid (see \ref plumed_valid), this command will exit.

    \param p The plumed object on which command is acting
    \param key The name of the command to be executed
    \param val The argument. It is declared as const to allow calls like plumed_cmd(p,"A","B"),
               but for some choice of key it can change the content.

    Notice that within PLUMED we use a const_cast to remove any const qualifier from the second
    argument of \ref plumed_cmd.

    In some cases val can be omitted: just pass a NULL pointer (in C++, val is optional and can be omitted,
    or you can equivalently pass NULL or nullptr).
    The set of possible keys is the real API of the plumed library, and will be expanded with time.
    New commands will be added, but backward compatibility will be retained as long as possible.
*/

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd(plumed p,const char*key,const void*val);
__PLUMED_WRAPPER_C_END

/**
  \relates plumed
  \brief Same as \ref plumed_cmd, but does not throw exceptions.

  This function is meant to be used when errors should be handled explicitly.
  if an exception is raised within PLUMED, the function nothrow.handler() will
  be called with arguments (nothrow.ptr,code,message,opt). This allows the C++ interface
  to correctly rethrow exceptions, but might be used from C as well. opt can be used
  to pass further information (not used yet).
*/

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_nothrow(plumed p,const char*key,const void*val,plumed_nothrow_handler nothrow);
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_safe_nothrow(plumed p,const char*key,plumed_safeptr,plumed_nothrow_handler nothrow);
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_safe(plumed p,const char*key,plumed_safeptr);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Destructor.

    It must be used for any object created using \ref plumed_create(),
    even if the created object is not valid.

    \param p The plumed object to be deallocated
*/

__PLUMED_WRAPPER_C_BEGIN
void plumed_finalize(plumed p);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Check if plumed is installed (for runtime binding).

    Notice that this is equivalent to creating a dummy object and checking if it is valid.

\verbatim
  // this:
  //int a=plumed_installed();
  // is equivalent to this:

  plumed p=plumed_create();
  int a=plumed_valid(p);
  plumed_finalize(p);

\endverbatim

    This function is mostly provided for compatibility with PLUMED 2.4, where \ref plumed_valid()
    was not available. Using \ref plumed_valid() is now preferred since it creates a single object
    instead of creating a dummy object that is then discarded.

    \return 1 if plumed is installed, 0 otherwise
*/

__PLUMED_WRAPPER_C_BEGIN
int plumed_installed(void);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Check if plumed object is valid. Available as of PLUMED 2.5

    It might return false if plumed is not available at runtime.

    \return 1 if plumed is valid, 0 otherwise
*/

__PLUMED_WRAPPER_C_BEGIN
int plumed_valid(plumed p);
__PLUMED_WRAPPER_C_END

/** \relates plumed
    \brief Returns the number of references to the underlying object. Available as of PLUMED 2.5.
*/

__PLUMED_WRAPPER_C_BEGIN
int plumed_use_count(plumed p);
__PLUMED_WRAPPER_C_END


/* routines to convert char handler from/to plumed objects */

/** \related plumed
    \brief Converts a C handler to a FORTRAN handler

    \param p The C handler
    \param c The FORTRAN handler (a char[32])

    This function can be used to convert a plumed object created in C to
    a plumed handler that can be used in FORTRAN. Notice that the reference counter
    is not incremented. In other words, the FORTRAN object will be a weak reference.
    If you later finalize the C handler, the FORTRAN handler will be invalid.
\verbatim
#include <plumed/wrapper/Plumed.h>
int main(int argc,char*argv[]){
  plumed p;
  p=plumed_create();
  char fortran_handler[32];
  plumed_c2f(p,fortran_handler);
  printf("DEBUG: this is a string representation for the plumed handler: %s\n",fortran_handler);
  fortran_routine(fortran_handler);
  plumed_finalize(p);
  return 0;
}
\endverbatim
  Here `fortran_routine` is a routine implemented in FORTRAN that manipulates the
  fortran_handler.
*/

__PLUMED_WRAPPER_C_BEGIN
void   plumed_c2f(plumed p,char* c);
__PLUMED_WRAPPER_C_END

/** \related plumed
    \brief Converts a FORTRAN handler to a C handler
    \param c The FORTRAN handler (a char[32])
    \return The C handler

    This function can be used to convert a plumed object created in FORTRAN
    to a plumed handler that can be used in C.  Notice that the reference counter
    is not incremented. In other words, the C object will be a weak reference.
    If you later finalize the FORTRAN handler, the C handler will be invalid.
\verbatim
void c_routine(char handler[32]){
  plumed p;
  p=plumed_f2c(handler);
  plumed_cmd(p,"init",NULL);
}
\endverbatim
  Here `c_routine` is a C function that can be called from FORTRAN
  and interact with the provided plumed handler.
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_f2c(const char* c);
__PLUMED_WRAPPER_C_END

/** \related plumed
    \brief Converts a plumed object to a void pointer. Available as of PLUMED 2.5.

    It returns a void pointer that can be converted back to a plumed object using \ref plumed_v2c.
    When compiling without NDEBUG, it checks if the plumed object was properly created.
    Notice that an invalid object (see \ref plumed_valid) can be converted to void* and back.

    Can be used to store a reference to a plumed object without including the Plumed.h header.
*/

__PLUMED_WRAPPER_C_BEGIN
void* plumed_c2v(plumed p);
__PLUMED_WRAPPER_C_END


/** \related plumed
    \brief Converts a void pointer to a plumed object. Available as of PLUMED 2.5.

    It returns a plumed object from a void pointer obtained with \ref plumed_c2v.
    When compiling without NDEBUG, it checks if the plumed object was properly created.

    Can be used to store a reference to a plumed object without including the Plumed.h header.
*/

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_v2c(void*);
__PLUMED_WRAPPER_C_END


#if __PLUMED_WRAPPER_GLOBAL /*{*/

/* Global C functions are always extern */
__PLUMED_WRAPPER_EXTERN_C_BEGIN /*{*/

/** \relates plumed
    \brief Retrieves an handler to the global structure.

  You can use this if you work on a code that uses the global structure and you want to
  pass to a generic routine an handler to the same structure. E.g.

\verbatim
  plumed p=plumed_global();
  some_routine(p);
\endverbatim
*/
extern
plumed plumed_global(void);

/** \relates plumed
    \brief Check if the global interface has been initialized.

    \return 1 if plumed has been initialized, 0 otherwise
*/
extern
int plumed_ginitialized(void);

/** \relates plumed
    \brief Constructor for the global interface.

    \note Equivalent to plumed_create(), but initialize the static global plumed object
*/
extern
void plumed_gcreate(void);

/** \relates plumed
    \brief Tells to the global interface to execute a command.

    \param key The name of the command to be executed
    \param val The argument. It is declared as const to allow calls like plumed_gcmd("A","B"),
               but for some choice of key it can change the content

    `plumed_gcmd(a,b);` is equivalent to `plumed_cmd(plumed_global(),a,b);`.
*/
extern
void plumed_gcmd(const char* key,const void* val);

/** \relates plumed
    \brief Tells to the global interface to execute a command.

    \param key The name of the command to be executed
    \param safe A safe pointer

    `plumed_gcmd_safe(a,b);` is equivalent to `plumed_cmd_safe(plumed_global(),a,b);`.
*/
extern
void plumed_gcmd_safe(const char* key,plumed_safeptr);

/** \relates plumed
    \brief Destructor for the global interface.

    `plumed_gfinalize(a,b);` is similar to `plumed_finalize(plumed_global(),a,b);`, but not completely
    equivalent. In particular, plumed_gfinalize() also makes sure that the global object
    is reset to its initial status. After calling it, \ref plumed_ginitialized() will thus return 0.
*/
extern
void plumed_gfinalize(void);

/** \relates plumed
    \brief Check if global plumed object is valid. Available as of PLUMED 2.5

    It might return zero if plumed is not available at runtime.

    \return 1 if plumed is valid, 0 otherwise.
*/
extern
int plumed_gvalid();

__PLUMED_WRAPPER_EXTERN_C_END /*}*/

#endif /*}*/

#if defined( __cplusplus) && __PLUMED_WRAPPER_CXX /*{*/

#if __PLUMED_WRAPPER_CXX_STD
#include <cstdlib> /* NULL getenv */
#include <cstddef> /* nullptr_t */
#include <cstring> /* strncat strlen */
#include <cstdio> /* fprintf */
#include <cassert> /* assert */
#else
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#endif

#include <exception> /* exception bad_exception */
#include <stdexcept> /* runtime_error logic_error invalid_argument domain_error length_error out_of_range range_error overflow_error underflow_error */
#include <string> /* string */
#include <ios> /* iostream_category (C++11) ios_base::failure (C++11 and C++<11) */
#include <new> /* bad_alloc bad_array_new_length (C++11) */
#include <typeinfo> /* bad_typeid bad_cast */
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
#include <system_error> /* system_error generic_category system_category */
#include <future> /* future_category */
#include <memory> /* bad_weak_ptr */
#include <functional> /* bad_function_call */
#endif

/* C++ interface is hidden in PLMD namespace (same as plumed library) */
namespace PLMD {

/* Optionally, it is further hidden in an anonymous namespace */

__PLUMED_WRAPPER_ANONYMOUS_BEGIN /*{*/

/**
  Retrieve PLUMED_EXCEPTIONS_DEBUG (internal utility).

  This function should not be used by external programs. It is defined
  as inline static so that it can store a static variable (for quicker access)
  without adding a unique global symbol to a library including this header file.
*/
inline static bool PlumedGetenvExceptionsDebug() __PLUMED_WRAPPER_CXX_NOEXCEPT {
  static const char* res=__PLUMED_WRAPPER_STD getenv("PLUMED_EXCEPTIONS_DEBUG");
  return res;
}

/**
  Retrieve PLUMED_TYPESAFE_DEBUG (internal utility).

  This function should not be used by external programs. It is defined
  as inline static so that it can store a static variable (for quicker access)
  without adding a unique global symbol to a library including this header file.
*/
inline static bool PlumedGetenvTypesafeDebug() __PLUMED_WRAPPER_CXX_NOEXCEPT {
  static const char* res=__PLUMED_WRAPPER_STD getenv("PLUMED_TYPESAFE_DEBUG");
  return res;
}

/**
  C++ wrapper for \ref plumed.

  This class provides a C++ interface to PLUMED.
  It only containts a \ref plumed object, but wraps it with a number of useful methods.
  All methods are inlined so as to avoid the compilation of an extra c++ file.

*/

class Plumed {
  /**
    C structure.
  */
  plumed main;

  /**
    Error handler used to rethrow exceptions.
  */

  struct NothrowHandler {
    /** code used for translating messages */
    int code;
    /** short message buffer for non-throwing exceptions */
    char exception_buffer[__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER];
    /** if exception_buffer='\0', message stored as an allocatable string */
    ::std::string what;
    /** error code for system_error */
    int error_code;
  };

  /**
    Callback function that sets the error handler.

    opt argument is interpreted as the pointer to a null terminated array of void*.
    The number of non-null element is expected to be even, and there should be a null element
    that follows. Every pair of pointers should point
    to a char, identifying the type of argument passed, and an arbitrary object.
    Currently used to (optionally) pass error_code.
  */
  static void nothrow_handler(void*ptr,int code,const char*what,const void* opt) {
    NothrowHandler* h=(NothrowHandler*) ptr;
    h->code=code;
    h->exception_buffer[0]='\0';
    h->what.clear();
    h->error_code=0;
    /*
       These codes correspond to exceptions that should not allocate a separate buffer but use the fixed one.
       Notice that a mismatch between the exceptions using the stack buffer here and those implementing
       the stack buffer would be in practice harmless. However, it makes sense to be consistent.
    */
    if(code==10000 || (code>=11000 && code<12000)) {
      __PLUMED_WRAPPER_STD strncat(h->exception_buffer,what,__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1);
    } else {
      h->what=what;
    }

    /* interpret optional arguments */
    const void** options=(const void**)opt;
    if(options) while(*options) {
        if(*((char*)*options)=='c') h->error_code=*((int*)*(options+1));
        options+=2;
      }

    if(PlumedGetenvExceptionsDebug()) {
      __PLUMED_WRAPPER_STD fprintf(stderr,"+++ PLUMED_EXCEPTIONS_DEBUG\n");
      __PLUMED_WRAPPER_STD fprintf(stderr,"+++ code: %d error_code: %d message:\n%s\n",h->code,h->error_code,what);
      if(__PLUMED_WRAPPER_STD strlen(what) > __PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1) __PLUMED_WRAPPER_STD fprintf(stderr,"+++ WARNING: message will be truncated\n");
      __PLUMED_WRAPPER_STD fprintf(stderr,"+++ END PLUMED_EXCEPTIONS_DEBUG\n");
    }

  }

  /**
    Rethrow the exception based on the information saved in the NothrowHandler.
  */

  static void rethrow(const NothrowHandler&h) {
    /* The interpretation of the codes should be kept in sync with core/PlumedMainInitializer.cpp */
    /* check if we are using a full string or a fixes size buffer */
    const char* msg=(h.exception_buffer[0]?h.exception_buffer:h.what.c_str());
    if(h.code==1) throw Plumed::Invalid(msg);
    /* logic errors */
    if(h.code>=10100 && h.code<10200) {
      if(h.code>=10105 && h.code<10110) throw ::std::invalid_argument(msg);
      if(h.code>=10110 && h.code<10115) throw ::std::domain_error(msg);
      if(h.code>=10115 && h.code<10120) throw ::std::length_error(msg);
      if(h.code>=10120 && h.code<10125) throw ::std::out_of_range(msg);
      throw ::std::logic_error(msg);
    }
    /* runtime errors */
    if(h.code>=10200 && h.code<10300) {
      if(h.code>=10205 && h.code<10210) throw ::std::range_error(msg);
      if(h.code>=10210 && h.code<10215) throw ::std::overflow_error(msg);
      if(h.code>=10215 && h.code<10220) throw ::std::underflow_error(msg);
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
      if(h.code==10220) throw ::std::system_error(h.error_code,::std::generic_category(),msg);
      if(h.code==10221) throw ::std::system_error(h.error_code,::std::system_category(),msg);
      if(h.code==10222) throw ::std::system_error(h.error_code,::std::iostream_category(),msg);
      if(h.code==10223) throw ::std::system_error(h.error_code,::std::future_category(),msg);
#endif
      if(h.code>=10230 && h.code<10240) {
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
// These cases are probably useless as it looks like this should always be std::iostream_category
        if(h.code==10230) throw ::std::ios_base::failure(msg,::std::error_code(h.error_code,::std::generic_category()));
        if(h.code==10231) throw ::std::ios_base::failure(msg,::std::error_code(h.error_code,::std::system_category()));
        if(h.code==10232) throw ::std::ios_base::failure(msg,::std::error_code(h.error_code,::std::iostream_category()));
        if(h.code==10233) throw ::std::ios_base::failure(msg,::std::error_code(h.error_code,::std::future_category()));
#endif
        throw ::std::ios_base::failure(msg);
      }
      throw ::std::runtime_error(msg);
    }
    /* "bad" errors */
    if(h.code>=11000 && h.code<11100) throw Plumed::std_bad_typeid(msg);
    if(h.code>=11100 && h.code<11200) throw Plumed::std_bad_cast(msg);
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
    if(h.code>=11200 && h.code<11300) throw Plumed::std_bad_weak_ptr(msg);
    if(h.code>=11300 && h.code<11400) throw Plumed::std_bad_function_call(msg);
#endif
    if(h.code>=11400 && h.code<11500) {
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
      if(h.code>=11410 && h.code<11420) throw Plumed::std_bad_array_new_length(msg);
#endif
      throw Plumed::std_bad_alloc(msg);
    }
    if(h.code>=11500 && h.code<11600) throw Plumed::std_bad_exception(msg);
    /* lepton error */
    if(h.code>=19900 && h.code<20000) throw Plumed::LeptonException(msg);
    /* plumed exceptions */
    if(h.code>=20000 && h.code<30000) {
      /* debug - only raised with debug options */
      if(h.code>=20100 && h.code<20200) throw Plumed::ExceptionDebug(msg);
      /* error - runtime check */
      if(h.code>=20200 && h.code<20300) throw Plumed::ExceptionError(msg);
      /* error - type error */
      if(h.code>=20300 && h.code<20400) throw Plumed::ExceptionTypeError(msg);
      throw Plumed::Exception(msg);
    }
    /* fallback for any other exception */
    throw Plumed::std_exception(msg);
  }

  /**
    Rethrow the current exception.

    This is useful in order to handle an exception thrown by a kernel <=2.4.
    Only std exceptions are handled, though some of them are thrown as special
    Plumed exceptions in order to be attached a message.
  */
  static void rethrow() {
    try {
      throw;
    } catch(const ::std::bad_exception & e) {
      throw Plumed::std_bad_exception(e.what());
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
    } catch(const ::std::bad_array_new_length & e) {
      throw Plumed::std_bad_array_new_length(e.what());
#endif
    } catch(const ::std::bad_alloc & e) {
      throw Plumed::std_bad_alloc(e.what());
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
    } catch(const ::std::bad_function_call & e) {
      throw Plumed::std_bad_function_call(e.what());
    } catch(const ::std::bad_weak_ptr & e) {
      throw Plumed::std_bad_weak_ptr(e.what());
#endif
    } catch(const ::std::bad_cast & e) {
      throw Plumed::std_bad_cast(e.what());
    } catch(const ::std::bad_typeid & e) {
      throw Plumed::std_bad_typeid(e.what());
      // not implemented yet: std::regex_error
      // we do not allow regex yet due to portability problems with gcc 4.8
      // as soon as we transition to using <regex> it should be straightforward to add
    } catch(const ::std::ios_base::failure & e) {
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
      throw ::std::ios_base::failure(e.what(),e.code());
#else
      throw ::std::ios_base::failure(e.what());
#endif
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
    } catch(const ::std::system_error & e) {
      throw ::std::system_error(e.code(),e.what());
#endif
    } catch(const ::std::underflow_error &e) {
      throw ::std::underflow_error(e.what());
    } catch(const ::std::overflow_error &e) {
      throw ::std::overflow_error(e.what());
    } catch(const ::std::range_error &e) {
      throw ::std::range_error(e.what());
    } catch(const ::std::runtime_error & e) {
      throw ::std::runtime_error(e.what());
      // not implemented yet: std::future_error
      // not clear how useful it would be.
    } catch(const ::std::out_of_range & e) {
      throw ::std::out_of_range(e.what());
    } catch(const ::std::length_error & e) {
      throw ::std::length_error(e.what());
    } catch(const ::std::domain_error & e) {
      throw ::std::domain_error(e.what());
    } catch(const ::std::invalid_argument & e) {
      throw ::std::invalid_argument(e.what());
    } catch(const ::std::logic_error & e) {
      throw ::std::logic_error(e.what());
    } catch(const ::std::exception & e) {
      throw Plumed::std_exception(e.what());
    } catch(...) {
      throw Plumed::std_bad_exception("plumed could not translate exception");
    }
  }

public:

  /**
    Base class used to rethrow PLUMED exceptions.
  */

  class Exception :
    public ::std::exception
  {
    ::std::string msg;
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT Exception(const char* msg): msg(msg) {}
    const char* what() const __PLUMED_WRAPPER_CXX_NOEXCEPT __PLUMED_WRAPPER_CXX_OVERRIDE {return msg.c_str();}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~Exception() throw() {}
#endif
  };

  /**
    Used to rethrow a PLMD::ExceptionError
  */

  class ExceptionError :
    public Exception {
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT ExceptionError(const char* msg): Exception(msg) {}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~ExceptionError() throw() {}
#endif
  };

  /**
    Used to rethrow a PLMD::ExceptionDebug
  */

  class ExceptionDebug :
    public Exception {
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT ExceptionDebug(const char* msg): Exception(msg) {}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~ExceptionDebug() throw() {}
#endif
  };

  /**
    Thrown when trying to access an invalid plumed object
  */

  class Invalid :
    public Exception {
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT Invalid(const char* msg): Exception(msg) {}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~Invalid() throw() {}
#endif
  };

  /**
    Thrown when a wrong pointer is passed to the PLUMED interface.
  */
  class ExceptionTypeError:
    public Exception {
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT ExceptionTypeError(const char* msg): Exception(msg) {}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~ExceptionTypeError() throw() {}
#endif
  };

  /**
    Class used to rethrow Lepton exceptions.
  */

  class LeptonException :
    public ::std::exception
  {
    ::std::string msg;
  public:
    __PLUMED_WRAPPER_CXX_EXPLICIT LeptonException(const char* msg): msg(msg) {}
    const char* what() const __PLUMED_WRAPPER_CXX_NOEXCEPT __PLUMED_WRAPPER_CXX_OVERRIDE {return msg.c_str();}
#if ! (__cplusplus > 199711L)
    /* Destructor should be declared in order to have the correct throw() before C++11 */
    /* see https://stackoverflow.com/questions/50025862/why-is-the-stdexception-destructor-not-noexcept */
    ~LeptonException() throw() {}
#endif
  };

private:
  /*
    These exceptions are declared as private as they are not supposed to be
    catched by value. they only exist to allow a buffer to be attached to
    the std::exceptions that do not contain it already.
    Notice that these exceptions are those whose constructor should never throw, and as
    such they use a fixed size buffer.
  */

#define __PLUMED_WRAPPER_NOSTRING_EXCEPTION(name) \
  class std_ ## name : \
    public ::std::name \
  { \
    char msg[__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER]; \
  public: \
    __PLUMED_WRAPPER_CXX_EXPLICIT std_ ## name(const char * msg) __PLUMED_WRAPPER_CXX_NOEXCEPT { \
      this->msg[0]='\0'; \
      __PLUMED_WRAPPER_STD strncat(this->msg,msg,__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1); \
      if(PlumedGetenvExceptionsDebug() && __PLUMED_WRAPPER_STD strlen(msg) > __PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1) __PLUMED_WRAPPER_STD fprintf(stderr,"+++ WARNING: message will be truncated\n"); \
    } \
    std_ ## name(const std_ ## name & other) __PLUMED_WRAPPER_CXX_NOEXCEPT { \
      msg[0]='\0'; \
      __PLUMED_WRAPPER_STD strncat(msg,other.msg,__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1); \
    } \
    std_ ## name & operator=(const std_ ## name & other) __PLUMED_WRAPPER_CXX_NOEXCEPT { \
      if(this==&other) return *this;\
      msg[0]='\0'; \
      __PLUMED_WRAPPER_STD strncat(msg,other.msg,__PLUMED_WRAPPER_CXX_EXCEPTION_BUFFER-1); \
      return *this; \
    } \
    const char* what() const __PLUMED_WRAPPER_CXX_NOEXCEPT __PLUMED_WRAPPER_CXX_OVERRIDE {return msg;} \
    ~std_ ## name() __PLUMED_WRAPPER_CXX_NOEXCEPT __PLUMED_WRAPPER_CXX_OVERRIDE {} \
  };

  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_typeid)
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_cast)
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_weak_ptr)
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_function_call)
#endif
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_alloc)
#if __cplusplus > 199711L && __PLUMED_WRAPPER_LIBCXX11
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_array_new_length)
#endif
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(bad_exception)
  __PLUMED_WRAPPER_NOSTRING_EXCEPTION(exception)

  /// Small class that wraps plumed_safeptr in order to make its initialization easier
  class SafePtr {
    /// non copyable (copy would require managing buffer, could be added in the future if needed)
    SafePtr(const SafePtr&);
    /// non assignable (assignment would require managing buffer, could be added in the future if needed)
    SafePtr& operator=(SafePtr const&);
  public:
    plumed_safeptr safe;
    /// This buffer holds a copy of the data when they are passed by value.
    /// The size is sufficient to hold any primitive type.
    char buffer[32];
    /// Default constructor, nullptr
    SafePtr() __PLUMED_WRAPPER_CXX_NOEXCEPT {
      safe.ptr=NULL;
      safe.nelem=0;
      safe.shape=NULL;
      safe.flags=0x10000*2;
      safe.opt=NULL;
      buffer[0]='\0';
    }

#if __cplusplus > 199711L
    /// Construct from null
    SafePtr(__PLUMED_WRAPPER_STD nullptr_t,__PLUMED_WRAPPER_STD size_t nelem, const __PLUMED_WRAPPER_STD size_t* shape) noexcept {
      safe.ptr=nullptr;
      safe.nelem=0;
      safe.shape=nullptr;
      safe.flags=0x10000*2;
      safe.opt=nullptr;
      buffer[0]='\0';
      (void) nelem;
      (void) shape;
    }
#endif

/// Macro that generate a constructor with given type and flags
#define __PLUMED_WRAPPER_SAFEPTR_INNER(type_,flags_) \
  SafePtr(type_*ptr, __PLUMED_WRAPPER_STD size_t nelem, const __PLUMED_WRAPPER_STD size_t* shape) __PLUMED_WRAPPER_CXX_NOEXCEPT { \
    safe.ptr=ptr; \
    safe.nelem=nelem; \
    safe.shape=const_cast<size_t*>(shape); \
    safe.flags=flags_; \
    safe.opt=NULL; \
    buffer[0]='\0'; \
  }

/// Macro that uses __PLUMED_WRAPPER_SAFEPTR_INNER to generate constructors with
/// all possible pointer-const combinations
#define __PLUMED_WRAPPER_SAFEPTR(type,code,size) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type,             size | (0x10000*(code)) | (0x2000000*2)) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type const,       size | (0x10000*(code)) | (0x2000000*3)) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type*,            size | (0x10000*(code)) | (0x2000000*4)) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type*const,       size | (0x10000*(code)) | (0x2000000*5)) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type const*,      size | (0x10000*(code)) | (0x2000000*6)) \
  __PLUMED_WRAPPER_SAFEPTR_INNER(type const*const, size | (0x10000*(code)) | (0x2000000*7))

/// Macro that generates the constructors from empy types (those of which sizeof cannot be computed)
#define __PLUMED_WRAPPER_SAFEPTR_EMPTY(type,code) __PLUMED_WRAPPER_SAFEPTR(type,code,0)

/// Macro that generates the constructors from sized types (those of which sizeof can be computed).
/// In addition to generating constructors with all pointer types, it generates a constructor to
/// allow pass-by-value
#define __PLUMED_WRAPPER_SAFEPTR_SIZED(type,code) \
  __PLUMED_WRAPPER_SAFEPTR(type,code,sizeof(type)) \
  SafePtr(type val, __PLUMED_WRAPPER_STD size_t nelem, const __PLUMED_WRAPPER_STD size_t* shape) __PLUMED_WRAPPER_CXX_NOEXCEPT { \
    assert(sizeof(type)<=32); \
    safe.ptr=buffer; \
    safe.nelem=1; \
    safe.shape=NULL; \
    safe.flags=sizeof(type) | (0x10000*(code)) | (0x2000000*1); \
    safe.opt=NULL; \
    __PLUMED_WRAPPER_STD memcpy(buffer,&val,sizeof(type)); \
  }

/// Here we create all the required instances
/// 1: void
/// 3: integral
/// 4: floating
/// 5: FILE
/// 0x100: unsigned
    __PLUMED_WRAPPER_SAFEPTR_EMPTY(void,1)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(char,3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(unsigned char,0x100+3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(short,3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(unsigned short,0x100+3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(int,3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(unsigned int,0x100+3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(long,3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(unsigned long,0x100+3)
#if __PLUMED_WRAPPER_CXX_LONGLONG
    __PLUMED_WRAPPER_SAFEPTR_SIZED(long long,3)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(unsigned long long,0x100+3)
#endif
    __PLUMED_WRAPPER_SAFEPTR_SIZED(float,4)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(double,4)
    __PLUMED_WRAPPER_SAFEPTR_SIZED(long double,4)
    __PLUMED_WRAPPER_SAFEPTR_EMPTY(FILE,5)

    /// Return the contained plumed_safeptr
    plumed_safeptr get_safeptr() const __PLUMED_WRAPPER_CXX_NOEXCEPT {
      return safe;
    }

  };

public:

  /**
     Check if plumed is installed (for runtime binding)
     \return true if plumed is installed, false otherwise
     \note Equivalent to plumed_installed() but returns a bool
  */
  static bool installed() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_installed();
  }
  /**
     Check if Plumed object is valid. Available as of PLUMED 2.5
     \return true if plumed is valid, false otherwise
     \note Equivalent to plumed_valid() but returns a bool
  */
  bool valid() const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_valid(main);
  }
#if __cplusplus > 199711L
  /**
     Same as \ref valid(). Available as of PLUMED 2.5.

  Allow code such as
  \verbatim
  Plumed p;
  if(!p) raise_error();
  p.cmd("init");
  \endverbatim

  In order to avoid ambiguous conversions, this is only allowed when compiling with C++11
  where it is marked as explicit.
  */
  explicit
  operator bool() const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_valid(main);
  }
#endif

  /**
     Returns the number of references to this object. Available as of PLUMED 2.5.
    \note Equivalent to plumed_use_count()
  */
  int useCount() const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_use_count(main);
  }

#if __PLUMED_WRAPPER_GLOBAL /*{*/
  /**
     Check if global-plumed has been initialized
     \return true if global plumed object (see global()) is initialized (i.e. if gcreate() has been
             called), false otherwise.
     \note Equivalent to plumed_ginitialized() but returns a bool
  */
  static bool ginitialized() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_ginitialized();
  }
  /**
     Check if global-plumed is valid
     \return true if global plumed object (see global()) is valid.
     \note Equivalent to plumed_gvalid() but returns a bool
  */
  static bool gvalid() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_gvalid();
  }
  /**
     Initialize global-plumed.
     \warning Using the global objects in C++ is not recommended since they are difficult to use in
              an exception safe manner. In particular, one should explicitly catch exceptions to
              properly call gfinalize()
     \note Equivalent to plumed_gcreate()
  */
  static void gcreate() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    plumed_gcreate();
  }
  /**
     Send a command to global-plumed
      \param key The name of the command to be executed
     \note Equivalent to plumed_gcmd()
  */
  static void gcmd(const char*key) {
    global().cmd(key);
  }
  /**
     Send a command to global-plumed
      \param key The name of the command to be executed
      \param val The argument.
     \note Equivalent to plumed_gcmd()
  */
  template<typename T>
  static void gcmd(const char*key,T val) {
    global().cmd(key,val);
  }
  /**
     Send a command to global-plumed
      \param key The name of the command to be executed
      \param val The argument.
      \param nelem Number of elements in the passed array, for typechecking.
     \note Equivalent to plumed_gcmd()
  */
  template<typename T>
  static void gcmd(const char*key,T* val,__PLUMED_WRAPPER_STD size_t nelem) {
    global().cmd(key,val,nelem);
  }

  /**
     Send a command to global-plumed
      \param key The name of the command to be executed
      \param val The argument.
      \param shape The shape of the argument.
     \note Equivalent to plumed_gcmd()
  */
  template<typename T>
  static void gcmd(const char*key,T* val, const __PLUMED_WRAPPER_STD size_t* shape) {
    global().cmd(key,val,shape);
  }
  /**
     Send a command to global-plumed
      \param key The name of the command to be executed
      \param val The argument.
      \param nelem Number of elements in the passed array, for typechecking.
      \param shape The shape of the argument.
     \note Equivalent to plumed_gcmd()
  */
  template<typename T>
  static void gcmd(const char*key,T* val, __PLUMED_WRAPPER_STD size_t nelem, const __PLUMED_WRAPPER_STD size_t* shape) {
    global().cmd(key,val,nelem,shape);
  }

  /**
     Finalize global-plumed
  */
  static void gfinalize() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    plumed_gfinalize();
  }
  /**
     Returns the Plumed global object

     Notice that the object is copied, thus increasing the reference counter of the
     global object. In this manner, the global object will survive after a call to
     \ref gfinalize() if the resulting object is still in scope.

     \return The Plumed global object
  */
  static Plumed global() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return Plumed(plumed_global());
  }
#endif /*}*/
  /**
     Constructor.

    Notice that when using runtime binding the constructed object might be
    invalid. One might check it using the \ref valid() method.

    \note Performs the same task a plumed_create()
  */
Plumed()__PLUMED_WRAPPER_CXX_NOEXCEPT :
#if __PLUMED_WRAPPER_CXX_DEFAULT_INVALID
  main(plumed_create_invalid())
#else
  main(plumed_create())
#endif
  {
  }

  /**
     Clone a Plumed object from a FORTRAN char* handler.

     \param c The FORTRAN handler (a char[32]).

     The reference counter for the corresponding object will be increased
     to make sure that the object will be available after plumed_f_finalize is called
     if the created object is still in scope.
  */
__PLUMED_WRAPPER_CXX_EXPLICIT Plumed(const char*c)__PLUMED_WRAPPER_CXX_NOEXCEPT :
  main(plumed_create_reference_f(c))
  {
  }

  /**
    Create a reference from a void* pointer. Available as of PLUMED 2.5.
  */
__PLUMED_WRAPPER_CXX_EXPLICIT Plumed(void*v)__PLUMED_WRAPPER_CXX_NOEXCEPT :
  main(plumed_create_reference_v(v))
  {
  }

  /**
     Clone a Plumed object from a C plumed structure

     \param p The C plumed structure.

     The reference counter for the corresponding object will be increased
     to make sure that the object will be available after plumed_finalize is called
     if the created object is still in scope.
  */
__PLUMED_WRAPPER_CXX_EXPLICIT Plumed(plumed p)__PLUMED_WRAPPER_CXX_NOEXCEPT :
  main(plumed_create_reference(p))
  {
  }

  /** Copy constructor.

    Takes a reference, incrementing the reference counter of the corresponding object.
  */
Plumed(const Plumed& p)__PLUMED_WRAPPER_CXX_NOEXCEPT :
  main(plumed_create_reference(p.main))
  {
  }

  /** Assignment operator. Available as of PLUMED 2.5.

    Takes a reference,incrementing the reference counter of the corresponding object.
  */
  Plumed&operator=(const Plumed&p) __PLUMED_WRAPPER_CXX_NOEXCEPT {
    if(this != &p) {
// the check is needed to avoid calling plumed_finalize on moved objects
      if(main.p) decref();
      main=plumed_create_reference(p.main);
    }
    return *this;
  }

  /*
    PLUMED >= 2.4 requires a C++11 compiler.
    Anyway, since Plumed.h file might be redistributed with other codes
    and it should be possible to combine it with earlier PLUMED versions,
    we here explicitly check if C+11 is available before enabling move semantics.
  */
#if __cplusplus > 199711L
  /** Move constructor. Available as of PLUMED 2.5.
    Only if move semantics is enabled.
  */
Plumed(Plumed&&p)__PLUMED_WRAPPER_CXX_NOEXCEPT :
  main(p.main)
  {
    p.main.p=nullptr;
  }
  /** Move assignment. Available as of PLUMED 2.5.
    Only if move semantics is enabled.
  */
  Plumed& operator=(Plumed&&p)__PLUMED_WRAPPER_CXX_NOEXCEPT  {
    if(this != &p) {
// the check is needed to avoid calling plumed_finalize on moved objects
      if(main.p) decref();
      main=p.main;
      p.main.p=nullptr;
    }
    return *this;
  }
#endif
  /**
    Create a PLUMED object loading a specific kernel. Available as of PLUMED 2.5.

    It returns an object created with \ref plumed_create_dlopen. The object is owned and
    is then finalized in the destructor. It can be used as follows:
  \verbatim
    PLMD::Plumed p = PLMD::Plumed::dlopen("/path/to/libplumedKernel.so");
  // or, equivalenty:
  //    PLMD::Plumed p(PLMD::Plumed::dlopen("/path/to/libplumedKernel.so"));
    p.cmd("init");
  \endverbatim
    or, equivalently, as
  \verbatim
    auto p = PLMD::Plumed::dlopen("/path/to/libplumedKernel.so");
    p.cmd("init");
  \endverbatim
  */
  static Plumed dlopen(const char* path)__PLUMED_WRAPPER_CXX_NOEXCEPT  {
// use decref to remove the extra reference
    return Plumed(plumed_create_dlopen(path)).decref();
  }

  /**
    Create a PLUMED object loading a specific kernel. Available as of PLUMED 2.5.

    Same as \ref dlopen(const char* path), but allows a dlopen mode to be chosen explicitly.
  */
  static Plumed dlopen(const char* path,int mode)__PLUMED_WRAPPER_CXX_NOEXCEPT  {
// use decref to remove the extra reference
    return Plumed(plumed_create_dlopen2(path,mode)).decref();
  }
  /** Invalid constructor. Available as of PLUMED 2.5.

    Can be used to initialize an invalid object. It might be useful to postpone
    the initialization of a Plumed object. Consider the following case
  \verbatim
    Plumed p;
    setenv("PLUMED_KERNEL","/path/to/kernel/libplumedKernel.so",1);
    p.cmd("init")
  \endverbatim
    Here the `p` object will be initialized *before* the `PLUMED_KERNEL` env var has been set.
    This can be particularly problematic if `p` is stored in some high level class.
    The following case would do the job
  \verbatim
    Plumed p;
    setenv("PLUMED_KERNEL","/path/to/kernel/libplumedKernel.so",1);
    p=Plumed();
    p.cmd("init")
  \endverbatim
    However, there will be some error reported related to the attempt to load the kernel
    when `p` is initialized. The following solution is the optimal one:
  \verbatim
    Plumed p(Plumed::makeInvalid());
    setenv("PLUMED_KERNEL","/path/to/kernel/libplumedKernel.so",1);
    p=Plumed();
    p.cmd("init")
  \endverbatim
  */
  static Plumed makeInvalid() __PLUMED_WRAPPER_CXX_NOEXCEPT  {
// use decref to remove the extra reference
    return Plumed(plumed_create_invalid()).decref();
  }

  /**
    Create a valid PLMD::Plumed object.

    Can be used to create a valid object e.g. when Plumed.h was compiled with
    `-D__PLUMED_WRAPPER_CXX_DEFAULT_INVALID`. For internal usage.
  */

  static Plumed makeValid()__PLUMED_WRAPPER_CXX_NOEXCEPT  {
// use decref to remove the extra reference
    return Plumed(plumed_create()).decref();
  }


  /**
     Retrieve the C plumed structure for this object.

     Notice that the resulting plumed structure is a weak reference and
     should NOT be finalized, unless a new reference is explicitly added
  \verbatim
  Plumed p;
  plumed c=p;
  plumed_finalize(c); // <- this is wrong
  \endverbatim
  \verbatim
  Plumed p;
  plumed c=plumed_create_reference(p);
  plumed_finalize(c); // <- this is right
  \endverbatim
  */
  operator plumed()const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return main;
  }

  /**
     Retrieve a FORTRAN handler for this object
      \param c The FORTRAN handler (a char[32]).
    Notice that the resulting plumed structure is a weak reference and
    should NOT be finalized, unless a new reference is explicitly added.
  */
  void toFortran(char*c)const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    plumed_c2f(main,c);
  }

  /**
     Retrieve a void* handler for this object. Available as of PLUMED 2.5.
    Notice that the resulting plumed structure is a weak reference and
    should NOT be finalized, unless a new reference is explicitly added.
  */
  void* toVoid()const __PLUMED_WRAPPER_CXX_NOEXCEPT {
    return plumed_c2v(main);
  }

  /**
    Increase reference counter. Available as of PLUMED 2.5.

    Using this method improperly might interfere with correct object construction
    and destruction.
    If you want to play with this, also try to compile using `-D__PLUMED_WRAPPER_DEBUG_REFCOUNT=1` and see what happens.

    A possible usage is to transfer the ownership of a temporary
    object when it is converted
  \verbatim
  plumed p=Plumed::dlopen(path).incref()
  // without incref(), the just constructed object will be destroyed
  // when the temporary object is deleted.
  ... do stuff ...
  plumed_finalize(p);
  \endverbatim

  */
  Plumed& incref() __PLUMED_WRAPPER_CXX_NOEXCEPT {
    plumed_create_reference(main);
    return *this;
  }

  /**
    Decrease reference counter. Available as of PLUMED 2.5.

    Using this method improperly might interfere with correct object construction
    and destruction.
    If you want to play with this, also try to compile using `-D__PLUMED_WRAPPER_DEBUG_REFCOUNT=1` and see what happens.
  */
  Plumed& decref() __PLUMED_WRAPPER_CXX_NOEXCEPT {
// calling decref on a moved plumed object should give an error, so we do not check if main.p!=NULL here:
    plumed_finalize(main);
    return *this;
  }

private:

  /**
    Private version of cmd. It is used here to avoid duplication of code between typesafe and not-typesafe versions
  */
  void cmd_priv(const char*key, SafePtr*safe=NULL, const void* unsafe=NULL) {
    if(safe && PlumedGetenvTypesafeDebug()) {
      __PLUMED_WRAPPER_STD fprintf(stderr,"+++ PLUMED_TYPESAFE_DEBUG %s %p %zu",key,safe->safe.ptr,safe->safe.nelem);
      const __PLUMED_WRAPPER_STD size_t* shape=safe->safe.shape;
      if(shape) {
        __PLUMED_WRAPPER_STD fprintf(stderr," (");
        const __PLUMED_WRAPPER_STD size_t* shape=safe->safe.shape;
        while(*shape!=0) {
          __PLUMED_WRAPPER_STD fprintf(stderr," %zu",*shape);
          shape++;
        }
        __PLUMED_WRAPPER_STD fprintf(stderr," )");
      }
      __PLUMED_WRAPPER_STD fprintf(stderr," %lx %p\n",safe->safe.flags,safe->safe.opt);
    }
    NothrowHandler h;
    h.code=0;
    plumed_nothrow_handler nothrow= {&h,nothrow_handler};
    try {
      if(safe) {
        plumed_cmd_safe_nothrow(main,key,safe->get_safeptr(),nothrow);
      } else {
        plumed_cmd_nothrow(main,key,unsafe,nothrow);
      }
    } catch (...) {
      /*
        When loading a kernel <=2.4, plumed_cmd_nothrow could throw an exception.
        If the exception is transmitted through the C interface and arrives here,
        we translate it so as to free the virtual tables of the loaded kernel.
      */
      rethrow();
    }
    if(h.code!=0) rethrow(h);
  }

public:

  /**
     Send a command to this plumed object
      \param key The name of the command to be executed
      \note Similar to \ref plumed_cmd(). It actually called \ref plumed_cmd_nothrow() and
            rethrow any exception raised within PLUMED.
  */
  void cmd(const char*key) {
#if __PLUMED_WRAPPER_CXX_TYPESAFE
    SafePtr s;
    cmd_priv(key,&s);
#else
    cmd_priv(key);
#endif
  }

  /**
     Send a command to this plumed object
      \param key The name of the command to be executed
      \param val The argument, passed by value.
      \note Similar to \ref plumed_cmd(). It actually called \ref plumed_cmd_nothrow() and
            rethrow any exception raised within PLUMED.
      \note Unless one defines __PLUMED_WRAPPER_CXX_TYPESAFE=0 or PLUMED library is <=2.7,
             the type of the argument is checked.
  */
  template<typename T>
  void cmd(const char*key,T val) {
#if __PLUMED_WRAPPER_CXX_TYPESAFE
    SafePtr s(val,0,NULL);
    cmd_priv(key,&s);
#else
    cmd_priv(key,NULL,&val);
#endif
  }

  /**
     Send a command to this plumed object
      \param key The name of the command to be executed
      \param val The argument, passed by pointer.
      \note Similar to \ref plumed_cmd(). It actually called \ref plumed_cmd_nothrow() and
            rethrow any exception raised within PLUMED.
      \note Unless one defines __PLUMED_WRAPPER_CXX_TYPESAFE=0 or PLUMED library is <=2.7,
             the type of the argument is checked.
  */
  template<typename T>
  void cmd(const char*key,T* val) {
#if __PLUMED_WRAPPER_CXX_TYPESAFE
    SafePtr s(val,0,NULL);
    cmd_priv(key,&s);
#else
    cmd_priv(key,NULL,val);
#endif
  }

  /**
     Send a command to this plumed object
      \param key The name of the command to be executed
      \param val The argument, passed by pointer.
      \param shape A zero-terminated array containing the shape of the data.
      \note Similar to \ref plumed_cmd(). It actually called \ref plumed_cmd_nothrow() and
            rethrow any exception raised within PLUMED.
      \note Unless one defines __PLUMED_WRAPPER_CXX_TYPESAFE=0 or PLUMED library is <=2.7,
             the type of the argument is checked. If shape is passed, it is also
             checked that PLUMED access only compatible indexes.
  */
  template<typename T>
  void cmd(const char*key,T* val, const __PLUMED_WRAPPER_STD size_t* shape) {
#if __PLUMED_WRAPPER_CXX_TYPESAFE
    SafePtr s(val,0,shape);
    cmd_priv(key,&s);
#else
    cmd_priv(key,NULL,val);
#endif
  }

  /**
     Send a command to this plumed object
      \param key The name of the command to be executed
      \param val The argument, passed by pointer.
      \param nelem The number of elements passed.
      \param shape A zero-terminated array containing the shape of the data.
      \note Similar to \ref plumed_cmd(). It actually called \ref plumed_cmd_nothrow() and
            rethrow any exception raised within PLUMED.
      \note Unless one defines __PLUMED_WRAPPER_CXX_TYPESAFE=0 or PLUMED library is <=2.7,
             the type of the argument is checked. If shape is passed, it is also
             checked that PLUMED access only compatible indexes. nelem is used to check
             the maximum index interpreting the array as flattened.
  */
  template<typename T>
  void cmd(const char*key,T* val, __PLUMED_WRAPPER_STD size_t nelem, const __PLUMED_WRAPPER_STD size_t* shape=NULL) {
#if __PLUMED_WRAPPER_CXX_TYPESAFE
    SafePtr s(val,nelem,shape);
    cmd_priv(key,&s);
#else
    cmd_priv(key,NULL,val);
#endif
  }

  /**
     Destructor

     It calls \ref plumed_finalize(). Notice that this is done also if the
     constructor failed (that is, if it returned an invalid object). This allows
     declaring Plumed objects also if PLUMED is actually not available, provided
     one does not use the \ref cmd method.

     Destructor is virtual so as to allow correct inheritance from Plumed object.
  */
#if __PLUMED_WRAPPER_CXX_POLYMORPHIC
  virtual
#endif
  ~Plumed() __PLUMED_WRAPPER_CXX_NOEXCEPT {
// the check is needed to avoid calling plumed_finalize on moved objects
    if(main.p) decref();
  }
};

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator==(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()==b.toVoid();
}

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator!=(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()!=b.toVoid();
}

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator<=(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()<=b.toVoid();
}

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator<(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()<b.toVoid();
}

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator>=(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()>=b.toVoid();
}

/**
  \related Plumed
  Comparison operator. Available as of PLUMED 2.5.
*/
inline
bool operator>(const Plumed&a,const Plumed&b) __PLUMED_WRAPPER_CXX_NOEXCEPT {
  return a.toVoid()>b.toVoid();
}

__PLUMED_WRAPPER_ANONYMOUS_END /*}*/

}

#if __PLUMED_WRAPPER_CXX_BIND_C /*{*/

__PLUMED_WRAPPER_ANONYMOUS_BEGIN /*{*/

/**
  \related Plumed
  This function can be used to make plumed_cmd behave as the C++ wrapper PLMD::Plumed::cmd,
  namely implement typechecks and rethrowing exception.
  To be used through the macro plumed_cmd (defined when __PLUMED_WRAPPER_CXX_BIND_C==1).
  Available as of PLUMED 2.8.
*/
template<typename T>
void plumed_cmd_cxx(plumed p,const char*key,T val) {
  PLMD::Plumed(p).cmd(key,val);
}

#define __PLUMED_WRAPPER_REDEFINE_CMD plumed_cmd_cxx

#if __PLUMED_WRAPPER_GLOBAL /*{*/
/**
  \related Plumed
  This function can be used to make plumed_gcmd behave as the C++ wrapper PLMD::Plumed::gcmd,
  namely implement typechecks and rethrowing exception.
  To be used through the macro plumed_gcmd (defined when __PLUMED_WRAPPER_CXX_BIND_C==1).
  Available as of PLUMED 2.8.
*/
template<typename T>
void plumed_gcmd_cxx(const char*key,T val) {
  PLMD::Plumed::gcmd(key,val);
}

#define __PLUMED_WRAPPER_REDEFINE_GCMD plumed_gcmd_cxx
#endif /*}*/

__PLUMED_WRAPPER_ANONYMOUS_END /*}*/

#endif /*}*/

#endif /*}*/

#endif /*}*/

/* END OF DECLARATIONS */

/*

  1: emit implementation
  0: do not emit implementation

  Allows an implementation to be emitted together with the declarations.

  Used to decide if definitions should be emitted. This macro could have a different
  value when Plumed.h is reincluded. As a consequence, we map it to a local
  macro (__PLUMED_WRAPPER_IMPLEMENTATION_) that is reset at the end of this file.
*/

#ifdef __PLUMED_WRAPPER_IMPLEMENTATION
#define __PLUMED_WRAPPER_IMPLEMENTATION_ __PLUMED_WRAPPER_IMPLEMENTATION
#else
#define __PLUMED_WRAPPER_IMPLEMENTATION_ 0
#endif

/* BEGINNING OF DEFINITIONS */

#if __PLUMED_WRAPPER_IMPLEMENTATION_  /*{*/
#ifndef __PLUMED_wrapper_Plumed_implementation /*{*/
#define __PLUMED_wrapper_Plumed_implementation

/*
  the following macros only control the implementation
*/

/*
  1: enable the definition of plumed_symbol_table_reexport
  0: does not enable the definition of plumed_symbol_table_reexport

  This is only needed in the official plumed library to make
  the symbol table available. This is a hack to reexport the function table
  and is only needed when creating the library libplumed.so.
*/

#ifndef __PLUMED_WRAPPER_REEXPORT_SYMBOL_TABLE
#define __PLUMED_WRAPPER_REEXPORT_SYMBOL_TABLE 0
#endif

/*
  1: write on stderr changes in reference counters
  0: do not write changes in reference counters

  Used for debugging.

  Only used in definitions.
*/

#ifndef __PLUMED_WRAPPER_DEBUG_REFCOUNT
#define __PLUMED_WRAPPER_DEBUG_REFCOUNT 0
#endif

/*
  1: emit plumed_kernel_register function (default)
  0: do not emit plumed_kernel_register function

  This function is only needed to avoid an extra warning when loading old (<=2.4) kernels.
  We might change its default in the future.

  Used only in definitions.
*/

#ifndef __PLUMED_WRAPPER_KERNEL_REGISTER
#define __PLUMED_WRAPPER_KERNEL_REGISTER 1
#endif

/*
  1: emit Fortran wrappers
  0: do not emit Fortran wrappers (default)

  Used only in definitions.
*/

#ifndef __PLUMED_WRAPPER_FORTRAN
#define __PLUMED_WRAPPER_FORTRAN 0
#endif

/*
  With internal interface, it does not make sense to emit kernel register or fortran interfaces
*/

#if ! __PLUMED_WRAPPER_EXTERN /*{*/
#undef __PLUMED_WRAPPER_KERNEL_REGISTER
#define __PLUMED_WRAPPER_KERNEL_REGISTER 0
#undef __PLUMED_WRAPPER_FORTRAN
#define __PLUMED_WRAPPER_FORTRAN 0
#endif /*}*/

#ifdef __PLUMED_HAS_DLOPEN
#include <dlfcn.h> /* dlopen dlerror dlsym */
#endif

#if __PLUMED_WRAPPER_CXX_STD
#include <cstdio>  /* fprintf */
#include <cstring> /* memcpy strlen strncpy memcmp memmove strcmp memcpy */
#include <cassert> /* assert */
#include <cstdlib> /* getenv malloc free abort exit */
#include <climits> /* CHAR_BIT */
#else
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <limits.h>
#endif

/**
  Function pointer to plumed_create
*/

typedef void*(*plumed_create_pointer)(void);
/**
  Function pointer to plumed_cmd
*/
typedef void(*plumed_cmd_pointer)(void*,const char*,const void*);

/**
  Function pointer to plumed_finalize
*/
typedef void(*plumed_finalize_pointer)(void*);

/**
   Holder for plumedmain function pointers.
*/
typedef struct {
  plumed_create_pointer create;
  plumed_cmd_pointer cmd;
  plumed_finalize_pointer finalize;
} plumed_plumedmain_function_holder;

/**
  Holder for plumed symbol table.

  The table contains pointers to function exported from plumed. Functions can be added increasing the version number.
  Notice that the default way to extend functionalities is by adding cmd strings. This is a last resort, and all new
  functions should be explicitly motivated. Here's the addition:

  version=2, cmd_nothrow.

  This function accepts an extra argument `plumed_nothrow_handler*handler`.
  In case an exception is thrown withint plumed, it just calls `handler->handler(handler->ptr,code,message,opt)` and return.
  An alternative would have been to install an error handler (with a call to cmd("setErrorHandler")). However, the cost
  of doing it everytime Plumed::cmd is called is too high. On the other hand, installing it only at object construction
  is very risky since and object created in that way would not report any error if manipulated from the C interface.
  So, it looks like this is the only possibility.

*/
typedef struct {
  /**
    Version number.

    Minimum value is 1.
  */
  int version;
  /**
    Pointers to standard plumed functions (create/cmd/finalize).

    Always available.
  */
  plumed_plumedmain_function_holder functions;
  /**
    Pointer to a cmd function guaranteed not to throw exceptions.

    Available with version>=2.
  */
  void (*cmd_nothrow)(void*plumed,const char*key,const void*val,plumed_nothrow_handler);
  /**
    Pointer to a cmd function that accepts typeinfos.

    Available with version>=3.
  */
  void (*cmd_safe)(void*plumed,const char*key,plumed_safeptr);

  /**
    Pointer to a cmd function guaranteed not to throw exceptions and that accepts typeinfos.

    Available with version>=3.
  */
  void (*cmd_safe_nothrow)(void*plumed,const char*key,plumed_safeptr,plumed_nothrow_handler);

} plumed_symbol_table_type;

/* Utility to convert function pointers to pointers, just for the sake of printing them */
#define __PLUMED_CONVERT_FPTR(ptr,fptr) { ptr=NULL; __PLUMED_WRAPPER_STD memcpy(&ptr,&fptr,(sizeof(fptr)>sizeof(ptr)?sizeof(ptr):sizeof(fptr))); }

#define __PLUMED_GETENV __PLUMED_WRAPPER_STD getenv
#define __PLUMED_FPRINTF __PLUMED_WRAPPER_STD fprintf
#define __PLUMED_MALLOC __PLUMED_WRAPPER_STD malloc
#define __PLUMED_FREE __PLUMED_WRAPPER_STD free

/**
  Historically (PLUMED<=2.4) register for plumedmain function pointers.
  As of PLUMED>=2.5, this function does not do anything except for reporting the attempt to register
  something. It always returns NULL. The function should be here anyway to allow an incomplete
  libplumedKernel (<=2.4), expecting this function to be present, to be loaded correctly.
*/
#if __PLUMED_WRAPPER_KERNEL_REGISTER
/* Since it is only called from outside, it must be hardcoded to be extern */
__PLUMED_WRAPPER_EXTERN_C_BEGIN /*{*/
extern plumed_plumedmain_function_holder* plumed_kernel_register(const plumed_plumedmain_function_holder*);
plumed_plumedmain_function_holder* plumed_kernel_register(const plumed_plumedmain_function_holder* f) {
  void* tmpptr;
  if(f) {
    if(__PLUMED_GETENV("PLUMED_LOAD_DEBUG")) {
      __PLUMED_FPRINTF(stderr,"+++ Ignoring registration at %p (",(void*)f);
      __PLUMED_CONVERT_FPTR(tmpptr,f->create);
      __PLUMED_FPRINTF(stderr,"%p,",tmpptr);
      __PLUMED_CONVERT_FPTR(tmpptr,f->cmd);
      __PLUMED_FPRINTF(stderr,"%p,",tmpptr);
      __PLUMED_CONVERT_FPTR(tmpptr,f->finalize);
      __PLUMED_FPRINTF(stderr,"%p) +++\n",tmpptr);
    }
  }
  return NULL;
}
__PLUMED_WRAPPER_EXTERN_C_END /*}*/
#endif

#if defined( __PLUMED_HAS_DLOPEN) /*{*/
/**
Try to dlopen a path with a given mode.
If the dlopen command fails, it tries to strip the `Kernel` part of the name.

This function is declared static (internal linkage) so that it is not visible from outside.
It is first declared then defined to make sure it is a regular C static function.
*/

__PLUMED_WRAPPER_INTERNALS_BEGIN
void* plumed_attempt_dlopen(const char*path,int mode) {
  char* pathcopy;
  void* p;
  char* pc;
  __PLUMED_WRAPPER_STD size_t strlenpath;
  FILE* fp;
  pathcopy=NULL;
  p=NULL;
  pc=NULL;
  strlenpath=0;
  fp=__PLUMED_WRAPPER_STD fopen(path,"r");
  if(!fp) {
    __PLUMED_FPRINTF(stderr,"+++ File %s does not exist or cannot be read\n",path);
    return NULL;
  }
  __PLUMED_WRAPPER_STD fclose(fp);
  dlerror();
  p=dlopen(path,mode);
  if(!p) {
    /*
      Something went wrong. We try to remove "Kernel" string from the PLUMED_KERNEL variable
      and load directly the shared library. Notice that this particular path is only expected
      to be necessary when using PLUMED<=2.4 and the symbols in the main executable are
      not visible. All the other cases (either PLUMED>=2.5 or symbols in the main executable visible)
      should work correctly without entering here.
    */
    __PLUMED_FPRINTF(stderr,"+++ An error occurred. Message from dlopen(): %s +++\n",dlerror());
    strlenpath=__PLUMED_WRAPPER_STD strlen(path);
    pathcopy=(char*) __PLUMED_MALLOC(strlenpath+1);
    __PLUMED_WRAPPER_STD strncpy(pathcopy,path,strlenpath+1);
    pc=pathcopy+strlenpath-6;
    while(pc>=pathcopy && __PLUMED_WRAPPER_STD memcmp(pc,"Kernel",6)) pc--;
    if(pc>=pathcopy) {
      __PLUMED_WRAPPER_STD memmove(pc, pc+6, __PLUMED_WRAPPER_STD strlen(pc)-5);
      __PLUMED_FPRINTF(stderr,"+++ This error is expected if you are trying to load a kernel <=2.4\n");
      __PLUMED_FPRINTF(stderr,"+++ Trying %s +++\n",pathcopy);
      fp=__PLUMED_WRAPPER_STD fopen(path,"r");
      if(!fp) {
        __PLUMED_FPRINTF(stderr,"+++ File %s does not exist or cannot be read\n",pathcopy);
        __PLUMED_FREE(pathcopy);
        return NULL;
      }
      __PLUMED_WRAPPER_STD fclose(fp);
      dlerror();
      p=dlopen(pathcopy,mode);
      if(!p) __PLUMED_FPRINTF(stderr,"+++ An error occurred. Message from dlopen(): %s +++\n",dlerror());
    }
    __PLUMED_FREE(pathcopy);
  }
  return p;
}
__PLUMED_WRAPPER_INTERNALS_END

/**
  Utility to search for a function.
*/
#define __PLUMED_SEARCH_FUNCTION(tmpptr,handle,func,name,debug) \
  if(!func) { \
    tmpptr=dlsym(handle,name); \
    if(tmpptr) { \
      *(void **)(&func)=tmpptr; \
      if(debug) __PLUMED_FPRINTF(stderr,"+++ %s found at %p +++\n",name,tmpptr); \
    } else { \
      if(debug) __PLUMED_FPRINTF(stderr,"+++ Function %s not found\n",name); \
    } \
  }

/**
Search symbols in a dlopened library.

This function is declared static (internal linkage) so that it is not visible from outside.
*/
__PLUMED_WRAPPER_INTERNALS_BEGIN
void plumed_search_symbols(void* handle, plumed_plumedmain_function_holder* f,plumed_symbol_table_type** table) {
  plumed_plumedmain_function_holder functions;
  plumed_symbol_table_type* table_ptr;
  void* tmpptr;
  char* debug;
  functions.create=NULL;
  functions.cmd=NULL;
  functions.finalize=NULL;
  table_ptr=NULL;
  tmpptr=NULL;
  /*
    Notice that as of PLUMED 2.5 we ignore self registrations.
    Pointers are searched in the form of a single pointer to a structure, which
    is the standard way in PLUMED 2.5, as well as using alternative names used in
    PLUMED 2.0 to 2.4 (e.g. plumedmain_create) and in some intermediate versions between
    PLUMED 2.4 and 2.5 (e.g. plumed_plumedmain_create). The last chance is probably
    unnecessary and might be removed at some point.
  */
  debug=__PLUMED_GETENV("PLUMED_LOAD_DEBUG");
  table_ptr=(plumed_symbol_table_type*) dlsym(handle,"plumed_symbol_table");
  if(table_ptr) functions=table_ptr->functions;
  if(debug) {
    if(table_ptr) {
      __PLUMED_FPRINTF(stderr,"+++ plumed_symbol_table version %i found at %p +++\n",table_ptr->version,(void*)table_ptr);
      __PLUMED_FPRINTF(stderr,"+++ plumed_function_pointers found at %p (",(void*)&table_ptr->functions);
      __PLUMED_CONVERT_FPTR(tmpptr,functions.create);
      __PLUMED_FPRINTF(stderr,"%p,",tmpptr);
      __PLUMED_CONVERT_FPTR(tmpptr,functions.cmd);
      __PLUMED_FPRINTF(stderr,"%p,",tmpptr);
      __PLUMED_CONVERT_FPTR(tmpptr,functions.finalize);
      __PLUMED_FPRINTF(stderr,"%p) +++\n",tmpptr);
    } else {
      __PLUMED_FPRINTF(stderr,"+++ plumed_symbol_table (available in PLUMED>=2.5) not found, perhaps kernel is older +++\n");
    }
  }
  /* only searches if they were not found already */
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.create,"plumedmain_create",debug);
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.create,"plumed_plumedmain_create",debug);
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.cmd,"plumedmain_cmd",debug);
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.cmd,"plumed_plumedmain_cmd",debug);
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.finalize,"plumedmain_finalize",debug);
  __PLUMED_SEARCH_FUNCTION(tmpptr,handle,functions.finalize,"plumed_plumedmain_finalize",debug);
  if(functions.create && functions.cmd && functions.finalize) {
    if(debug) __PLUMED_FPRINTF(stderr,"+++ PLUMED was loaded correctly +++\n");
    *f=functions;
    if(table) *table=table_ptr;
  } else {
    if(!functions.create) __PLUMED_FPRINTF(stderr,"+++ Pointer to (plumed_)plumedmain_create not found +++\n");
    if(!functions.cmd) __PLUMED_FPRINTF(stderr,"+++ Pointer to (plumed_)plumedmain_cmd not found +++\n");
    if(!functions.finalize) __PLUMED_FPRINTF(stderr,"+++ Pointer to (plumed_)plumedmain_finalize not found +++\n");
    f->create=NULL;
    f->cmd=NULL;
    f->finalize=NULL;
    if(table) *table=NULL;
  }
}
__PLUMED_WRAPPER_INTERNALS_END

#endif /*}*/


#if __PLUMED_WRAPPER_REEXPORT_SYMBOL_TABLE

/*
  Here is the case where plumed_symbol_table is
  visible as extern. We first declare it (together with plumed_symbol_table_init) ...
*/

__PLUMED_WRAPPER_EXTERN_C_BEGIN
extern
plumed_symbol_table_type plumed_symbol_table;
__PLUMED_WRAPPER_EXTERN_C_END
__PLUMED_WRAPPER_EXTERN_C_BEGIN
extern
void plumed_symbol_table_init(void);
__PLUMED_WRAPPER_EXTERN_C_END

/*
  ... and then make available a function that returns the address
  of the symbol table.
*/
__PLUMED_WRAPPER_C_BEGIN
plumed_symbol_table_type* plumed_symbol_table_reexport() {
  /* make sure the table is initialized */
  plumed_symbol_table_init();
  return &plumed_symbol_table;
}
__PLUMED_WRAPPER_C_END

#else

/*
  Here is the case where plumed_symbol_table is not
  visible as extern. We thus assume that plumed_symbol_table_reexport is
  available.
*/

__PLUMED_WRAPPER_EXTERN_C_BEGIN
extern plumed_symbol_table_type* plumed_symbol_table_reexport();
__PLUMED_WRAPPER_EXTERN_C_END
#endif


/*
  Returns the global pointers, either those available at link time or those
  found in the library loaded at PLUMED_KERNEL env var.
  If plumed_symbol_table_ptr is not NULL, it is used to return a pointer to the symbol table
  (if available).
  Notice that problems can be detected checking if the functions have a NULL ptr.
  On the other hand, the symbol table pointer might be NULL just because the plumed version is <=2.4.
  If handle is not NULL, it is used to return a dlopen handle that could be subsequently dlclosed.
*/
__PLUMED_WRAPPER_INTERNALS_BEGIN
void plumed_retrieve_functions(plumed_plumedmain_function_holder* functions, plumed_symbol_table_type** plumed_symbol_table_ptr,void** handle) {
#if ! __PLUMED_WRAPPER_LINK_RUNTIME
  /*
    Real interface, constructed using the symbol table obtained with plumed_symbol_table_reexport.
    This makes the symbols hardcoded and independent of a mis-set PLUMED_KERNEL variable.
  */
  plumed_symbol_table_type* ptr=plumed_symbol_table_reexport();
  if(plumed_symbol_table_ptr) *plumed_symbol_table_ptr=ptr;
  if(handle) *handle=NULL;
  if(functions) *functions=ptr->functions;
#elif ! defined(__PLUMED_HAS_DLOPEN)
  /*
    When dlopen is not available, we hard code them to NULL
  */
  fprintf(stderr,"+++ PLUMED has been compiled without dlopen and without a static kernel +++\n");
  plumed_plumedmain_function_holder g= {NULL,NULL,NULL};
  if(plumed_symbol_table_ptr) *plumed_symbol_table_ptr=NULL;
  if(handle) *handle=NULL;
  if(functions) *functions=g;
#else
  /*
    On the other hand, for runtime binding, we use dlsym to find the relevant functions.
  */
  plumed_plumedmain_function_holder g;
  /* search is done once and only once */
  const char* path;
  void* p;
  char* debug;
  int dlopenmode;
  g.create=NULL;
  g.cmd=NULL;
  g.finalize=NULL;
  path=__PLUMED_GETENV("PLUMED_KERNEL");
  p=NULL;
  debug=__PLUMED_GETENV("PLUMED_LOAD_DEBUG");
  dlopenmode=0;
  if(plumed_symbol_table_ptr) *plumed_symbol_table_ptr=NULL;
  if(handle) *handle=NULL;
#ifdef __PLUMED_DEFAULT_KERNEL
  /*
    This variable allows a default path for the kernel to be hardcoded.
    Can be useful for hardcoding the predefined plumed location
    still allowing the user to override this choice setting PLUMED_KERNEL.
    The path should be chosen at compile time adding e.g.
    -D__PLUMED_DEFAULT_KERNEL=/opt/local/lib/libplumed.dylib
  */
  /* This is required to add quotes */
#define PLUMED_QUOTE_DIRECT(name) #name
#define PLUMED_QUOTE(macro) PLUMED_QUOTE_DIRECT(macro)
  if(! (path && (*path) )) path=PLUMED_QUOTE(__PLUMED_DEFAULT_KERNEL);
#endif
  if(path && (*path)) {
    fprintf(stderr,"+++ Loading the PLUMED kernel runtime +++\n");
    fprintf(stderr,"+++ PLUMED_KERNEL=\"%s\" +++\n",path);
    if(debug) __PLUMED_FPRINTF(stderr,"+++ Loading with mode RTLD_NOW");
    dlopenmode=RTLD_NOW;
    if(__PLUMED_GETENV("PLUMED_LOAD_NAMESPACE") && !__PLUMED_WRAPPER_STD strcmp(__PLUMED_GETENV("PLUMED_LOAD_NAMESPACE"),"LOCAL")) {
      dlopenmode=dlopenmode|RTLD_LOCAL;
      if(debug) __PLUMED_FPRINTF(stderr,"|RTLD_LOCAL");
    } else {
      dlopenmode=dlopenmode|RTLD_GLOBAL;
      if(debug) __PLUMED_FPRINTF(stderr,"|RTLD_GLOBAL");
    }
#ifdef RTLD_DEEPBIND
    if(!__PLUMED_GETENV("PLUMED_LOAD_NODEEPBIND")) {
      dlopenmode=dlopenmode|RTLD_DEEPBIND;
      if(debug) __PLUMED_FPRINTF(stderr,"|RTLD_DEEPBIND");
    }
#endif
    if(debug) __PLUMED_FPRINTF(stderr," +++\n");
    p=plumed_attempt_dlopen(path,dlopenmode);
    if(p) plumed_search_symbols(p,&g,plumed_symbol_table_ptr);
  }
  if(handle) *handle=p;
  if(functions) *functions=g;
#endif
}
__PLUMED_WRAPPER_INTERNALS_END

/**
  Implementation.
  Small object used to store pointers directly into the plumed object defined in Plumed.h.
  This allows avoiding the extra function call to plumed_retrieve_functions at every cmd,
  at the cost of an extra indirection.
*/
typedef struct {
  /* allows errors with pointers to be found when debugging */
  char magic[6];
  /* reference count */
  int refcount;
  /* handler to dlopened library. NULL if there was no library opened */
  void* dlhandle;
  /* non zero if, upon destruction, the library should be dlclosed */
  int dlclose;
  /* 1 if path to kernel was taken from PLUMED_KERNEL var, 0 otherwise */
  int used_plumed_kernel;
  /* function pointers */
  plumed_plumedmain_function_holder functions;
  /* pointer to the symbol table. NULL if kernel <=2.4 */
  plumed_symbol_table_type* table;
  /* pointer to plumed object */
  void* p;
} plumed_implementation;

__PLUMED_WRAPPER_INTERNALS_BEGIN
plumed_implementation* plumed_malloc_pimpl() {
  plumed_implementation* pimpl;
  /* allocate space for implementation object. this is free-ed in plumed_finalize(). */
  pimpl=(plumed_implementation*) __PLUMED_MALLOC(sizeof(plumed_implementation));
  if(!pimpl) {
    __PLUMED_FPRINTF(stderr,"+++ Allocation error +++\n");
    __PLUMED_WRAPPER_STD abort();
  }
  __PLUMED_WRAPPER_STD memcpy(pimpl->magic,"pLuMEd",6);
  pimpl->refcount=1;
#if __PLUMED_WRAPPER_DEBUG_REFCOUNT
  fprintf(stderr,"refcount: new at %p\n",(void*)pimpl);
#endif
  pimpl->dlhandle=NULL;
  pimpl->dlclose=0;
  pimpl->used_plumed_kernel=0;
  pimpl->functions.create=NULL;
  pimpl->functions.cmd=NULL;
  pimpl->functions.finalize=NULL;
  pimpl->table=NULL;
  pimpl->p=NULL;
  return pimpl;
}
__PLUMED_WRAPPER_INTERNALS_END

#ifndef NDEBUG

__PLUMED_WRAPPER_INTERNALS_BEGIN
int plumed_check_pimpl(plumed_implementation*pimpl) {
  if(!pimpl) return 0;
  if(__PLUMED_WRAPPER_STD memcmp(pimpl->magic,"pLuMEd",6)) return 0;
  return 1;
}
__PLUMED_WRAPPER_INTERNALS_END
#endif

/* C wrappers: */

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create(void) {
  /* returned object */
  plumed p;
  /* pointer to implementation */
  plumed_implementation* pimpl;
  /* allocate space for implementation object. this is free-ed in plumed_finalize(). */
  pimpl=plumed_malloc_pimpl();
  /* store pointers in pimpl */
  plumed_retrieve_functions(&pimpl->functions,&pimpl->table,&pimpl->dlhandle);
#if __PLUMED_WRAPPER_LINK_RUNTIME
  /* note if PLUMED_KERNEL variable was used */
  pimpl->used_plumed_kernel=1;
#endif
  /* note if handle should not be dlclosed */
  pimpl->dlclose=1;
  if(__PLUMED_GETENV("PLUMED_LOAD_DLCLOSE") && !__PLUMED_WRAPPER_STD strcmp(__PLUMED_GETENV("PLUMED_LOAD_DLCLOSE"),"no")) pimpl->dlclose=0;
  /* in case of failure, return */
  /* the resulting object should be plumed_finalized, though you cannot use plumed_cmd */
  if(!pimpl->functions.create) {
    /* store pimpl in returned object */
    p.p=pimpl;
    return p;
  }
  assert(pimpl->functions.cmd);
  assert(pimpl->functions.finalize);
  /* obtain object */
  pimpl->p=(*(pimpl->functions.create))();
  /* notice: we do not assert pimpl->p since in principle it might be nullptr */
  /* user might identify this using plumed_valid() */
  /* store pimpl in returned object */
  p.p=pimpl;
  return p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_dlopen(const char*path) {
  int dlopenmode;
  /* plumed_create_dlopen always uses RTLD_LOCAL and, when possible, RTLD_DEEPBIND to allow multiple versions */
#ifdef __PLUMED_HAS_DLOPEN
  dlopenmode=RTLD_NOW|RTLD_LOCAL;
#ifdef RTLD_DEEPBIND
  dlopenmode=dlopenmode|RTLD_DEEPBIND;
#endif
#else
  dlopenmode=0;
#endif
  return plumed_create_dlopen2(path,dlopenmode);
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_dlopen2(const char*path,int mode) {
  /* returned object */
  plumed p;
  /* pointer to implementation */
  plumed_implementation* pimpl;
  /* allocate space for implementation object. this is free-ed in plumed_finalize(). */
  pimpl=plumed_malloc_pimpl();
#ifdef __PLUMED_HAS_DLOPEN
  if(path) pimpl->dlhandle=plumed_attempt_dlopen(path,mode);
  /* mark this library to be dlclosed when the object is finalized */
  pimpl->dlclose=1;
  if(pimpl->dlhandle) plumed_search_symbols(pimpl->dlhandle,&pimpl->functions,&pimpl->table);
#endif
  if(!pimpl->functions.create) {
    p.p=pimpl;
    return p;
  }
  assert(pimpl->functions.cmd);
  assert(pimpl->functions.finalize);
  /* obtain object */
  pimpl->p=(*(pimpl->functions.create))();
  /* notice: we do not assert pimpl->p since in principle it might be nullptr */
  /* user might identify this using plumed_valid() */
  /* store pimpl in returned object */
  p.p=pimpl;
  return p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference(plumed p) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  /* increase reference count */
  pimpl->refcount++;
#if __PLUMED_WRAPPER_DEBUG_REFCOUNT
  fprintf(stderr,"refcount: increase at %p\n",(void*)pimpl);
#endif
  return p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference_v(void*v) {
  return plumed_create_reference(plumed_v2c(v));
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_reference_f(const char*f) {
  return plumed_create_reference(plumed_f2c(f));
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_create_invalid() {
  plumed p;
  plumed_implementation* pimpl;
  pimpl=plumed_malloc_pimpl();
  p.p=pimpl;
  return p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd(plumed p,const char*key,const void*val) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  if(!pimpl->p) {
    __PLUMED_FPRINTF(stderr,"+++ ERROR: You are trying to use an invalid plumed object. +++\n");
    if(pimpl->used_plumed_kernel) __PLUMED_FPRINTF(stderr,"+++ Check your PLUMED_KERNEL environment variable. +++\n");
    __PLUMED_WRAPPER_STD exit(1);
  }
  assert(pimpl->functions.create);
  assert(pimpl->functions.cmd);
  assert(pimpl->functions.finalize);
  /* execute */
  (*(pimpl->functions.cmd))(pimpl->p,key,val);
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_safe_nothrow(plumed p,const char*key,plumed_safeptr safe,plumed_nothrow_handler nothrow) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  if(!pimpl->p) {
    if(pimpl->used_plumed_kernel) {
      nothrow.handler(nothrow.ptr,1,"You are trying to use plumed, but it is not available.\nCheck your PLUMED_KERNEL environment variable.",NULL);
    } else {
      nothrow.handler(nothrow.ptr,1,"You are trying to use plumed, but it is not available.",NULL);
    }
    return;
  }
  assert(pimpl->functions.create);
  assert(pimpl->functions.cmd);
  assert(pimpl->functions.finalize);
  /* execute */
  if(pimpl->table && pimpl->table->version>2) (*(pimpl->table->cmd_safe_nothrow))(pimpl->p,key,safe,nothrow);
  else if(pimpl->table && pimpl->table->version>1) (*(pimpl->table->cmd_nothrow))(pimpl->p,key,safe.ptr,nothrow);
  else (*(pimpl->functions.cmd))(pimpl->p,key,safe.ptr);
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_nothrow(plumed p,const char*key,const void*val,plumed_nothrow_handler nothrow) {
  plumed_safeptr safe;
  safe.ptr=val;
  safe.flags=0;
  safe.nelem=0;
  safe.shape=NULL;
  safe.opt=NULL;
  plumed_cmd_safe_nothrow(p,key,safe,nothrow);
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void plumed_cmd_safe(plumed p,const char*key,plumed_safeptr safe) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  if(!pimpl->p) {
    __PLUMED_FPRINTF(stderr,"+++ ERROR: You are trying to use an invalid plumed object. +++\n");
    if(pimpl->used_plumed_kernel) __PLUMED_FPRINTF(stderr,"+++ Check your PLUMED_KERNEL environment variable. +++\n");
    __PLUMED_WRAPPER_STD exit(1);
  }
  assert(pimpl->functions.create);
  assert(pimpl->functions.cmd);
  assert(pimpl->functions.finalize);
  /* execute */
  if(pimpl->table && pimpl->table->version>2) (*(pimpl->table->cmd_safe))(pimpl->p,key,safe);
  else (*(pimpl->functions.cmd))(pimpl->p,key,safe.ptr);
}
__PLUMED_WRAPPER_C_END


__PLUMED_WRAPPER_C_BEGIN
void plumed_finalize(plumed p) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  /* decrease reference count */
  pimpl->refcount--;
#if __PLUMED_WRAPPER_DEBUG_REFCOUNT
  fprintf(stderr,"refcount: decrease at %p\n",(void*)pimpl);
#endif
  if(pimpl->refcount>0) return;
  /* to allow finalizing an invalid plumed object, we only call
     finalize if the object is valid */
  if(pimpl->p) {
    assert(pimpl->functions.create);
    assert(pimpl->functions.cmd);
    assert(pimpl->functions.finalize);
    /* finalize */
    (*(pimpl->functions.finalize))(pimpl->p);
  }
#ifdef __PLUMED_HAS_DLOPEN
  /* dlclose library */
  if(pimpl->dlhandle && pimpl->dlclose) {
    if(__PLUMED_GETENV("PLUMED_LOAD_DEBUG")) fprintf(stderr,"+++ Unloading library\n");
    dlclose(pimpl->dlhandle);
  }
#endif
#if __PLUMED_WRAPPER_DEBUG_REFCOUNT
  fprintf(stderr,"refcount: delete at %p\n",(void*)pimpl);
#endif
  /* free pimpl space */
  __PLUMED_FREE(pimpl);
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
int plumed_valid(plumed p) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  if(pimpl->p) return 1;
  else return 0;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
int plumed_use_count(plumed p) {
  plumed_implementation* pimpl;
  /* obtain pimpl */
  pimpl=(plumed_implementation*) p.p;
  assert(plumed_check_pimpl(pimpl));
  return pimpl->refcount;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
int plumed_installed(void) {
  plumed p;
  int result;
  p=plumed_create();
  result=plumed_valid(p);
  plumed_finalize(p);
  return result;
}
__PLUMED_WRAPPER_C_END

#if __PLUMED_WRAPPER_GLOBAL /*{*/

__PLUMED_WRAPPER_EXTERN_C_BEGIN

/* we declare a Plumed_g_main object here, in such a way that it is always available */

static plumed plumed_gmain= {NULL};

plumed plumed_global(void) {
  return plumed_gmain;
}

void plumed_gcreate(void) {
  /* should be created once */
  assert(plumed_gmain.p==NULL);
  plumed_gmain=plumed_create();
}

void plumed_gcmd(const char*key,const void*val) {
  plumed_cmd(plumed_gmain,key,val);
}

/* cppcheck-suppress passedByValue */
void plumed_gcmd_safe(const char*key,plumed_safeptr safe) {
  plumed_cmd_safe(plumed_gmain,key,safe);
}

void plumed_gfinalize(void) {
  plumed_finalize(plumed_gmain);
  plumed_gmain.p=NULL;
}

int plumed_ginitialized(void) {
  if(plumed_gmain.p) return 1;
  else        return 0;
}

int plumed_gvalid() {
  assert(plumed_gmain.p);
  return plumed_valid(plumed_gmain);
}

__PLUMED_WRAPPER_EXTERN_C_END

#endif /*}*/

__PLUMED_WRAPPER_C_BEGIN
void plumed_c2f(plumed p,char*c) {
  unsigned i;
  unsigned char* cc;
  /*
    Convert the address stored in p.p into a proper FORTRAN string
    made of only ASCII characters. For this to work, the two following
    assertions should be satisfied:
  */
  assert(CHAR_BIT<=12);
  assert(sizeof(p.p)<=16);

  assert(c);
  cc=(unsigned char*)&p.p;
  for(i=0; i<sizeof(p.p); i++) {
    /*
      characters will range between '0' (ASCII 48) and 'o' (ASCII 111=48+63)
    */
    /* cppcheck-suppress objectIndex */
    c[2*i]=cc[i]/64+48;
    /* cppcheck-suppress objectIndex */
    c[2*i+1]=cc[i]%64+48;
  }
  for(; i<16; i++) {
    c[2*i]=' ';
    c[2*i+1]=' ';
  }
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_f2c(const char*c) {
  plumed p;
  unsigned i;
  unsigned char* cc;

  assert(CHAR_BIT<=12);
  assert(sizeof(p.p)<=16);

  assert(c);

  /*
     needed to avoid cppcheck warning on uninitialized p
  */
  p.p=NULL;
  cc=(unsigned char*)&p.p;
  for(i=0; i<sizeof(p.p); i++) {
    assert(c[2*i]>=48 && c[2*i]<48+64);
    assert(c[2*i+1]>=48 && c[2*i+1]<48+64);
    /*
      perform the reversed transform
    */
    /* cppcheck-suppress objectIndex */
    cc[i]=(c[2*i]-48)*64 + (c[2*i+1]-48);
  }
  for(; i<16; i++) {
    assert(c[2*i]==' ');
    assert(c[2*i+1]==' ');
  }
  return p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
void* plumed_c2v(plumed p) {
  assert(plumed_check_pimpl((plumed_implementation*)p.p));
  return p.p;
}
__PLUMED_WRAPPER_C_END

__PLUMED_WRAPPER_C_BEGIN
plumed plumed_v2c(void* v) {
  assert(plumed_check_pimpl((plumed_implementation*)v));
  plumed p;
  p.p=v;
  return p;
}
__PLUMED_WRAPPER_C_END

#if __PLUMED_WRAPPER_FORTRAN /*{*/

/*
  Fortran wrappers
  These are just like the global C wrappers. They are
  just defined here and not declared since they
  should not be used from c/c++ anyway.

  We use a macro that does the following:
  - declare a static function named NAME_static
  - declare a number of functions named NAME_ etc, with all possible
    fortran mangling schemes (zero, one, or two underscores, lower and upper case)
  - define the NAME_static function.

  The static function is used basically as an inline function in a C-compatible manner.
*/

#define __PLUMED_IMPLEMENT_FORTRAN(lower,upper,arg1,arg2) \
  static void lower ## _static arg1; \
  extern void lower      arg1 {lower ## _static arg2;} \
  extern void lower ##_  arg1 {lower ## _static arg2;} \
  extern void lower ##__ arg1 {lower ## _static arg2;} \
  extern void upper      arg1 {lower ## _static arg2;} \
  extern void upper ##_  arg1 {lower ## _static arg2;} \
  extern void upper ##__ arg1 {lower ## _static arg2;} \
  static void lower ## _static arg1

/* FORTRAN wrappers would only make sense as extern "C" */

__PLUMED_WRAPPER_EXTERN_C_BEGIN

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_create,PLUMED_F_CREATE,(char*c),(c)) {
  plumed_c2f(plumed_create(),c);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_create_dlopen,PLUMED_F_CREATE_DLOPEN,(char*path,char*c),(path,c)) {
  plumed_c2f(plumed_create_dlopen(path),c);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_create_reference,PLUMED_F_CREATE_REFERENCE,(char* r,char*c),(r,c)) {
  plumed_c2f(plumed_create_reference_f(r),c);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_create_invalid,PLUMED_F_CREATE_INVALID,(char* c),(c)) {
  plumed_c2f(plumed_create_invalid(),c);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_cmd,PLUMED_F_CMD,(char*c,char*key,void*val),(c,key,val)) {
  plumed_cmd(plumed_f2c(c),key,val);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_finalize,PLUMED_F_FINALIZE,(char*c),(c)) {
  plumed_finalize(plumed_f2c(c));
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_installed,PLUMED_F_INSTALLED,(int*i),(i)) {
  assert(i);
  *i=plumed_installed();
}

/* New in PLUMED 2.5 */
__PLUMED_IMPLEMENT_FORTRAN(plumed_f_valid,PLUMED_F_VALID,(char*c,int*i),(c,i)) {
  assert(i);
  *i=plumed_valid(plumed_f2c(c));
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_use_count,PLUMED_F_USE_COUNT,(char*c,int*i),(c,i)) {
  assert(i);
  *i=plumed_use_count(plumed_f2c(c));
}

/* New in PLUMED 2.8 */

#define __PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE_INNER(type,type_,code,suffix) \
void plumed_f_cmd_safe_ ## type_ ## suffix(char*c,char*key,type*val,__PLUMED_WRAPPER_STD size_t*shape) { \
  plumed_safeptr safe; \
  safe.ptr=val; \
  safe.nelem=0; \
  safe.shape=shape; \
  safe.flags= 0x2000000*2 + 0x10000*code + sizeof(type); \
  safe.opt=NULL; \
  plumed_cmd_safe(plumed_f2c(c),key,safe); \
}

#define __PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(type,type_,code) \
  __PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE_INNER(type,type_,code,) \
  __PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE_INNER(type,type_,code,_scalar)

__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(float,float,4)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(double,double,4)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(long double,long_double,4)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(int,int,3)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(short,short,3)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(long,long,3)
__PLUMED_IMPLEMENT_FORTRAN_CMD_SAFE(char,char,3)

#if __PLUMED_WRAPPER_GLOBAL /*{*/

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_global,PLUMED_F_GLOBAL,(char*c),(c)) {
  plumed_c2f(plumed_gmain,c);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_ginitialized,PLUMED_F_GINITIALIZED,(int*i),(i)) {
  assert(i);
  *i=plumed_ginitialized();
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_gcreate,PLUMED_F_GCREATE,(void),()) {
  plumed_gcreate();
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_gcmd,PLUMED_F_GCMD,(char*key,void*val),(key,val)) {
  plumed_gcmd(key,val);
}

__PLUMED_IMPLEMENT_FORTRAN(plumed_f_gfinalize,PLUMED_F_GFINALIZE,(void),()) {
  plumed_gfinalize();
}

/* New in PLUMED 2.5 */
__PLUMED_IMPLEMENT_FORTRAN(plumed_f_gvalid,PLUMED_F_GVALID,(int*i),(i)) {
  assert(i);
  *i=plumed_gvalid();
}

/* New in PLUMED 2.8 */

#define __PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE_INNER(type,type_,code, suffix) \
void plumed_f_gcmd_safe_ ## type_ ## suffix(char*key,type*val,__PLUMED_WRAPPER_STD size_t*shape) { \
  plumed_safeptr safe; \
  safe.ptr=val; \
  safe.nelem=0; \
  safe.shape=shape; \
  safe.flags= 0x2000000*2 + 0x10000*code + sizeof(type); \
  safe.opt=NULL; \
  plumed_gcmd_safe(key,safe); \
}

#define __PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(type,type_,code) \
  __PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE_INNER(type,type_,code,) \
  __PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE_INNER(type,type_,code,_scalar)

__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(float,float,4)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(double,double,4)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(long double,long_double,4)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(int,int,3)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(short,short,3)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(long,long,3)
__PLUMED_IMPLEMENT_FORTRAN_GCMD_SAFE(char,char,3)

#endif /*}*/

__PLUMED_WRAPPER_EXTERN_C_END

#endif /*}*/

#endif /*}*/

#endif /*}*/

/* END OF DEFINITIONS */

/* reset variable to allow it to be redefined upon re-inclusion */

#undef __PLUMED_WRAPPER_IMPLEMENTATION_

/* this macro is set in declarations */
#ifdef __PLUMED_WRAPPER_REDEFINE_CMD
#if defined(plumed_cmd)
#undef plumed_cmd
#endif
#define plumed_cmd __PLUMED_WRAPPER_REDEFINE_CMD
#endif

/* this macro is set in declarations */
#ifdef __PLUMED_WRAPPER_REDEFINE_GCMD
#if defined(plumed_gcmd)
#undef plumed_gcmd
#endif
#define plumed_gcmd __PLUMED_WRAPPER_REDEFINE_GCMD
#endif

