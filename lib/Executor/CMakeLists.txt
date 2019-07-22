cmake_minimum_required (VERSION 2.6)

if (${WIN32})
    # /Gz flag forses the compiler to use _stdcall calling convention as required by Win32 API as opposed to the default _cdecl
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++latest /Gz /W4")
endif (${WIN32})

# set compiler and default options
if (${UNIX})
  if (${APPLE})
    set (CMAKE_CXX_COMPILER "clang++" CACHE STRING "CXX Compiler.")
    set (CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++ -DGTEST_USE_OWN_TR1_TUPLE=1 -Wall -Wextra"
      CACHE STRING "Flags used by the compiler during all build types.")
  else (${APPLE})
    set (CMAKE_CXX_FLAGS "-std=c++0x -Wall -Wextra -pthread"
      CACHE STRING "Flags used by the compiler during all build types.")
  endif (${APPLE})
endif (${UNIX})

# include custom cmake modules (e.g. FindOpenCL.cmake)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake-modules)

project (Executor)

option (BUILD_SHARED_LIBS "Build shared libraries (DLLs)." ON)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release)
endif (NOT CMAKE_BUILD_TYPE)

find_package (OpenCL)
find_package (JNI)

include_directories (
  "${PROJECT_SOURCE_DIR}/include/executor"
  "${PROJECT_SOURCE_DIR}/include"
  "${JNI_INCLUDE_DIRS}")

set (SOURCES
  src/Core.cpp
  src/Device.cpp
  src/DeviceBuffer.cpp
  src/DeviceID.cpp
  src/DeviceList.cpp
  src/DeviceProperties.cpp
  src/Event.cpp
  src/Executor.cpp
  src/GlobalArg.cpp
  src/JNIHandle.cpp
  src/Kernel.cpp
  src/KernelArg.cpp
  src/LocalArg.cpp
  src/PlatformID.cpp
  src/Source.cpp
  src/ValueArg.cpp

  src/util/Assert.cpp
  src/util/Logger.cpp

  src/jni/Handle.cpp
  src/jni/opencl_executor_Executor.cpp
  src/jni/opencl_executor_GlobalArg.cpp
  src/jni/opencl_executor_JNIHandle.cpp
  src/jni/opencl_executor_Kernel.cpp
  src/jni/opencl_executor_LocalArg.cpp
  src/jni/opencl_executor_ValueArg.cpp
  )


add_library (executor-jni ${SOURCES})
if (${WIN32})
    target_link_libraries (executor-jni ${OPENCL_LIBRARIES} ${JAVA_JVM_LIBRARY})
else (${WIN32})
    target_link_libraries (executor-jni ${OPENCL_LIBRARIES} ${JAVA_JVM_LIBRARY} pthread)
endif (${WIN32})

install(TARGETS executor-jni DESTINATION lib)

