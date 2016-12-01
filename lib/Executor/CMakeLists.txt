cmake_minimum_required (VERSION 2.6)

if (${WIN32})
  message ("Build on Windows not (yet) supported.")
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

  set (EXECUTOR_CXX_FLAGS "-Weffc++ -Wold-style-cast -Woverloaded-virtual -Wsign-promo -Werror -pedantic-errors -Wfatal-errors"
    CACHE STRING "More strict flags used to compile the Executor sources.")
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

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${EXECUTOR_CXX_FLAGS}")

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
  src/PlatformID.cpp
  src/Source.cpp

  src/util/Assert.cpp
  src/util/Logger.cpp

  src/jni/handle.cpp
  src/jni/jni-executor.cpp
  src/jni/globalArg.cpp
  src/jni/kernelArg.cpp
  src/jni/localArg.cpp
  src/jni/valueArg.cpp
  )


add_library (executor-jni ${SOURCES})
target_link_libraries (executor-jni ${OPENCL_LIBRARIES} ${JNI_LIBRARIES} pthread)


