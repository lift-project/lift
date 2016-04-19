cmake_minimum_required(VERSION 2.8)
project(Executor)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMakeModules/")

include_directories(common)
include_directories(include)

find_package(OpenCL)

set(SOURCE_FILES
    src/run.cpp
    src/file_utils.cpp
    src/csv_utils.cpp
    src/opencl_utils.cpp)

add_library (Executor ${SOURCE_FILES})

function(add_app name)
    add_executable(harness_${name} apps/${name}.cpp)
    target_link_libraries(harness_${name} Executor pthread ${OpenCL_LIBRARIES})
endfunction()

add_app(mm)
add_app(mv)
add_app(gemv)
#add_app(mandelbrot)
