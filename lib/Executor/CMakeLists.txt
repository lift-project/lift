cmake_minimum_required(VERSION 2.8)
project(Executor)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")

include_directories(common)
include_directories(include)

set(SOURCE_FILES src/main.cpp src/run.cpp src/file_utils.cpp)
add_executable(Executor ${SOURCE_FILES})
target_link_libraries(Executor OpenCL)