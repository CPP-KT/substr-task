cmake_minimum_required(VERSION 3.17)
project(substr_task)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-sign-compare -pedantic-errors")

add_executable(solution solution.cpp)
