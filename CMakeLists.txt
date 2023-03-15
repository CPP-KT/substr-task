cmake_minimum_required(VERSION 3.17)
project(substr_task)

set(CMAKE_CXX_STANDARD 20)

add_executable(solution solution.cpp)

if (NOT MSVC)
    target_compile_options(solution PRIVATE -Werror -Wall -Wextra -Wshadow=compatible-local -Wno-sign-compare -pedantic)
endif()
