#!/bin/bash

BUILD_TYPE=${1:-SanitizedDebug}

# Configure CMake
cmake -S . -B cmake-build-"$BUILD_TYPE" --preset "$BUILD_TYPE"

# Build
cmake --build cmake-build-"$BUILD_TYPE"
