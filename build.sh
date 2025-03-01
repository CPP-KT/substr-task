#!/usr/bin/env bash

BUILD_TYPE=${1:-SanitizedDebug}

# Configure CMake
cmake -S . -B build-"$BUILD_TYPE" --preset "$BUILD_TYPE"

# Build
cmake --build build-"$BUILD_TYPE" -j
