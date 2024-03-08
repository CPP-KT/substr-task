#!/bin/bash
set -euo pipefail

BUILD_TYPE=$1

SCRIPT_DIR="$(realpath "$(dirname "${BASH_SOURCE[0]}")")"

valgrind --tool=memcheck \
  --gen-suppressions=all \
  --leak-check=full \
  --show-leak-kinds=all \
  --leak-resolution=med \
  --track-origins=yes \
  --vgdb=no \
  --error-exitcode=1 \
  --suppressions="${SCRIPT_DIR}/valgrind.suppressions" \
  stack run cmake-build-"$BUILD_TYPE"/substr
