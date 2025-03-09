#!/usr/bin/env bash
set -euo pipefail

BUILD_TYPE=$1

if [[ $BUILD_TYPE == Sanitized* ]]; then
  export ASAN_OPTIONS=alloc_dealloc_mismatch=0
fi

SUBSTR_EXE="$(realpath cmake-build-"$BUILD_TYPE"/substr)"

pushd test

if [[ $BUILD_TYPE == 'Debug' && -n $(which gdb) && $(uname -s) != MINGW* ]]; then
  gdb -q -return-child-result --batch \
    -ex 'handle SIGHUP nostop pass' \
    -ex 'handle SIGQUIT nostop pass' \
    -ex 'handle SIGPIPE nostop pass' \
    -ex 'handle SIGALRM nostop pass' \
    -ex 'handle SIGTERM nostop pass' \
    -ex 'handle SIGUSR1 nostop pass' \
    -ex 'handle SIGUSR2 nostop pass' \
    -ex 'handle SIGCHLD nostop pass' \
    -ex 'set style enabled on' \
    -ex 'set print frame-arguments all' \
    -ex 'set print inferior-events off' \
    -ex 'set print thread-events off' \
    -ex 'run' \
    -ex 'thread apply all bt -frame-info source-and-location -full' \
    --args stack run "$SUBSTR_EXE"
else
  stack run "$SUBSTR_EXE"
fi

popd
