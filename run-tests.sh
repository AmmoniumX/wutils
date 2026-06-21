#!/usr/bin/env bash

cmake --build build/debug && ctest --test-dir build/debug --output-on-failure
cmake --build build/release && ctest --test-dir build/release --output-on-failure
