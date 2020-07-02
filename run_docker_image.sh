#!/usr/bin/env bash
docker run --rm -ti --volume $PWD:$PWD -w $PWD --volume ~/.cache:/.cache --user "$(id -u):$(id -g)" demo_ps-env "$@"

