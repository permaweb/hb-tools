#!/bin/bash

ENVIRONMENT="${1:-local}"  # Defaults to "local" if no argument is provided

rebar3 shell --eval "loadhb_main:main([\"$ENVIRONMENT\"]), init:stop()."