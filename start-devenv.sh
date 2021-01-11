#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${CURRENT_DIR}"

export LC_ALL="C"
dosbox -conf "dosbox-devenv.conf" -no-console

