#!/bin/bash

set -ev

YCMD_VENV_DIR=${HOME}/venvs/ycmd_test

# Requirements of OS-specific install:
#  - install any software which is not installed by Travis configuration
#  - create (but don't activate) a virtualenv for the python version
#    ${YCMD_PYTHON_VERSION} in the directory ${YCMD_VENV_DIR}, e.g.
#    virtualenv -p python${YCMD_PYTHON_VERSION} ${YCMD_VENV_DIR}
source ci/travis/travis_install.${TRAVIS_OS_NAME}.sh

# virtualenv doesn't copy python-config https://github.com/pypa/virtualenv/issues/169
# but our build system uses it
cp /usr/bin/python${YCMD_PYTHON_VERSION}-config ${YCMD_VENV_DIR}/bin/python-config

# virtualenv script is noisy, so don't print every command
set +v
source ${YCMD_VENV_DIR}/bin/activate
set -v

# It is quite easy to get the above series of steps wrong. Verify that the
# version of python actually in the path and used is the version that was
# requested, and fail the build if we broke the travis setup
python_version=$(python -c 'import sys; print "{0}.{1}".format( sys.version_info[0], sys.version_info[1] )')
echo "Checking python version (actual ${python_version} vs expected ${YCMD_PYTHON_VERSION})"
test ${python_version} == ${YCMD_PYTHON_VERSION}

pip install -U pip wheel setuptools
pip install -r test_requirements.txt
npm install -g typescript

# We run coverage tests only on a single build, where COVERAGE=true
if [ x"${COVERAGE}" = x"true" ]; then
  pip install coveralls
fi

# Need rust available, but travis doesn't give it to you without language: rust
pushd ${HOME}
git clone --recursive https://github.com/brson/multirust
cd multirust
git reset --hard f3974f2b966476ad656afba311b50a9c23fe6d2e
./build.sh
./install.sh --prefix=${HOME}
popd

multirust update stable
multirust default stable

# The build infrastructure prints a lot of spam after this script runs, so make
# sure to disable printing, and failing on non-zero exit code after this script
# finishes
set +ev
