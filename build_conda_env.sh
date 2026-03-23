#!/bin/bash

# Path to conda installation
CONDA_PATH=$(conda info --base)

# Make conda tools visible
source "${CONDA_PATH}"/etc/profile.d/conda.sh

conda create -n regressive-autism-env -c conda-forge r-base=4.5.2 -y
conda install -n regressive-autism-env -c conda-forge --file R_packages_conda.txt -y

