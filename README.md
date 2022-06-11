# Exploratory Visual Modeling `modelcheck` R Package API

This is the codebase for the `modelcheck` R Package, which is [hosted on an OpenCPU server](https://www.opencpu.org/cloud.html) at https://cloud.opencpu.org/ocpu/apps/kalealex/modelcheck/
You can find the corresponding github repository at https://github.com/kalealex/modelcheck

## Directory description

The `DESCRIPTION` and `NAMESPACE` files list the dependencies and functions of the package. 
The `R/` directory contains the code for each function exported by the `modelcheck` package.
These functions run on a server in response to POST requests from client-side Javascript in [Exploratory Visual Modeling (EVM)](https://github.com/MUCollective/exploratory_modeling). 
The only function currently used by EVM is `add_model` in the file `evm.R`. This is described in Chapter 6 as a main event handler.
The other `.R` files contain the component fucntions of `add_model`, which were called via separate POST requests in a previous version of EVM.