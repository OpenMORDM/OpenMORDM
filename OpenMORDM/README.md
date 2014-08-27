# OpenMORDM #

OpenMORDM is an open-source R library for multiobjective robust decision making (MORDM).
It includes support for loading optimization output files (from [Borg](http://www.borgmoea.org/)
or the [MOEA Framework](http://www.moeaframework.org/)), visualizing the data sets using
various 2D and 3D plots, performing scenario discovery and tradeoff analysis, and computing
uncertainty/robustness metrics.

Note: This software requires a custom version of RGL and ShinyRGL.  Other software that uses
RGL or ShinyRGL should be compatible, but if problems arise, you can always reinstall the
original versions from CRAN.

### Prerequisite Software ###
1. A compatible C/C++ compiler
    * Windows - Install RTools (http://cran.r-project.org/bin/windows/Rtools/)
    * Linux - Install GCC (e.g., `sudo apt-get install build-essential`)
    * Mac OS X - Install XCode
2. [Git](http://git-scm.com/downloads) (for downloading the source code)
3. [R version 3.0](http://www.r-project.org/) or later

### Installation ###

* Install the prerequisite software
* Clone the OpenMORDM repository: `git clone https://bitbucket.org/dmh309/openmordm.git`
* Start R: `R`
* If using Windows, run the following command:
  * `options(devtools.install.args="--force-biarch")`
* Run the following commands:
  * `install_github("rgl", "dhadka", "mobile")`
  * `install_github("shinyRGL", "dhadka")`
  * `install_local("path/to/openmordm/OpenMORDM")`

### Running the Demo ###

* Start R: `R`
* Load the OpenMORDM library: `library(OpenMORDM)`
* Run the demo: `runVisDemo()`

### Notes ###
* Linux users may need to run R with admin permissions when installing: `sudo R`
* Mac OS X users who see an error about llvm-g++-4.2 missing, run:
    * `cd /usr/bin`
    * `sudo ln -fs clang llvm-gcc-4.2`
    * `sudo ln -fs clang++ llvm-g++-4.2`