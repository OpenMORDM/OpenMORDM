# OpenMORDM #

OpenMORDM is an open-source R library for multiobjective robust decision making (MORDM).
It includes support for loading datasets from a number of sources including
CSV, XLS, XLSX, optimization output files (from [Borg](http://www.borgmoea.org/)
or the [MOEA Framework](http://www.moeaframework.org/)), and R matrices and data frames;
visualizing the data sets using various 2D and 3D plots; performing scenario
discovery and tradeoff analysis; and computing uncertainty/robustness metrics.
OpenMORDM also includes a web-based data exploration and visualization toolkit
that can be launched through the `runVisDemo` or `explore` commands.

### Note on Custom Packages ###
This software requires a custom version of RGL and ShinyRGL.  Other software that uses
RGL or ShinyRGL should be compatible, but if problems arise, you can always reinstall the
original versions from CRAN.  This software will still work with the original versions,
but the web-based visualization tool will not support picking and will reorient to the
default view every time the 3D scene is redrawn.

Note: The latest versions of RGL appear to have a bug in `rgl.setMouseCallbacks`
(see Bug 5928 on r-forge).  Until this is resolved, we have created the
`working` branch for RGL that uses an earlier version of RGL.

### Prerequisite Software ###
1. A compatible C/C++ compiler
    * Windows - Install RTools (http://cran.r-project.org/bin/windows/Rtools/)
    * Linux - Install GCC (e.g., `sudo apt-get install build-essential`)
    * Mac OS X - Install XCode
2. [Git](http://git-scm.com/downloads) (for downloading the source code)
3. [R version 3.0](http://www.r-project.org/) or later

### Installation ###
1. Install the prerequisite software
2. Clone the OpenMORDM repository: `git clone https://bitbucket.org/dmh309/openmordm.git`
3. Start R: `R`
4. If using Windows, run the following command:
    * `options(devtools.install.args="--force-biarch")`
5. Run the following commands:
    * `install_github("rgl", "dhadka", "working")`
    * `install_github("shinyRGL", "dhadka")`
    * `install_local("C:/path/to/openmordm/OpenMORDM")`

### Running the Demo ###
1. Start R: `R`
2. Load the OpenMORDM library: `library(OpenMORDM)`
3. Run the demo: `runVisDemo()`
4. Use `explore` to visualize CSV or XLS files, matrices, or data.frames: `library(datasets); data(iris); explore(iris)`

### Building the R Package on Windows ###
1. Open a new command prompt window
2. You may need to setup the PATH to include:
    * `C:\Program Files\RTools\bin`
    * `C:\Program Files\RTools\gcc-4.6.3\bin`
    * `C:\Program Files\R\R-3.1.1\bin` (or your installed version of R)
    * `C:\Program Files (x86)\TeXLive\2011\bin\win32` (or any other LaTeX build)
3. Start R and run `roxygen2::roxygenize("C:/path/to/openmordm/OpenMORDM")`
4. Also within R, run `promptPackage("OpenMORDM", filename="C:/path/to/openmordm/OpenMORDM/man/OpenMORDM-package.Rd", final=TRUE)`
5. Run `R CMD check --preclean --force-biarch C:/path/to/openmordm/OpenMORDM`
6. Run `R CMD build C:/path/to/openmordm/OpenMORDM`
7. Alternatively, upload to [win-builder.r-project.org](win-builder.r-project.org) to build a binary package

### Troubleshooting ###
* Linux users may need to run R with admin permissions when installing: `sudo R`
* Mac OS X users who see an error about `llvm-g++-4.2` missing, run:
    * `cd /usr/bin`
    * `sudo ln -fs clang llvm-gcc-4.2`
    * `sudo ln -fs clang++ llvm-g++-4.2`
* When running the Shiny web visualizations, we must set `options(rgl.useNULL=TRUE)`.  This is required to run on headless systems.  If you try to run `mordm.plot` and do not see the 3D window, you may need to restart R, set `options(rgl.useNULL=FALSE)`, then `library(OpenMORDM)`.

