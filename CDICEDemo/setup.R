# Copyright 2014 The Pennsylvania State University
#
# OpenMORDM was developed by Dr. David Hadka with guidance from Dr. Klaus
# Keller and Dr. Patrick Reed.  This work was supported by the National
# Science Foundation through the Network for Sustainable Climate Risk
# Management (SCRiM) under NSF cooperative agreement GEO-1240507.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# This script will attempt to setup the OpenMORDM Lake Problem demo.  Some of
# the R libraries will need a compatible C compiler.  If you do not have a
# compatible compiler or experience compilation issues, try installing RTools
# from http://cran.r-project.org/bin/windows/Rtools/.  Run this script from the
# <OpenMORDM Directory>/demo/ folder.

# Configuration options
install.local <- FALSE         # install from local Git clones
install.local.dir <- "E:/Git/" # directory containing local Git clones
install.data <- FALSE          # install data files (if downloaded)
install.run.after <- FALSE     # start shiny server when finished

# Check if the working directory contains the required files
required.files <- c("OpenMORDM.R", "evaluate.R", "plischke.R", "pareto.R",
					"server.R", "ui.R")
required.files.exist <- TRUE

for (name in required.files) {
	if (!file.exists(file.path(getwd(), name))) {
		required.files.exist <- FALSE
		cat(sprintf("Missing required file: %s\n", name))
	}
}

if (!required.files.exist) {
	stop("Missing required file(s), please ensure this script is run from the demo folder");
}

# Install any required packages
cat("Installing required libraries\n");

required.libraries <- c("shiny", "shinyRGL", "rgl", "scales", "R.cache",
						"grid", "prim", "MASS", "animation", "sensitivity",
						"boot", "pracma", "emoa", "devtools", "stringr",
						"functional", "dichromat")

for (name in required.libraries) {
	if (! name %in% rownames(installed.packages())) {
		install.packages(name, dependencies=TRUE)
	}
}

# Load the devtools library that will allow us to install the custom libraries
cat("Installing custom libraries\n")

library(devtools)

# For some reason (haven't had time to debug), shinyRGL does not install with
# the custom RGL.  So first install the default RGL, install our custom version
# of shiny, then install the custom RGL.
install.packages("rgl")

if (install.local) {
	install_local(file.path(install.local.dir, "shinyRGL"))
	install_local(file.path(install.local.dir, "rgl"))	
} else {
	install_github("shinyRGL", "dhadka")
	install_github("rgl", "dhadka", "js-class", args=c("--enable-libpng"))
}

# Unzip the data files - these are available on the "demo" branch
if (install.data) {
	response <- "y"
	
	if ((file.exists("data.zip") || file.exists("cache.zip")) && (file.exists("data") || file.exists("cache"))) {
		cat("Data folders already exists, overwrite? [y/n]: ")
		response <- readline()
	}
	
	if (grepl("[yY]", response)) {
		if (file.exists("data.zip")) {
			cat("Unzipping data.zip\n")
			unzip("data.zip")
		} else {
			cat("Unable to locate data.zip, please download and try again.\n")
		}
		
		if (file.exists("cache.zip")) {
			cat("Unzipping cache.zip\n")
			unzip("cache.zip")
		} else {
			cat("Unable to locate cache.zip, please download and try again.\n")
		}
	}
}

cat("Installation finished!\n")

# Run the demo
if (install.run.after) {
	cat("Starting shiny server\n")
	library(shiny)
	runApp(getwd())
}
