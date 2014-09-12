\name{OpenMORDM-package}
\alias{OpenMORDM-package}
\alias{OpenMORDM}
\docType{package}
\title{
A library for multiobjective robust decision making (MORDM) and
visualizing high-dimensional datasets.
}
\description{
OpenMORDM provides support for visualizing high-dimensional
    datasets loaded from matrices or data.frames and a variety of file formats
    including CSV, XLS, XLSX, and MOEA runtime outputs (from the Borg MOEA or
    MOEA Framework).  This visualization is contained within a web-based viewer
    capable of generating various 2D and 3D plots as well as performing a
    number of analyses.  Additionally, the R functions provide the means for
    evaluating models under uncertainty and deep uncertainty and computing
    different robustness metrics.
}
\details{
\tabular{ll}{
Package: \tab OpenMORDM\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-08-26\cr
License: \tab MIT + file LICENSE\cr
Copyright: \tab Copyright 2014 The Pennsylvania State University.  This work was
supported by the National Science Foundation through the Network for
Sustainable Climate Risk Management (SCRiM) under NSF cooperative agreement
GEO-1240507.\cr
Depends: \tab 
R (>= 3.0)\cr
Imports: \tab 
shiny,
shinyRGL,
rgl,
scales,
grid,
prim,
MASS,
animation,
sensitivity,
boot,
pracma,
emoa,
stringr,
functional,
dichromat,
rpart,
rpart.plot,
party,
stats,
corrgram,
rdyncall\cr
Suggests: \tab 
gdata\cr
Built: \tab R 3.1.1; ; 2014-09-12 17:04:52 UTC; windows\cr
}

Index:
\preformatted{
OpenMORDM-package       A library for multiobjective robust decision
                        making (MORDM) and visualizing high-dimensional
                        datasets.
borg.optimize           Optimize the problem using the borg.exe
                        program.
borg.optimize.function
                        Optimize a problem using the shared library
                        (borg.dll or libborg.so).
check.robustness        Computes the robustness metric.
deltafast               Fast version of Plischke's delta-moment
                        sensitivity analysis method.
deltamim                Plischke's delta-moment sensitivity analysis
                        method.
evaluate                Evaluates the decision variables for a given
                        problem.
explore                 A web-based tool (powered by Shiny) for
                        exploring high-dimensional data sets.
mordm.animate           Animates the time series in a GIF.
mordm.as.data.frame     Converts a data set into a data frame.
mordm.attributes        Extracts the common attributes from the time
                        series data.
mordm.cbind             Adds extra colums to the end of a data set.
mordm.colorize          Returns a vector of colors to be used when
                        plotting a data set.
mordm.correlation       Displays the correlations among pairwise
                        factors.
mordm.differences       Identifies key similarities/differences between
                        two sets.
mordm.getset            Returns the individual data set entry to be
                        analyzed.
mordm.identify          Identify and highlight points using the middle
                        mouse button.
mordm.mark.box          Creates a marking rule from PRIM boxes.
mordm.mark.difference   Computes the difference of two markings.
mordm.mark.intersection
                        Computes the intersection of two markings.
mordm.mark.not          Computes the inverse of a marking.
mordm.mark.points       Creates a marking rule identifying specific
                        points.
mordm.mark.rule         Creates a marking rule based on a function.
mordm.mark.selection    Creates a marking rule identifying the selected
                        points.
mordm.mark.subtract     Computes the subtraction of two markings.
mordm.mark.union        Computes the union of two markings.
mordm.normalize         Normalizes the objectives.
mordm.plot              Sets the current data set and displays a 3D
                        scatter plot.
mordm.plotbox           Display plot of PRIM boxes.
mordm.plotmark          Display the markings in a box plot (candle
                        stick plot).
mordm.plotops           Plot operator probabilities.
mordm.plotpar           Display a parallel plot of the current data
                        set.
mordm.prim              Patient rule induction method.
mordm.printbox          Print descriptive representation of PRIM boxes.
mordm.read              Loads the time series data output from an
                        optimization algorithm.
mordm.read.csv          Loads a data set stored in a CSV file.
mordm.read.matrix       Loads a data set stored in a matrix or
                        data.frame.
mordm.read.xls          Loads a data set stored in a XLS or XLSX file.
mordm.recommend         Make recommendations for analyzing the data.
mordm.robustness        Computes robustness under uncertainty.
mordm.select            Returns the subset of rows that are marked.
mordm.select.indices    Returns the row indices in the data set that
                        are marked.
mordm.sensitivity       Computes the sensitivities of the decision
                        variables.
mordm.subset            Returns a subset of a data set.
mordm.uncertainty       Computes robustness under deep uncertainty.
nsample                 Generate normally distributed random inputs.
robustness.constraints
                        Robustness metric based on constraint
                        violations.
robustness.default      Default robustness metric.
robustness.distance     Robustness metric based on distance.
robustness.gap          Experimental robustness metric based on info
                        gap.
robustness.variance     Robustness metric based on variance.
runVisDemo              Runs the exploration tool on a 5-objective
                        problem.
sensitivity             Standardized interface for sensitivity analysis
                        methods.
sensitivity.levels      Determines number of replicates for sensitivity
                        analysis.
setup                   Setup a new problem formulation.
usample                 Generate uniformly distributed random inputs.
}
}
\author{
David Hadka

Maintainer: David Hadka <dmh309@psu.edu>
}
\keyword{ package }
