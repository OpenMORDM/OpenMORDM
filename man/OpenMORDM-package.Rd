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
lhs,
rdyncall\cr
Suggests: \tab 
gdata\cr
Built: \tab R 3.1.1; ; 2015-08-26 19:42:48 UTC; windows\cr
}

Index:
\preformatted{
OpenMORDM-package       A library for multiobjective robust decision
                        making (MORDM) and visualizing high-dimensional
                        datasets.
adjust.command          Prepends a ./ to commands on non-Windows
                        systems.
analyze.cart            Determines the vulernabilities due to deep
                        uncertainties using Classification and
                        Regression Trees (CART).
analyze.prim            Patient rule induction method.
borg.optimize           Optimize the problem using the Borg MOEA.
borg.optimize.external
                        Optimize the problem using the Borg standalone
                        executable (borg.exe).
borg.optimize.function
                        Optimize a problem using the Borg shared
                        library (borg.dll or libborg.so).
check.robustness        Computes the robustness metric.
compute.robustness      Computes robustness under deep
                        compute.robustness.
compute.robustness.guassian
                        Computes robustness under well-characterized
                        compute.robustness (i.e., Gaussian noise).
compute.sensitivity     Standardized interface for sensitivity analysis
                        methods.
create.uncertainty.models
                        Creates models for each uncertainty
                        parameterization.
define.problem          Define a new problem formulation.
deltafast               Fast version of Plischke's delta-moment
                        sensitivity analysis method.
deltamim                Plischke's delta-moment sensitivity analysis
                        method.
evaluate                Evaluates the decision variables for a given
                        problem.
explore                 A web-based tool (powered by Shiny) for
                        exploring high-dimensional data sets.
lhsample                Generate Latin Hypercube sampled random inputs.
mordm.animate           Animates the time series in a GIF.
mordm.as.data.frame     Converts a data set into a data frame.
mordm.cbind             Adds extra colums to the end of a data set.
mordm.colorize          Returns a vector of colors to be used when
                        plotting a data set.
mordm.correlation       Displays the correlations among pairwise
                        factors.
mordm.differences       Identifies key similarities/differences between
                        two sets.
mordm.evaluate.uncertainties
                        Computes robustness under deep
                        compute.robustness.
mordm.extract.attributes
                        Extracts the common attributes from the time
                        series data.
mordm.get.set           Returns the individual data set entry to be
                        analyzed.
mordm.get.subset        Returns a subset of a data set.
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
mordm.plot.box          Display plot of PRIM boxes.
mordm.plot.markings     Display the markings in a box plot (candle
                        stick plot).
mordm.plot.operators    Plot operator probabilities.
mordm.plot.parallel     Display a parallel plot of the current data
                        set.
mordm.print.box         Print descriptive representation of PRIM boxes.
mordm.rbind             Adds extra rows to the end of a data set.
mordm.read              Loads the time series data output from an
                        optimization algorithm.
mordm.read.csv          Loads a data set stored in a CSV file.
mordm.read.matrix       Loads a data set stored in a matrix or
                        data.frame.
mordm.read.xls          Loads a data set stored in a XLS or XLSX file.
mordm.recommend         Make recommendations for analyzing the data.
mordm.sample.uncertainties
                        Computes robustness under deep
                        compute.robustness.
mordm.select            Returns the subset of rows that are marked.
mordm.select.indices    Returns the row indices in the data set that
                        are marked.
mordm.variable.sensitivities
                        Computes the sensitivities of the decision
                        variables.
mordm.weight            Computes a vector of weighted preferences
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
sample.lhs              Generates uncertainty samples using Latin
                        hypercube sampling.
sensitivity.levels      Determines number of replicates for sensitivity
                        analysis.
usample                 Generate uniformly distributed random inputs.
with.parameters         Assigns the parameters for the given function,
                        returning a new function.
}
}
\author{
David Hadka

Maintainer: David Hadka <dmh309@psu.edu>
}
\keyword{ package }
