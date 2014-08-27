\name{OpenMORDM-package}
\alias{OpenMORDM-package}
\alias{OpenMORDM}
\docType{package}
\title{
OpenMORDM is an open-source R library for multiobjective robust decision
making (MORDM).
}
\description{
OpenMORDM is an open-source R library for multiobjective robust
    decision making (MORDM). It includes support for loading optimization
    output files (from Borg or the MOEA Framework), visualizing the data sets
    using various 2D and 3D plots, performing scenario discovery and tradeoff
    analysis, and computing uncertainty/robustness metrics.
}
\details{
\tabular{ll}{
Package: \tab OpenMORDM\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-08-26\cr
License: \tab MIT License\cr
Depends: \tab 
R (>= 3.0),
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
corrgram\cr
Built: \tab R 3.1.0; ; 2014-08-27 13:32:24 UTC; windows\cr
}

Index:
\preformatted{
bootstats               Internal method copied from the statistics
                        library.
check.bounds            Returns 'TRUE' if the decision variables are
                        within bounds.
check.length            Ensures the given set contains the correct
                        number of decision variables.
check.robustness        Computes the robustness metric.
cleanup                 Removes any sampled point that has NA values.
default.names           Creates the default column names.
deltafast               Fast version of Plischke's delta-moment
                        sensitivity analysis method.
deltamim                Plischke's delta-moment sensitivity analysis
                        method.
evaluate                Evaluates the decision variables for a given
                        problem.
evaluate.external       Evaluates a problem representing a command line
                        executable.
evaluate.function       Evaluates a problem representing a R function.
explore                 A web-based tool (powered by Shiny) for
                        exploring high-dimensional data sets.
mordm.animate           Animates the time series in a GIF.
mordm.attributes        Extracts the common attributes from the time
                        series data.
mordm.colorize          Returns a vector of colors to be used when
                        plotting a data set.
mordm.correlation       Displays the correlations among pairwise
                        factors.
mordm.defaultnames      Creates the default column names.
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
mordm.read.matrix       Loads a data set stored in a matrix.
mordm.recommend         Make recommendations for analyzing the data.
mordm.robustness        Computes robustness under uncertainty.
mordm.select            Returns the subset of rows that are marked.
mordm.select.indices    Returns the row indices in the data set that
                        are marked.
mordm.sensitivity       Computes the sensitivities of the decision
                        variables.
mordm.uncertainty       Computes robustness under deep uncertainty.
nsample                 Generate normally distributed random inputs.
optimize                Optimize the problem.
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
\references{
~~ Todo ~~
}
\keyword{ package }
\seealso{
~~ Todo ~~
}
\examples{
~~ Todo ~~
}
