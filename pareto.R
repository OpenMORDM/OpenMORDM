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
library(emoa)

mordm.join <- function(data, index=length(data), unique=TRUE) {
	if (length(index) == 1) {
		set <- mordm.getset(data, index)
	} else {
		set <- mordm.getset(data, index[1])
		
		for (i in 2:length(index)) {
			set <- rbind(set, mordm.getset(data, index[i]))
		}
	}
	
	if (unique) {
		set <- unique(set)
	}
	
	attr(set, "nvars") <- attr(data, "nvars")
	attr(set, "nobjs") <- attr(data, "nobjs")
	attr(set, "nconstrs") <- attr(data, "nconstrs")
	attr(set, "bounds") <- attr(data, "bounds")
	attr(set, "maximize") <- attr(data, "maximize")
	
	set
}

mordm.pareto.normalize <- function(set) {
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	maximize <- attr(set, "maximize")
	
	# determine which objectives need to be negated (for maximization)
	if (is.null(maximize)) {
		maximizeTF <- rep(FALSE, nobjs)
	} else {
		maximizeTF <- rep(FALSE, nvars+nobjs)
		names(maximizeTF) <- colnames(set)
		maximizeTF[maximize] <- TRUE
		maximizeTF <- maximizeTF[(nvars+1):(nvars+nobjs)]
	}
	
	# negate the maximized objectives
	minset <- set[,(nvars+1):(nvars+nobjs)]
	minset[,maximizeTF] <- -minset[,maximizeTF]
	minset
}

mordm.pareto.rank <- function(set, objectives=1:ncol(set)) {
	set <- mordm.pareto.normalize(set)
	t(nds_rank(t(set[,objectives])))
}

mordm.pareto.set <- function(set, objectives=1:ncol(set)) {
	set <- mordm.pareto.normalize(set)
	t(nondominated_points(t(set[,objectives])))
}
