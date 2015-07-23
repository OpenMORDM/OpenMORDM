# Copyright 2014-2015 The Pennsylvania State University
#
# OpenMORDM was developed by Dr. David Hadka with guidance from Dr. Klaus
# Keller and Dr. Patrick Reed.  This work was supported by the National
# Science Foundation through the Network for Sustainable Climate Risk
# Management (SCRiM) under NSF cooperative agreement GEO-1240507.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
