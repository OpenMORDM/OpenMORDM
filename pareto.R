library(emoa)

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

mordm.pareto.rank <- function(data) {
	set <- mordm.getset(data)
	set <- mordm.pareto.normalize(set)
	
	nds_rank(t(set))
}


mordm.pareto.set <- function(set, maximize) {
	set <- mordm.getset(data)
	set <- mordm.pareto.normalize(set)
	
	nondominated_points(t(set))
}
