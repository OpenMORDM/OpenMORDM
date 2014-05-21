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

mordm.pareto.rank <- function(set) {
	set <- mordm.pareto.normalize(set)
	nds_rank(t(set))
}

mordm.pareto.set <- function(set) {
	set <- mordm.pareto.normalize(set)
	nondominated_points(t(set))
}
