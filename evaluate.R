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
library(pracma)
library(sensitivity)
library(boot)

setup <- function(command, nvars, nobjs, nconstrs=0, bounds=NULL) {
	if (is.null(bounds)) {
		bounds <- matrix(rep(range(0, 1), nvars), nrow=2)
	}
	
	container <- list(command=command, nvars=nvars, nobjs=nobjs, nconstrs=nconstrs, bounds=bounds)
	class(container) <- "mop"
	container
}

evaluate <- function(set, problem) {
	check.length(set, problem)
	
	if (is.function(problem$command)) {
		output <- evaluate.function(set, problem)
	} else if (is.character(problem$command)) {
		output <- evaluate.external(set, problem)
	} else {
		stop("Command must be a R function or an system command")
	}
	
	if (problem$nconstrs > 0) {
		list(vars=set, objs=output[,1:problem$nobjs,drop=FALSE], constrs=output[,(problem$nobjs+1):(problem$nobjs+problem$nconstrs),drop=FALSE])
	} else {
		list(vars=set, objs=output[,1:problem$nobjs,drop=FALSE])
	}
}

evaluate.external <- function(set, problem) {
	input <- apply(set, 1, function(x) paste(x, collapse=" "))
	input <- append(input, "")
	
	output <- system(problem$command, intern=TRUE, input=input)
	
	t(sapply(output, function(line) as.double(unlist(strsplit(line, " ", fixed=TRUE))), USE.NAMES=FALSE))
}

evaluate.function <- function(set, problem) {
	t(apply(set, 1, function(x) problem$command(x)))
}

check.length <- function(set, problem) {
	if (is.matrix(set)) {
		if (ncol(set) != problem$nvars) {
			stop("Number of columns must match number of variables")
		}
	} else {
		if (length(set) != problem$nvars) {
			stop("Length of vector must match number of variables")
		}
	}
}

usample <- function(nsamples, problem) {
	points <- rand(nsamples, problem$nvars)
	
	for (i in 1:problem$nvars) {
		points[,i] <- (problem$bounds[2,i]-problem$bounds[1,i])*points[,i] + problem$bounds[1,i]
	}
	
	output <- evaluate(points, problem)
}

check.bounds <- function(points, problem) {
	check.length(points, problem)
	
	if (is.matrix(points)) {
		for (i in 1:nrow(points)) {
			if (!check.bounds(points[i,], problem)) {
				return(FALSE)
			}
		}
	} else {
		for (i in 1:problem$nvars) {
			if (points[i] < problem$bounds[1,i] || points[i] > problem$bounds[2,i]) {
				return(FALSE)
			}
		}
	}
	
	return(TRUE)
}

nsample <- function(mean, sd, nsamples, problem) {
	check.length(mean, problem)
	points <- zeros(nsamples, problem$nvars)
	
	for (i in 1:nsamples) {
		# resample if the point is out of bounds
		repeat {
			point <- rnorm(problem$nvars, mean, sd)
			
			if (check.bounds(point, problem)) {
				break
			}
		}
		
		points[i,] <- point
	}
	
	evaluate(points, problem)
}

check.robustness <- function(output, problem, method="default", verbose=FALSE, ...) {
	varargs <- list(...)
	varargs$verbose <- verbose
	
	if (is.function(method)) {
		robustness <- do.call(method, c(list(output, problem), varargs))
	} else if (is.character(method)) {
		if (method == "default") {
			robustness <- do.call(robustness.default, c(list(output, problem), varargs))
		} else if (method == "variance") {
			robustness <- do.call(robustness.variance, c(list(output, problem), varargs))
		} else if (method == "constraints") {
			robustness <- do.call(robustness.constraints, c(list(output, problem), varargs))
		} else if (method == "infogap" || method == "gap") {
			robustness <- do.call(robustness.gap, c(list(output, problem), varargs))
		} else {
			stop("Unsupported robustness method")
		}
	} else {
		stop("Unsupported robustness method")
	}
	
	if (verbose) {
		cat("    Overall Robustness: ")
		cat(robustness)
		cat("\n\n")
	}
	
	robustness
}

robustness.gap <- function(output, problem, weights=NULL, verbose=FALSE, original.point=NULL) {
	if (problem$nconstrs > 0) {
		if (is.null(original.point)) {
			# estimate the original point since one was not provided
			original.point <- apply(output$vars, 2, mean)
		}
		
		distances <- apply(output$vars, 1, function(x) dist(rbind(original.point, x))[1])
		feasible <- apply(output$constrs, 1, function(x) all(x == 0.0))
		
		if (any(!feasible)) {
			indx <- order(distances)
			last <- min(which(!feasible[indx]))
			distances[last]
		} else {
			max(distances)
		}
	} else {
		# Can't compute stability region if there are no constraints
		1
	}
}

robustness.constraints <- function(output, problem, weights=NULL, verbose=FALSE, original.point=NULL) {
	nsamples <- nrow(output$vars)
	robustness <- 1
	
	if (problem$nconstrs > 0) {
		nviolations <- sum(1*apply(output$constrs, 1, function(x) any(x != 0.0)))
		robustness <- robustness-nviolations/nsamples
		
		if (verbose) {
			cat("    Constraint Violations: ")
			cat(sprintf("%0.1f", 100*nviolations/nsamples))
			cat(" %\n")
		}
	}
	
	robustness
}

robustness.default <- function(output, problem, weights=NULL, verbose=FALSE, original.point=NULL) {
	robustness <- robustness.variance(output, problem, weights, verbose, original.point)
	robustness * (2-robustness.constraints(output, problem, weights, verbose, original.point))
}

robustness.variance <- function(output, problem, weights=NULL, verbose=FALSE, original.point=NULL) {
	nsamples <- nrow(output$vars)
	robustness <- 0
	
	if (is.null(weights)) {
		weights <- rep(1, problem$nobjs)
	}
	
	for (i in 1:problem$nobjs) {
		sd.norm <- sd(output$objs[,i])
		robustness <- robustness - weights[i]*sd.norm
		
		if (verbose) {
			cat("    Objective ")
			cat(i)
			cat(" Stdev: ")
			cat(sd.norm)
			cat("\n")
		}
	}
	
	robustness
}

cleanup <- function(data) {
	na.indx <- apply(data$objs, 1, function(x) !any(is.na(x)))
	
	if (is.null(data$constrs)) {
		list(vars=data$vars[na.indx,], objs=data$objs[na.indx,])
	} else {
		list(vars=data$vars[na.indx,], objs=data$objs[na.indx,], constrs=data$constrs[na.indx,])
	}
}

# Calculates the number of replicates / levels required by the sensitivity
# analysis method to produce approximately the given number of samples
sensitivity.levels <- function(problem, samples, method) {
	if (method == "fast99") {
		ceiling(samples / problem$nvars)
	} else if (method == "sobol") {
		ceiling(samples / (problem$nvars+1))
	} else if (method == "sobol2002") {
		ceiling(samples / (problem$nvars+2))
	} else if (method == "sobol2007") {
		ceiling(samples / (problem$nvars+2))
	} else if (method == "sobolEff") {
		ceiling(samples / (problem$nvars+1))
	} else if (method == "soboljansen") {
		ceiling(samples / (problem$nvars+2))
	} else if (method == "sobolmara") {
		ceiling(samples / 2)
	} else if (method == "sobolroalhs") {
		ceiling(samples / 2)
	} else if (method == "morris") {
		ceiling(samples / (problem$nvars+1))
	} else if (method == "pcc" || method == "src") {
		samples
	} else if (method == "plischke") {
		samples
	} else {
		stop("Unsupported method")
	}
}

sensitivity <- function(problem, objective, samples, method="fast99", verbose=FALSE, plot=FALSE, raw=FALSE, ...) {
	varargs <- list(...)
	
	n <- sensitivity.levels(problem, samples, method)
	
	if (method == "fast99") {
		if (is.null(varargs$q)) {
			varargs$q <- "qunif"
		}
		
		if (is.null(varargs$q.arg)) {
			varargs$q.arg <- list(min=0, max=1)
		}
		
		model <- do.call(fast99, c(list(model=NULL, factors=problem$nvars, n=n), varargs))
	} else if (method == "sobol") {
		X1 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		X2 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(sobol, c(list(model=NULL, X1, X2), varargs))
	} else if (method == "sobol2002") {
		X1 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		X2 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(sobol2002, c(list(model=NULL, X1, X2), varargs))
	} else if (method == "sobol2007") {
		X1 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		X2 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(sobol2007, c(list(model=NULL, X1, X2), varargs))
	} else if (method == "sobolEff") {
		X1 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		X2 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(sobolEff, c(list(model=NULL, X1, X2), varargs))
	} else if (method == "soboljansen") {
		X1 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		X2 <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(soboljansen, c(list(model=NULL, X1, X2), varargs))
	} else if (method == "sobolmara") {
		X <- data.frame(matrix(runif(problem$nvars*n), nrow=n))
		model <- do.call(sobolmara, c(list(model=NULL, X), varargs))
	} else if (method == "sobolroalhs") {
		if (is.null(varargs$order)) {
			varargs$order <- 1
		}
		
		model <- do.call(sobolroalhs, c(list(model=NULL, factors=problem$nvars, levels=n), varargs))
	} else if (method == "morris") {
		if (is.null(varargs$design)) {
			varargs$design <- list(type="oat", levels=5, grid.jump=3)
		}
		
		model <- do.call(morris, c(list(model=NULL, factors=problem$nvars, r=n), varargs))
	} else if (method == "pcc" || method == "src") {
		model <- list(X=data.frame(matrix(runif(problem$nvars*n), nrow=n)))
	} else if (method == "plischke") {
		model <- list(X=matrix(runif(problem$nvars*n), nrow=n))
	} else {
		stop("Unsupported method")
	}
	
	# ensure the model inputs are valid
	if (any(is.nan(unlist(model$X)))) {
		stop("Invalid sampling method, try a different method or increase the number of samples")
	}
	
	# scale the model inputs
	vars <- t(apply(model$X, 1, function(x) x*(problem$bounds[2,]-problem$bounds[1,]) + problem$bounds[1,]))
	
	# evaluate the model
	y <- evaluate(vars, lake.problem)
	
	# compute the sensitivity indices
	if (method == "pcc") {
		model <- do.call(pcc, c(list(model$X, y$objs[,objective]), varargs))
	} else if (method == "src") {
		model <- do.call(src, c(list(model$X, y$objs[,objective]), varargs))
	} else if (method == "plischke") {
		model <- do.call(deltamim, c(list(model$X, y$objs[,objective]), varargs))
	} else {
		tell(model, y$objs[,objective])
	}
	
	if (verbose) {
		print(model)
	}
	
	if (plot) {
		plot(model)
	}
	
	if (raw) {
		model
	} else {
		if (method == "fast99") {
			Si <- model$D1/model$V
			rank <- rev(order(Si))
			Si.total <- 1 - model$Dt / model$V
			rank.total <- rev(order(Si.total))
			list(Si=Si, rank=rank, Si.total=Si.total, rank.total=rank.total)
		} else if (method == "sobol" || method == "sobolEff" || method == "sobolmara") {
			Si <- model$S[,"original"]
			rank <- rev(order(Si))
			
			if ("min. c.i." %in% names(model$S)) {
				Ci <- model$S[,c("min. c.i.", "max. c.i.")]
				list(Si=Si, rank=rank, Ci=Ci)
			} else {
				list(Si=Si, rank=rank)
			}
		} else if (method == "sobolroalhs") {
			Si <- model$S[1:problem$nvars,"original"]
			rank <- rev(order(Si))
			
			if ("min. c.i." %in% names(model$S)) {
				Ci <- model$S[1:problem$nvars,c("min. c.i.", "max. c.i.")]
				list(Si=Si, rank=rank, Ci=Ci)
			} else {
				list(Si=Si, rank=rank)
			}
		} else if (method == "sobol2002" || method == "sobol2007" || method == "soboljansen") {
			Si <- model$S[,"original"]
			rank <- rev(order(Si))
			Si.total <- model$T[,"original"]
			rank.total <- rev(order(Si.total))
			
			if ("min. c.i." %in% names(model$S)) {
				Ci <- model$S[,c("min. c.i.", "max. c.i.")]
				Ci.total <- model$T[,c("min. c.i.", "max. c.i.")]
				list(Si=Si, rank=rank, Ci=Ci, Si.total=Si.total, rank.total=rank.total, Ci.total=Ci.total)
			} else {
				list(Si=Si, rank=rank, Si.total=Si.total, rank.total=rank.total)
			}
		} else if (method == "morris") {
			Si <- apply(model$ee, 2, mean)
			rank <- rev(order(Si))
			list(Si=Si, rank=rank)
		} else if (method == "pcc") {
			Si <- model$PCC[,"original"]
			rank <- rev(order(Si))
			
			if ("min. c.i." %in% names(model$PCC)) {
				Ci <- model$PCC[,c("min. c.i.", "max. c.i.")]
				list(Si=Si, rank=rank, Ci=Ci)
			} else {
				list(Si=Si, rank=rank)
			}
		} else if (method == "src") {
			Si <- model$SRC[,"original"]
			rank <- rev(order(Si))
			
			if ("min. c.i." %in% names(model$SRC)) {
				Ci <- model$SRC[,c("min. c.i.", "max. c.i.")]
				list(Si=Si, rank=rank, Ci=Ci)
			} else {
				list(Si=Si, rank=rank)
			}
		} else if (method == "plischke") {
			if (!is.null(varargs$nboot)) {
				if (is.null(varargs$conf)) {
					varargs$conf = 0.95
				}
				
				estim.plischke <- function(data, i = 1:nrow(data)) {
					d <- as.matrix(data[i, ])
					k <- ncol(d)
					res <- do.call(deltamim, c(list(d[,-k], d[,k]), varargs))
					c(res$Si)
				}
				
				V.boot <- boot(cbind(vars, y$objs[,objective]), estim.plischke, R = varargs$nboot)
				V <- bootstats(V.boot, varargs$conf, "basic")
				rownames(V) <- paste("X", 1:problem$nvars, sep="")

				list(Si=model$Si, rank=model$rank, Ci=V[,c("min. c.i.", "max. c.i.")])
			} else {
				list(Si=model$Si, rank=model$rank)
			}
		}
	}
}

# Copied from statistics library since it is not exported
bootstats <- function(b, conf = 0.95, type = "norm") {
	p <- length(b$t0)
	lab <- c("original", "bias", "std. error", "min. c.i.", "max. c.i.")
	out <-  as.data.frame(matrix(nrow = p, ncol = length(lab),
								 dimnames = list(NULL, lab)))
	
	for (i in 1 : p) {
		
		# original estimation, bias, standard deviation
		
		out[i, "original"] <- b$t0[i]
		out[i, "bias"] <- mean(b$t[, i]) - b$t0[i]
		out[i, "std. error"] <- sd(b$t[, i])
		
		# confidence interval
		
		if (type == "norm") {
			ci <- boot.ci(b, index = i, type = "norm", conf = conf)
			if (!is.null(ci)) {
				out[i, "min. c.i."] <- ci$norm[2]
				out[i, "max. c.i."] <- ci$norm[3]
			}
		} else if (type == "basic") {
			ci <- boot.ci(b, index = i, type = "basic", conf = conf)
			if (!is.null(ci)) {
				out[i, "min. c.i."] <- ci$basic[4]
				out[i, "max. c.i."] <- ci$basic[5]
			}
		}
	}
	
	return(out)
}