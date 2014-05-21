library("pracma")

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
	#cbind(points, output)
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

check.robustness <- function(output, problem, method="variance", ...) {
	varargs <- list(...)
	
	if (is.function(method)) {
		do.call(method, c(list(output, problem), varargs))
	} else if (is.character(method)) {
		if (method == "variance") {
			do.call(robustness.variance, c(list(output, problem), varargs))
		} else if (method == "infogap" || method == "gap") {
			do.call(robustness.gap, c(list(output, problem), varargs))
		} else {
			stop("Unsupported robustness method")
		}
	} else {
		stop("Unsupported robustness method")
	}
}

robustness.gap <- function(output, problem, original.point=NULL, verbose=FALSE, weights="unused") {
	if (is.null(original.point)) {
		# estimate the original point since one was not provided
		original.point <- apply(output$vars, 2, mean)
	}
	
	distances <- apply(output$vars, 1, function(x) dist(rbind(original.point, x))[1])
	feasible <- apply(output$constrs, 1, function(x) all(x == 0.0))
	indx <- order(distances)
	
	last <- min(which(feasible[indx]))
	distances[last]
}

robustness.variance <- function(output, problem, weights=NULL, verbose=FALSE, original.point="unused") {
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
	
	if (problem$nconstrs > 0) {
		nviolations <- sum(1*apply(output$constrs, 1, function(x) any(x != 0.0)))
		robustness <- robustness*(1+nviolations/nsamples)
		
		if (verbose) {
			cat("    Constraint Violations: ")
			cat(sprintf("%0.1f", 100*nviolations/nsamples))
			cat(" %\n")
		}
	}
	
	if (verbose) {
		cat("    Overall Robustness: ")
		cat(robustness)
		cat("\n\n")
	}
	
	robustness
}

lake.problem <- setup("lake5obj.exe", 20, 5, 1,
					  bounds=matrix(rep(range(0, 0.1), 20), nrow=2))

point1 <- c(0.065978253827694289,0.00059485252872073027,0.0063720060647708392,0.01399249646941669,0.0056629099188366446,0.0055362375342112077,0.018313533745478977,0.0014380325082422526,0.00030581191411225422,0.0076185427381808245,0.012503012142105578,0.016869974082367092,0.0054916027479675472,0.023683915240550205,0.013480490539820471,0.0042869043217465472,0.01595043318628675,0.0020746685117287713,0.014651631115411706,0.05198357577908868)
point2 <- c(0.066253334084896962,0.00334795911265487,0.013684771705565526,0.033774694935984506,0.020464866435693098,0.017354991123983653,0.033706031688876065,0.015864700818353863,0.027034699046830382,0.016527956408633422,0.01890247862009007,0.033791229890777923,0.0089172364984769961,0.035674941490447969,0.019146078244952128,0.001644145344431495,0.037471429222283685,0.0045358730851966372,0.017658269745000683,0.072713021295378738)

d1 <- nsample(point1, 0.01, 100, lake.problem)
d2 <- nsample(point2, 0.01, 100, lake.problem)

print(check.robustness(d1, lake.problem, method="gap"))
print(check.robustness(d2, lake.problem, method="gap"))


#check.robustness(d1, lake.problem)
#check.robustness(d2, lake.problem)