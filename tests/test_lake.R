# Runs a shortened version of the Lake Problem demo described in the Wiki.
test.lakedemo <- function() {
	require(RUnit)
	require(OpenMORDM)

	# Set up the model
	lake.eval <- function(decisions, samples=100, days=100, b=0.42, q=2, mean=0.02,
						  stdev=0.001, delta=0.98, alpha=0.4, beta=0.08,
						  reliability.threshold=0.9, inertia.threshold=-0.02) {
		pcrit <- uniroot(function(y) y^q/(1+y^q) - b*y, c(0.01, 1.5))$root
		X <- rep(0, days)
		objs <- rep(0, 4)
		
		for (i in 1:samples) {
			X[1] <- 0
			natural.pollution <- rlnorm(days,
										log(mean^2/sqrt(stdev^2 + mean^2)),
										sqrt(log(1+stdev^2/mean^2)))
			
			for (t in 2:days) {
				X[t] <- (1-b)*X[t-1] + X[t-1]^q/(1+X[t-1]^q) + decisions[t] + natural.pollution[t]
			}
			
			objs[1] <- objs[1] + max(X) / samples
			objs[4] <- objs[4] - sum(X < pcrit) / (samples*days)
		}
		
		objs[2] <- -sum(alpha*decisions*delta^(1:days))
		objs[3] <- -sum(diff(decisions) > inertia.threshold) / (days-1)
		
		list(objectives=objs, constraints=max(reliability.threshold+objs[4], 0))
	}
	
	# Test the model
	result <- lake.eval(rep(0.1, 100))
	checkEqualsNumeric(c(1.679454, -1.700066, -1.000000, -0.140000), result$objectives, tolerance=0.01)
	checkEqualsNumeric(0.76, result$constraints, tolerance=0.01)
	
	# Define the problem within OpenMORDM
	nvars <- 100
	nobjs <- 4
	nconstrs <- 1
	
	problem <- define.problem(lake.eval, nvars, nobjs, nconstrs,
							  bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
							  names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
							  maximize=c("Economic Benefit", "Inertia", "Reliability"),
							  epsilons=c(0.0001,0.0001,0.000001,0.000001))
	
	# Skip optimization step - load precomputed optimization results
	load("data.RData")
	
	# Generate standard plots
	palette <- colorRampPalette(c("green", "yellow", "red"))(100)
	#mordm.plot(data, color="Reliability", palette=palette)
	
	png(filename="plot2d.png")
	mordm.plot2d(data)
	dev.off()
	checkTrue(isSimilar("plot2d.png", "plot2d_expected.png"))
	
	png(filename="parallel.png")
	mordm.plot.parallel(line.width=3)
	dev.off()
	checkTrue(isSimilar("parallel.png", "parallel_expected.png"))
	
	png(filename="matrix.png")
	mordm.plot.matrix()
	dev.off()
	checkTrue(isSimilar("matrix.png", "matrix_expected.png"))
	
	# Set the baseline SOW
	param_names <- c("b", "q", "mean", "stdev", "delta")
	baseline_SOW <- c(0.42, 2, 0.02, 0.0017, 0.98)
	
	# Add deep uncertainties (with reduced sampling size)
	temp <- data[[1]][1:20,]
	attr(temp, "nvars") <- attr(data[[1]], "nvars")
	attr(temp, "nobjs") <- attr(data[[1]], "nobjs")
	attr(temp, "nconstrs") <- attr(data[[1]], "nconstrs")
	attr(temp, "bounds") <- attr(data[[1]], "bounds")
	attr(temp, "maximize") <- attr(data[[1]], "maximize")
	data[[1]] <- temp
	
	nsamples <- 50
	
	set.seed(1337)
	SOWS <- sample.lhs(nsamples,
					   b=c(0.1, 0.45),
					   q=c(2, 4.5),
					   mean=c(0.01, 0.05),
					   stdev=c(0.001, 0.005),
					   delta=c(0.93, 0.99))
	checkEquals(c(nsamples, 5), dim(SOWS))
	
	models <- create.uncertainty.models(problem, SOWS)
	checkEquals(nsamples, length(models))
	
	uncertainty.samples <- mordm.sample.uncertainties(data, nsamples, models)
	
	# Compute and plot robustness metrics
	robustness <- mordm.evaluate.uncertainties(uncertainty.samples,
											   function(x) sum(abs(x$constrs)) == 0 && x$objs[2]>0.1,
											   SOWS,
											   baseline_SOW)
	mordm.plot(data, color=robustness[,"Regret Type I"], palette=palette)
	mordm.plot(data, color=robustness[,"Satisficing Type I"], palette=palette)
	
	# Vulnerability Analysis
	selected.point <- uncertainty.samples[[17]]
	
	png(filename="prim.png")
	analyze.prim(SOWS, selected.point$objs[,"Reliability"], threshold.type=-1)
	dev.off()
	checkTrue(isSimilar("prim.png", "prim_expected.png"))
	
	png(filename="cart.png")
	analyze.cart(SOWS, ifelse(selected.point$objs[,"Reliability"]<1,
								 "Failure", "Success"))
	dev.off()
	checkTrue(isSimilar("cart.png", "cart_expected.png"))
}
