library(OpenMORDM)
library(lhs)

nvars <- 100
nobjs <- 4
nconstrs <- 1
nsamples <- 100
param_names <- c("b", "q", "mean", "sigma", "delta")
baseline_factors <- c(0.42, 2, 0.02, 0.0017, 0.98)
param_bounds <- matrix(c(c(0.1, 0.45), c(2, 4.5), c(0.01, 0.05), c(0.001, 0.005), c(0.93, 0.99)), nrow=2)
palette <- colorRampPalette(c("red", "yellow", "green"))(100)

# Step 1. Load and plot the Pareto approximate set.  Note that the optimization function is
# only supported on Unix/Linux.  On Windows, we load the Pareto approximate set from a file.
if (.Platform$OS.type == "windows") {
	data <- mordm.read("outputSOW4_85.set", nvars, nobjs, nconstrs,
			bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
			names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
			maximize=c("Economic Benefit", "Inertia","Reliability"))	
} else {
	problem <- setup("lake4obj.exe", nvars, nobjs, nconstrs,
			bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
			names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
			maximize=c("Economic Benefit", "Inertia", "Reliability"))
	
	data <- borg.optimize(problem, 100000)
}

mordm.plot(data, color="Reliability", palette=rev(palette))

# Step 2. Setup the computational models for MORDM
factors <- randomLHS(nsamples, length(param_names))
factors <- t(apply(factors, 1, function(x) x*diff(param_bounds)+param_bounds[1,]))
colnames(factors) <- param_names

model_calls <- do.call(sprintf, c(list("E:/Git/lake-mordm/lake.exe -b %f -q %f -m %f -s %f -d %f"),
 								  lapply(1:ncol(factors), function(i) factors[,i])))

models <- lapply(model_calls, function(command) {
	setup(command, nvars, nobjs, nconstrs,
		  bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
		  names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
		  maximize=c("Economic Benefit", "Inertia","Reliability"))
})

# Step 3. Evaluate each Pareto approximate solution under deep uncertainty
samples <- mordm.uncertainty.sample(data, nsamples, models)
robustness <- mordm.uncertainty.evaluate(samples,
		satisficing.fcn=function(x) sum(abs(x$constrs)) == 0 && x$objs[2]>0.1,
		factors=factors)

mordm.plot(data, color=robustness[,"Regret Type I"])
mordm.plot(data, color=robustness[,"Regret Type II"])
mordm.plot(data, color=robustness[,"Satisficing Type I"])
mordm.plot(data, color=robustness[,"Satisficing Type II"])

# Step 4. Scenario discovery
design_id <- 274
mordm.prim(factors, samples[[design_id]]$objs[,"Reliability"], threshold.type=-1)
mordm.cart(factors, ifelse(samples[[design_id]]$objs[,"Reliability"]<1, "Failure", "Success"))

# Optional Step. Display results with web visualization toolkit
display.set <- mordm.cbind(mordm.getset(data), robustness)
explore(display.set, nobjs=4+ncol(robustness))
