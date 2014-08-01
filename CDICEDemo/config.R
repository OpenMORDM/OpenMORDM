model <- "lake"

if (model == "lake") {
	filename <- "../demo/data/lake5obj.txt"
	objectives <- c("Phosphorus in Lake", "Bentham's Utility", "Today's Utility", "Future Utility", "Reliability")
	nvars <- 20
	nobjs <- length(objectives)
	nconstrs <- 1
	bounds <- matrix(rep(range(0.0, 0.1), 20), nrow=2)
	maximize <- objectives[2:5]
	order <- 1:5
} else if (model == "cdice") {
	filename <- "runtime_util_ptemp_pvdam_pvabate_100cs_short_10k.txt"
	objectives <- c("Expected Utility", "Reliability of Temperature Goal", "Climate Damages", "Abatement Costs")
	nvars <- 0
	nobjs <- 4
	nconstrs <- 0
	bounds <- NULL
	maximize <- objectives[1:2]
	order <- c(2,3,4,1)
}

colors <- list("Rainbow (Red to Blue)",
			   "Heat (White to Red)",
			   "Grayscale (White to Black)",
			   "Dark Red to Blue*",
			   "Brown to Blue*",
			   "Blue to Green*",
			   "Blue to Gray*",
			   "Light to Dark Blue*",
			   "Green to Magenta*",
			   "Categorical*")
