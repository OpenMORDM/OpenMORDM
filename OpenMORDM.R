library(grid)
library(prim)
library(plot3D)
library(MASS)
library(hypervolume)
library(rgl)
library(animation)

mordm.defaultnames <- function(nvars, nobjs) {
	names <- vector()

	for (i in 1:nvars) {
		names <- append(names, paste("Var", as.character(i), sep=""))
	}

	for (i in 1:nobjs) {
		names <- append(names, paste("Obj", as.character(i), sep=""))
	}

	return(names)
}

mordm.read <- function(file, nvars, nobjs, nconstrs=0, bounds=NULL) {
	text <- readLines(file)
	solutions <- vector()
	attributes <- vector()
	result <- list()
	names <- mordm.defaultnames(nvars, nobjs)

	for (line in text) {
		if (substr(line, 1, 1) == "#") {
			if (length(solutions) > 0 | length(attributes) > 0) {
				entry <- matrix(nrow=length(solutions), ncol=nvars+nobjs)

				for (i in 1:length(solutions)) {
					tokens <- unlist(strsplit(solutions[i], " ", fixed=TRUE))

					for (j in 1:length(tokens)) {
						entry[i,j] <- as.double(tokens[j])
					}
				}

				colnames(entry) <- names

				for (line in attributes) {
					tokens <- unlist(strsplit(line, "=", fixed=TRUE))

					if (length(tokens) == 1) {
						attr(entry, tokens[1]) <- TRUE
					} else {
						attr(entry, tokens[1]) <- as.double(tokens[2])
					}
				}

				attr(entry, "nvars") <- nvars
				attr(entry, "nobjs") <- nobjs
				attr(entry, "nconstrs") <- nconstrs
				attr(entry, "bounds") <- bounds

				result <- append(result, list(entry))
			}

			solutions <- vector()
			attributes <- vector()
		} else if (substr(line, 1, 2) == "//") {
			line <- substr(line, 3, nchar(line))
			attributes <- append(attributes, line)
		} else {
			solutions <- append(solutions, line)
		}
	}

	attr(result, "nvars") <- nvars
	attr(result, "nobjs") <- nobjs
	attr(result, "nconstrs") <- nconstrs
	attr(result, "bounds") <- bounds
	class(result) <- "mordm"

	return(result)
}

mordm.attributes <- function(data) {
	names <- c("NFE", "ElapsedTime", "SBX", "DE", "PCX", "SPX", "UNDX", "UM", "Improvements", "Restarts", "PopulationSize", "ArchiveSize")
	result <- matrix(nrow=length(data), ncol=length(names))

	for (i in 1:length(data)) {
		entry <- data[[i]]
		
		for (j in 1:length(names)) {
			result[i, j] = attr(entry, names[j])
		}
	}

	colnames(result) <- names
	return(result)
}

mordm.plotpar <- function(data) {
	last <- data[[length(data)]]

	parcoord(last)
}

mordm.plotbox <- function(data) {
	set <- mordm.getset(data)
	boxplot(set)
}

mordm.plothist <- function(data) {
	set <- mordm.getset(data)
	names <- colnames(set)

	pairs(set)
#, xlim=range(0,1), ylim=range(0,1))

	#par(mfrow=c(1,length(names)))

	#for (i in 1:length(names)) {
	#	hist(last[,i], main=names[i], xlim=range(0,1))
	#}
}

mordm.plotset <- function(data, index=-1, mark=NULL, objectives=NULL) {
	set <- mordm.getset(data, index)
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	names <- colnames(set)

	if (is.null(objectives)) {
		objectives <- (1:nobjs) + nvars
	}

	nobjs <- length(objectives)

	# save the state to a global location so other functions can reference
	mordm.currentset <<- set
	mordm.currentobjectives <<- objectives
	
	# setup the plot
	if (nobjs >= 2) {
		x <- set[,objectives[1]]
		y <- set[,objectives[2]]
		xlab <- names[objectives[1]]
		ylab <- names[objectives[2]]
	}

	if (nobjs >= 3) {
		z <- set[,objectives[3]]
		zlab <- names[objectives[3]]
	} else {
		z <- rep(0,nrow(set))
		zlab <- ""
	}

	if (nobjs >= 4) {
		sizes <- set[,objectives[4]]
		sizes <- (sizes - min(sizes)) / (max(sizes) - min(sizes))
		sizes <- 1.6*sizes + 0.4
	} else {
		sizes <- rep(2,nrow(set))
	}

	if (nobjs >= 5 | !is.null(mark)) {
		if (is.null(mark)) {
			colors <- set[,objectives[5]]
			colors <- (colors - min(colors)) / (max(colors) - min(colors))
		} else if (is.list(mark) & class(mark) != "mark") {
			colors <- rep(0, nrow(set))

			for (i in 1:length(mark)) {
				indices <- mordm.select.indices(set, mark[i])
				colors[indices] <- i
			}

			colors <- colors / length(mark)
		} else {
			colors <- rep(0, nrow(set))
			indices <- mordm.select.indices(set, mark)
			colors[indices] <- 1
		}

		colormap <- rev(heat.colors(101))
		colors <- colormap[colors*100+1]
	} else {
		colors <- rep("#888888",nrow(set))
	}

	plot3d(x, y, z,
		type="s",
		col=colors,
		size=sizes,
		xlab=xlab,
		ylab=ylab,
		zlab=zlab)
}

mordm.identify <- function() {
	rgl.setMouseCallbacks(3, begin=function(x, y) { 
		userMatrix <- par3d("userMatrix")
		viewport <- par3d("viewport")
		scale <- par3d("scale")
		projection <- rgl.projection()
		set <- mordm.currentset
		objectives <- mordm.currentobjectives

		d = matrix(nrow=nrow(set), ncol=1)

		for (i in 1:nrow(set)) {
			pos <- rgl.user2window(set[i,objectives[1]], set[i,objectives[2]], set[i,objectives[3]], projection=projection)
			d[i] = as.double(dist(rbind(pos[1:2], c(x/viewport[3],1-y/viewport[4]))))
		}

		i <- order(d)
		cat("You selected point ")
		cat(i[1])
		cat(" with a distance ")
		cat(d[i[1]])
		cat("\n")
	})
}

mordm.plotops <- function(data, time=FALSE, improvements=FALSE, log=FALSE) {
	names <- c("SBX", "DE", "PCX", "SPX", "UNDX", "UM")
	colors <- c("cyan", "red", "blue", "green", "orange", "purple")
	attributes <- mordm.attributes(data)

	par(xpd=TRUE, mar=c(10,5,2,ifelse(improvements, 5, 2))+0.1)

	if (improvements) {
		improve.diff <- c(attributes[1,"Improvements"], diff(attributes[,"Improvements"]))

		if (time) {
			time.diff <- c(attributes[1,"ElapsedTime"], diff(attributes[,"ElapsedTime"]))
			improve.per <- improve.diff / (time.diff)
		} else {
			nfe.diff <- c(attributes[1,"NFE"], diff(attributes[,"NFE"]))
			improve.per <- improve.diff / (nfe.diff/1000)
		}

		improve.min <- 0.1
		improve.max <- max(improve.per)
		improve.per <- sapply(improve.per, function(x) ifelse(x < improve.min, improve.min, x))

		improve.label <- vector()
		improve.at <- vector()

		for (i in -1:100) {
			improve.label <- append(improve.label, paste("10^", i, sep=""))
			improve.at <- append(improve.at, 10^i)

			if (10^i >= improve.max) {
				break
			}
		}

		yrange <- range(improve.at)
		xrange <- range(attributes[,ifelse(time, "ElapsedTime", "NFE")])

		plot(xrange, yrange, type="n", xlab="", ylab="", log=ifelse(log, "xy", "y"),
			bty="L", xaxt="n", yaxt="n")

		lines(attributes[,ifelse(time, "ElapsedTime", "NFE")], improve.per,
			t="l", lty=1, lwd=1, col="gray50", bty="l", yaxt="n", xaxt="n")

		axis(4, at=improve.at, label=improve.label, las="1", col="gray50", col.axis="gray50")
		mtext(paste("Pareto Improvements per ", ifelse(time, "Second", "1000 NFE"), sep=""), side=4, line=3, col="gray50")

		par(new=TRUE)
	}

	yrange <- range(0:100)
	xrange <- range(attributes[,ifelse(time, "ElapsedTime", "NFE")])

	plot(xrange, yrange, type="n", log=ifelse(log, "x", ""),
		xlab=ifelse(time, "Elapsed Time (seconds)", "NFE"),
		ylab="Operator Probability", bty="L")

	for (i in 1:length(names)) {
		lines(attributes[,ifelse(time, "ElapsedTime", "NFE")], attributes[,names[i]]*100, lwd=4, col=colors[i])
	}

	legend(x=grconvertX(unit(ifelse(improvements, 0.5, 0.45), "npc"), from="npc", to="user"),
		y=grconvertY(unit(-0.3, "npc"), from="npc", to="user"),
		legend=names, col=colors, lwd=4, bty="o", cex=1.0, horiz=TRUE, xjust=0.5)
}

mordm.correlation <- function(data, ht=0.75, lt=0.25) {
	set <- mordm.getset(data)
	names <- colnames(last)

	# compute and classify the correlations
	correlation.high <- matrix(nrow=0, ncol=3)
	correlation.inverse <- matrix(nrow=0, ncol=3)
	correlation.low <- matrix(nrow=0, ncol=3)
	correlation.medium <- matrix(nrow=0, ncol=3)
	
	for (i in 1:(length(names)-1)) {
		for (j in (i+1):length(names)) {
			correlation <- cor(set[,i], set[,j])

			if (correlation >= ht) {
				correlation.high <- rbind(correlation.high, c(i, j, correlation))
			} else if (correlation <= -ht) {
				correlation.inverse <- rbind(correlation.inverse, c(i, j, correlation))
			} else if (abs(correlation) < lt) {
				correlation.low <- rbind(correlation.low, c(i, j, correlation))
			} else {
				correlation.medium <- rbind(correlation.medium, c(i, j, correlation))
			}
		}
	}

	# display the results
	cat("Summary:\n")
	cat("    Highly Correlated:    ")
	cat(nrow(correlation.high))
	cat("\n")
	cat("    Inversely Correlated: ")
	cat(nrow(correlation.inverse))
	cat("\n")
	cat("    Weakly Correlated:    ")
	cat(nrow(correlation.medium))
	cat("\n")
	cat("    Uncorrelated:         ")
	cat(nrow(correlation.low))
	cat("\n\n")

	cat("High Correlations:\n")
	if (length(correlation.high) > 0) {
		for (i in order(correlation.high[,3])) {
			cat("    ")
			cat(sprintf("%-8s", names[correlation.high[i,1]]))
			cat(" <-> ")
			cat(sprintf("%-8s", names[correlation.high[i,2]]))
			cat(" (")
			cat(round(correlation.high[i,3], 2))
			cat(")\n")
		}
	} else {
		cat("    None\n")
	}

	cat("\n")

	cat("Inverse Correlations:\n")
	if (nrow(correlation.inverse) > 0) {
		for (i in order(correlation.inverse[,3])) {
			cat("    ")
			cat(sprintf("%-8s", names[correlation.inverse[i,1]]))
			cat(" <-> ")
			cat(sprintf("%-8s", names[correlation.inverse[i,2]]))
			cat(" (")
			cat(round(correlation.inverse[i,3], 2))
			cat(")\n")
		}
	} else {
		cat("    None\n")
	}
}

mordm.brush <- function(data, conditions) {
	attr(data, "brush") <- conditions

	last <- data[[length(data)]]
	print(min(last[,1]))
	print(max(last[,1]))
	print(apply(last, 1, conditions))
}

mordm.mark.points <- function(points) {
	mark <- points
	class(mark) <- "mark"
	return(mark)
}

mordm.mark.rule <- function(condition) {
	mark <- condition
	class(mark) <- "mark"
	return(mark)
}

mordm.mark.selection <- function() {
	cat("Use the mouse to select the points in the plot\n")
	flush.console()

	selection <- selectpoints3d(value=FALSE)

	cat("Selected ")
	cat(nrow(selection))
	cat(" points!\n")

	return(mordm.mark.points(mordm.currentset[selection[,"index"],]))
}

mordm.mark.box <- function(box) {
	mordm.mark.rule(function(x) {
		names <- colnames(box)
		all(sapply(1:length(names), function(i) x[names[i]] >= box[1,names[i]] & x[names[i]] <= box[2,names[i]]))
	})
}

mordm.mark.union <- function(...) {
	mordm.mark.rule(function(x) {
		any(sapply(list(...), function(rule) rule(x)))
	})
}

mordm.mark.intersection <- function(...) {
	mordm.mark.rule(function(x) {
		all(sapply(list(...), function(rule) rule(x)))
	})
}

mordm.mark.not <- function(rule) {
	mordm.mark.rule(function(x) {
		!rule(x)
	})
}

mordm.getset <- function(data, index=-1) {
	if (is.list(data)) {
		if (index < 1 | index > length(data)) {
			index <- length(data)
		}

		set <- data[[index]]
	} else {
		set <- data
	}

	return(set)
}

mordm.select <- function(data, marking, index=-1, not=FALSE, or=FALSE) {
	set <- mordm.getset(data, index)
	subset <- set[mordm.select.indices(set, marking, not, or),]
	attr(subset, "nvars") <- attr(set, "nvars")
	attr(subset, "nobjs") <- attr(set, "nobjs")
	attr(subset, "nconstrs") <- attr(set, "nconstrs")
	attr(subset, "bounds") <- attr(set, "bounds")
	return(subset)
}

mordm.select.indices <- function(set, marking, not=FALSE, or=FALSE) {
	if (is.list(marking) & class(marking) != "mark") {
		indices <- rep(TRUE, nrow(set))

		for (mark in marking) {
			if (or) {
				indices <- indices | mordm.select.indices(set, mark, not, or)
			} else {
				indices <- indices & mordm.select.indices(set, mark, not, or)
			}
		}
	} else if (is.matrix(marking)) {
		indices <- apply(set, 1, function(x) {
			any(apply(marking, 1, function(y) isTRUE(all.equal(x, y))))
		})
	} else if (is.function(marking)) {
		indices <- apply(set, 1, marking)
	} else {
		stop("Markings must be either a matrix of points or a function")
	}

	if (not) {
		indices <- !indices
	}

	return(indices)
}

mordm.similarities.singleset <- function(data) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	names <- colnames(set)

	mean <- apply(set[,1:nvars], 2, mean)
	variance <- apply(set[,1:nvars], 2, var)
	cat("Variance:\n")	

	for (i in order(variance)) {
		cat("    ")
		cat(names[i])
		cat(" (")
		cat(variance[i])
		cat(")\n")
	}
}

mordm.similarities <- function(set1, ...) {
	if (length(list(...)) == 0) {
		mordm.similarities.singleset(set1)
	} else {
		
	}
}

mordm.differences.calc <- function(mu1, sd1, mu2, sd2) {
	int_f <- function(x, mu1, sd1, mu2, sd2) {
		f1 <- dnorm(x, mean=mu1, sd=sd1)
		f2 <- dnorm(x, mean=mu2, sd=sd2)
		pmin(f1, f2)
	}

	integrate(int_f, -Inf, Inf, mu1=mu1, sd1=sd1, mu2=mu2, sd2=sd2)$value
}

mordm.differences <- function(set1, set2, scale=TRUE, n=NULL) {
	nvars <- attr(set1, "nvars")
	names <- colnames(set1)
	mu1 <- apply(set1[,1:nvars], 2, mean)
	sd1 <- apply(set1[,1:nvars], 2, sd)
	mu2 <- apply(set2[,1:nvars], 2, mean)
	sd2 <- apply(set2[,1:nvars], 2, sd)

	overlap = sapply(1:nvars, function(i) mordm.differences.calc(mu1[i], sd1[i], mu2[i], sd2[i]))

	cat("Differences Between ")
	cat(deparse(substitute(set1)))
	cat(" and ")
	cat(deparse(substitute(set2)))
	cat(":\n")

	for (i in order(overlap, decreasing=TRUE)) {
		cat("    ")
		cat(sprintf("%-8s", names[i]))
		cat(" (")
		cat(overlap[i])
		cat(")\n")
	}

	if (scale & !is.null(attr(set1, "bounds"))) {
		xlim <- attr(set1, "bounds")
	} else {
		xlim = lapply(1:nvars, function(i) range(set1[,i], set2[,i]))
	}

	ylim = lapply(1:nvars, function(i) range(hist(set1[,i], plot=FALSE, breaks=10)$density, hist(set2[,i], plot=FALSE, breaks=10)$density))

	if (is.null(n)) {
		n <- nvars
	}

	par(mfrow=c(2,n))

	for (i in order(overlap, decreasing=TRUE)[1:n]) {
		hist(set1[,i], main=names[i], xlab="Set 1", xlim=xlim[[i]], ylim=ylim[[i]], freq=FALSE, breaks=10)
	}

	for (i in order(overlap, decreasing=TRUE)[1:n]) {
		hist(set2[,i], main="", xlab="Set 2", xlim=xlim[[i]], ylim=ylim[[i]], freq=FALSE, breaks=10)
	}
}

mordm.prim <- function(data, objective, minimize=TRUE, ...) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	x <- set[,1:nvars]

	if (is.character(objective)) {
		y <- set[,objective]
	} else {
		y <- set[,nvars+objective]
	}

	if (minimize) {
		y <- -y
	}

	result <- prim.box(x, y, ...)
	summary(result)
	
	marks <- lapply(1:result$num.hdr.class, function(i) {
		# for some reason, the following statement is needed for i to evaluate to the correct value
		i <- eval(i)
		mordm.mark.box(result$box[[i]])
	})

	return(rev(marks))
}

mordm.recommend <- function(data) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	names <- colnames(set)

	# Can the user fully utilize the plotting capabilities?
	if (nobjs > 5) {
		cat("Problem has more than 5 objectives, will need to downselect!\n\n")
	}

	# Check the importance of each objective
	cat("Order of importance of objectives:\n")
	obj <- set[,(nvars+1):(nvars+nobjs)]
	obj.var <- apply(obj, 2, var)
	obj.cov <- apply(abs(cov(obj)), 2, sum)
	obj.imp <- sqrt(obj.var + obj.cov)

	for (i in order(obj.imp, decreasing=TRUE)) {
		cat("    ")
		cat(sprintf("%-8s", names[nvars+i]))
		cat(" (")
		cat(sprintf("%.3f", obj.imp[i]))
		cat(")")

		if (obj.imp[i] < 0.001) {
			cat(" - Degenerate!")
		}

		cat("\n")
	}
}



#hypervolume(data[[length(data)]], bandwidth=1, repsperpoint=10)
#mordm.plothist(data)
#mordm.correlation(data)
#parcoord(cbind(mordm.getset(data)[,"Var1"], y=mordm.getset(data)[,"Obj3"]))
#mordm.plotset(data)
#mordm.brush(data, function(x) x[1] < 0.02)
#last <- data[[length(data)]]
#pairs(last)
#last.prim <- prim.box(x=last[,1:11], y=last[,12], threshold.type=0, threshold=c(0.75, 0.25))
#summary(last.prim)
#plot(last.prim)






data <- mordm.read("E:/Git/openmordm/lakeoutput.txt", 20, 5, 1,
	bounds=rep(list(range(0.0,0.1)), 20))

# Plot Operators
#mordm.plotops(data, time=FALSE, improvements=TRUE)

# Marking Demo
mark1 <- mordm.mark.rule(function(x) x[21] < 0.1)
mark2 <- mordm.mark.rule(function(x) x[21] > 0.125)
mordm.select(data, mark1)
mordm.select(data, mark2)
mordm.select(data, mark2, not=TRUE)
mordm.select(data, list(mark1, mark2))
mordm.select(data, list(mark1, mark2), or=TRUE)
mordm.plotset(data, mark=list(mark1, mark2))

#mark3 <- mordm.mark.not(mordm.mark.union(mark1, mark2))
#mordm.plotset(data, mark=mark3)

# Similarity / Differences Demo
#set1 <- mordm.select(data, mark1)
#set2 <- mordm.select(data, mark2)
#mordm.differences(set1, set2, scale=FALSE, n=8)

# Prim Analysis - Bump Hunting
#boxes <- mordm.prim(data, "Obj2", minimize=TRUE, threshold.type=1)
#mordm.plotset(data, mark=mordm.mark.union(boxes))

mordm.recommend(data)