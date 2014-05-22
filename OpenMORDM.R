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
library(grid)
library(prim)
library(rgl)
library(scales)
library(MASS)
library(animation)
source("plischke.R")
source("evaluate.R")
source("pareto.R")

mordm.defaultnames <- function(nvars, nobjs) {
	names <- vector()

	if (nvars > 0) {
		names <- append(names, sprintf("Var%d", 1:nvars))
	}
	
	if (nobjs > 0) {
		names <- append(names, sprintf("Obj%d", 1:nobjs))
	}

	return(names)
}

mordm.read <- function(file, nvars, nobjs, nconstrs=0, bounds=NULL, names=NULL, maximize=NULL) {
	text <- readLines(file)
	solutions <- vector()
	attributes <- vector()
	result <- list()
	
	if (is.null(names)) {
		names <- mordm.defaultnames(nvars, nobjs)
	} else if (length(names) == nobjs) {
		names <- append(mordm.defaultnames(nvars, 0), names)
	} else if (length(names) != nvars + nobjs) {
		warning("Incorrect number of names, using defaults")
		names <- mordm.defaultnames(nvars, nobjs)
	}

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
				attr(entry, "maximize") <- maximize

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
	
	if (!is.null(maximize)) {
		result <- mordm.normalize(result, maximize)
	}

	attr(result, "nvars") <- nvars
	attr(result, "nobjs") <- nobjs
	attr(result, "nconstrs") <- nconstrs
	attr(result, "bounds") <- bounds
	attr(result, "maximize") <- maximize
	class(result) <- "mordm"

	return(result)
}

mordm.normalize <- function(data, maximize) {
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	
	for (i in 1:length(data)) {
		entry <- data[[i]]
		entry[,maximize] <- -entry[,maximize]
		data[[i]] <- entry
	}
	
	return(data)
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

mordm.colorize <- function(set, objectives, mark, palette=heat.colors, n=100, offset=0, colors=NULL, unmarked="#888888FF", alpha=1) {
	if (!is.null(colors)) {
		colors <- (colors - min(colors, na.rm=TRUE)) / (max(colors, na.rm=TRUE) - min(colors, na.rm=TRUE))
	} else if (is.null(mark)) {
        colors <- set[,objectives[5]]
        colors <- (colors - min(colors, na.rm=TRUE)) / (max(colors, na.rm=TRUE) - min(colors, na.rm=TRUE))
    } else if (is.list(mark)) {
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
    
    colormap <- rev(palette(n+1, alpha=alpha))
    colormap[1] <- unmarked
    colormap[(colors*(n-offset))+offset+1]
}

mordm.plotpar <- function(highlight=NULL) {
    set <- mordm.currentset
    objectives <- mordm.currentobjectives
    mark <- mordm.currentmark
    colors <- alpha(mordm.currentcolors, 0.4)
    
    # highlight selected solutions
    if (is.null(highlight)) {
        lwd <- 1
    } else {
        lwd <- rep(1, nrow(set))
        lwd[highlight] <- 3
        colors[highlight] <- alpha(colors[highlight], 1.0)
        
        order <- 1:nrow(set)
        order <- append(order[-highlight], highlight)
        
        set <- set[order,]
        lwd <- lwd[order]
        colors <- colors[order]
    }
    
    # reset plot settings
    if (exists("mordm.defaultpar")) {
    	par(mordm.defaultpar)
    } else {
    	mordm.defaultpar <<- par(no.readonly=TRUE)
    }
    
    # create the plot
	parcoord(set, col=colors, lwd=lwd, var.label=TRUE)
    
    # store the plot settings
	mordm.currentplot <<- "parallel"
}

mordm.plotmark <- function(highlight=NULL) {
	set <- mordm.currentset
	mark <- mordm.currentmark
	colors <- mordm.currentcolors
    nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
    
	# get the variable bounds for normalization
    if (!is.null(attr(set, "bounds"))) {
        xlim <- attr(set, "bounds")
    } else {
        xlim = sapply(1:nvars, function(i) range(set[,i], set[,i]))
    }
	
	# normalize the data
	normset <- matrix(data=set, nrow=nrow(set), ncol=ncol(set))
	colnames(normset) <- colnames(set)
    
    for (i in 1:nvars) {
        normset[,i] <- (normset[,i] - xlim[1,i]) / (xlim[2,i] - xlim[1,i])
    }
	
	for (i in 1:nobjs) {
		lim <- range(normset[,(nvars+i)])
		normset[,(nvars+i)] <- (normset[,(nvars+i)] - lim[1]) / (lim[2] - lim[1])
	}
	
	# reset plot settings
	if (exists("mordm.defaultpar")) {
		par(mordm.defaultpar)
	} else {
		mordm.defaultpar <<- par(no.readonly=TRUE)
	}
	
	# create the plot
	boxplot(normset, xlab="Variables and Objectives", ylab="Range (Scaled to [0, 1])", range=0, outline=FALSE)
	
	if (is.null(mark)) {
		# no markings to plot
	} else if (is.list(mark)) {
		if (length(mark) %% 2 == 0) {
			n <- length(mark)%/%2
			offsets <- 0.2*(-n:n)[-(n+1)]
		} else {
			n <- length(mark)%/%2
			offsets <- 0.2*(-n:n)
		}

		for (i in 1:length(mark)) {
			indices <- mordm.select.indices(set, mark[i])
			boxplot(normset[indices,], col=colors[indices][1], add=TRUE, range=0, outline=FALSE, at=1:(nvars+nobjs) - offsets[i], names=rep("", nvars+nobjs), boxwex=0.5/length(mark))
		}
	} else {
		indices <- mordm.select.indices(set, mark)
		boxplot(normset[indices,], col=colors[indices][1], add=TRUE, range=0, outline=FALSE, names=rep("", nvars+nobjs), boxwex=0.5)
	}
	
	# highlight selected solutions
	if (!is.null(highlight)) {
		lines(normset[highlight,], lwd=3, col=alpha(colors[highlight], 0.75))
	}
	
	# store the plot settings
	mordm.currentplot <<- "mark"
}

mordm.plot <- function(data, mark=NULL, index=-1, objectives=NULL, stay=TRUE, identify=TRUE, colors=NULL, ...) {
	set <- mordm.getset(data, index)
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	names <- colnames(set)

	if (is.null(objectives)) {
		objectives <- (1:nobjs) + nvars
	}

	nobjs <- length(objectives)
	
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

	if (nobjs >= 5 || !is.null(mark) || !is.null(colors)) {
        colors <- mordm.colorize(set, objectives, mark, colors=colors)
	} else {
		colors <- rep("#888888",nrow(set))
	}

    # create the plot
	plot3d(x, y, z,
		type="s",
		col=colors,
		size=sizes,
		xlab=xlab,
		ylab=ylab,
		zlab=zlab,
		...)
	
	rgl.bringtotop(stay)
	
	if (identify) {
		mordm.identify()
	}
    
	# save the state to a global location so other functions can reference
	mordm.currentset <<- set
	mordm.currentobjectives <<- objectives
	mordm.currentmark <<- mark
    mordm.currentcolors <<- colors
}

mordm.identify <- function(enabled=TRUE, label=FALSE) {
	if (enabled) {
		rgl.setMouseCallbacks(3, begin=function(x, y) { 
			userMatrix <- par3d("userMatrix")
			viewport <- par3d("viewport")
			scale <- par3d("scale")
			projection <- rgl.projection()
			set <- mordm.currentset
			objectives <- mordm.currentobjectives
	
			d = matrix(nrow=nrow(set), ncol=1)
	
			for (i in 1:nrow(set)) {
				pos <- rgl.user2window(set[i,objectives[1]],
									   set[i,objectives[2]],
									   set[i,objectives[3]],
									   projection=projection)
				d[i] = as.double(dist(rbind(pos[1:2],
											c(x/viewport[3],1-y/viewport[4]))))
			}
	
			i <- order(d)
			cat("You selected point ")
			cat(i[1])
			cat(" with a distance ")
			cat(d[i[1]])
			cat("\n")
			
			if (label) {
				offset <- apply(set[,c(objectives[1], objectives[2], objectives[3])], 2, range)
				offset.diff <- apply(offset, 2, diff) / 20
				text3d(x=set[i[1],objectives[1]]+offset.diff[1],
					   y=set[i[1],objectives[2]]+offset.diff[2],
					   z=set[i[1],objectives[3]]+offset.diff[3],
					   i[1])
			}
	        
			if (exists("mordm.currentplot")) {
		        if (mordm.currentplot == "parallel") {
		            mordm.plotpar(highlight=i[1])
		        } else if (mordm.currentplot == "mark") {
		        	mordm.plotmark(highlight=i[1])
		        }
			}
		})
	} else {
		rgl.setMouseCallbacks(3)
	}
}

mordm.plotops <- function(data, time=FALSE, improvements=FALSE, log=FALSE) {
	names <- c("SBX", "DE", "PCX", "SPX", "UNDX", "UM")
	colors <- c("cyan", "red", "blue", "green", "orange", "purple")
	attributes <- mordm.attributes(data)
	
	# reset plot settings
	if (exists("mordm.defaultpar")) {
		par(mordm.defaultpar)
	} else {
		mordm.defaultpar <<- par(no.readonly=TRUE)
	}

	# the operator plot uses special margins for the legend and right-axis
	layout(c(1,2), heights=c(7,1))
	par(mar=c(4,5,2,ifelse(improvements, 5, 2))+0.1)

	# plot the number of improvements
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

	# plot the operator probabilities
	yrange <- range(0:100)
	xrange <- range(attributes[,ifelse(time, "ElapsedTime", "NFE")])

	plot(xrange, yrange, type="n", log=ifelse(log, "x", ""),
		xlab=ifelse(time, "Elapsed Time (seconds)", "NFE"),
		ylab="Operator Probability", bty="L")

	for (i in 1:length(names)) {
		lines(attributes[,ifelse(time, "ElapsedTime", "NFE")], attributes[,names[i]]*100, lwd=4, col=colors[i])
	}

	par(mar=c(0,0,0,0))
	plot.new()
	legend("center", legend=names, col=colors, lwd=4, bty="o", cex=1.0, horiz=TRUE, xjust=0.5)
}

mordm.correlation <- function(data, ht=0.75, lt=0.25) {
	set <- mordm.getset(data)
	names <- colnames(set)

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

mordm.mark.rule <- function(condition) {
	mark <- condition
	class(mark) <- "mark"
	return(mark)
}

mordm.mark.points <- function(points) {
    mordm.mark.rule(function(x) {
    	# This is a fast implementation to determine if points contains x
    	for (i in 1:nrow(points)) {
    		match <- TRUE
    		
    		for (j in 1:ncol(points)) {
    			if (points[i,j] != x[j]) {
    				match <- FALSE
    				break
    			}
    		}
    		
    		if (match) {
    			return(TRUE)
    		}
    	}
    	
    	return(FALSE)
    	
    	# This is the cleaner but slow version
        #any(apply(points, 1, function(y) isTRUE(all.equal(x, y))))
    })
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
	result <- mordm.mark.rule(function(x) {
		names <- colnames(box)
		all(sapply(1:length(names), function(i) x[names[i]] >= box[1,names[i]] & x[names[i]] <= box[2,names[i]]))
	})
	
	attr(result, "box") <- box
	return(result)
}

mordm.mark.union <- function(...) {
	mordm.mark.rule(function(x) {
		# faster version with shortcircuiting
		rules <- unlist(list(...))
		
		for (rule in rules) {
			if (rule(x)) {
				return(TRUE)
			}
		}
		
		return(FALSE)
		
		# cleaner version without shortcircuiting
		#any(sapply(unlist(list(...)), function(rule) rule(x)))
	})
}

mordm.mark.intersection <- function(...) {
	mordm.mark.rule(function(x) {
		# faster version with shortcircuiting
		rules <- unlist(list(...))
		
		for (rule in rules) {
			if (!rule(x)) {
				return(FALSE)
			}
		}
		
		return(TRUE)
		
		# cleaner version without shortcircuiting
		#all(sapply(unlist(list(...)), function(rule) rule(x)))
	})
}

mordm.mark.difference <- function(...) {
	mordm.mark.rule(function(x) {
		# faster version with shortcircuiting
		rules <- unlist(list(...))
		count <- 0
		
		for (rule in rules) {
			if (rule(x)) {
				count <- count + 1
				
				if (count > 1) {
					break
				}
			}
		}
		
		return(count == 1)
		
		# cleaner version without shortcircuiting
		#sum(sapply(unlist(list(...)), function(rule) rule(x))) == 1
	})
}

mordm.mark.subtract <- function(rule1, rule2) {
	mordm.mark.rule(function(x) {
		rule1(x) & !rule2(x)
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
	if (is.list(marking)) {
		indices <- rep(TRUE, nrow(set))

		for (mark in marking) {
			if (or) {
				indices <- indices | mordm.select.indices(set, mark, not, or)
			} else {
				indices <- indices & mordm.select.indices(set, mark, not, or)
			}
		}
	} else if (is.function(marking)) {
		indices <- apply(set, 1, marking)
	} else {
		stop("Markings must be a function or a list of functions")
	}

	if (not) {
		indices <- !indices
	}

	return(indices)
}

mordm.differences <- function(set1, set2, scale=TRUE, decreasing=TRUE, splits=20, n=NULL, all=FALSE) {
	nvars <- attr(set1, "nvars")
	names <- colnames(set1)
	
	if (all) {
		n <- nvars
	} else if (is.null(n)) {
		n <- min(5, nvars)
	} else {
		n <- min(n, nvars)
	}
	
	if (scale & !is.null(attr(set1, "bounds"))) {
		xlim <- attr(set1, "bounds")
	} else {
		xlim = sapply(1:nvars, function(i) range(set1[,i], set2[,i]))
	}
	
	ylim = rep(0, nvars)
	overlap = rep(0, nvars)
	
	for (i in 1:nvars) {
		breaks <- seq(from=xlim[1,i], to=xlim[2,i], length.out=splits)
		hist1 <- hist(set1[,i], breaks=breaks, plot=FALSE)
		hist2 <- hist(set2[,i], breaks=breaks, plot=FALSE)
		ylim[i] = max(hist1$density, hist2$density)
		overlap[i] = sum(apply(rbind(hist1$density, hist2$density), 2, min)) / sum(hist1$density)
	}

	cat("Differences Between ")
	cat(deparse(substitute(set1)))
	cat(" and ")
	cat(deparse(substitute(set2)))
	cat(":\n")

	for (i in order(overlap, decreasing=decreasing)) {
		cat("    ")
		cat(sprintf("%-8s", names[i]))
		cat(" ")
		cat(sprintf("%4.1f %%", 100*overlap[i]))
		cat("\n")
	}
	
	# reset plot settings
	if (exists("mordm.defaultpar")) {
		par(mordm.defaultpar)
	} else {
		mordm.defaultpar <<- par(no.readonly=TRUE)
	}

	# create the plot
	par(mfrow=c(2,n))

	for (i in order(overlap, decreasing=decreasing)[1:n]) {
		breaks <- seq(from=xlim[1,i], to=xlim[2,i], length.out=splits)
		hist(set1[,i], main=names[i], xlab="Set 1", xlim=xlim[,i], ylim=range(0, ylim[i]), freq=FALSE, breaks=breaks)
	}

	for (i in order(overlap, decreasing=decreasing)[1:n]) {
		breaks <- seq(from=xlim[1,i], to=xlim[2,i], length.out=splits)
		hist(set2[,i], main="", xlab="Set 2", xlim=xlim[,i], ylim=range(0, ylim[i]), freq=FALSE, breaks=breaks)
	}
}

mordm.prim <- function(data, objective, minimize=TRUE, percentages=FALSE, expand=TRUE, ...) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	x <- set[,1:nvars]
	varargs <- list(...)

	if (is.function(objective)) {
		y <- 1*mordm.select.indices(set, objective)
		minimize <- FALSE
		varargs$threshold.type=1
		varargs$threshold=0.5
	} else if (is.character(objective)) {
		y <- set[,objective]
	} else {
		y <- set[,nvars+objective]
	}

	if (minimize) {
		y <- -y
	}
	
	if (expand) {
		varargs$paste.alpha = 1
	}

	result <- do.call(prim.box, c(list(x, y), varargs))
	
	marks <- lapply(1:result$num.hdr.class, function(i) {
		# for some reason, the following statement is needed for i to evaluate
		# to the correct value
		i <- eval(i)
		mordm.mark.box(result$box[[i]])
	})
	
	if (is.function(objective)) {
		total <- nrow(set)
		union <- mordm.mark.union(marks)
		total.expected <- sum(mordm.select.indices(set, objective))
		total.captured <- sum(mordm.select.indices(set, mordm.mark.intersection(objective, union)))
		total.incorrect <- sum(mordm.select.indices(set, mordm.mark.intersection(mordm.mark.not(objective), union)))
		
		cat("PRIM Results:\n")
		cat("    Number of Boxes: ")
		cat(result$num.hdr.class)
		cat("\n")
		cat("    Points to Capture: ")
		cat(total.expected)
		cat("\n")
		cat("    Captured:  ")
		cat(ifelse(percentages, sprintf("%4.1f %%", 100*total.captured / total.expected), paste(total.captured)))
		cat("\n")
		cat("    Incorrect: ")
		cat(ifelse(percentages, sprintf("%4.1f %%", 100*total.incorrect / total.captured), paste(total.incorrect)))
		cat("\n")
	
		for (i in 1:length(marks)) {
			if (i == 1) {
				box.mark <- mordm.mark.intersection(objective, marks[[i]])
				box.diff <- mordm.mark.intersection(mordm.mark.not(objective), marks[[i]])
			} else {
				box.prev <- mordm.mark.union(marks[1:(i-1)])
				box.mark <- mordm.mark.intersection(mordm.mark.difference(objective, box.prev), marks[[i]])
				box.diff <- mordm.mark.intersection(mordm.mark.not(objective), marks[[i]])
			}
			
			captured <- sum(mordm.select.indices(set, box.mark))
			incorrect <- sum(mordm.select.indices(set, box.diff))
			
			cat("\n")
			cat("    Box ")
			cat(i)
			cat(":\n")
			cat("        Captured:  ")
			cat(ifelse(percentages, sprintf("%4.1f %%", 100*captured / total.expected), paste(captured)))
			cat("\n")
			cat("        Incorrect: ")
			cat(ifelse(percentages, sprintf("%4.1f %%", 100*incorrect / total.captured), paste(incorrect)))
			cat("\n")
		}
		
		cat("\n")
	} else {
		summary(result)
	}

	return(rev(marks))
}
mordm.printbox <- function(data, mark, threshold=0.01, digits=3) {
	nvars <- attr(data, "nvars")
	bounds <- attr(data, "bounds")
	
	if (is.null(bounds)) {
		stop("Bounds must be defined for the dataset")
	}
	
	box <- attr(mark, "box")
			
	if (is.null(box)) {
		warning("Given mark was not generated by PRIM")
		next
	}
	
	names <- colnames(box)
	unbound <- vector()
	
	cat("Bound Variables:\n")

	for (i in 1:nvars) {
		show.min <- FALSE
		show.max <- FALSE
		show.equals <- FALSE
		
		limits <- c(max(bounds[1,i], box[1,i]), min(bounds[2,i], box[2,i]))
		limits <- round(limits, digits=digits)
		
		if (abs(limits[1] - bounds[1,i]) > threshold*(bounds[2,i] - bounds[1,i])) {
			show.min <- TRUE
		}
		
		if (abs(limits[2] - bounds[2,i]) > threshold*(bounds[2,i] - bounds[1,i])) {
			show.max <- TRUE
		}
		
		if (abs(limits[1] - bounds[2,i]) <= threshold*(bounds[2,i] - bounds[1,i]) |
			abs(limits[2] - bounds[1,i]) <= threshold*(bounds[2,i] - bounds[1,i])) {
			show.equals <- TRUE
		}
		
		if (show.min & show.max) {
			cat("  ")
			cat(limits[1])
			cat(" <= ")
			cat(names[i])
			cat(" <= ")
			cat(limits[2])
			cat("\n")
		} else if (show.min) {
			cat("  ")
			cat(names[i])
			
			if (show.equals) {
				cat(" = ")
			} else {
				cat(" >= ")
			}
			
			cat(limits[1])
			cat("\n")
		} else if (show.max) {
			cat("  ")
			cat(names[i])
			
			if (show.equals) {
				cat(" = ")
			} else {
				cat(" <= ")
			}
			
			cat(limits[2])
			cat("\n")
		} else {
			unbound <- append(unbound, names[i])
		}
	}
	
	cat("\n")
	cat("Unbound Variables:\n")
	
	if (length(unbound) > 0) {
		for (i in 1:length(unbound)) {
			cat("  ")
			cat(unbound[i])
			cat("\n")
		}
	} else {
		cat("  None\n")
	}
	
	cat("\n")
}

mordm.plotbox <- function(data, mark, main="PRIM Box", scale.width=TRUE, bar.width=3, col=NULL, names=NULL) {
	nvars <- attr(data, "nvars")
	bounds <- attr(data, "bounds")
	
	if (is.null(bounds)) {
		stop("Bounds must be defined for the dataset")
	}
	
	longcol <- "black"
	outcol <- "transparent"
	
	# reset plot settings
	if (exists("mordm.defaultpar")) {
		par(mordm.defaultpar)
	} else {
		mordm.defaultpar <<- par(no.readonly=TRUE)
	}
	
	# create the plot
	layout(c(1,2), heights=c(7,1))
	par(xpd=TRUE, mar=c(4.1, 2.1, 4.1, 2.1))
	barplot(matrix(rep(1,3*nvars),ncol=nvars), add=FALSE, main=main, col="transparent", beside=TRUE, width=1, names.arg=rep("",nvars), axes=FALSE, space=c(0,3), border=c("transparent",longcol,"transparent"))
	
	if (!is.list(mark)) {
		mark = list(mark)
	}
	
	if (is.null(col)) {
		colors <- rainbow(length(mark), v=0.9)
	} else {
		colors <- col
	}
		
	for (i in 1:length(mark)) {
		# allow each mark to either be a single PRIM box or a group of boxes
		if (is.list(mark[[i]])) {
			mark.as.list <- unlist(mark[[i]])
		} else {
			mark.as.list <- list(mark[[i]])
		}
		
		# display each box in the group
		for (j in 1:length(mark.as.list)) {
			box <- attr(mark.as.list[[j]], "box")
			
			if (is.null(box)) {
				warning("Given mark was not generated by PRIM")
				next
			}
			
			# scale the box dimensions
			values <- sapply(1:nvars, function(i) (box[,i]-bounds[1,i]) / (bounds[2,i]-bounds[1,i]))
			
			# ensure the scaled values do not exceed the variable bounds (PRIM boxes can
			# extend outside the bounds)
			values[1,] <- sapply(1:nvars, function(i) max(values[1,i], 0))
			values[2,] <- sapply(1:nvars, function(i) min(values[2,i], 1))
			
			# create the matrix that is plotted, row1=outside, row2=inside, row3=outside
			mat <- rbind(values[1,], values[2,]-values[1,], 1-values[2,])
			colnames(mat) <- colnames(box)
			
			width <- ifelse(scale.width, bar.width/i, bar.width)
			spacer <- bar.width - width
			space <- append(1+spacer, rep(1+(4/3)*spacer, nvars-1))
			
			barplot(mat[,1:nvars],
					add=TRUE,
					col=c(outcol,colors[i],outcol),
					border=NA,
					names.arg=rep("",nvars),
					axes=FALSE,
					main="",
					width=width,
					space=space)
		}
	}
	
	axis(1,at = seq(4.5,by=6,length.out=nvars),labels=colnames(mat[,1:nvars]),las=1, line=2)
	text(seq(4.5,by=6,length.out=nvars), y=rep(1, nvars), pos=3, labels=sprintf("%.2f", bounds[2,]), cex=0.8)
	text(seq(4.5,by=6,length.out=nvars), y=rep(0, nvars), pos=1, labels=sprintf("%.2f", bounds[1,]), cex=0.8)
	
	# create the legend
	par(mar=c(0,0,0,0))
	plot.new()
	
	if (is.null(names)) {
		names <- sprintf("Box %i", 1:length(mark))
	}
	
	legend("center",
		   legend=names,
		   fill=colors,
		   bty="o",
		   cex=1.0,
		   horiz=TRUE,
		   xjust=0.5)
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
	obj.cov <- apply(abs(cov(obj)), 2, sum)
	obj.imp <- sqrt(obj.cov)

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

mordm.animate <- function(data, indices=1:length(data)) {
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	
	# first compute the limits
	bounds <- apply(data[[indices[1]]][,(nvars+1):(nvars+nobjs)], 2, range)
	
	for (i in indices[-1]) {
		bounds.temp <- apply(data[[i]][,(nvars+1):(nvars+nobjs)], 2, range)
		
		for (j in 1:nobjs) {
			bounds[,j] <- range(bounds[,j], bounds.temp[,j])
		}
	}
	
	# generate the snapshots
	files = vector()
	
	for (i in indices) {
		file <- paste("snapshot_", i, ".png", sep="")
		mordm.plot(data, index=i, xlim=bounds[,1], ylim=bounds[,2], zlim=bounds[,3])
		rgl.snapshot(file)
		files <- append(files, file)
	}
	
	opt <- ani.options(interval=0.1)
	im.convert(files, convert="gm convert")
}

mordm.sensitivity <- function(data, objective, all=FALSE, ...) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	names <- colnames(set)
	varargs <- list(...)

	if (all) {
		for (i in seq(length(data)-1, 1, -1)) {
			set <- rbind(set, mordm.getset(data, i))
		}
	}
	
	if (is.function(objective)) {
		if (class(objective) == "mark") {
			y <- 1*mordm.select.indices(set, objective)
			
			# the cheap kd estimator does not work too well on binary data
			if (is.null(varargs$kd.estimator) || varargs$kd.estimator == "cheap") {
				varargs$kd.estimator = "hist"
			}
		} else {
			y <- apply(set, 1, objective)
		}
	} else if (is.character(objective)) {
		y <- set[,objective]
	} else {
		y <- set[,nvars+objective]
	}
	
	print(do.call(deltamim, c(list(set[,1:nvars], y), varargs)))
}

mordm.robustness <- function(data, sd, nsamples, problem, method="variance") {
	set <- mordm.getset(data)
	set <- set[,1:problem$nvars]
	apply(set, 1, function(x) check.robustness(nsample(x, sd, nsamples, problem), problem, verbose=FALSE, method=method))
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






data <- mordm.read("lakeoutput.txt", 20, 5, 1,
	bounds=matrix(rep(range(0.0, 0.1), 20), nrow=2),
	maximize=c("Obj2", "Obj3", "Obj4", "Obj5"))
#mordm.plot(data)

#mordm.animate(data)

# Plot Operators
#mordm.plotops(data, time=FALSE, improvements=TRUE)

# Marking Demo
mark1 <- mordm.mark.rule(function(x) x[21] < 0.1)
#mark2 <- mordm.mark.rule(function(x) x[21] > 0.125)
#mark3 <- mordm.mark.not(mordm.mark.union(mark1, mark2))
#mordm.plot(data, mark=list(mark1, mark2, mark3))

# Select Solutions to Mark
#mark4 <- mordm.mark.selection()

# Prim Analysis - Bump Hunting
#boxes <- mordm.prim(data, mark1)
#mordm.plotprim(data, boxes)

# Identify Differences in Datasets
#boxes.union <- mordm.mark.union(boxes)
#prim <- mordm.select(data, boxes.union)
#non.prim <- mordm.select(data, mordm.mark.not(boxes.union))
#mordm.differences(prim, non.prim, decreasing=TRUE)

#mordm.recommend(data)

# Parallel Plot
#mordm.plotpar()
#mordm.identify()

# Box (Candlestick) Plot
#mordm.plotbox()
#mordm.identify()

# Find Scenarios with High Utility
#mark.high <- mordm.mark.rule(function(x) x["Obj2"] > 0.25)
#boxes.high <- mordm.prim(data, mark.high, paste.alpha=1)
#boxes.low <- mordm.prim(data, mordm.mark.not(mark.high))
#mordm.plotbox(data, boxes.high[[1]])
#mordm.printbox(data, boxes.high[[1]])

#mordm.plotprim(data, list(boxes.high, boxes.low), names=c("High Bentham Utility", "Low Bentham Utility"))

#mordm.sensitivity(data, function(x) x["Obj3"] + x["Obj4"], all=FALSE)


# Non-dominated ranking
#set <- mordm.join(data, index=1:length(data))
#print(nrow(set))
#ranks <- mordm.pareto.rank(set)
#mordm.plot(set, color=-ranks)


# Compute robustness at each point and color the plot
lake.problem <- setup("lake5obj.exe", 20, 5, 1,
					  bounds=matrix(rep(range(0, 0.1), 20), nrow=2))
y <- mordm.robustness(data, 0.01, 10, lake.problem, method="gap")
mordm.plot(data, color=y)

