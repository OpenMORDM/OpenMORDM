library(grid)
library(prim)
library(rgl)
library(scales)
library(MASS)
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

mordm.read <- function(file, nvars, nobjs, nconstrs=0, bounds=NULL, names=NULL) {
	text <- readLines(file)
	solutions <- vector()
	attributes <- vector()
	result <- list()
	
	if (is.null(names)) {
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

mordm.colorize <- function(set, objectives, mark, palette=heat.colors, n=100, offset=0, unmarked="#888888FF", alpha=1) {
    if (is.null(mark)) {
        colors <- set[,objectives[5]]
        colors <- (colors - min(colors)) / (max(colors) - min(colors))
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

mordm.plotbox <- function(highlight=NULL) {
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
	mordm.currentplot <<- "box"
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

mordm.plot <- function(data, index=-1, mark=NULL, objectives=NULL, ...) {
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

	if (nobjs >= 5 | !is.null(mark)) {
        colors <- mordm.colorize(set, objectives, mark)
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
	        
	        if (mordm.currentplot == "parallel") {
	            mordm.plotpar(highlight=i[1])
	        } else if (mordm.currentplot == "box") {
	        	mordm.plotbox(highlight=i[1])
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
	par(xpd=TRUE, mar=c(10,5,2,ifelse(improvements, 5, 2))+0.1)

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
		any(sapply(unlist(list(...)), function(rule) rule(x)))
	})
}

mordm.mark.intersection <- function(...) {
	mordm.mark.rule(function(x) {
		all(sapply(unlist(list(...)), function(rule) rule(x)))
	})
}

mordm.mark.difference <- function(...) {
	mordm.mark.rule(function(x) {
		sum(sapply(unlist(list(...)), function(rule) rule(x))) == 1
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

mordm.prim <- function(data, objective, minimize=TRUE, percentages=FALSE, ...) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	x <- set[,1:nvars]

	if (is.function(objective)) {
		y <- 1*mordm.select.indices(set, objective)
		minimize <- FALSE
	} else if (is.character(objective)) {
		y <- set[,objective]
	} else {
		y <- set[,nvars+objective]
	}

	if (minimize) {
		y <- -y
	}

	result <- prim.box(x, y, ...)
	
	marks <- lapply(1:result$num.hdr.class, function(i) {
		# for some reason, the following statement is needed for i to evaluate to the correct value
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
				box.union <- marks[[i]]
				box.mark <- mordm.mark.intersection(objective, marks[[i]])
				box.diff <- mordm.mark.intersection(mordm.mark.not(objective), marks[[i]])
			} else {
				box.prev <- box.union
				box.union <- mordm.mark.union(box.union, marks[[i]])
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
	} else {
		summary(result)
	}

	return(rev(marks))
}

mordm.plotprim <- function(data, mark, main="PRIM Box") {
	box <- attr(mark, "box")
	nvars <- attr(data, "nvars")
	bounds <- attr(data, "bounds")
	
	if (is.null(box)) {
		stop("Given mark was not generated by PRIM")
	}
	
	if (is.null(bounds)) {
		stop("Bounds must be defined for the dataset")
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
	
	longcol <- "black"
	incol <- "lightblue"
	outcol <- "transparent"
	
	# reset plot settings
	if (exists("mordm.defaultpar")) {
		par(mordm.defaultpar)
	} else {
		mordm.defaultpar <<- par(no.readonly=TRUE)
	}
	
	# create the plot
	par(xpd=TRUE, mar=c(6.1, 2.1, 4.1, 2.1))
	barplot(matrix(rep(1,3*nvars),ncol=nvars), add=FALSE, main=main, col="transparent", beside=TRUE, width=1, names.arg=rep("",nvars), axes=FALSE, space=c(0,3), border=c("transparent",longcol,"transparent"))
	barplot(mat[,1:nvars], add=TRUE, col=c(outcol,incol,outcol), border=FALSE, names.arg=rep("",nvars), axes=FALSE, main="", width=3,space=1)
	axis(1,at = seq(4.5,by=6,length.out=nvars),labels=colnames(mat[,1:nvars]),las=1, line=2)
	text(seq(4.5,by=6,length.out=nvars), y=rep(1, nvars), pos=3, labels=sprintf("%.2f", bounds[2,]), cex=0.8)
	text(seq(4.5,by=6,length.out=nvars), y=rep(0, nvars), pos=1, labels=sprintf("%.2f", bounds[1,]), cex=0.8)
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
	bounds=matrix(rep(range(0.0, 0.1), 20), nrow=2))

#mordm.animate(data)

# Plot Operators
#mordm.plotops(data, time=FALSE, improvements=TRUE)

# Marking Demo
mark1 <- mordm.mark.rule(function(x) x[21] < 0.1)
#mark2 <- mordm.mark.rule(function(x) x[21] > 0.125)
#mark3 <- mordm.mark.not(mordm.mark.union(mark1, mark2))
#mordm.plot(data, mark=list(mark1, mark2, mark3))

#mark3 <- mordm.mark.not(mordm.mark.union(mark1, mark2))
#mordm.plot(data, mark=mark3)

# Similarity / Differences Demo
#set1 <- mordm.select(data, mark1)
#set2 <- mordm.select(data, mark2)
#mordm.differences(set1, set2, scale=FALSE, n=8)

# Prim Analysis - Bump Hunting
boxes <- mordm.prim(data, mark1, threshold.type=1, threshold=0.5)
mordm.plotprim(data, boxes[[2]])

#boxes.union <- mordm.mark.union(boxes)
#boxes.diff <- mordm.mark.difference(mark1, boxes.union)
#boxes.int <- mordm.mark.intersection(mark1, boxes.union)
#mordm.plot(data, mark=boxes.int)

#prim <- mordm.select(data, boxes.union)
#non.prim <- mordm.select(data, mordm.mark.not(boxes.union))
#mordm.differences(prim, non.prim, decreasing=TRUE)

#mordm.recommend(data)

# Parallel Plot
#mordm.plotset(data, objectives=c("Obj1", "Obj2", "Obj3", "Obj4", "Obj5"))
#mordm.plotpar()
#mordm.identify()

# Box (Candlestick) Plot
#mordm.plotbox()
#mordm.identify()