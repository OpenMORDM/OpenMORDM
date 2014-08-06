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
options(rgl.useNULL=TRUE)
library("rgl")
library("shiny")
library("shinyRGL")
library("scales")
library("functional")
library("dichromat")
source("OpenMORDM.R")

# Setup and load the data
cat("Loading data, this may take several minutes...\n")
data <- mordm.read(filename, nvars, nobjs, nconstrs, bounds=bounds,
				   names=objectives, maximize=maximize, digits=5)
cat("Finished loading data!\n")

min.nfe <- attr(data[[1]], "NFE")
max.nfe <- attr(data[[length(data)]], "NFE")
step.nfe <- max.nfe / length(data)

# Compute the limits on the data
compute.limits <- function(data) {
	limits <- NULL
	
	for (i in 1:length(data)) {
		temp.set <- mordm.getset(data, i)
		
		if (is.null(limits)) {
			limits <- apply(temp.set, 2, range)
		} else {
			limits <- apply(rbind(temp.set, limits), 2, range)
		}
	}

	names <- colnames(limits)
	limits <- cbind(limits, range(0)) # add range for constant objective
	colnames(limits) <- c(names, "Constant")
	limits
}

limits <- compute.limits(data)

# Returns the column index for the given objective.
plot.toobj <- function(name) {
	if (name %in% objectives) {
		nvars + which(objectives == name)
	} else if (name == "Constant") {
		nvars + length(objectives) + 1
	} else {
		0
	}
}

to.limits <- function(input, ignore.constant=FALSE) {
	brush.limits <- limits
	
	for (i in 1:nobjs) {
		name <- paste("brush.", objectives[i], sep="")
		
		if (!is.null(input[[name]])) {
			brush.limits[,nvars+i] <- input[[name]]
		}
	}
	
	if (all(brush.limits == limits)) {
		NULL
	} else if (ignore.constant) {
		brush.limits[,1:(nvars+nobjs)]
	} else {
		brush.limits
	}
}

plot.brush <- function(set, limits=NULL, slider.transparency=0.01) {
	if (is.null(limits)) {
		rep(1, nrow(set))
	} else {
		alpha <- 1.*apply(set, 1, function(x) all(x >= limits[1,] & x <= limits[2,]))
		alpha[alpha==0] = slider.transparency
		alpha
	}
}

to.palette <- function(name) {
	if (name == "Rainbow (Red to Blue)") {
		palette <- rainbow(100, start=0, end=0.66)
	} else if (name == "Heat (White to Red)") {
		palette <- rev(heat.colors(100))
	} else if (name == "Grayscale (White to Black)") {
		palette <- rev(gray(seq(0, 1, length.out=100)))
	} else if (name == "Dark Red to Blue*") {
		palette <- colorRampPalette(colorschemes$DarkRedtoBlue.18)(100)
	} else if (name == "Brown to Blue*") {
		palette <- colorRampPalette(colorschemes$BrowntoBlue.12)(100)
	} else if (name == "Blue to Green*") {
		palette <- colorRampPalette(colorschemes$BluetoGreen.14)(100)
	} else if (name == "Blue to Gray*") {
		palette <- colorRampPalette(colorschemes$BluetoGray.8)(100)
	} else if (name == "Light to Dark Blue*") {
		palette <- colorRampPalette(colorschemes$LightBluetoDarkBlue.10)(100)
	} else if (name == "Green to Magenta*") {
		palette <- colorRampPalette(colorschemes$GreentoMagenta.16)(100)
	} else if (name == "Categorical*") {
		palette <- colorschemes$Categorical.12
	}
}

do.plot3d <- function(input) {
	show.x <- input$x
	show.y <- input$y
	show.z <- input$z
	show.size <- input$size
	show.color <- input$color
	show.label <- input$label
	show.ideal <- input$ideal
	colormap <- input$colormap
	reverse <- input$colormap.reverse
	slider.transparency <- input$slider.transparency
	fontsize <- input$fontsize
	tick.size <- input$tick.size
	label.size <- input$label.size
	label.line <- input$label.line
	radius.scale <- input$radius.scale
	selection <- NULL
	
	if (is.null(input$nfe) || is.na(input$nfe)) {
		index <- length(data)
	} else {
		index <- input$nfe / step.nfe
	}
	
	if (!is.null(input$selection) && !is.na(input$selection) && input$selection.enabled) {
		selection <- input$selection
	}
	
	set <- mordm.getset(data, index)
	
	objectives <- vector()
	names <- colnames(set)
	
	# append a list of 0's in case the constant (column 5) option is selected
	set <- cbind(set, rep(0, nrow(set)))
	names <- c(names, "Constant")
	
	# brush the set
	brush.limits <- to.limits(input)
	alpha <- plot.brush(set, brush.limits, slider.transparency)
	
	# pick the user-defined axes
	xlim <- NULL
	ylim <- NULL
	zlim <- NULL
	slim <- NULL
	clim <- NULL
	
	if (plot.toobj(show.x) > 0) {
		objectives <- c(objectives, plot.toobj(show.x))
		xlim <- limits[,plot.toobj(show.x)]
		
		if (show.label) {
			names[plot.toobj(show.x)] <- show.x
		} else {
			names[plot.toobj(show.x)] <- "X"
		}
	}
	
	if (plot.toobj(show.y) > 0) {
		objectives <- c(objectives, plot.toobj(show.y))
		ylim <- limits[,plot.toobj(show.y)]
		
		if (show.label) {
			names[plot.toobj(show.y)] <- show.y
		} else {
			names[plot.toobj(show.y)] <- "Y"
		}
	}
	
	if (plot.toobj(show.z) > 0) {
		objectives <- c(objectives, plot.toobj(show.z))
		zlim <- limits[,plot.toobj(show.z)]
		
		if (show.label) {
			names[plot.toobj(show.z)] <- show.z
		} else {
			names[plot.toobj(show.z)] <- "Z"
		}
	}
	
	if (plot.toobj(show.size) > 0) {
		objectives <- c(objectives, plot.toobj(show.size))
		slim <- limits[,plot.toobj(show.size)]
	}
	
	if (show.color == "Selected Point") {
		if (!is.null(selection)) {
			mark <- mordm.mark.points(set[selection,,drop=FALSE])
		}
	} else {
		mark <- NULL
		
		if (plot.toobj(show.color) > 0) {
			objectives <- c(objectives, plot.toobj(show.color))
			clim <- limits[,plot.toobj(show.color)]
		}
	}
	
	# update maximize array to match renamed axes
	maximize <- attr(data, "maximize")
	maximize <- sapply(maximize, function(name) ifelse(is.character(name), which(colnames(set)==name), maximize))
	attr(set, "maximize") <- maximize
	
	# update column names
	colnames(set) <- names
	
	# select the color map
	palette <- to.palette(colormap)
	
	if (reverse) {
		palette <- rev(palette)
	}
	
	# generate the plot
	mordm.plot(set, mark=mark, objectives=objectives, identify=FALSE,
			   stay=FALSE, ideal=show.ideal, selection=selection,
			   palette=palette, crev=FALSE, alpha=alpha,
			   xlim=xlim, ylim=ylim, zlim=zlim, clim=clim,
			   tick.size=tick.size, label.size=label.size,
			   label.line=label.line, radius.scale=radius.scale,
			   window=window)
}

# Generates the colorbar
do.colorbar <- function(input) {
	show.color <- input$color
	colormap <- input$colormap
	reverse <- input$colormap.reverse
	
	if (is.null(input$nfe) || is.na(input$nfe)) {
		index <- length(data)
	} else {
		index <- input$nfe / step.nfe
	}
	
	oldmai <- par()$mai
	par(mai=c(1, 2, 0.5, 2))
	set <- mordm.getset(data, index)
	
	if (show.color == "None" || show.color == "Selected Point" || show.color == "Constant") {
		plot.new()
	} else {
		if (plot.toobj(show.color) > 0) {
			crange <- limits[,plot.toobj(show.color)]
			palette <- to.palette(colormap)
			
			if (reverse) {
				palette <- rev(palette)
			}
			
			image(seq(crange[1], crange[2], (crange[2]-crange[1])/100), 0, matrix(seq(0, 1, 0.01), ncol=1), col=palette, axes=FALSE, xlab=show.color, ylab="")
			box("plot")
			axis(1)
		} else {
			plot.new()
		}
	}
	
	par(mai=oldmai)
}

to.columns <- function(input, ignore.constant=FALSE) {
	vars.null <- is.null(input$visible.variables) || is.na(input$visible.variables)
	objs.null <- is.null(input$visible.objectives) || is.na(input$visible.objectives)
	
	if (vars.null) {
		if (objs.null) {
			vars <- rep(TRUE, nvars)
		} else {
			vars <- rep(FALSE, nvars)
		}
	} else {
		vars <- colnames(data[[1]])[1:nvars] %in% input$visible.variables
	}
	
	if (objs.null) {
		if (vars.null) {
			objs <- rep(TRUE, nobjs)
		} else {
			objs <- rep(FALSE, nobjs)
		}
	} else {
		objs <- colnames(data[[1]])[(nvars+1):(nvars+nobjs)] %in% input$visible.objectives
	}
	
	cols <- c(vars, objs)
	
	# remove the constant column
	if (!ignore.constant) {
		cols <- c(cols, FALSE)
	}
	
	cols
}

to.index <- function(input) {
	if (is.null(input$nfe) || is.na(input$nfe)) {
		index <- length(data)
	} else {
		index <- input$nfe / step.nfe
	}
	
	index
}

do.plotParallel <- function(input) {
	# plot 3D plot to set the appropriate colors
	do.plot3d(input)
	
	original.set <- mordm.currentset
	original.colors <- mordm.currentcolors
	
	# determine which columns to plot
	cols <- to.columns(input)
	
	# modify the colors for brushing
	set <- original.set[,cols,drop=FALSE]
	colnames(set) <- c(colnames(data[[1]]), "Constant")[cols]
	brush.limits <- to.limits(input)
	transparency <- input$parallel.transparency * plot.brush(original.set, brush.limits, input$slider.transparency)
	colors <- alpha(original.colors, transparency)
	
	# highlight the selected point
	if (is.null(input$selection) || !input$selection.enabled) {
		highlight <- NULL
	} else {
		highlight <- input$selection
		highlight <- highlight[highlight > 0 && highlight < nrow(set)]
		
		if (length(highlight) == 0) {
			highlight <- NULL
		}
	}
	
	# apply a custom ordering
	if (input$depth.order != "Default") {
		ordering <- order(original.set[,plot.toobj(input$depth.order)])
		
		if (input$depth.order.rev) {
			ordering <- rev(ordering)
		}
		
		set <- set[ordering,,drop=FALSE]
		colors <- colors[ordering]
		
		if (!is.null(highlight)) {
			highlight <- highlight[ordering]
		}
	}
	
	mordm.currentset <<- set
	mordm.currentcolors <<- colors
	
	# generate the parallel coordinates plot
	mordm.plotpar(alpha=NA, highlight=highlight, label.size=input$parallel.cex,
				  line.width=input$parallel.lwd, selection.scale=input$selection.scale)
	
	# restore the original settings
	mordm.currentset <<- original.set
	mordm.currentcolors <<- original.colors
}

do.plotOps <- function(input) {
	if (length(data) <= 1) {
		stop("The data does not contain any operator information.")
	}
	
	if (input$operators.current) {
		current <- to.index(input)
	} else {
		current <- NULL
	}
	
	mordm.plotops(data, time=input$operators.time,
				  improvements=input$operators.improvements,
				  log=input$operators.log,
				  current=current)
}

do.tradeoff <- function(input) {
	do.plot3d(input)
	
	original.set <- mordm.currentset
	original.colors <- mordm.currentcolors
	
	# determine which columns to plot
	cols <- c(plot.toobj(input$tradeoff.x), plot.toobj(input$tradeoff.y))
	
	set <- original.set[,cols,drop=FALSE]
	colnames(set) <- c(colnames(data[[1]]), "Constant")[cols]
	brush.limits <- to.limits(input)
	transparency <- input$tradeoff.transparency * plot.brush(original.set, brush.limits, input$slider.transparency)	
	colors <- alpha(original.colors, transparency)
	point.sizes <- rep(input$tradeoff.point, nrow(set))
	
	# highlight the selected point
	if (is.null(input$selection) || !input$selection.enabled) {
		highlight <- NULL
	} else {
		highlight <- input$selection
		highlight <- highlight[highlight > 0 && highlight < nrow(set)]
		
		if (length(highlight) == 0) {
			highlight <- NULL
		}
	}
	
	# apply a custom ordering
	if (input$depth.order != "Default") {
		ordering <- order(original.set[,plot.toobj(input$depth.order)])
		
		if (input$depth.order.rev) {
			ordering <- rev(ordering)
		}
		
		set <- set[ordering,,drop=FALSE]
		colors <- colors[ordering]
		
		if (!is.null(highlight)) {
			highlight <- highlight[ordering]
		}
	}
	
	# highlight selected solutions
	if (!is.null(highlight)) {
		original.colors <- alpha(colors[highlight], 1.0)
		colors[highlight] <- "black"
		point.sizes[highlight] <- 2*input$selection.scale*input$tradeoff.point	
		
		order <- 1:nrow(set)
		order <- append(order[-highlight], highlight)
		
		set <- rbind(set[order,], set[highlight,])
		colors <- c(colors[order], original.colors)
		point.sizes <- c(point.sizes[order], rep(input$selection.scale*input$tradeoff.point, length(highlight)))		
	}
	
	xlim <- range(set[,1])
	ylim <- range(set[,2])
	
	if (input$tradeoff.pareto) {
		# determine which objectives need to be negated (for maximization)
		maximizeTF <- rep(FALSE, nvars+nobjs)
		names(maximizeTF) <- colnames(data[[1]])
		
		if (!is.null(maximize)) {
			maximizeTF[maximize] <- TRUE
		}
		
		# negate the maximized objectives
		minset <- set
		minset[,maximizeTF[cols]] <- -minset[,maximizeTF[cols]]
		minset
		
		subset <- t(nondominated_points(t(minset)))
		
		# determine which indices were selected so we can select the appropriate colors/sizes
		indices <- apply(minset, 1, function(x) {
			for (i in 1:nrow(subset)) {
				match <- TRUE
				
				for (j in 1:ncol(subset)) {
					if (subset[i,j] != x[j]) {
						match <- FALSE
						break
					}
				}
				
				if (match) {
					return(TRUE)
				}
			}
			
			return(FALSE)
		})
		
		colors <- colors[indices]
		point.sizes <- point.sizes[indices]
		
		# return the maximized objectives to their original sign
		subset[,maximizeTF[cols]] <- -subset[,maximizeTF[cols]]		
	} else {
		subset <- set
	}

	if (input$tradeoff.circle) {
		plot(subset, bg=colors, col="black", cex.axis=input$tradeoff.tick, cex.lab=input$tradeoff.label, cex=point.sizes, pch=21, xlim=xlim, ylim=ylim)
	} else {
		plot(subset, col=colors, cex.axis=input$tradeoff.tick, cex.lab=input$tradeoff.label, cex=point.sizes, pch=20, xlim=xlim, ylim=ylim)
	}
	
	
	if (input$tradeoff.curve) {
		ordering <- order(subset[,1])
		
		#lo <- loess(subset[ordering,2]~subset[ordering,1])
		#lines(subset[ordering,1], predict(lo), lwd=3)
		
		fit <- lm(subset[ordering,2] ~ subset[ordering,1] + I(subset[ordering,1]^2), x=TRUE)
		lines(subset[ordering,1], predict(fit), lwd=3)
		
		return(fit)
	} else {
		return(NULL)
	}
}

do.scatter <- function(input) {
	do.plot3d(input)
	
	original.set <- mordm.currentset
	original.colors <- mordm.currentcolors
	
	# determine which columns to plot
	cols <- to.columns(input)
	
	set <- original.set[,cols,drop=FALSE]
	colnames(set) <- c(colnames(data[[1]]), "Constant")[cols]
	brush.limits <- to.limits(input)
	transparency <- input$scatter.transparency * plot.brush(original.set, brush.limits, input$slider.transparency)	
	colors <- alpha(original.colors, transparency)
	point.sizes <- rep(input$scatter.point, nrow(set))
	
	# highlight the selected point
	if (is.null(input$selection) || !input$selection.enabled) {
		highlight <- NULL
	} else {
		highlight <- input$selection
		highlight <- highlight[highlight > 0 && highlight < nrow(set)]
		
		if (length(highlight) == 0) {
			highlight <- NULL
		}
	}
	
	# apply a custom ordering
	if (input$depth.order != "Default") {
		ordering <- order(original.set[,plot.toobj(input$depth.order)])
		
		if (input$depth.order.rev) {
			ordering <- rev(ordering)
		}
		
		set <- set[ordering,,drop=FALSE]
		colors <- colors[ordering]
		
		if (!is.null(highlight)) {
			highlight <- highlight[ordering]
		}
	}
	
	# highlight selected solutions
	if (!is.null(highlight)) {
		original.colors <- alpha(colors[highlight], 1.0)
		colors[highlight] <- "black"
		point.sizes[highlight] <- 2*input$selection.scale*input$scatter.point	
		
		order <- 1:nrow(set)
		order <- append(order[-highlight], highlight)
			
		set <- rbind(set[order,], set[highlight,])
		colors <- c(colors[order], original.colors)
		point.sizes <- c(point.sizes[order], rep(input$selection.scale*input$scatter.point, length(highlight)))		
	}
	
	pairs(set, col=colors, cex.labels=input$scatter.label, cex=point.sizes, pch=20)
}

do.histogram <- function(input) {
	oldpar <- par(no.readonly=TRUE)
	index <- to.index(input)
	set <- mordm.getset(data, index)
	brush.limits <- to.limits(input, ignore.constant=TRUE)
	alpha <- plot.brush(set, brush.limits, 0.0)
	brushed.set <- set[alpha==1,,drop=FALSE]
	
	layout(matrix(c(1:nobjs, rep(nobjs+1, nobjs)), nrow=2, byrow=TRUE), heights=matrix(c(rep(7, nobjs), rep(1, nobjs)), nrow=2, byrow=TRUE))
	
	for (i in 1:nobjs) {
		if (input$histogram.smooth) {
			par(cex=input$histogram.label)
			d <- density(set[,nvars+i], n=input$histogram.splits*10)
			plot(d, main=colnames(set)[nvars+i], xlim=limits[,nvars+i], xlab="")
			polygon(d, col="red", border="black")
			
			if (input$histogram.brushed && nrow(set) != nrow(brushed.set)) {
				d2 <- density(brushed.set[,nvars+i], n=input$histogram.splits*10)
				d2$y <- d2$y * (d2$n / d$n)
				polygon(d2, col="blue", border="black")
			}	
		} else {
			breaks <- seq(from=limits[1,nvars+i], to=limits[2,nvars+i], length.out=input$histogram.splits+1)
			par(cex=input$histogram.label)
			hist(set[,nvars+i], main=colnames(set)[nvars+i], xlim=limits[,nvars+i], xlab="", col="red", breaks=breaks)
			
			if (input$histogram.brushed && nrow(set) != nrow(brushed.set)) {
				hist(brushed.set[,nvars+i], main="", xlim=limits[,nvars+i], xlab="", col="blue", add=TRUE, breaks=breaks)
			}	
		}
	}
	
	par(mar=c(0,0,0,0))
	plot.new()
	
	if (input$histogram.brushed && nrow(set) != nrow(brushed.set)) {
		legend("center", c("Original Set", "Brushed Set"), fill=c("red", "blue"), horiz=TRUE)
	} else {
		legend("center", c("Frequency"), fill=c("red"), horiz=TRUE)
	}
	
	par(oldpar)
}

do.raw <- function(input) {
	index <- to.index(input)
	cols <- to.columns(input, ignore.constant=TRUE)

	set <- mordm.getset(data, index)
	brush.limits <- to.limits(input, ignore.constant=TRUE)
	alpha <- plot.brush(set, brush.limits, 0.0)
	set[alpha==1,cols]
}

to.image.size <- function(session, id, dpi=72) {
	if (session$input$custom_image) {
		list(width=session$input$image.width, height=session$input$image.height)
	} else {
		list(width=session$clientData[[paste("output_", id, "_width", sep="")]]/dpi,
			 height=session$clientData[[paste("output_", id, "_height", sep="")]]/dpi)
	}
}

to.mark <- function(limits) {
	mordm.mark.rule(function(x) {
		all(x >= limits[1,] & x <= limits[2,])
	})
}

do.sensitivity <- function(input) {
	if (nvars == 0) {
		stop("Unable to compute sensitivities on data set with no decision variables")
	}
	
	index <- to.index(input)
	
	if (input$sensitivity.response == "Brushed Set") {
		brush.limits <- to.limits(input, ignore.constant=TRUE)
		
		if (is.null(brush.limits)) {
			stop("Must brush at least one response")
		}
		
		objective <- to.mark(brush.limits)
	} else {
		objective <- plot.toobj(input$sensitivity.response)-nvars
	}
	
	result <- tryCatch(mordm.sensitivity(data, objective, index=index,
								all=input$sensitivity.all,
								kd.estimator=input$sensitivity.kd.estimator,
								plot.enabled=input$sensitivity.pdfs),
					   error=function(msg) stop("Unable to compute sensitivities, try a different option"))
	
	indices <- result$Si
	
	if (input$sensitivity.order) {
		indices <- indices[,result$rank,drop=FALSE]
	}
	
	if (!input$sensitivity.pdfs) {
		barplot(indices, ylim=c(0,1), ylab="Sensitivity Indices")
	}
}

do.prim <- function(input) {
	if (nvars == 0) {
		stop("Unable to perform PRIM analysis on data set with no decision variables")
	}
	
	index <- to.index(input)
	
	if (input$prim.response == "Brushed Set") {
		brush.limits <- to.limits(input, ignore.constant=TRUE)
		
		if (is.null(brush.limits)) {
			stop("Must brush at least one response")
		}
		
		objective <- to.mark(brush.limits)
		
		# these values will be ignored within mordm.prim
		threshold.type <- NULL
		threshold <- NULL
	} else {
		objective <- plot.toobj(input$prim.response)-nvars
		objective.range <- range(mordm.getset(data, index)[,plot.toobj(input$prim.response)])
		
		if (input$prim_threshold_type == ">=") {
			threshold.type <- 1
		} else if (input$prim_threshold_type == "<=") {
			threshold.type <- -1
		}
		
		threshold <- (input$prim.threshold * (objective.range[2]-objective.range[1])) + objective.range[1]
	}

	result <- mordm.prim(data, objective, threshold.type=threshold.type,
						 threshold=threshold, minimize=FALSE,
						 expand=input$prim.expand)
	
	mordm.plotbox(data, result[[1]])
	
	result
}

renderForPicking <- function(expr, width="auto", height="auto", env = parent.frame(), 
						 quoted = FALSE){
	func <- exprToFunction(expr, env, quoted)
	return(function(shinysession, name, ...) {
		prefix <- "gl_output_"
		
		# Read in WebGL's width and height from the browser
		if (width == "auto") width <- isolate(shinysession$clientData[[paste(prefix, name, "_width", sep = "")]])
		if (height == "auto") height <- isolate(shinysession$clientData[[paste(prefix, name, "_height", sep = "")]])
		
		if (is.null(width) || is.null(height) || width <= 0 || height <= 0) return(NULL)
		
		if (is.null(width) || !is.numeric(width)) {
			stop("Can't support non-numeric width parameter. 'width' must be in px.")
		}
		
		if (is.null(height) || !is.numeric(height)) {
			stop("Can't support non-numeric height parameter. 'height' must be in px.")
		}
		
		#Open a null RGL device.
		open3d(useNULL = TRUE)
		#open3d(windowRect=c(0, 0, width, height))
		func()
		
		# Read in current values as they're updated so that we can regenerate 
		# the graph honoring the user's changes to the view, but isolate() so we
		# don't force a new graph every time the user interacts with it.
		zoom <- isolate(shinysession$clientData[[paste(prefix, name, "_zoom", sep="")]])
		fov <- isolate(shinysession$clientData[[paste(prefix, name, "_fov", sep="")]])
		pan <- isolate(shinysession$clientData[[paste(prefix, name, "_pan", sep="")]])
		proj <- isolate(shinysession$clientData[[paste(prefix, name, "_proj", sep="")]])
		
		if (!is.null(zoom)) {
			par3d(zoom = zoom)  
		}
		
		if (!is.null(fov)) {
			par3d(FOV=fov)
		}
		
		if (!is.null(pan)) {
			mat <- matrix(pan, ncol=4)
			par3d(userMatrix=mat)
		}
		
		if (is.null(proj) || is.null(pan)) {
			projection <- rgl.projection()	
		} else {
			# When the 3D window is not shown, the projection matrices do not match
			# what the user sees in the browser. This attempts to reconstruct the
			# correct projection. Proj and pan will be set when the user clicks
			# the window
			bbox <- par3d("bbox")
			scale <- par3d("scale")
			modelMatrix <- matrix(pan, ncol=4) %*% scaleMatrix(scale[1], scale[2], scale[3]) %*% t(translationMatrix(-(bbox[2]+bbox[1])/2, -(bbox[4]+bbox[3])/2, -(bbox[6]+bbox[5])/2))
			
			# Compute the observer distance
			diag <- scale * (c(bbox[2], bbox[4], bbox[6]) - c(bbox[1], bbox[3], bbox[5]))
			blen <- 1.266666 * sqrt(sum(diag*diag)) / 2 # 1.266666 is a magic scaling factor to get the two radii to match
			s <- sin(0.261799388) # FOV angle in radians / 2
			
			obs <- -c(0, 0, blen / s)
			newmodel <- modelMatrix
			oldmodelMatrix <- t(translationMatrix(obs[1], obs[2], obs[3])) %*% newmodel
			
			view <- c(0, 0, width, height)
			names(view) <- c("x", "y", "width", "height")
			
			projection <- list(model = oldmodelMatrix,
							   proj = matrix(proj, ncol=4),
							   view = view,
							   observer = -obs,
							   newmodel = newmodel)
		}
		
		rgl.close()
		return(projection)
	})
}

shinyServer(
	function(input, output, session) {
		output$plot3d <- renderWebGL({
			do.plot3d(input)
		})
		
		output$colorbar <- renderPlot({
			do.colorbar(input)
		})
		
		output$plot2d.parallel.colorbar <- renderPlot({
			do.colorbar(input)
		})
		
		output$plot2d.scatter.colorbar <- renderPlot({
			do.colorbar(input)
		})
		
		output$plot2d.tradeoff.colorbar <- renderPlot({
			do.colorbar(input)
		})
		
		output$download.colorbar.png <- downloadHandler(
			filename = "colorbar.png",
			content = function(file) {
				size <- to.image.size(session, "colorbar")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.colorbar(input)
				dev.off()
			})
		
		output$download.colorbar.svg <- downloadHandler(
			filename = "colorbar.svg",
			content = function(file) {
				size <- to.image.size(session, "colorbar")
				svg(file, height=size$height, width=size$width)
				do.colorbar(input)
				dev.off()
			})
		
		output$download.colorbar.eps <- downloadHandler(
			filename = "colorbar.eps",
			content = function(file) {
				size <- to.image.size(session, "colorbar")
				postscript(file, height=size$height, width=size$width)
				do.colorbar(input)
				dev.off()
			})
		
		output$brush.sliders <- renderUI({
			lapply(1:nobjs, function(i) {
				name <- paste("brush.", objectives[i], sep="")
				
				if (limits[2,nvars+i] - limits[1,nvars+i] < 0.8) {
					min <- limits[1,nvars+i]-0.00001
					max <- limits[2,nvars+i]+0.00001
				} else {
					min <- floor(limits[1,nvars+i])
					max <- ceil(limits[2,nvars+i])
				}

				sliderInput(name, objectives[i], min=min, max=max, value=c(min, max), step=0.00001)
			})
		})
		
		output$slider.nfe <- renderUI({
			if (length(data) > 1) {
				sliderInput("nfe", "Current NFE", min=min.nfe, max=max.nfe, value=max.nfe, step=step.nfe, animate=TRUE)
			} else {
				p("The data does not contain any time series.")
			}
		})
		
		output$download.plot3d.png <- downloadHandler(
			filename = "snapshot.png",
			content = function(file) {
				open3d(useNULL=FALSE, windowRect=c(0, 0, 600, 600))
				do.plot3d(input)
				
				zoom <- isolate(session$clientData[["gl_output_plot3d_zoom"]])
				fov <- isolate(session$clientData[["gl_output_plot3d_fov"]])
				pan <- isolate(session$clientData[["gl_output_plot3d_pan"]])

				if (!is.null(zoom)){
					par3d(zoom=zoom)  
				}
				
				if (!is.null(fov)){
					par3d(FOV=fov)
				}
				
				if (!is.null(pan)){
					mat <- matrix(pan, ncol=4)
					par3d(userMatrix=mat)
				}
				
				rgl.snapshot(file, fmt="png")
				rgl.close()
			})
		
		output$download.plot3d.svg <- downloadHandler(
			filename = "snapshot.svg",
			content = function(file) {
				open3d(useNULL=FALSE)#, windowRect=c(0, 0, 600, 600))
				do.plot3d(input)
				
				zoom <- isolate(session$clientData[["gl_output_plot3d_zoom"]])
				fov <- isolate(session$clientData[["gl_output_plot3d_fov"]])
				pan <- isolate(session$clientData[["gl_output_plot3d_pan"]])
				
				if (!is.null(zoom)){
					par3d(zoom=zoom)  
				}
				
				if (!is.null(fov)){
					par3d(FOV=fov)
				}
				
				if (!is.null(pan)){
					mat <- matrix(pan, ncol=4)
					par3d(userMatrix=mat)
				}
				
				rgl.postscript(file, fmt="svg", drawText=FALSE)
				rgl.close()
			})
		
		output$download.plot3d.eps <- downloadHandler(
			filename = "snapshot.eps",
			content = function(file) {
				open3d(useNULL=FALSE)#, windowRect=c(0, 0, 600, 600))
				do.plot3d(input)
				
				zoom <- isolate(session$clientData[["gl_output_plot3d_zoom"]])
				fov <- isolate(session$clientData[["gl_output_plot3d_fov"]])
				pan <- isolate(session$clientData[["gl_output_plot3d_pan"]])
				
				if (!is.null(zoom)){
					par3d(zoom=zoom)  
				}
				
				if (!is.null(fov)){
					par3d(FOV=fov)
				}
				
				if (!is.null(pan)){
					mat <- matrix(pan, ncol=4)
					par3d(userMatrix=mat)
				}
				
				rgl.postscript(file, fmt="eps", drawText=FALSE)
				rgl.close()
			})
		
		output$download.csv <- downloadHandler(
			filename = "dataset.csv",
			content = function(file) {
				if (is.null(input$nfe) || is.na(input$nfe)) {
					index = length(data)
				} else {
					index = input$nfe / step.nfe
				}
				
				set <- mordm.getset(data, index)
				brush.limits <- to.limits(input, ignore.constant=TRUE)
				alpha <- plot.brush(set, brush.limits, 0.0)
				write.csv(set[alpha==1,,drop=FALSE], file)
			})
		
		output$download.rotate.gif <- downloadHandler(
			filename = "rotate.gif",
			content = function(file) {
				palette <- to.palette(input$colormap)
				
				if (input$colormap.reverse) {
					palette <- rev(palette)
				}
				
				mordm.animate(data, output=file, indices=rep(length(data), 60), transform=spin3d(rpm=60/60), objectives=c(2,3,4), color=1, palette=palette, window=500)
			})
		
		output$download.converge.gif <- downloadHandler(
			filename = "converge.gif",
			content = function(file) {
				palette <- to.palette(input$colormap)
				
				if (input$colormap.reverse) {
					palette <- rev(palette)
				}
				
				mordm.animate(data, output=file, indices=1:length(data), transform=spin3d(rpm=0), objectives=c(2,3,4), color=1, palette=color.map, window=500)
			})
		
		output$plot2d.parallel <- renderPlot({
			do.plotParallel(input)
		})
		
		output$download.parallel.png <- downloadHandler(
			filename = "parallel.png",
			content = function(file) {
				size <- to.image.size(session, "plot2d.parallel")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.plotParallel(input)
				dev.off()
			})
		
		output$download.parallel.svg <- downloadHandler(
			filename = "parallel.svg",
			content = function(file) {
				size <- to.image.size(session, "plot2d.parallel")
				svg(file, height=size$height, width=size$width)
				do.plotParallel(input)
				dev.off()
			})
		
		output$download.parallel.eps <- downloadHandler(
			filename = "parallel.eps",
			content = function(file) {
				size <- to.image.size(session, "plot2d.parallel")
				postscript(file, height=size$height, width=size$width)
				do.plotParallel(input)
				dev.off()
			})
		
		output$plot2d.operators <- renderPlot({
			do.plotOps(input)
		})
		
		output$download.operators.png <- downloadHandler(
			filename = "operators.png",
			content = function(file) {
				size <- to.image.size(session, "plot2d.operators")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.plotOps(input)
				dev.off()
			})
		
		output$download.operators.svg <- downloadHandler(
			filename = "operators.svg",
			content = function(file) {
				size <- to.image.size(session, "plot2d.operators")
				svg(file, height=size$height, width=size$width)
				do.plotOps(input)
				dev.off()
			})
		
		output$download.operators.eps <- downloadHandler(
			filename = "operators.eps",
			content = function(file) {
				size <- to.image.size(session, "plot2d.operators")
				postscript(file, height=size$height, width=size$width)
				do.plotOps(input)
				dev.off()
			})
		
		output$plot2d.scatter <- renderPlot({
			do.scatter(input)
		})
		
		output$download.scatter.png <- downloadHandler(
			filename = "scatter.png",
			content = function(file) {
				size <- to.image.size(session, "plot2d.scatter")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.scatter(input)
				dev.off()
			})
		
		output$download.scatter.svg <- downloadHandler(
			filename = "scatter.svg",
			content = function(file) {
				size <- to.image.size(session, "plot2d.scatter")
				svg(file, height=size$height, width=size$width)
				do.scatter(input)
				dev.off()
			})
		
		output$download.scatter.eps <- downloadHandler(
			filename = "scatter.eps",
			content = function(file) {
				size <- to.image.size(session, "plot2d.scatter")
				postscript(file, height=size$height, width=size$width)
				do.scatter(input)
				dev.off()
			})
		
		output$plot2d.tradeoff <- renderPlot({
			fit <- do.tradeoff(input)
			
			if (is.null(fit)) {
				output$plot2d.tradeoff.function <- renderUI({})
			} else {
				fit.range <- range(fit$x[,2])
				output$plot2d.tradeoff.function <- renderUI({
					pre(paste("The displayed curve is defined by the function\n    f(x) = ",
							  ifelse(!is.na(fit$coefficients[3]) && fit$coefficients[3] > 0.0000001, paste(fit$coefficients[3], "x^2 + ", sep=""), ""),
							  ifelse(!is.na(fit$coefficients[2]) && fit$coefficients[2] > 0.0000001, paste(fit$coefficients[2], "x + ", sep=""), ""),
							  fit$coefficients[1], "\nfor x in [", fit.range[1], ", ",
							  fit.range[2], "].", sep=""))
				})
			}
		})
		
		output$download.tradeoff.png <- downloadHandler(
			filename = "tradeoff.png",
			content = function(file) {
				size <- to.image.size(session, "plot2d.tradeoff")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.tradeoff(input)
				dev.off()
			})
		
		output$download.tradeoff.svg <- downloadHandler(
			filename = "tradeoff.svg",
			content = function(file) {
				size <- to.image.size(session, "plot2d.tradeoff")
				svg(file, height=size$height, width=size$width)
				do.tradeoff(input)
				dev.off()
			})
		
		output$download.tradeoff.eps <- downloadHandler(
			filename = "tradeoff.eps",
			content = function(file) {
				size <- to.image.size(session, "plot2d.tradeoff")
				postscript(file, height=size$height, width=size$width)
				do.tradeoff(input)
				dev.off()
			})
		
		output$plot2d.histogram <- renderPlot({
			do.histogram(input)
		})
		
		output$download.histogram.png <- downloadHandler(
			filename = "histogram.png",
			content = function(file) {
				size <- to.image.size(session, "plot2d.histogram")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.histogram(input)
				dev.off()
			})
		
		output$download.histogram.svg <- downloadHandler(
			filename = "histogram.svg",
			content = function(file) {
				size <- to.image.size(session, "plot2d.histogram")
				svg(file, height=size$height, width=size$width)
				do.histogram(input)
				dev.off()
			})
		
		output$download.histogram.eps <- downloadHandler(
			filename = "histogram.eps",
			content = function(file) {
				size <- to.image.size(session, "plot2d.histogram")
				postscript(file, height=size$height, width=size$width)
				do.histogram(input)
				dev.off()
			})
		
		output$variable.selection <- renderUI({
			if (nvars > 0) {
				names <- colnames(data[[1]])[1:nvars]
				checkboxGroupInput("visible.variables", NULL, names, names, inline=TRUE)
			} else {
				p("No variables in data.")
			}
		})
		
		output$objective.selection <- renderUI({
			if (nobjs > 0) {
				names <- colnames(data[[1]])[(nvars+1):(nvars+nobjs)]
				checkboxGroupInput("visible.objectives", NULL, names, names, inline=TRUE)
			} else {
				p("No objectives in data.")
			}
		})
		
		observe({
			if (input$variables.all > 0) {
				updateCheckboxGroupInput(session, "visible.variables", selected=colnames(data[[1]])[1:nvars])
			}
		})
		
		observe({
			if (input$variables.none > 0) {
				updateCheckboxGroupInput(session, "visible.variables", selected=list())
			}
		})
		
		observe({
			if (input$objectives.all > 0) {
				updateCheckboxGroupInput(session, "visible.objectives", selected=colnames(data[[1]])[(nvars+1):(nvars+nobjs)])
			}
		})
		
		observe({
			if (input$objectives.none > 0) {
				updateCheckboxGroupInput(session, "visible.objectives", selected=list())
			}
		})
		
		output$raw.data <- renderDataTable({
			do.raw(input)
		})
		
		output$correlations <- renderPrint({
			mordm.correlation(data)
		})

		output$sensitivity <- renderPlot({
			do.sensitivity(input)
		})
		
		output$download.sensitivity.png <- downloadHandler(
			filename = "sensitivity.png",
			content = function(file) {
				size <- to.image.size(session, "sensitivity")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.sensitivity(input)
				dev.off()
			})
		
		output$download.sensitivity.svg <- downloadHandler(
			filename = "sensitivity.svg",
			content = function(file) {
				size <- to.image.size(session, "sensitivity")
				svg(file, height=size$height, width=size$width)
				do.sensitivity(input)
				dev.off()
			})
		
		output$download.sensitivity.eps <- downloadHandler(
			filename = "sensitivity.eps",
			content = function(file) {
				size <- to.image.size(session, "sensitivity")
				postscript(file, height=size$height, width=size$width)
				do.sensitivity(input)
				dev.off()
			})
		
		output$prim <- renderPlot({
			result <- do.prim(input)

			output$prim.text <- renderPrint({
				count <- 1
				
				for (box in result) {
					cat("--------------------------------------\n")
					cat(" Box ")
					cat(count)
					cat("\n")
					cat("--------------------------------------\n")
					
					mordm.printbox(data, box, indent="    ")
					count <- count + 1
				}
			})
		})
		
		output$download.prim.png <- downloadHandler(
			filename = "prim.png",
			content = function(file) {
				size <- to.image.size(session, "prim")
				png(file, height=size$height, width=size$width, units="in", res=72)
				do.prim(input)
				dev.off()
			})
		
		output$download.prim.svg <- downloadHandler(
			filename = "prim.svg",
			content = function(file) {
				size <- to.image.size(session, "prim")
				svg(file, height=size$height, width=size$width)
				do.prim(input)
				dev.off()
			})
		
		output$download.prim.eps <- downloadHandler(
			filename = "prim.eps",
			content = function(file) {
				size <- to.image.size(session, "prim")
				postscript(file, height=size$height, width=size$width)
				do.prim(input)
				dev.off()
			})
		
		observe({
			if (isolate(input$selection.enabled) && !is.null(input$plot3d.click)) {
				x <- input$plot3d.click[1]
				y <- input$plot3d.click[2]
				nfe <- isolate(input$nfe)
				radius <- isolate(input$radius.scale)
				x.axis <- isolate(input$x)
				y.axis <- isolate(input$y)
				z.axis <- isolate(input$z)
				old.selection <- isolate(input$selection)
				
				if (is.null(nfe) || is.na(nfe)) {
					index <- length(data)
				} else {
					index <- nfe / step.nfe
				}
				
				set <- mordm.getset(data, index)
				
				draw <- renderForPicking({
					isolate(do.plot3d(input))
				})
				
				proj <- draw(session, "plot3d")
				d = matrix(nrow=nrow(set), ncol=1)
				
				# scale x coordinates since the plot in the browser is slightly
				# larger than the plot in the RGL window
				x <- 0.85*(x - proj$view[3]/2) + proj$view[3]/2
				y <- 0.85*(y - proj$view[4]/2) + proj$view[4]/2
				
				for (i in 1:nrow(set)) {
					pos <- rgl.user2window(
											 ifelse(x.axis == "Constant", 0, set[i,plot.toobj(x.axis)]),
										   ifelse(y.axis == "Constant", 0, set[i,plot.toobj(y.axis)]),
										   ifelse(z.axis == "Constant", 0, set[i,plot.toobj(z.axis)]),
										   projection=proj)
					d[i] = as.double(dist(rbind(pos[1:2],
												c(x/proj$view[3], 1-y/proj$view[4]))))
				}
				
				i <- order(d)
				
				if (d[i[1]] <= radius*0.02) {
					cat("Selected point ")
					cat(i[1])
					cat(" with a distance ")
					cat(d[i[1]])
					cat("\n")
					
					if (old.selection != i[1]) {
						updateNumericInput(session, "selection", value=i[1])
					}
				} else {
					if (old.selection != 0) {
						updateNumericInput(session, "selection", value=0)
					}
				}
			}
		})
		
		observe({
			if (!is.null(input$selection) && input$selection != 0) {
				session$sendCustomMessage(type="selection", message=paste("Selected item", input$selection, sep=""))
			}
		})
		
		if (is.null(selection.panel)) {
			output$selection.panel <- renderUI({
				p("Detailed views are not available for this data set.")
			})
		} else {
			output$selection.panel <- selection.panel(data, input, output, session)
		}
	}
)
