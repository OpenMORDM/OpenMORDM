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

#' A web-based tool (powered by Shiny) for exploring high-dimensional data sets.
#' 
#' If you are providing a custom welcome or selection panel and would like to
#' display custom resources, use \code{\link{addResourcePath}} to register the
#' directory containing the resources.
#' 
#' @param filename the name of the file, a matrix, or a data frame
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param nconstrs the number of constraints
#' @param names see \code{mordm.read} for details
#' @param bounds see \code{mordm.read} for details
#' @param maximize see \code{mordm.read} for details
#' @param order ordering of the objectives in the dropdown menus
#' @param visible.variables determines if variables are visible by default
#' @param plot3d.width the width of the 3D window
#' @param plot3d.height the hight of the 3D window
#' @param welcome.panel omordm.plotptional panel for displaying a intro message
#' @param selection.panel optional panel for displaying info about the selected
#'        point
#' @export
explore <- function(filename, nvars=NULL, nobjs=NULL, nconstrs=0, names=NULL, bounds=NULL,
					maximize=NULL, order=NULL, visible.variables=FALSE,
					plot3d.width="600px", plot3d.height="500px",
					welcome.panel=NULL, selection.panel=NULL) {
	# The available color palettes.  To add new palettes, you must also modify
	# to.palette(...)
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
	
	addResourcePath("OpenMORDM", system.file("www", package="OpenMORDM"))
	
	
	
	
	############################################################################
	# Server Code                                                              #
	############################################################################
	
	# Setup and load the data
	if (is.data.frame(filename) || is.matrix(filename)) {
		data <- mordm.read.matrix(filename, nvars=nvars, nobjs=nobjs, bounds=bounds, maximize=maximize, names=names)
	} else if (is.character(filename)) {
		if (!file.exists(filename)) {
			stop("The file does not exist")
		}
		
		if (tolower(substr(filename, nchar(filename)-3, nchar(filename))) == ".csv") {
			data <- mordm.read.csv(filename, nvars=nvars, nobjs=nobjs, bounds=bounds, maximize=maximize, names=names)
		} else if (tolower(substr(filename, nchar(filename)-3, nchar(filename))) == ".xls" || tolower(substr(filename, nchar(filename)-4, nchar(filename))) == ".xlsx") {
			data <- mordm.read.xls(filename, nvars=nvars, nobjs=nobjs, bounds=bounds, maximize=maximize, names=names)
		} else {
			if (is.null(nvars) || is.null(nobjs)) {
				stop("Must specify the number of variables and objectives when loading an MOEA runtime file")
			}
			
			data <- mordm.read(filename, nvars, nobjs, nconstrs, bounds=bounds,
							   names=names, maximize=maximize, digits=5)
		}
	} else {
		stop("The first argument must be a filename, a matrix, or a data frame")
	}

	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	nconstrs <- attr(data, "nconstrs")
	min.nfe <- attr(data[[1]], "NFE")
	max.nfe <- attr(data[[length(data)]], "NFE")
	step.nfe <- max.nfe / length(data)
	
	if (is.null(order)) {
		order <- 1:nobjs
	}
	
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
	
	set.par <- function(input, ...) {
		input$colormap.black # read value so that plots get redrawn
		
		if (exists("mordm.defaultpar")) par(mordm.defaultpar)
		par(...)
	}
	
	to.preference <- function(input, ignore.constant=FALSE) {
		weights <- rep(0, nobjs)
		
		for (i in 1:nobjs) {
			name <- paste("preference.", objectives[i], sep="")
			
			if (!is.null(input[[name]])) {
				weights[i] <- input[[name]]
			}
		}
		
		if (sum(weights) > 0) {
			weights <- weights / sum(weights)
		}
		
		function(x) {
			sum(sapply(1:nobjs, function(i) {
				weights[i] * (x[nvars+i] - limits[1,nvars+i]) / (limits[2,nvars+i]-limits[1,nvars+i])
			}))
		}
	}
	
	to.weighted.preference <- function(input, set, ignore.constant=FALSE, err=NULL, brush=NULL) {
		orig.preference <- to.preference(input, ignore.constant)
		weights <- sapply(1:nrow(set), function(i) { orig.preference(set[i,]) })
		weights.range <- range(weights)
		
		if (weights.range[2]-weights.range[1] == 0) {
			if (is.null(err)) {
				NULL
			} else {
				stop(err)
			}
		} else {
			preference <- function(x) {
				(orig.preference(x) - weights.range[1]) / (weights.range[2] - weights.range[1])
			}
			
			if (is.null(brush)) {
				preference
			} else {
				function(x) {
					value <- preference(x)
					value >= brush[1] && value <= brush[2]
				}
			}
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
	
	plot.brush.preference <- function(set, alpha, input, slider.transparency=0.01) {
		if (!is.null(input[["brush.Preference"]])) {
			limits <- input[["brush.Preference"]]
			preference <- to.preference(input)
			weights <- sapply(1:nrow(set), function(i) { preference(set[i,]) })
			weights.range <- range(weights)
			
			if (weights.range[2]-weights.range[1] == 0) {
				alpha
			} else {
				# normalize the weights to be [0, 1]
				weights <- (weights - weights.range[1]) / (weights.range[2] - weights.range[1])
				alpha[weights < limits[1] | weights > limits[2]] <- slider.transparency
				alpha
			}
		} else {
			alpha
		}
	}
	
	plot.brush <- function(set, limits=NULL, slider.transparency=0.01, preference=NULL, preference.limits=NULL) {
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
		set <- mordm.cbind(set, rep(0, nrow(set)))
		names <- c(names, "Constant")
		
		# brush the set
		brush.limits <- to.limits(input)
		alpha <- plot.brush(set, brush.limits, slider.transparency)
		alpha <- plot.brush.preference(set, alpha, input, slider.transparency)
		
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
		
		mark <- NULL
		colors <- NULL
		
		if (show.color == "Selected Point") {
			if (!is.null(selection) && selection > 0) {
				mark <- mordm.mark.points(set[selection,,drop=FALSE])
			} else {
				mark <- mordm.mark.rule(function(x) { FALSE })
			}
		} else if (show.color == "Preference") {
			preference <- to.preference(input)
			colors <- sapply(1:nrow(set), function(i) { preference(set[i,]) })
		} else {
			if (plot.toobj(show.color) > 0) {
				objectives <- c(objectives, plot.toobj(show.color))
				clim <- limits[,plot.toobj(show.color)]
			}
		}
		
		# update maximize array to match renamed axes
		maximize <- attr(data, "maximize")
		
		if (!is.null(maximize)) {
			maximize <- sapply(maximize, function(name) ifelse(is.character(name), which(colnames(set)==name), maximize))
			attr(set, "maximize") <- maximize
		}
		
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
				   palette=palette, colors=colors, crev=FALSE, alpha=alpha,
				   xlim=xlim, ylim=ylim, zlim=zlim, clim=clim,
				   tick.size=tick.size, label.size=label.size,
				   label.line=label.line, radius.scale=radius.scale,
				   window=window, bg=ifelse(input$colormap.black, "black", "white"),
				   fg=ifelse(input$colormap.black, "white", "black"))
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
		
		oldpar <- par(no.readonly=TRUE)
		set.par(input, mai=c(1, 2, 0.5, 2))
		
		set <- mordm.getset(data, index)
		
		if (show.color == "None" || show.color == "Constant") {
			plot.new()
		} else if (show.color == "Selected Point") {
			palette <- to.palette(colormap)
			legend("top", c("Selected Point", "All Other Points"), fill=c(palette[length(palette)], "#888888"), horiz=TRUE, bty="n")
		} else if (show.color == "Preference") {
			crange <- range(0, 1)
			palette <- to.palette(colormap)
			
			if (reverse) {
				palette <- rev(palette)
			}
			
			image(seq(crange[1], crange[2], (crange[2]-crange[1])/100), 0, matrix(seq(0, 1, 0.01), ncol=1), col=palette, axes=FALSE, xlab="Preference", ylab="")
			box("plot")
			axis(1, at=c(0, 1), labels=c("Least Preferred (0)", "Most Preferred (1)"))
		} else {
			if (plot.toobj(show.color) > 0) {
				crange <- limits[,plot.toobj(show.color)]
				palette <- to.palette(colormap)
				
				if (reverse) {
					palette <- rev(palette)
				}
				
				image(seq(crange[1], crange[2], (crange[2]-crange[1])/100), 0, matrix(seq(0, 1, 0.01), ncol=1), col=palette, axes=FALSE, xlab=show.color, ylab="")
				box("plot")
				
				factors <- attr(data, "factors")
				cfactors <- factors[[plot.toobj(show.color)]]
				
				if (!is.null(cfactors)) {
					axis(1, at=1:length(cfactors), labels=sprintf("%s (%d)", cfactors, 1:length(cfactors)))
				} else {
					axis(1)
				}
			} else {
				plot.new()
			}
		}
		
		par(oldpar)
	}
	
	to.columns <- function(input, ignore.constant=FALSE) {
		vars.null <- is.null(input$visible.variables)
		objs.null <- is.null(input$visible.objectives)
		
		if (vars.null) {
			if (objs.null) {
				vars <- rep(visible.variables, nvars)
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
		alpha <- plot.brush(original.set, brush.limits, input$slider.transparency)
		alpha <- plot.brush.preference(original.set, alpha, input, input$slider.transparency)
		transparency <- input$parallel.transparency * alpha
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
		set.par(input)
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
		
		set.par(input)
		
		# determine which columns to plot
		cols <- c(plot.toobj(input$tradeoff.x), plot.toobj(input$tradeoff.y))
		
		set <- original.set[,cols,drop=FALSE]
		colnames(set) <- c(colnames(data[[1]]), "Constant")[cols]
		brush.limits <- to.limits(input)
		alpha <- plot.brush(original.set, brush.limits, input$slider.transparency)
		alpha <- plot.brush.preference(original.set, alpha, input, input$slider.transparency)
		transparency <- input$tradeoff.transparency * alpha
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
			colors[highlight] <- alpha(par("fg"), 0.8)
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
			plot(subset, bg=colors, col=par("fg"), cex.axis=input$tradeoff.tick, cex.lab=input$tradeoff.label, cex=point.sizes, pch=21, xlim=xlim, ylim=ylim)
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
		
		set.par(input)
		
		# determine which columns to plot
		cols <- to.columns(input)
		
		set <- original.set[,cols,drop=FALSE]
		colnames(set) <- c(colnames(data[[1]]), "Constant")[cols]
		brush.limits <- to.limits(input)
		alpha <- plot.brush(original.set, brush.limits, input$slider.transparency)
		alpha <- plot.brush.preference(original.set, alpha, input, input$slider.transparency)
		transparency <- input$scatter.transparency * alpha
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
			colors[highlight] <- alpha(par("fg"), 0.8)
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
		alpha <- plot.brush.preference(set, alpha, input, 0.0)
		brushed.set <- set[alpha==1,,drop=FALSE]
		
		set.par(input)
		layout(matrix(c(1:nobjs, rep(nobjs+1, nobjs)), nrow=2, byrow=TRUE), heights=matrix(c(rep(7, nobjs), rep(1, nobjs)), nrow=2, byrow=TRUE))
		
		for (i in 1:nobjs) {
			if (input$histogram.smooth) {
				par(cex=input$histogram.label)
				d <- density(set[,nvars+i], n=input$histogram.splits*10)
				plot(d, main=colnames(set)[nvars+i], xlim=limits[,nvars+i], xlab="")
				polygon(d, col="red", border=par("fg"))
				
				if (input$histogram.brushed && nrow(set) != nrow(brushed.set)) {
					d2 <- density(brushed.set[,nvars+i], n=input$histogram.splits*10)
					d2$y <- d2$y * (d2$n / d$n)
					polygon(d2, col="blue", border=par("fg"))
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
		alpha <- plot.brush.preference(set, alpha, input, 0.0)
		
		result <- mordm.subset(set, columns=cols, rows=alpha==1)
		result <- mordm.as.data.frame(result)
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
	
	do.correlogram <- function(input) {
		set.par(input)
		
		set <- mordm.getset(data)
		
		if (input$correlations.objectives) {
			set <- set[,(nvars+1):(nvars+nobjs),drop=FALSE]
		} else {
			cols <- to.columns(input, ignore.constant=TRUE)
			set <- set[,cols,drop=FALSE]
		}
		
		if (input$colormap.black) {
			col.regions <- colorRampPalette(c("red","salmon","black","royalblue","navy"))
		} else {
			col.regions <- colorRampPalette(c("red","salmon","white","royalblue","navy"))
		}
		
		corrgram(set, order=FALSE, lower.panel=panel.shade,
				 upper.panel=panel.pie, text.panel=panel.txt,
				 main="", cex.labels=input$correlations.label,
				 col.regions=col.regions)
	}
	
	do.sensitivity <- function(input) {
		index <- to.index(input)
		
		if (input$sensitivity.response == "Brushed Set") {
			brush.limits <- to.limits(input, ignore.constant=TRUE)
			preference <- to.weighted.preference(input, mordm.getset(data), brush=input[["brush.Preference"]])
			
			if (is.null(brush.limits) && is.null(preference)) {
				stop("Must brush at least one response")
			}
			
			if (is.null(brush.limits)) {
				objective <- mordm.mark.rule(preference)
			} else if (is.null(preference)) {
				objective <- to.mark(brush.limits)
			} else {
				fcn1 <- to.mark(brush.limits)
				fcn2 <- mordm.mark.rule(preference)
				
				objective <- mordm.mark.rule(function(x) {
					fcn1(x) && fcn2(x)
				})
			}
		} else if (input$sensitivity.response == "Preference") {
			objective <- to.weighted.preference(input, mordm.getset(data), err="Please specify preferences on the 3D plot tab")
		} else {
			objective <- plot.toobj(input$sensitivity.response)-nvars
		}
		
		set.par(input)
		
		result <- tryCatch(
			mordm.sensitivity(data, objective, index=index,
							  all=input$sensitivity.all,
							  kd.estimator=input$sensitivity.kd.estimator,
							  plot.enabled=input$sensitivity.pdfs),
			error=function(msg) stop("Unable to compute sensitivities, try a different kernel density estimator"))
		
		indices <- result$Si
		
		if (input$sensitivity.order) {
			indices <- indices[,result$rank,drop=FALSE]
		}
		
		if (!input$sensitivity.pdfs) {
			barplot(indices, ylim=c(0,1), ylab="Sensitivity Indices")
		}
	}
	
	do.prim <- function(input) {
		if (input$prim.response == "Brushed Set") {
			brush.limits <- to.limits(input, ignore.constant=TRUE)
			preference <- to.weighted.preference(input, mordm.getset(data), brush=input[["brush.Preference"]])
			
			if (is.null(brush.limits) && is.null(preference)) {
				stop("Must brush at least one response")
			}
			
			if (is.null(brush.limits)) {
				objective <- mordm.mark.rule(preference)
			} else if (is.null(preference)) {
				objective <- to.mark(brush.limits)
			} else {
				fcn1 <- to.mark(brush.limits)
				fcn2 <- mordm.mark.rule(preference)
				
				objective <- mordm.mark.rule(function(x) {
					fcn1(x) && fcn2(x)
				})
			}
			
			# these values will be ignored within mordm.prim
			threshold.type <- NULL
			threshold <- NULL
		} else if (input$prim.response == "Preference") {
			objective <- to.weighted.preference(input, mordm.getset(data), err="Please specify preferences on the 3D plot tab")
			
			if (input$prim.threshold.type == ">=") {
				threshold.type <- 1
			} else if (input$prim.threshold.type == "<=") {
				threshold.type <- -1
			}
			
			threshold <- input$prim.threshold
		} else {
			objective <- plot.toobj(input$prim.response)-nvars
			objective.range <- range(mordm.getset(data)[,plot.toobj(input$prim.response)])
			
			if (input$prim.threshold.type == ">=") {
				threshold.type <- 1
			} else if (input$prim.threshold.type == "<=") {
				threshold.type <- -1
			}
			
			threshold <- (input$prim.threshold * (objective.range[2]-objective.range[1])) + objective.range[1]
		}
		
		result <- tryCatch(
			mordm.prim(data, objective, threshold.type=threshold.type,
					   threshold=threshold, minimize=FALSE,
					   expand=input$prim.expand),
			error=function(msg) stop("Unable to find any PRIM boxes with the given threshold, try CART instead"))
		
		# The first arg to mordm.plotbox is used to read nvars and bounds
		# attributes.  If this info is not available, estimate the bounds.
		if (!is.null(attr(data, "bounds"))) {
			est.bounds <- data
		} else {
			est.bounds <- list()
			set <- mordm.getset(data)
			
			if (is.function(objective)) {
				x <- set[,1:nvars]
			} else if (is.character(objective)) {
				if (nvars == 0) {
					x <- set[,-which(colnames(set) %in% objective)]
				} else {
					x <- set[,1:nvars]
				}
			} else {
				if (nvars == 0) {
					x <- set[,-(nvars+objective)]
				} else {
					x <- set[,1:nvars]
				}
			}
			
			attr(est.bounds, "nvars") <- ncol(x)
			attr(est.bounds, "bounds") <- apply(x, 2, range)
		}
		
		set.par(input)
		mordm.plotbox(est.bounds, result[[1]])
		
		result
	}
	
	do.cart <- function(input) {
		oldpar <- par(no.readonly=TRUE)
		set.par(input, xpd=TRUE)
		
		index <- to.index(input)
		set <- mordm.getset(data, index)
		
		if (input$cart.response == "Brushed Set") {
			brush.limits <- to.limits(input, ignore.constant=TRUE)
			preference <- to.weighted.preference(input, mordm.getset(data), brush=input[["brush.Preference"]])
			
			if (is.null(brush.limits) && is.null(preference)) {
				stop("Must brush at least one response")
			}
			
			if (is.null(brush.limits)) {
				objective <- mordm.mark.rule(preference)
			} else if (is.null(preference)) {
				objective <- to.mark(brush.limits)
			} else {
				fcn1 <- to.mark(brush.limits)
				fcn2 <- mordm.mark.rule(preference)
				
				objective <- mordm.mark.rule(function(x) {
					fcn1(x) && fcn2(x)
				})
			}
			
			y <- mordm.select.indices(set, objective)
			
			if (sum(y) == nrow(set)) {
				stop("The brushed set contains all points, unable to perform CART")
			} else if (sum(!y) == nrow(set)) {
				stop("The brushed set contains no points, unable to perform CART")
			}
			
			y[y==TRUE] <- "Survived"
			y[y==FALSE] <- "Removed"
			
			extended.set <- data.frame(mordm.as.data.frame(set), .internalResponse=y)
			
			if (nvars == 0) {
				factors <- colnames(set)
			} else {
				factors <- colnames(set)[1:nvars]
			}
			
			formula <- sprintf("`.internalResponse` ~ %s", paste(sprintf("`%s`", factors), collapse="+"))
			
			if (input$cart.method == "Conditional Inference Trees") {
				fit <- ctree(as.formula(formula), data=extended.set)
			} else {
				fit <- rpart(as.formula(formula), data=extended.set, method="class")
			}
		} else if (input$cart.response == "Preference") {
			preference <- to.preference(input)
			weights <- sapply(1:nrow(set), function(i) { preference(set[i,]) })
			weights.range <- range(weights)
			
			if (weights.range[2]-weights.range[1] == 0) {
				stop("Please specify preferences on the 3D plot tab")
			} else {
				# normalize the weights to be [0, 1]
				weights <- (weights - weights.range[1]) / (weights.range[2] - weights.range[1])
			}
			
			if (nvars == 0) {
				factors <- colnames(set)
			} else {
				factors <- colnames(set)[1:nvars]
			}
			
			extended.set <- data.frame(mordm.as.data.frame(set), .internalResponse=weights)
			formula <- sprintf("`.internalResponse` ~ %s", paste(sprintf("`%s`", factors), collapse="+"))
			
			if (input$cart.method == "Conditional Inference Trees") {
				fit <- ctree(as.formula(formula), data=extended.set)
			} else {
				fit <- rpart(as.formula(formula), data=extended.set, method=tolower(input$cart.method))
			}
		} else {
			if (nvars == 0) {
				factors <- colnames(set)[-which(colnames(set) %in% input$cart.response)]
			} else {
				factors <- colnames(set)[1:nvars]
			}
			
			formula <- sprintf("`%s` ~ %s", input$cart.response, paste(sprintf("`%s`", factors), collapse="+"))
			extended.set <- mordm.as.data.frame(set)
			
			if (is.factor(extended.set[[input$cart.response]])) {
				method = "class"
			} else {
				method = tolower(input$cart.method)
			}
			
			if (input$cart.method == "Conditional Inference Trees") {
				fit <- ctree(as.formula(formula), data=extended.set)
			} else {
				fit <- rpart(as.formula(formula), data=extended.set, method=method)
			}
		}
		
		if (input$cart.method != "Conditional Inference Trees" && input$cart.prune) {
			fit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
		}
		
		# plot the pruned tree
		if (input$cart.method == "Conditional Inference Trees") {
			plot(fit, type=ifelse(input$cart.n, "extended", "simple"))
		} else if (input$cart.fancy) {
			fg <- ifelse(input$colormap.black, "white", "black")
			prp(fit, type=2, extra=ifelse(input$cart.n, 1, 0), tweak=input$cart.label, branch.col=fg, under.col=fg, split.col=fg, nn.col=fg)
		} else {
			plot(fit, uniform=TRUE, compress=TRUE, branch=0, main="")
			text(fit, use.n=input$cart.n, all=FALSE, cex=input$cart.label)		
		}
		
		par(oldpar)
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
	
	server <- function(input, output, session) {
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
			
			output$preference.sliders <- renderUI({
				widgets <- lapply(1:nobjs, function(i) {
					name <- paste("preference.", objectives[i], sep="")
					list(div(sliderInput(name, objectives[i], min=-1, max=1, value=0, step=0.01, ticks=c("Min", "Unimportant", "Max")), style="padding-left: 10px; padding-bottom: 20px"))
					
				})
				
				widgets <- append(widgets, list(
					br(),
					br(),
					h4("Brushing on Preference"),
					sliderInput("brush.Preference", "Preference", min=0, max=1, value=c(0, 1), step=0.00001)))
				
				widgets
			})
			
			observe({
				if (!is.null(input$preference.color) && input$preference.color > 0) {
					updateSelectInput(session, "color", selected="Preference")
				}
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
					alpha <- plot.brush.preference(set, alpha, input, 0.0)
					
					result <- mordm.subset(set, rows=alpha==1)
					result <- mordm.as.data.frame(result)
					
					write.csv(result, file, row.names=FALSE)
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
					
					if (visible.variables) {
						selected <- names
					} else {
						selected <- NULL
					}
					
					checkboxGroupInput("visible.variables", NULL, names, selected=selected, inline=TRUE)
				} else {
					p("No variables in data.")
				}
			})
			
			output$objective.selection <- renderUI({
				if (nobjs > 0) {
					names <- colnames(data[[1]])[(nvars+1):(nvars+nobjs)]
					checkboxGroupInput("visible.objectives", NULL, names, selected=names, inline=TRUE)
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
			
			observe({
				output$raw.data <- renderDataTable({
					do.raw(input)
				})
			})
			
			output$correlations <- renderPlot({
				do.correlogram(input)
			})
			
			output$download.correlations.png <- downloadHandler(
				filename = "correlations.png",
				content = function(file) {
					size <- to.image.size(session, "correlations")
					png(file, height=size$height, width=size$width, units="in", res=72)
					do.correlogram(input)
					dev.off()
				})
			
			output$download.correlations.svg <- downloadHandler(
				filename = "correlations.svg",
				content = function(file) {
					size <- to.image.size(session, "correlations")
					svg(file, height=size$height, width=size$width)
					do.correlogram(input)
					dev.off()
				})
			
			output$download.correlations.eps <- downloadHandler(
				filename = "correlations.eps",
				content = function(file) {
					size <- to.image.size(session, "correlations")
					postscript(file, height=size$height, width=size$width)
					do.correlogram(input)
					dev.off()
				})
			
			output$correlations.text <- renderPrint({
				output$correlations.text <- renderPrint({ mordm.correlation(data, all=input$correlations.all, objectives=input$correlations.objectives) })
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
				
				output$prim.text <- renderUI({
					msg <- capture.output({
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
					
					list(pre(paste(msg, collapse="\n")))
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
			
			output$cart <- renderPlot({
				do.cart(input)
			})
			
			output$download.cart.png <- downloadHandler(
				filename = "cart.png",
				content = function(file) {
					size <- to.image.size(session, "cart")
					png(file, height=size$height, width=size$width, units="in", res=72)
					do.cart(input)
					dev.off()
				})
			
			output$download.cart.svg <- downloadHandler(
				filename = "cart.svg",
				content = function(file) {
					size <- to.image.size(session, "cart")
					svg(file, height=size$height, width=size$width)
					do.cart(input)
					dev.off()
				})
			
			output$download.cart.eps <- downloadHandler(
				filename = "cart.eps",
				content = function(file) {
					size <- to.image.size(session, "cart")
					postscript(file, height=size$height, width=size$width)
					do.cart(input)
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
					
					# calculate brushing so we can ignore hidden solutions
					brush.limits <- isolate(to.limits(input, ignore.constant=TRUE))
					alpha <- plot.brush(set, brush.limits, 0.0)
					alpha <- plot.brush.preference(set, alpha, input, 0.0)
					
					# render the 3d plot so we can get the correct projection
					draw <- renderForPicking({
						isolate(do.plot3d(input))
					})
					
					proj <- draw(session, "plot3d")
					d = matrix(nrow=nrow(set), ncol=1)
					
					# scale x coordinates since the plot in the browser is slightly
					# larger than the plot in the RGL window
					x <- 0.85*(x - proj$view[3]/2) + proj$view[3]/2
					y <- 0.85*(y - proj$view[4]/2) + proj$view[4]/2
					
					# calculate the distance from the point to the mouse position,
					# setting brushed solutions to Inf
					for (i in 1:nrow(set)) {
						if (alpha[i] == 1.0) {
							pos <- rgl.user2window(ifelse(x.axis == "Constant", 0, set[i,plot.toobj(x.axis)]),
												   ifelse(y.axis == "Constant", 0, set[i,plot.toobj(y.axis)]),
												   ifelse(z.axis == "Constant", 0, set[i,plot.toobj(z.axis)]),
												   projection=proj)
							d[i] = as.double(dist(rbind(pos[1:2],
														c(x/proj$view[3], 1-y/proj$view[4]))))
						} else {
							d[i] = Inf
						}
					}
					
					i <- order(d)
					
					# only select if the click was near the point
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
			
			if (!is.null(welcome.panel)) {
				output$welcome.panel <- welcome.panel(input, output, session)
			}
			
			if (is.null(selection.panel)) {
				output$selection.panel <- renderUI({
					p("Detailed views are not available for this data set.")
				})
			} else {
				output$selection.panel <- selection.panel(data, input, output, session)
			}
			
			observe({
				session$sendCustomMessage("bgChange", ifelse(input$colormap.black, "black", "white"))
				
				if (input$colormap.black) {
					if (exists("mordm.defaultpar")) par(mordm.defaultpar)
					par(bg="black", fg="white", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
					mordm.defaultpar <<- par(no.readonly=TRUE)
				} else {
					if (exists("mordm.defaultpar")) par(mordm.defaultpar)
					par(bg="white", fg="black", col="black", col.axis="black", col.lab="black", col.main="black", col.sub="black")
					mordm.defaultpar <<- par(no.readonly=TRUE)
				}
			})
	}
	


	
	############################################################################
	# UI Code                                                                  #
	############################################################################
	objectives <- colnames(data[[1]])[(nvars+1):(nvars+nobjs)]
	options <- c(objectives, "Constant")
	
	if (FALSE) {
		selectionListener <- '$(document).ready(function() { Shiny.addCustomMessageHandler("selection", function(message) { var elem = $("a").filter(function(index) { return $(this).text() === "Selection"; }); var bkgd = elem.css("background-color"); elem.animate({backgroundColor: "#FFCCCC"}).animate({backgroundColor: bkgd}) })})'
	} else {
		selectionListener <- '$(document).ready(function() { Shiny.addCustomMessageHandler("selection", function(message) { var elem = $("a").filter(function(index) { return $(this).text() === "Selection"; }); elem.effect("pulsate", { times:1 }, 2000); })})'
	}
	
	bgListener <- HTML('$(document).ready(function() { Shiny.addCustomMessageHandler("bgChange", function(message) { if (message == "black") { $("head").append(\'<link id="blackStylesheet" href="OpenMORDM/bootstrap.css" rel="stylesheet" type="text/css"></link>\'); } else { $("#blackStylesheet").remove(); } })})')
	
	# Define each of the tabs
	tab.welcome <- tabPanel("Welcome", 
							uiOutput("welcome.panel"))
	
	tab.3d <- tabPanel("3D Plot",
					   sidebarLayout(
					   	sidebarPanel(
					   		tabsetPanel(
					   			tabPanel("Plot",
					   					 div(
					   					 	h3("Plotting Controls"),
					   					 	helpText("Use the controls below to explore the multidimensional tradeoffs."),
					   					 	selectInput("x", "X-Axis", options, options[order[1]]),
					   					 	selectInput("y", "Y-Axis", options, ifelse(length(objectives) < 2, "Constant", options[order[2]])),
					   					 	selectInput("z", "Z-Axis", options, ifelse(length(objectives) < 3, "Constant", options[order[3]])),
					   					 	selectInput("color", "Color", c(options, "Preference","Selected Point"), ifelse(length(objectives) < 4, "Constant", options[order[4]])),
					   					 	selectInput("size", "Size", options, ifelse(length(objectives) < 5, "Constant", options[order[5]])),
					   					 	style="height: 550px")),
					   			tabPanel("Brush",
					   					 div(
					   					 	h3("Brushing Controls"),
					   					 	helpText("Brushing selects a subset of the data for further analysis."),
					   					 	uiOutput("brush.sliders"),
					   					 	br(),
					   					 	br(),
					   					 	h4("Additional Options"),
					   					 	sliderInput("slider.transparency", "Brush Transparency", min=0, max=0.1, value=0.005, step=0.005),
					   					 	style="height: 550px")),
					   			tabPanel("Preference",
					   					 div(
					   					 	h3("Preference Controls"),
					   					 	helpText("Explore how your preferences impact your decisions."),
					   					 	actionButton("preference.color", "Change Color to Preference"),
					   					 	br(),
					   					 	br(),
					   					 	uiOutput("preference.sliders"),
					   					 	style="height: 550px")),
					   			#tabPanel("Animate",
					   			#		 div(
					   			#		 	h3("Animation Controls"),
					   			#		 	helpText("User the slider below to show the set at different times during optimization."),
					   			#		 	uiOutput("slider.nfe"),
					   			#br(),
					   			#br(),
					   			#h4("GIF Plots"),
					   			#downloadButton("download.rotate.gif", "Rotate 360"),
					   			#downloadButton("download.converge.gif", "Convergence"),
					   			#		 	style="height: 550px")),
					   			tabPanel("Download",
					   					 div(
					   					 	h3("3D Plot Snapshot"),
					   					 	helpText("Download images of the 3D plot.  SVG/EPS generation may take a while."),
					   					 	downloadButton("download.plot3d.png", "PNG Image"),
					   					 	downloadButton("download.plot3d.svg", "SVG Image"),
					   					 	downloadButton("download.plot3d.eps", "EPS File"),
					   					 	br(),
					   					 	br(),
					   					 	h3("Colorbar Snapshot"),
					   					 	helpText("Download images of the colorbar."),
					   					 	downloadButton("download.colorbar.png", "PNG Image"),
					   					 	downloadButton("download.colorbar.svg", "SVG Image"),
					   					 	downloadButton("download.colorbar.eps", "EPS File"),
					   					 	br(),
					   					 	br(),
					   					 	h3("Data Download"),
					   					 	helpText("Download the current data set."),
					   					 	downloadButton("download.csv", "CSV File"),
					   					 	style="height: 550px")),
					   			tabPanel("Options",
					   					 div(
					   					 	h3("Additional Options"),
					   					 	checkboxInput("label", "Show Full Labels", value=FALSE),
					   					 	checkboxInput("ideal", "Show Ideal Point", value=TRUE),
					   					 	sliderInput("tick.size", "Tick Size", min=0.1, max=2, value=1, step=0.1),
					   					 	sliderInput("label.size", "Label Size", min=0.1, max=2, value=1.2, step=0.1),
					   					 	sliderInput("label.line", "Label Offset", min=1, max=5, value=2, step=0.25),
					   					 	sliderInput("radius.scale", "Sphere Radius", min=0.1, max=2, value=1, step=0.1),
					   					 	style="height: 550px")))),
					   	mainPanel(
					   		conditionalPanel("!output.plot3d", div(HTML("<noscript>You must enable Javascript to view this page properly.</noscript><script>var nav = navigator.userAgent.toLowerCase(); if (nav.indexOf('msie') != -1 && parseInt(nav.split('msie')[1]) <= 9) { document.write('This website will not work with Internet Explorer 9 or earlier versions.  This website works best in the latest versions of <a href=\"http://www.mozilla.org/firefox\">Firefox</a>, <a href=\"http://www.google.com/chrome/browser/\">Chrome</a>, <a href=\"http://www.opera.com\">Opera 22+</a>, Safari 8, and Internet Explorer 11.'); } else { document.write('Loading data, please wait...'); }</script>"))),
					   		div(webGLOutput("plot3d", width="100%", height="500px"), style=sprintf("overflow: hidden; width: %s; height: %s; margin: 0px auto;", plot3d.width, plot3d.height)),
					   		plotOutput("colorbar", height="150px"))))
	
	tab.2d <- tabPanel("2D Plots",
					   tabsetPanel(
					   	tabPanel("Parallel Coords.",
					   			 sidebarLayout(
					   			 	sidebarPanel(
					   			 		h3("Plotting Options"),
					   			 		sliderInput("parallel.lwd", "Line Width", min=1, max=4, value=2, step=1),
					   			 		sliderInput("parallel.transparency", "Transparency", min=0.1, max=1, value=1, step=0.1),
					   			 		sliderInput("parallel.cex", "Label Size", min=0.5, max=2, value=1, step=0.1),
					   			 		br(),
					   			 		br(),
					   			 		h4("Download"),
					   			 		downloadButton("download.parallel.png", "PNG Image"),
					   			 		downloadButton("download.parallel.svg", "SVG Image"),
					   			 		downloadButton("download.parallel.eps", "EPS File"),
					   			 		br(),
					   			 		br(),
					   			 		helpText("Note: EPS export does not support transparency.  Transparent lines will not appear.")),
					   			 	mainPanel(
					   			 		plotOutput("plot2d.parallel"),
					   			 		plotOutput("plot2d.parallel.colorbar", height="150px")))),
					   	tabPanel("Scatter Plots",
					   			 sidebarLayout(
					   			 	sidebarPanel(
					   			 		h3("Plotting Options"),
					   			 		sliderInput("scatter.point", "Point Size", min=0.5, max=4, value=2, step=0.5),
					   			 		sliderInput("scatter.transparency", "Point Transparency", min=0.1, max=1, value=1, step=0.1),
					   			 		sliderInput("scatter.label", "Label Size", min=0.5, max=2, value=1, step=0.1),
					   			 		br(),
					   			 		br(),
					   			 		h4("Download"),
					   			 		downloadButton("download.scatter.png", "PNG Image"),
					   			 		downloadButton("download.scatter.svg", "SVG Image"),
					   			 		downloadButton("download.scatter.eps", "EPS File"),
					   			 		br(),
					   			 		br(),
					   			 		helpText("Note: EPS export does not support transparency.  Transparent points will not appear.")),
					   			 	mainPanel(
					   			 		plotOutput("plot2d.scatter"),
					   			 		plotOutput("plot2d.scatter.colorbar", height="150px")))),
					   	tabPanel("Tradeoffs",
					   			 sidebarLayout(
					   			 	sidebarPanel(
					   			 		h3("Plotting Options"),
					   			 		selectInput("tradeoff.x", "X-Axis", objectives, objectives[order[1]]),
					   			 		selectInput("tradeoff.y", "Y-Axis", objectives, objectives[order[2]]),
					   			 		sliderInput("tradeoff.point", "Point Size", min=0.5, max=4, value=2, step=0.5),
					   			 		sliderInput("tradeoff.transparency", "Point Transparency", min=0.1, max=1, value=1, step=0.1),
					   			 		sliderInput("tradeoff.label", "Label Size", min=0.5, max=2, value=1, step=0.1),
					   			 		sliderInput("tradeoff.tick", "Tick Size", min=0.5, max=2, value=1, step=0.1),
					   			 		checkboxInput("tradeoff.circle", "Draw border around points", value=FALSE),
					   			 		checkboxInput("tradeoff.pareto", "Show only Pareto set", value=FALSE),
					   			 		checkboxInput("tradeoff.curve", "Fit curve to points", value=FALSE),
					   			 		br(),
					   			 		br(),
					   			 		h4("Download"),
					   			 		downloadButton("download.tradeoff.png", "PNG Image"),
					   			 		downloadButton("download.tradeoff.svg", "SVG Image"),
					   			 		downloadButton("download.tradeoff.eps", "EPS File"),
					   			 		br(),
					   			 		br(),
					   			 		helpText("Note: EPS export does not support transparency.  Transparent points will not appear.")),
					   			 	mainPanel(
					   			 		plotOutput("plot2d.tradeoff"),
					   			 		plotOutput("plot2d.tradeoff.colorbar", height="150px"),
					   			 		uiOutput("plot2d.tradeoff.function")))),
					   	tabPanel("Histograms",
					   			 sidebarLayout(
					   			 	sidebarPanel(
					   			 		h3("Plotting Options"),
					   			 		sliderInput("histogram.splits", "Number of Splits", min=2, max=20, value=10, step=1),
					   			 		sliderInput("histogram.label", "Label Size", min=0.5, max=2, value=1, step=0.1),
					   			 		checkboxInput("histogram.smooth", "Show smooth density", value=FALSE),
					   			 		checkboxInput("histogram.brushed", "Show brushed set overlay", value=TRUE),
					   			 		br(),
					   			 		br(),
					   			 		h4("Download"),
					   			 		downloadButton("download.histogram.png", "PNG Image"),
					   			 		downloadButton("download.histogram.svg", "SVG Image"),
					   			 		downloadButton("download.histogram.eps", "EPS File")),
					   			 	mainPanel(
					   			 		plotOutput("plot2d.histogram")))),
					   	tabPanel("Operators",
					   			 sidebarLayout(
					   			 	sidebarPanel(
					   			 		h3("Plotting Options"),
					   			 		checkboxInput("operators.time", "Show Wall Time", value=FALSE),
					   			 		checkboxInput("operators.improvements", "Show Number of Improvements", value=TRUE),
					   			 		checkboxInput("operators.log", "Log Scale", value=FALSE),
					   			 		checkboxInput("operators.current", "Show Current Time", value=TRUE),
					   			 		br(),
					   			 		br(),
					   			 		h4("Download"),
					   			 		downloadButton("download.operators.png", "PNG Image"),
					   			 		downloadButton("download.operators.svg", "SVG Image"),
					   			 		downloadButton("download.operators.eps", "EPS File")),
					   			 	mainPanel(
					   			 		plotOutput("plot2d.operators"))))))
	
	tab.selection <- tabPanel("Selection", 
							  uiOutput("selection.panel"))
	
	tab.analyze <- tabPanel("Analyze",
							tabsetPanel(
								tabPanel("Correlations",
										 sidebarLayout(
										 	sidebarPanel(
										 		h3("Correlation Options"),
										 		checkboxInput("correlations.all", "Show all correlations", FALSE),
										 		checkboxInput("correlations.objectives", "Only show correlations between objectives", FALSE),
										 		br(),
										 		br(),
										 		h4("Graphical Controls"),
										 		checkboxInput("correlations_graphical", "Show correlogram", FALSE),
										 		sliderInput("correlations.label", "Label Size", min=0.5, max=2, value=1, step=0.1),
										 		br(),
										 		br(),
										 		h4("Download"),
										 		downloadButton("download.correlations.png", "PNG Image"),
										 		downloadButton("download.correlations.svg", "SVG Image"),
										 		downloadButton("download.correlations.eps", "EPS File"),
										 		br(),
										 		br(),
										 		helpText("Note: Use the Options tab to select which variables/objectives are shown in the correlogram.")),
										 	mainPanel(
										 		conditionalPanel("input.correlations_graphical", div(plotOutput("correlations", width="500px", height="500px"), style="overflow: hidden; width: 500px; height: 500px; margin: 0px auto;")),
										 		pre(textOutput("correlations.text"))))),
								tabPanel("Sensitivity",
										 sidebarLayout(
										 	sidebarPanel(
										 		h3("Sensitivity Options"),
										 		selectInput("sensitivity.response", "Response", c(objectives, "Brushed Set", "Preference")),
										 		selectInput("sensitivity.kd.estimator", "Kernel Density Estimator", c("cheap", "stats", "diffusion", "hist")),
										 		checkboxInput("sensitivity.all", "Use all time series data", value=FALSE),
										 		checkboxInput("sensitivity.order", "Rank factors in plot", value=FALSE),
										 		checkboxInput("sensitivity.pdfs", "Plot PDFs", value=FALSE),
										 		br(),
										 		br(),
										 		h4("Download"),
										 		downloadButton("download.sensitivity.png", "PNG Image"),
										 		downloadButton("download.sensitivity.svg", "SVG Image"),
										 		downloadButton("download.sensitivity.eps", "EPS File")),
										 	mainPanel(
										 		plotOutput("sensitivity")))),
								tabPanel("PRIM",
										 sidebarLayout(
										 	sidebarPanel(
										 		h3("PRIM Options"),
										 		selectInput("prim.response", "Response", c(objectives, "Brushed Set", "Preference")),
										 		selectInput("prim.threshold.type", "Threshold Type", c(">=", "<=")),
										 		sliderInput("prim.threshold", "Threshold", min=0.0, max=1.0, value=0.5, step=0.01),
										 		checkboxInput("prim.expand", "Aggressively grow box", value=FALSE),
										 		br(),
										 		br(),
										 		h4("Download"),
										 		downloadButton("download.prim.png", "PNG Image"),
										 		downloadButton("download.prim.svg", "SVG Image"),
										 		downloadButton("download.prim.eps", "EPS File"),
										 		br(),
										 		br(),
										 		helpText("Note: The threshold is scaled to the data.  E.g., 0.5 corresponds to the mean value.")),
										 	mainPanel(
										 		plotOutput("prim"),
										 		uiOutput("prim.text")))),
								tabPanel("CART",
										 sidebarLayout(
										 	sidebarPanel(
										 		h3("CART Options"),
										 		selectInput("cart.response", "Response", c(objectives, "Brushed Set", "Preference")),
										 		selectInput("cart.method", "Method", c("ANOVA", "Poisson")), #, "Class", "Conditional Inference Trees")),
										 		sliderInput("cart.label", "Label Size", min=0.5, max=2, value=1, step=0.1),
										 		checkboxInput("cart.prune", "Prune tree to minimize error", value=TRUE),
										 		checkboxInput("cart.n", "Show number of observations", value=TRUE),
										 		checkboxInput("cart.fancy", "Use fancy plot (PRP)", value=TRUE),
										 		br(),
										 		br(),
										 		h4("Download"),
										 		downloadButton("download.cart.png", "PNG Image"),
										 		downloadButton("download.cart.svg", "SVG Image"),
										 		downloadButton("download.cart.eps", "EPS File")),
										 	mainPanel(
										 		plotOutput("cart"))))))
	
	tab.raw <- tabPanel("Raw Data",
						dataTableOutput("raw.data"))
	
	tab.options <- tabPanel("Options",
							h4("Visible Decision Variables / Objectives"),
							helpText("By default, all decision variables and objectives are plotted.  Unselect any fields below to remove from plots."),
							div(
								h5("Visible Decision Variables"),
								uiOutput("variable.selection"),
								actionButton("variables.all", "All"),
								actionButton("variables.none", "None"),
								br(),
								br(),
								h5("Visible Objectives"),
								uiOutput("objective.selection"),
								actionButton("objectives.all", "All"),
								actionButton("objectives.none", "None"),
								style="margin-left: 25px"),
							hr(),
							h4("Color Scheme"),
							helpText("Set the color scheme used in all figures.  * indicates a color scheme suitable for colorblind individuals with deficient or anomalous red-green vision."),
							div(
								selectInput("colormap", "Color Scheme", colors),
								checkboxInput("colormap.reverse", "Reverse color map", value=FALSE),
								checkboxInput("colormap.black", "Black background", value=FALSE),
								style="margin-left: 25px"),
							hr(),
							h4("Selection"),
							helpText("Enable/disable selection and control plotting options."),
							div(
								checkboxInput("selection.enabled", "Selection enabled", value=TRUE),
								sliderInput("selection.scale", "Scale Selected Solutions", min=0.1, max=4.0, value=2, step=0.2),
								style="margin-left: 25px"),
							hr(),
							h4("Depth Ordering in 2D Plots"),
							helpText("The order in which glyphs / lines are plotted can impact how visible they are in the 2D plots.  Specify here how they are ordered."),
							div(
								selectInput("depth.order", "Depth Order", c("Default", objectives)),
								checkboxInput("depth.order.rev", "Reverse order (smaller values on top)", value=FALSE),
								style="margin-left: 25px"),
							hr(),
							h4("Downloaded Image Size"),
							helpText("By default, downloaded images are sized similarly to what you see in the browser.  Check the option below to customize the image size."),
							div(
								checkboxInput("custom_image", "Customize downloaded image size", value=FALSE),
								conditionalPanel("input.custom_image == true",
												 numericInput("image.width", "Width (in)", 8, min=1, step=0.5),
												 numericInput("image.height", "Height (in)", 6, min=1, step=0.5)),
								style="margin-left: 25px"),
							div(numericInput("selection", "Selected Point", 0), style="display: none;"))
	
	tab.about <- tabPanel("About",
						  sidebarLayout(
						  	sidebarPanel(
						  		h4("License"),
						  		p("Copyright 2014 The Pennsylvania State University"),
						  		p("OpenMORDM was developed by Dr. David Hadka with guidance from Dr. Klaus
						  		  Keller, Dr. Patrick Reed, and Dr. Robert Nicholas.  This work was supported by the National
						  		  Science Foundation through the Network for Sustainable Climate Risk
						  		  Management (SCRiM) under NSF cooperative agreement GEO-1240507."),
						  		p("Permission is hereby granted, free of charge, to any person obtaining a copy
						  		  of this software and associated documentation files (the \"Software\"), to deal
						  		  in the Software without restriction, including without limitation the rights
						  		  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
						  		  copies of the Software, and to permit persons to whom the Software is
						  		  furnished to do so, subject to the following conditions:"),
						  		p("The above copyright notice and this permission notice shall be included in
						  		  all copies or substantial portions of the Software."),
						  		p("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
						  		  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
						  		  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
						  		  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
						  		  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
						  		  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
						  		  THE SOFTWARE.")),
						  	mainPanel(
						  		h3("About OpenMORDM"),
						  		p("OpenMORDM is an open-source R library for multiobjective
						  		  robust decision making (MORDM). It includes support for
						  		  loading optimization output files (from the ",
						  		  a("Borg MOEA", href="http://borgmoea.org/"), " and ",
						  		  a("MOEA Framework", href="http://moeaframework.org/"),
						  		  " software), visualizing the data sets using various 2D and
						  		  3D plots, performing scenario discovery and tradeoff
						  		  analysis, and computing uncertainty/robustness metrics.
						  		  Development of OpenMORDM was supported by the National Science
						  		  Foundation through the Network for Sustainable Climate Risk Management
						  		  (SCRiM) under NSF cooperative agreement GEO-1240507.  Visit ",
						  		  a("http://scrimhub.org/", href="http://scrimhub.org/"), " for more details."),
						  		p("This web application is a demonstration of the capabilities
						  		  of OpenMORDM.  OpenMORDM was written in the R statistical
						  		  programming languages, and leverages a number of freely-available
						  		  tools from the R ecosystem.  Not all functionality is available
						  		  in this web application; please refer to the OpenMORDM R package
						  		  to access the full functionality.")),
					   	position="right"))

	# Collect the tabs into a list for display
	tabs <- list()

	if (!is.null(welcome.panel)) {
		tabs <- append(tabs, list(tab.welcome))
	}

	tabs <- append(tabs, list(tab.3d, tab.2d))

	if (!is.null(selection.panel)) {
		tabs <- append(tabs, list(tab.selection))
	}

	tabs <- append(tabs, list(tab.analyze, tab.raw, tab.options, tab.about))

	# Generate the UI
	navbar <- do.call(navbarPage, c(list("OpenMORDM", id="main"), tabs))
	
	ui <- fluidPage(
			tags$head(
				tags$script(src="OpenMORDM/jquery-ui.min.js"),
				tags$script(src="OpenMORDM/jquery.color.js"),
				tags$script(selectionListener),
				tags$script(bgListener)),
			navbar)
	
	runApp(list(server=server, ui=ui))
}