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

#' Creates the default column names.
#' 
#' Creates default column names of the form \code{VarN}, \code{ObjN}, and
#' \code{ConstrN}.
#' 
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param nconstrs the number of constraints
#' @keywords internal
mordm.defaultnames <- function(nvars, nobjs, nconstrs=0) {
	names <- vector()
	
	if (nvars > 0) {
		names <- append(names, sprintf("Var%d", 1:nvars))
	}
	
	if (nobjs > 0) {
		names <- append(names, sprintf("Obj%d", 1:nobjs))
	}
	
	if (nconstrs > 0) {
		names <- append(names, sprintf("Constr%d", 1:nconstrs))
	}
	
	return(names)
}

#' Converts a data set into a data frame.
#' 
#' When loading data into OpenMORDM, any non-numeric columns are converted into
#' factors and represented internally as integers.  This method reverses that
#' process to get a data frame storing the original values.
#' 
#' @param entry the data set to convert
#' @export
mordm.as.data.frame <- function(entry) {
	result <- as.data.frame(entry)
	factors <- attr(entry, "factors")
	
	for (i in 1:length(factors)) {
		if (!is.null(factors[[i]])) {
			result[[i]] <- factor(factors[[i]][entry[,i]], factors[[i]])
		}
	}
	
	result
}

#' Loads a data set stored in a matrix or data.frame.
#' 
#' This method assumes the first N columns are decision variables, and all other
#' columns are objectives.  N is determined by \code{ncol(bounds)}.
#' 
#' @param mat the matrix or data.frame
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param bounds the lower and upper bounds of each decision variable
#' @param maximize vector indicating the columns to be maximized
#' @param names override the column names
#' @param ignore columns to remove from the dataset
#' @param metadata columns to retain in a metadata attribute
#' @export
mordm.read.matrix <- function(mat, nvars=NULL, nobjs=NULL, bounds=NULL, maximize=NULL, names=NULL, ignore=NULL, metadata=NULL) {
	entry <- mat
	meta <- NULL
	tmp.names <- NULL
	
	# Provide temporary column names (will generate final names later)
	if (!is.null(names) && length(names) == ncol(entry)) {
		tmp.names <- names
	} else if (!is.null(colnames(entry))) {
		tmp.names <- colnames(entry)
	} else {
		tmp.names <- sprintf("Col%d", 1:ncol(entry))
	}
	
	# Copy any metadata columns
	if (!is.null(metadata)) {
		if (!is.logical(metadata)) {
			metadata <- sapply(1:ncol(entry), function(i) { i %in% metadata | tmp.names[i] %in% metadata})
		}
		
		meta <- entry[,metadata,drop=FALSE]
	} else {
		metadata <- rep(FALSE, ncol(entry))
	}
	
	# Strip any ignored/metadata columns
	if (!is.null(ignore)) {
		if (!is.logical(ignore)) {
			ignore <- sapply(1:ncol(entry), function(i) { i %in% ignore | tmp.names[i] %in% ignore})
		}

		entry <- entry[,!metadata & !ignore,drop=FALSE]
	}
	
	# Determine number of variables and objectives
	if (is.null(nvars) && is.null(nobjs)) {
		nvars <- ifelse(is.null(bounds), 0, ncol(bounds))
		nobjs <- ncol(entry) - nvars
	} else if (is.null(nobjs)) {
		nobjs <- ncol(entry) - nvars
	} else if (is.null(nvars)) {
		nvars <- ncol(entry) - nobjs
	}
	
	# Provide actual column names
	if (is.null(names)) {
		if (is.null(colnames(entry))) {
			names <- mordm.defaultnames(nvars, nobjs)
		} else {
			names <- colnames(entry)
		}
	} else if (length(names) == nobjs) {
		names <- append(mordm.defaultnames(nvars, 0), names)
	} else if (length(names) != nvars + nobjs) {
		warning("Incorrect number of names, using defaults")
		names <- mordm.defaultnames(nvars, nobjs)
	}
	
	colnames(entry) <- names
	
	# Check if any columns contain character data, treat as factors
	factors <- list()
	
	for (i in 1:ncol(entry)) {
		if (!all(apply(entry[,i,drop=FALSE], 1, is.numeric))) {
			x <- as.factor(unlist(entry[,i]))
			factors <- append(factors, list(levels(x)))
			levels(x) <- 1:length(levels(x))
			entry[,i] <- as.numeric(x)
		} else {
			factors <- append(factors, list(NULL))
		}
	}
	
	# If entry is a data.frame, convert to a matrix
	entry <- as.matrix(entry)
	
	# Construct the data object used by OpenMORDM
	attr(entry, "nvars") <- nvars
	attr(entry, "nobjs") <- nobjs
	attr(entry, "nconstrs") <- 0
	attr(entry, "bounds") <- bounds
	attr(entry, "maximize") <- maximize
	attr(entry, "factors") <- factors
	
	if (!is.null(meta)) {
		attr(entry, "metadata") <- meta
	}
	
	result <- list()
	result <- append(result, list(entry))
	
	attr(result, "nvars") <- nvars
	attr(result, "nobjs") <- nobjs
	attr(result, "nconstrs") <- 0
	attr(result, "bounds") <- bounds
	attr(result, "maximize") <- maximize
	attr(result, "factors") <- factors
	class(result) <- "mordm"
	
	return(result)
}

#' Loads a data set stored in a CSV file.
#' 
#' This method assumes the first N columns are decision variables, and all other
#' columns are objectives.  N is determined by \code{ncol(bounds)}.  Unless
#' overridden, this method sets \code{check.names=FALSE} and
#' \code{header=TRUE}.
#' 
#' @param file the file name
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param bounds the lower and upper bounds of each decision variable
#' @param maximize vector indicating the columns to be maximized
#' @param names override the column names
#' @param ignore columns to remove from the dataset
#' @param metadata columns to retain in a metadata attribute
#' @param ... optional arguments passed to read.csv
#' @export
mordm.read.csv <- function(file, nvars=NULL, nobjs=NULL, bounds=NULL, maximize=NULL, names=NULL, ignore=NULL, metadata=NULL, ...) {
	varargs <- list(...)
	
	if (is.null(varargs$check.names)) {
		varargs$check.names <- FALSE
	}
	
	if (is.null(varargs$header)) {
		varargs$header <- TRUE
	}
	
	mat <- do.call(read.csv, c(list(file), varargs))
	
	mordm.read.matrix(mat, nvars=nvars, nobjs=nobjs, bounds=bounds, maximize=maximize, names=names, ignore=ignore, metadata=metadata)
}

#' Loads a data set stored in a XLS or XLSX file.
#' 
#' This method is similar in use to \code{\link{mordm.read.csv}}.  Requires
#' gdata and its dependencies, including a Perl interpreter on the host system.
#' 
#' @param file the file name
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param bounds the lower and upper bounds of each decision variable
#' @param maximize vector indicating the columns to be maximized
#' @param names override the column names
#' @param ignore columns to remove from the dataset
#' @param metadata columns to retain in a metadata attribute
#' @param ... optional arguments passed to read.xls
#' @export
mordm.read.xls <- function(file, nvars=NULL, nobjs=NULL, bounds=NULL, maximize=NULL, names=NULL, ignore=NULL, metadata=NULL, ...) {
	require(gdata)
	varargs <- list(...)
	
	if (is.null(varargs$check.names)) {
		varargs$check.names <- FALSE
	}
	
	if (is.null(varargs$header)) {
		varargs$header <- TRUE
	}
	
	mat <- do.call(read.xls, c(list(file), varargs))
	
	mordm.read.matrix(mat, nvars=nvars, nobjs=nobjs, bounds=bounds, maximize=maximize, names=names, ignore=ignore, metadata=metadata)
}

#' Loads the time series data output from an optimization algorithm.
#' 
#' Reads the time series data (a list of matrices) from an optimization
#' algorithm.  The format is defined by the Borg MOEA and MOEA Framework.
#' 
#' @param file the file name
#' @param nvars the number of decision variables
#' @param nobjs the number of objectives
#' @param nconstrs the number of constraints
#' @param bounds the lower and upper bounds of each decision variable
#' @param names override the column names
#' @param maximize vector indicating the columns to be maximized
#' @param digits number of digits to retain
#' @export
mordm.read <- function(file, nvars, nobjs, nconstrs=0, bounds=NULL, names=NULL, maximize=NULL, digits=NULL) {
	text <- readLines(file)
	solutions <- vector()
	attributes <- vector()
	result <- list()
	
	# Read arguments from mop class created by setup method
	if (class(nvars) == "mop") {
		problem <- nvars
		nvars <- problem$nvars
		nobjs <- problem$nobjs
		nconstrs <- problem$nconstrs
		bounds <- problem$bounds
		names <- problem$names[1:(nvars+nobjs)]
		maximize <- problem$maximize
	}
	
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
						
						if (!is.null(digits)) {
							entry[i,j] <- round(entry[i,j], digits=digits)
						}
					}
				}
				
				colnames(entry) <- names
				
				for (line in attributes) {
					tokens <- unlist(strsplit(line, "=", fixed=TRUE))
					
					if (length(tokens) == 1) {
						attr(entry, str_trim(tokens[1])) <- TRUE
					} else {
						attr(entry, str_trim(tokens[1])) <- as.double(tokens[2])
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

#' Normalizes the objectives.
#' 
#' By default, the objectives are all minimized.  Maximized objectives are
#' negated.  This function negates the maximized objectives, returning them to
#' their original, positive form.
#' 
#' @param data the data set
#' @param maximize vector indicating the columns to be maximized
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

#' Extracts the common attributes from the time series data.
#' 
#' Reads the common attributes associated with each entry in the time series
#' data and returns them in a matrix.
#' 
#' @param data the time series data
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

#' Returns a vector of colors to be used when plotting a data set.
#' 
#' The color data that is plotted depends on the options given to this function
#' in the following order:
#' \itemize{
#'   \item{colors}{- displays the user-defined color values}
#'   \item{mark}{- displays the user-defined marking rules, each rule with a separate color}
#'   \item{objectives}{- use color values stored in the column defined by \code{objective[5]}}
#' }
#' 
#' @param set the data set
#' @param objectives the objectives being plotted (objectives[5] is color)
#' @param mark the optional marking rule
#' @param palette the color palette to use, either a function that generates the
#'        color palette or the color palette itself
#' @param n the number of distinct colors to display (only used if palette is
#'        a function)
#' @param offset DEPRECATED
#' @param colors user-defined color values
#' @param clim range (lower and upper bounds) of color values
#' @param unmarked the color value used for unmarked points
#' @param alpha the transparency applied to all colors
#' @param crev if \code{TRUE}, reverse the color palette
#' @export
mordm.colorize <- function(set, objectives, mark=NULL, palette=heat.colors, n=100, offset=0, colors=NULL, clim=NULL, unmarked="#888888FF", alpha=1, crev=TRUE) {
	if (is.function(palette)) {
		colormap <- palette(n+1, alpha=alpha)
	} else {
		colormap <- alpha(palette, alpha=alpha)
		n <- length(colormap)-1
	}
	
	if (crev) {
		colormap <- rev(colormap)
	}
	
	if (!is.null(colors)) {
		if (length(colors) == 1) {
			colors <- set[,colors]
		}
		
		if (is.null(clim)) {
			clim <- range(colors, na.rm=TRUE)
		}
		
		denominator <- clim[2] - clim[1]
		colors <- pmin(pmax(colors, clim[1]), clim[2])
		
		if (denominator > 0) {
			colors <- (colors - clim[1]) / denominator
		} else {
			colors <- 0*colors + 1
		}
	} else if (is.null(mark)) {
		colors <- set[,objectives[5]]
		
		if (is.null(clim)) {
			clim <- range(colors, na.rm=TRUE)
		}
		
		denominator <- clim[2] - clim[1]
		colors <- pmin(pmax(colors, clim[1]), clim[2])
		
		if (denominator > 0) {
			colors <- (colors - clim[1]) / denominator
		} else {
			colors <- 0*colors + 1
		}
	} else if (is.list(mark)) {
		colors <- rep(0, nrow(set))
		
		for (i in 1:length(mark)) {
			indices <- mordm.select.indices(set, mark[i])
			colors[indices] <- i
		}
		
		colors <- colors / length(mark)
		colormap[1] <- unmarked
	} else {
		colors <- rep(0, nrow(set))
		indices <- mordm.select.indices(set, mark)
		colors[indices] <- 1
		colormap[1] <- unmarked
	}
	
	colormap[(colors*(n-offset))+offset+1]
}

#' Display a parallel plot of the current data set.
#' 
#' Creates a parallel axis or parallel coordinates plot of the current data
#' set.  All display attributes are taken from the current plotting options.
#' 
#' @param highlight vector of row indices to be highlighted in the plot
#' @param alpha the transparency value; or \code{NA}
#' @param label.size the font size of labels
#' @param line.width the width of lines
#' @param selection.scale the 
#' @export
mordm.plotpar <- function(highlight=NULL, alpha=0.4, label.size=1, line.width=1, selection.scale=2) {
	set <- get("current.set", mordm.globals)
	objectives <- get("current.objectives", mordm.globals)
	mark <- get("current.mark", mordm.globals)
	colors <- alpha(get("current.colors", mordm.globals), alpha*get("current.alpha", mordm.globals))

	# highlight selected solutions
	if (is.null(highlight)) {
		lwd <- line.width
	} else {
		lwd <- rep(line.width, nrow(set))
		lwd[highlight] <- 2*selection.scale*line.width
		original.colors <- alpha(colors[highlight], 1.0)
		colors[highlight] <- alpha(par("fg"), 0.8)
			
		order <- 1:nrow(set)
		order <- append(order[-highlight], highlight)
			
		set <- rbind(set[order,], set[highlight,])
		lwd <- c(lwd[order], rep(selection.scale*line.width, length(highlight)))
		colors <- c(colors[order], original.colors)
	}
	
	# reset plot settings
	if (exists("default.par", mordm.globals)) {
		par(get("default.par", mordm.globals))
	} else {
		assign("default.par", par(no.readonly=TRUE), mordm.globals)
	}

	# create the plot
	par(mgp=c(3,1,0.9), cex=label.size)
	parcoord(set, col=colors, lwd=lwd, var.label=TRUE)
	
	# store the plot settings
	assign("current.plot", "parallel", mordm.globals)
}

#' Display the markings in a box plot (candle stick plot).
#' 
#' Creates a box plot showing the range (lower and upper bounds) encompassed by
#' each marking.
#' 
#' @param highlight highlight vector of row indices to be highlighted in the plot
#' @export
mordm.plotmark <- function(highlight=NULL) {
	set <- get("current.set", mordm.globals)
	mark <- get("current.mark", mordm.globals)
	colors <- get("current.colors", mordm.globals)
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
	if (exists("default.par", mordm.globals)) {
		par(get("default.par", mordm.globals))
	} else {
		assign("default.par", par(no.readonly=TRUE), mordm.globals)
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
	assign("current.plot", "mark", mordm.globals)
}

#' Sets the current data set and displays a 3D scatter plot.
#' 
#' Creates a 3D scatter plot of the data.  As a side effect, this method sets
#' a global variable identifying the current data set and plotting attributes
#' to be used by other plotting methods in this package.  This design allows
#' you to easily create secondary plots that are consistent with the primary
#' 3D scatter plot.
#' 
#' @param data the data set to be displayed (if data is a time series, then the
#'        last entry in the time series is displayed)
#' @param mark a list of the markings to be displayed
#' @param index if data is a time series, controls which entries to display
#'        (see \code{\link{mordm.getset}} for details)
#' @param objectives vector specifying the objectives to be plotted on the
#'        x, y, z, size, and color axes
#' @param stay forces the 3D scatter plot to stay on top of other windows
#' @param identify if \code{TRUE}, clicking a point with the middle mouse button
#'        will identify and highlight that point
#' @param ideal draw a visual indicator of where the ideal point is on the plot
#' @param selection draw a visual indicator on the given row indices
#' @param selection.enlarge if \code{TRUE}, enlarges the selected point; otherwise
#'        renders a transparent cube around the selected point
#' @param colors override the color values
#' @param xlim range (lower and upper bounds) for the x axis
#' @param ylim range (lower and upper bounds) for the y axis
#' @param zlim range (lower and upper bounds) for the z axis
#' @param slim range (lower and upper bounds) of size values
#' @param clim range (lower and upper bounds) of color values
#' @param window the window size (w, h)
#' @param alpha vector of transparency values applied to each point
#' @param tick.size the size of the tick labels
#' @param label.size the size of axis labels
#' @param label.line the offset of the labels
#' @param radius.scale scaling factor applied to the size of the points
#' @param bg background color
#' @param fg foreground color
#' @param ... additional options passed to \code{\link{plot3d}}
#' @export
mordm.plot <- function(data, mark=NULL, index=-1, objectives=NULL, stay=TRUE, identify=TRUE, colors=NULL, clim=NULL, ideal=FALSE, selection=NULL, selection.enlarge=FALSE, xlim=NULL, ylim=NULL, zlim=NULL, slim=NULL, window=NULL, alpha=1, tick.size=1, label.size=1.2, label.line=1, radius.scale=1, bg="white", fg="black", ...) {
	set <- mordm.getset(data, index)
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	factors <- attr(data, "factors")
	
	names <- colnames(set)
	
	if (is.null(objectives)) {
		objectives <- (1:nobjs) + nvars
	}
	
	nobjs <- length(objectives)
	
	# setup the plot
	if (nobjs >= 2) {
		x <- set[,objectives[1]]
		xlab <- names[objectives[1]]
		xfactors <- factors[[objectives[1]]]
		
		if (!is.null(xfactors)) {
			xat <- 1:length(xfactors)
			xtick <- xfactors
		} else {
			xat <- NULL
			xtick <- NULL
		}

		y <- set[,objectives[2]]
		ylab <- names[objectives[2]]
		yfactors <- factors[[objectives[2]]]
		
		if (!is.null(yfactors)) {
			yat <- 1:length(yfactors)
			ytick <- yfactors
		} else {
			yat <- NULL
			ytick <- NULL
		}
	}
	
	if (nobjs >= 3) {
		z <- set[,objectives[3]]
		zlab <- names[objectives[3]]
		zfactors <- factors[[objectives[3]]]
		
		if (!is.null(zfactors)) {
			zat <- 1:length(zfactors)
			ztick <- zfactors
		} else {
			zat <- NULL
			ztick <- NULL
		}
	} else {
		z <- rep(0,nrow(set))
		zlab <- ""
		ztick <- c("")
		zat <- NULL
	}
	
	if (nobjs >= 4) {
		sizes <- set[,objectives[4]]
		
		if (is.null(slim)) {
			slim <- range(sizes, na.rm=TRUE)
		}
		
		denominator <- slim[2] - slim[1]
		sizes <- pmin(pmax(sizes, slim[1]), slim[2])
		
		if (denominator > 0) {
			sizes <- (sizes - slim[1]) / denominator
			sizes <- 1.6*sizes + 0.4
		} else {
			sizes <- 0*sizes + 1
		}
	} else {
		sizes <- rep(1,nrow(set))
	}
	
	if (nobjs >= 5 || !is.null(mark) || !is.null(colors)) {
		colors <- mordm.colorize(set, objectives, mark, colors=colors, clim=clim, ...)
	} else {
		colors <- rep("#888888",nrow(set))
	}
	
	rangex <- range(x, xlim)
	rangey <- range(y, ylim)
	rangez <- range(z, zlim)
	
	# hide the labels on any collapsed dimensions
	if (diff(rangex) == 0) {
		xlab <- ""
		xtick <- c("")
		xat <- c(rangex[1])
	}
	
	if (diff(rangey) == 0) {
		ylab <- ""
		ytick <- c("")
		yat <- c(rangey[1])
	}
	
	if (diff(rangez) == 0) {
		zlab <- ""
		ztick <- c("")
		zat <- c(rangez[1])
	}
	
	# if not being called by explore, override useNULL
	exploring = FALSE
	
	for (call in sys.calls()) {
		if (call[[1]] == "explore") {
			exploring = TRUE
			break
		}
	}
	
	if (!exploring) {
		if (rgl.cur() == 0 || names(rgl.cur()) == "null") {
			open3d(useNULL=FALSE)
		}
	}
	
	# create the plot
	par3d(cex=tick.size)
	bg3d(color=bg)

	plot3d(x, y, z,
				 type="s",
				 col=colors,
				 size=sizes*radius.scale,
				 xlab="",
				 ylab="",
				 zlab="",
				 xlim=xlim,
				 ylim=ylim,
				 zlim=zlim,
				 alpha=alpha,
				 ...)
	
	aspect3d(1)
	material3d(color=fg)
	bbox3d(xat=xat, xlab=xtick, yat=yat, ylab=ytick, zat=zat, zlab=ztick, front="line", back="line")
	
	if (label.size > 0) {
		title3d(xlab=xlab, ylab=ylab, zlab=zlab, cex=label.size, line=label.line, color=fg)
	}
	
	if (!is.null(window)) {
		if (length(window) == 1) {
			window <- c(0, 0, window, window)
		} else if (length(window) == 2) {
			window <- c(0, 0, window)
		}
		
		par3d(windowRect=window)
	}
	
	# highlight any selected points
	if (!is.null(selection)) {
		scale <- radius.scale/20
		
		for (i in selection) {
			if (i > 0 && i <= nrow(set)) {
				if (selection.enlarge) {
					spheres3d(x[i], y[i], z[i], radius=0.05*sizes[i]*radius.scale, col=colors[i])
				} else {
					cube <- cube3d(scaleMatrix(scale*sizes[i]*(rangex[2]-rangex[1]), scale*sizes[i]*(rangey[2]-rangey[1]), scale*sizes[i]*(rangez[2]-rangez[1])) %*% translationMatrix(x[i], y[i], z[i]))
					wire3d(cube, col=colors[i], alpha=0.5)
				}
			}
		}
	}
	
	# indicate the ideal point
	maximize <- attr(data, "maximize")

	if (ideal && !is.null(maximize)) {
		ideal <- c(rangex[1] - (rangex[2]-rangex[1])*0.015,
							 rangey[1] - (rangey[2]-rangey[1])*0.015,
							 rangez[1] - (rangez[2]-rangez[1])*0.015)
		
		if (!is.null(maximize)) {
			if (objectives[1] %in% maximize || names[objectives[1]] %in% maximize) {
				ideal[1] <- rangex[2] + (rangex[2]-rangex[1])*0.015
			}
			
			if (objectives[2] %in% maximize || names[objectives[2]] %in% maximize) {
				ideal[2] <- rangey[2] + (rangey[2]-rangey[1])*0.015
			}
			
			if (objectives[3] %in% maximize || names[objectives[3]] %in% maximize) {
				ideal[3] <- rangez[2] + (rangez[2]-rangez[1])*0.015
			}
		}
		
		par3d(ignoreExtent=TRUE)
		cube <- cube3d(scaleMatrix((rangex[2]-rangex[1])/50, (rangey[2]-rangey[1])/50, (rangez[2]-rangez[1])/50) %*% translationMatrix(ideal[1], ideal[2], ideal[3]))
		shade3d(cube, col=fg, alpha=0.5)
	}
	
	if (stay) {
		rgl.bringtotop(stay=TRUE)
	}
	
	if (identify) {
		mordm.identify()
	}
	
	# save the state to a global location so other functions can reference
	assign("current.set", set, mordm.globals)
	assign("current.objectives", objectives, mordm.globals)
	assign("current.mark", mark, mordm.globals)
	assign("current.colors", colors, mordm.globals)
	assign("current.alpha", alpha, mordm.globals)
}

#' Identify and highlight points using the middle mouse button.
#' 
#' Enables a mouse callback for clicking points on the 3D scatter plot and
#' identifying those points.  If any secondary plots are displayed (e.g.,
#' parallel coordinates plot or marking plot), the selected point will become
#' highlighted in those plots.
#' 
#' @param enabled if \code{TRUE}, enables this functionality
#' @param label if \code{TRUE}, a label will be added to the 3D scatter plot
#'        identifying the selected point
#' @export
mordm.identify <- function(enabled=TRUE, label=FALSE) {
	if (enabled) {
		rgl.setMouseCallbacks(3, begin=function(x, y) { 
			userMatrix <- par3d("userMatrix")
			viewport <- par3d("viewport")
			scale <- par3d("scale")
			projection <- rgl.projection()
			set <- get("current.set", mordm.globals)
			objectives <- get("current.objectives", mordm.globals)
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
			
			if (exists("current.plot", mordm.globals)) {
				if (get("current.plot", mordm.globals) == "parallel") {
					mordm.plotpar(highlight=i[1])
				} else if (get("current.plot", mordm.globals) == "mark") {
					mordm.plotmark(highlight=i[1])
				}
			}
		})
	} else {
		rgl.setMouseCallbacks(3)
	}
}

#' Plot operator probabilities.
#'
#' Creates a plot of the Borg MOEA operator probabilities across an entire
#' time series.
#' 
#' @param data the time series data from a Borg MOEA run
#' @param time if \code{TRUE}, the x-axis displays elapsed time; otherwise the
#'        x-axis represents the number of function evaluations (NFE)
#' @param improvements if \code{TRUE}, displays a trace of the number of Pareto
#'        improvements
#' @param log if \code{TRUE}, plot the x-axis with a log scale
#' @param improvement.nfe the window size in NFE when computing the number of
#'        improvements
#' @param current draw a line at the current time
#' @export
mordm.plotops <- function(data, time=FALSE, improvements=FALSE, log=FALSE, improvement.nfe=1000, current=NULL) {
	names <- c("SBX", "DE", "PCX", "SPX", "UNDX", "UM")
	colors <- c("cyan", "red", "blue", "green", "orange", "purple")
	attributes <- mordm.attributes(data)
	
	# reset plot settings
	if (exists("default.par", mordm.globals)) {
		par(get("default.par", mordm.globals))
	} else {
		assign("default.par", par(no.readonly=TRUE), mordm.globals)
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
			improve.per <- improve.diff / (nfe.diff/improvement.nfe)
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
		
		axis(4, at=improve.at, labels=improve.label, las="1", col="gray50", col.axis="gray50")
		mtext(paste("Pareto Improvements per ", ifelse(time, "Second", paste(improvement.nfe, " NFE", sep="")), sep=""), side=4, line=3, col="gray50")
		
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
	
	if (!is.null(current)) {
		value <- attr(mordm.getset(data, current), ifelse(time, "ElapsedTime", "NFE"))
		segments(value, 0, value, 100)
	}
	
	par(mar=c(0,0,0,0))
	plot.new()
	legend("center", legend=names, col=colors, lwd=4, bty="o", cex=1.0, horiz=TRUE, xjust=0.5)
}

#' Displays the correlations among pairwise factors.
#' 
#' Computes the pairwise correlations between the decision variables and
#' objectives and prints the formatted results.
#' 
#' @param data the data set to use
#' @param ht the threshold for highly correlated pairs
#' @param lt the threshold for uncorrelated pairs
#' @param all show all correlations
#' @param objectives only compute correlations between objectives
#' @export
mordm.correlation <- function(data, ht=0.75, lt=0.25, all=FALSE, objectives=FALSE) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	names <- colnames(set)
	
	if (objectives) {
		columns <- (nvars+1):(nvars+nobjs)
	} else {
		columns <- 1:ncol(set)
	}
	
	# compute and classify the correlations
	correlation.high <- matrix(nrow=0, ncol=3)
	correlation.inverse <- matrix(nrow=0, ncol=3)
	correlation.low <- matrix(nrow=0, ncol=3)
	correlation.medium <- matrix(nrow=0, ncol=3)
	
	for (i in 1:(length(columns)-1)) {
		for (j in (i+1):length(columns)) {
			correlation <- cor(set[,columns[i]], set[,columns[j]])
			
			if (correlation >= ht) {
				correlation.high <- rbind(correlation.high, c(columns[i], columns[j], correlation))
			} else if (correlation <= -ht) {
				correlation.inverse <- rbind(correlation.inverse, c(columns[i], columns[j], correlation))
			} else if (abs(correlation) < lt) {
				correlation.low <- rbind(correlation.low, c(columns[i], columns[j], correlation))
			} else {
				correlation.medium <- rbind(correlation.medium, c(columns[i], columns[j], correlation))
			}
		}
	}
	
	# display the results
	if (all) {
		correlation.all <- rbind(correlation.high, correlation.inverse, correlation.low, correlation.medium)
		
		for (i in rev(order(correlation.all[,3]))) {
			cat("    ")
			cat(sprintf("%-8s", names[correlation.all[i,1]]))
			cat(" <-> ")
			cat(sprintf("%-8s", names[correlation.all[i,2]]))
			cat(" (")
			cat(round(correlation.all[i,3], 2))
			cat(")\n")
		}
	} else {
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
			for (i in rev(order(correlation.high[,3]))) {
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
}

mordm.brush <- function(data, conditions) {
	attr(data, "brush") <- conditions
	
	last <- data[[length(data)]]
	print(min(last[,1]))
	print(max(last[,1]))
	print(apply(last, 1, conditions))
}

#' Creates a marking rule based on a function.
#' 
#' Markings allow the user to highlight specific subsets of the data set.  These
#' marked sets can subsequently be plotted or used in supported calculations.
#' 
#' @param condition a function of the form \code{f:x -> boolean}, where \code{x}
#'        is a single row from the data set, returning \code{TRUE} if the row is
#'        part of the marking
#' @export
mordm.mark.rule <- function(condition) {
	mark <- condition
	class(mark) <- "mark"
	return(mark)
}

#' Creates a marking rule identifying specific points.
#' 
#' Markings allow the user to highlight specific subsets of the data set.  These
#' marked sets can subsequently be plotted or used in supported calculations.
#' 
#' @param points one or more rows from the data set considered within the
#'        marking
#' @export
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

#' Creates a marking rule identifying the selected points.
#' 
#' Allows the user to select a rectangular region in the 3D scatter plot and
#' returns a marking containing all points within the selected region.
#' 
#' @export
mordm.mark.selection <- function() {
	cat("Use the mouse to select the points in the plot\n")
	flush.console()
	
	set <- get("current.set", mordm.globals)
	selection <- selectpoints3d(value=FALSE)
	
	cat("Selected ")
	cat(nrow(selection))
	cat(" points!\n")
	
	return(mordm.mark.points(set[selection[,"index"],]))
}

#' Creates a marking rule from PRIM boxes.
#' 
#' PRIM identifies one or more boxes.  This method converts from the PRIM box
#' representation to a marking.
#' 
#' @param box the box generated by \code{\link{mordm.prim}}
#' @param mean the mean of the box
#' @param mass the mass of the box
#' @export
mordm.mark.box <- function(box, mean, mass) {
	result <- mordm.mark.rule(function(x) {
		names <- colnames(box)
		all(sapply(1:length(names), function(i) x[names[i]] >= box[1,names[i]] & x[names[i]] <= box[2,names[i]]))
	})
	
	attr(result, "mean") <- mean
	attr(result, "mass") <- mass
	attr(result, "box") <- box
	return(result)
}

#' Computes the union of two markings.
#' 
#' Markings behave like sets.  A point is contained in the union if it is
#' contained within any individual marking.
#' 
#' @param ... the markings
#' @export
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

#' Computes the intersection of two markings.
#' 
#' Markings behave like sets.  A point is contained within the intersection if
#' it is contained in all individual markings.
#' 
#' @param ... the markings
#' @export
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

#' Computes the difference of two markings.
#' 
#' Markings behave like sets.  A point is contained within the difference if
#' it is contained in exactly one individual marking.  This is similar to the
#' exclusive-or operator.
#' 
#' @param ... the markings
#' @export
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

#' Computes the subtraction of two markings.
#' 
#' Markings behave like sets.  A point is contained within the subtraction if
#' it is contained in \code{rule1} but not \code{rule2}.
#' 
#' @param rule1 the first marking
#' @param rule2 the second marking
#' @export
mordm.mark.subtract <- function(rule1, rule2) {
	mordm.mark.rule(function(x) {
		rule1(x) & !rule2(x)
	})
}

#' Computes the inverse of a marking.
#' 
#' Markings behave like sets.  A point is contained within the inverse if it is
#' not contained within the original marking.
#' 
#' @param rule the original marking
#' @export
mordm.mark.not <- function(rule) {
	mordm.mark.rule(function(x) {
		!rule(x)
	})
}

#' Returns the individual data set entry to be analyzed.
#' 
#' Determines which data set will be analyzed.  Almost all methods use this
#' function to convert their \code{data} arguments into a single data set.
#' The \code{data} argument can be either a time series or an individual
#' data set.
#' 
#' @param data a time series or individual data set
#' @param index if \code{data} is a time series, specifies which entry to
#'        return (default is the last entry)
#' @export
mordm.getset <- function(data, index=-1) {
	if (is.matrix(data) || is.data.frame(data)) {
		set <- data
	} else {
		if (index < 1 | index > length(data)) {
			index <- length(data)
		}
		
		set <- data[[index]]
	}
	
	return(set)
}

#' Returns the subset of rows that are marked.
#' 
#' Applies one or more markings to the data set and returns the subset that
#' are contained within the markings.
#' 
#' @param data the data set to be displayed (if data is a time series, then the
#'        last entry in the time series is displayed)
#' @param marking list of markings
#' @param index if data is a time series, controls which entries to display
#'        (see \code{\link{mordm.getset}} for details)
#' @param not DEPRECATED
#' @param or DEPRECATED
#' @export
mordm.select <- function(data, marking, index=-1, not=FALSE, or=FALSE) {
	set <- mordm.getset(data, index)
	subset <- set[mordm.select.indices(set, marking, not, or),]
	attr(subset, "nvars") <- attr(set, "nvars")
	attr(subset, "nobjs") <- attr(set, "nobjs")
	attr(subset, "nconstrs") <- attr(set, "nconstrs")
	attr(subset, "bounds") <- attr(set, "bounds")
	attr(subset, "maximize") <- attr(set, "maximize")
	attr(subset, "factors") <- attr(set, "factors")
	return(subset)
}

#' Returns a subset of a data set.
#' 
#' @param set the data set
#' @param columns the columns to retain
#' @param rows the rows to retain
#' @export
mordm.subset <- function(set, columns=1:ncol(set), rows=1:nrow(set)) {
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	maximize <- attr(set, "maximize")
	names <- colnames(set)
	
	col.vars <- c()
	col.objs <- c()
	
	# select only the variables and objectives requested
	if (is.logical(columns)) {
		if (nvars > 0) {
			col.vars <- (1:nvars)[columns[1:nvars]]
		}
		
		if (nobjs > 0) {
			col.objs <- (1:nobjs)[columns[(nvars+1):(nvars+nobjs)]]
		}
	} else {
		if (nvars > 0) {
			keep.objs <- sapply(1:nvars, function(i) {
				!(i %in% columns || names[i] %in% columns)
			})
			col.vars <- (1:nvars)[keep.objs]
		}
		
		if (nobjs > 0) {
			keep.objs <- sapply(1:nobjs, function(i) {
				!(i+nvars %in% columns || names[i+nvars] %in% columns)
			})
			col.objs <- (1:nobjs)[keep.objs]
		}
	}
	
	if (!is.null(maximize)) {
		if (is.logical(maximize)) {
			maximize <- maximize[col.objs]
		} else {
			maximize <- maximize[maximize %in% names[col.objs+nvars] | maximize %in% (col.objs+nvars)]
			
			if (length(maximize) == 0) {
				maximize <- NULL
			}
		}
	}

	result <- set[rows,c(col.vars, col.objs+nvars),drop=FALSE]
	attr(result, "nvars") <- length(col.vars)
	attr(result, "nobjs") <- length(col.objs)
	attr(result, "nconstrs") <- attr(set, "nconstrs")
	attr(result, "bounds") <- attr(set, "bounds")[col.vars]
	attr(result, "maximize") <- maximize
	attr(result, "factors") <- attr(set, "factors")[c(col.vars, col.objs+nvars)]
	
	return(result)
}

#' Adds extra colums to the end of a data set.
#' 
#' The added columns are treated like objectives.
#' 
#' @param set the data set
#' @param columns the extra columns
#' @export
mordm.cbind <- function(set, columns) {
	if (is.vector(columns)) {
		columns <- as.data.frame(columns)
	}
	
	# Convert the new columns to numeric types
	factors <- attr(set, "factors")
	
	if (!is.null(factors)) {
		for (i in 1:ncol(columns)) {
			if (!all(apply(columns[,i,drop=FALSE], 1, is.numeric))) {
				x <- as.factor(unlist(columns[,i]))
				factors <- append(factors, list(levels(x)))
				levels(x) <- 1:length(levels(x))
				columns[,i] <- as.numeric(x)
			} else {
				factors <- append(factors, list(NULL))
			}
		}
	}
	
	columns <- as.matrix(columns)
	
	# Correctly handle different ways to express the maximized objectives
	maximize <- attr(set, "maximize")
	
	if (is.logical(maximize)) {
		maximize <- c(maximize, rep(FALSE, ncol(columns)))
	} else {
		maximize <- c(maximize, attr(columns, "maximize"))
	}
	
	# Append the new columns to the data set
	result <- cbind(set, columns)
	attr(result, "nvars") <- attr(set, "nvars")
	attr(result, "nobjs") <- attr(set, "nobjs")+ncol(columns)
	attr(result, "nconstrs") <- attr(set, "nconstrs")
	attr(result, "bounds") <- attr(set, "bounds")
	attr(result, "maximize") <- maximize
	attr(result, "factors") <- factors

	return(result)
}

#' Adds extra rows to the end of a data set.
#' 
#' @param set the data set
#' @param rows the extra rows
#' @export
mordm.rbind <- function(set, rows) {
	rows <- as.matrix(rows)
	
	# Append the new rows to the data set
	result <- rbind(set, rows)
	attr(result, "nvars") <- attr(set, "nvars")
	attr(result, "nobjs") <- attr(set, "nobjs")
	attr(result, "nconstrs") <- attr(set, "nconstrs")
	attr(result, "bounds") <- attr(set, "bounds")
	attr(result, "maximize") <- attr(set, "maximize")
	attr(result, "factors") <- attr(set, "factors")
	
	return(result)
}

#' Returns the row indices in the data set that are marked.
#' 
#' Applies one or more markings to the data set to determine which rows are
#' contained within the markings.
#' 
#' @param set the data set
#' @param marking list of markings
#' @param not DEPRECATED
#' @param or DEPRECATED
#' @export
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

#' Identifies key similarities/differences between two sets.
#' 
#' Computes key differences between two data sets.  This method prints formatted
#' text as well as creates a histogram plot showing key differences.
#' 
#' @param set1 the first set
#' @param set2 the second set
#' @param scale if \code{TRUE}, scale the plot based on the range of the data
#' @param decreasing if \code{TRUE}, order differences in decreasing order
#' @param splits the number of bins used by the histogram method
#' @param n the number of variables to plot
#' @param all plot all variables
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
	if (exists("default.par", mordm.globals)) {
		par(get("default.par", mordm.globals))
	} else {
		assign("default.par", par(no.readonly=TRUE), mordm.globals)
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

#' Patient rule induction method.
#' 
#' Performs the patient rule induction method (PRIM) to identify boxes in input
#' space that correlate with data exceeding a given threshold.
#' 
#' @param data the data set
#' @param objective speecifies the objective index, column name, function, or
#'        marking to use
#' @param minimize if \code{TRUE}, flip the threshold direction so that smaller
#'        values are preferred
#' @param percentages display percentages in the printout
#' @param expand if \code{TRUE}, sets the paste option to 1 to enable expanding
#'        the boxes to fill as much space as possible
#' @param ... optional arguments passed to \code{\link{prim.box}}
#' @export
mordm.prim <- function(data, objective, minimize=TRUE, percentages=FALSE, expand=TRUE, ...) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	varargs <- list(...)
	
	if (is.function(objective)) {
		y <- 1*mordm.select.indices(set, objective)
		minimize <- FALSE
		varargs$threshold.type=1
		varargs$threshold=0.5
		
		x <- set[,1:nvars]
	} else if (is.character(objective)) {
		y <- set[,objective]
		
		if (nvars == 0) {
			x <- set[,-which(colnames(set) %in% objective)]
		} else {
			x <- set[,1:nvars]
		}
	} else {
		y <- set[,nvars+objective]
		
		if (nvars == 0) {
			x <- set[,-(nvars+objective)]
		} else {
			x <- set[,1:nvars]
		}
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
		mordm.mark.box(result$box[[i]], result$y.fun[i], result$mass[i])
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
	
	marks
}

#' Print descriptive representation of PRIM boxes.
#' 
#' Displays the PRIM boxes in a human-readable way.
#' 
#' @param data the original data set
#' @param mark the PRIM boxes
#' @param threshold fuzzy factor when determining if two numbers are equal
#' @param digits number of digits to round numbers
#' @param indent character string prepended to each line
#' @export
mordm.printbox <- function(data, mark, threshold=0.01, digits=3, indent="") {
	bounds <- attr(data, "bounds")
	box <- attr(mark, "box")
	
	if (is.null(box)) {
		warning("Given mark was not generated by PRIM")
		next
	}
	
	names <- colnames(box)
	unbound <- vector()
	
	cat(indent)
	cat("Mean Response: ")
	cat(attr(mark, "mean"))
	cat("\n\n")
	
	cat(indent)
	cat("Mass: ")
	cat(attr(mark, "mass"))
	cat("\n\n")
	
	cat(indent)
	cat("Bound Variables:\n")
	
	for (i in 1:ncol(box)) {
		show.min <- FALSE
		show.max <- FALSE
		show.equals <- FALSE
		
		if (is.null(bounds)) {
			limits <- c(box[1,i], box[2,i])
		} else {
			limits <- c(max(bounds[1,i], box[1,i]), min(bounds[2,i], box[2,i]))
		}
		
		limits <- round(limits, digits=digits)
		
		if (is.null(bounds)) {
			show.min <- TRUE
			show.max <- TRUE
		} else {
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
		}
		
		if (show.min & show.max) {
			cat(indent)
			cat("  ")
			cat(limits[1])
			cat(" <= ")
			cat(names[i])
			cat(" <= ")
			cat(limits[2])
			cat("\n")
		} else if (show.min) {
			cat(indent)
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
			cat(indent)
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
	cat(indent)
	cat("Unbound Variables:\n")
	
	if (length(unbound) > 0) {
		for (i in 1:length(unbound)) {
			cat(indent)
			cat("  ")
			cat(unbound[i])
			cat("\n")
		}
	} else {
		cat(indent)
		cat("  None\n")
	}
	
	cat("\n")
}

#' Display plot of PRIM boxes.
#' 
#' Generates a plot showing the bounds of the PRIM boxes.  Currently only works
#' well with one or two PRIM boxes.
#' 
#' @param data the original data set
#' @param mark a list of the PRIM boxes to display
#' @param main the plot title
#' @param scale.width if \code{TRUE}, reduce the width of the bars as more PRIM
#'        boxes are displayed
#' @param bar.width the width of the bars
#' @param col vector of bar colors
#' @param names names of each PRIM box to display in the legend
#' @param legend if \code{TRUE}, renders a legend on the plot
#' @param defaults draw horizontal lines to show default values
#' @export
mordm.plotbox <- function(data, mark, main="PRIM Box", scale.width=TRUE, bar.width=3, col=NULL, names=NULL, legend=TRUE, defaults=NULL) {
	nvars <- attr(data, "nvars")
	bounds <- attr(data, "bounds")

	if (is.null(bounds)) {
		stop("Bounds must be defined for the dataset")
	}
	
	longcol <- par("fg")
	outcol <- "transparent"
	
	# reset plot settings
	if (exists("default.par", mordm.globals)) {
		par(get("default.par", mordm.globals))
	} else {
		assign("default.par", par(no.readonly=TRUE), mordm.globals)
	}
	
	if (legend) {
		layout(c(1,2), heights=c(7,1))
	}
	
	par(xpd=TRUE, mar=c(4.1, 2.1, 4.1, 2.1))
	
	# create the plot
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
	text(seq(4.5,by=6,length.out=nvars), y=rep(1, nvars), pos=3, labels=sprintf("%g", bounds[2,]), cex=0.8)
	text(seq(4.5,by=6,length.out=nvars), y=rep(0, nvars), pos=1, labels=sprintf("%g", bounds[1,]), cex=0.8)
	
	if (!is.null(defaults)) {
		segments(seq(4.5,by=6,length.out=nvars)-1.5, (defaults-bounds[1,])/(bounds[2,]-bounds[1,]), seq(4.5,by=6,length.out=nvars)+1.45, (defaults-bounds[1,])/(bounds[2,]-bounds[1,]), lwd=3)
	}
	
	# create the legend
	if (legend) {
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
}

#' Make recommendations for analyzing the data.
#' 
#' Performs basic checks to ensure the data is formatted correctly.  If any
#' issues are identified, then it will attempt to provide details on correcting
#' or dealing with the problem.
#' 
#' @param data the data set to be analyzed
#' @export
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

#' Animates the time series in a GIF.
#' 
#' Animates the 3D scatter plot and saves the results to a GIF file.  Each
#' index in \code{indices} specifies the entry in the time series data that is
#' displayed at each frame.  Thus, to show all entries in succession, set
#' \code{indices=1:length(data)}, but to show a single entry across multiple
#' frames (e.g., if rotating) use \code{indices=rep(length(data), 50)}.  The
#' \code{transform} function is similar to the transformation function used by
#' \code{\link{play3d}}.  However, whereas the transformation in \code{play3d}
#' is based on the number of elapsed seconds, this method computes the transform
#' based on the frame number.  Thus, if you use \code{spin3d(rpm=1)}, then this
#' method will rotate the plot once every 60 frames (i.e., treating each frame
#' as one second).
#' 
#' @param data the time series data
#' @param output the location where the animated GIF is saved
#' @param indices a vector indicating the indices in data that are displayed in
#'        each frame
#' @param transform function that returns a transformation applied to each
#'        frame (see \code{\link{spin3d}}), or the user matrix for a constant
#'        projection
#' @param clean if \code{TRUE}, delete the temporary images once the GIF is
#'        created
#' @param close if \code{TRUE}, close the RGL window when finished
#' @param loop if \code{TRUE}, loop infinitely; otherwise play the animation
#'        once
#' @param scale amount to enlarge the plotting limits
#' @param ... additional arguments passed to \code{\link{mordm.plot}}
#' @export
mordm.animate <- function(data, output="animation.gif", indices=1:length(data), transform=NULL, clean=TRUE, close=TRUE, loop=FALSE, scale=0.1, ...) {
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	varargs <- list(...)
	
	# first compute the limits
	bounds <- apply(data[[indices[1]]][,(nvars+1):(nvars+nobjs)], 2, range)
	
	for (i in indices[-1]) {
		bounds.temp <- apply(data[[i]][,(nvars+1):(nvars+nobjs)], 2, range)
		
		for (j in 1:nobjs) {
			bounds[,j] <- range(bounds[,j], bounds.temp[,j])
		}
	}
	
	bounds <- apply(bounds, 2, function(x) x*(1+scale) - mean(x)*scale)
	
	# generate the snapshots
	files = vector()
	time <- 1
	
	for (i in indices) {
		file <- paste("snapshot_", time, ".png", sep="")

		if (!is.null(varargs$objectives)) {
			varargs$xlim <- bounds[,varargs$objectives[1]]
			varargs$ylim <- bounds[,varargs$objectives[2]]
			
			if (nobjs > 2) {
				varargs$zlim <- bounds[,varargs$objectives[3]]
			}
			
			if (nobjs > 3) {
				varargs$slim <- bounds[,varargs$objectives[4]]
			}
			
			if (!is.null(varargs$color) && length(varargs$color) == 1) {
				varargs$clim <- bounds[,varargs$color]
			} else if (nobjs > 4) {
				varargs$clim <- bounds[,varargs$objectives[5]]
			}
		} else {
			varargs$xlim <- bounds[,1]
			varargs$ylim <- bounds[,2]
			
			if (nobjs > 2) {
				varargs$zlim <- bounds[,3]
			}
			
			if (nobjs > 3) {
				varargs$slim <- bounds[,4]
			}
			
			if (!is.null(varargs$color) && length(varargs$color) == 1) {
				varargs$clim <- bounds[,varargs$color]
			} else if (nobjs > 4) {
				varargs$clim <- bounds[,5]
			}
		}

		do.call(mordm.plot, c(list(data=data, index=i), varargs))
		
		if (!is.null(transform)) {
			if (is.function(transform)) {
				par3d(transform(time))
			} else {
				par3d(userMatrix=transform)
			}
		}
		
		rgl.snapshot(file)
		files <- append(files, file)
		time <- time + 1
	}
	
	if (close) {
		rgl.close()
	}
	
	# Convert the individual frames into the GIF animation
	# Version 2.3 of the animation packages breaks the following code:
	#     opt <- ani.options(interval=0.1, autobrowse=FALSE, loop=loop, outdir=dirname(output))
	#     im.convert(files, output=basename(output), clean=clean)
	#     ani.options(opt)
	if (system("gm version", show.output.on.console=FALSE) == 0) {
		command <- sprintf("%s %s -delay 10 %s %s", "gm convert", ifelse(loop, "-loop 0", "-loop 1"), paste(files, collapse=" "), shQuote(output))
	} else {
		command <- sprintf("%s %s -delay 10 %s %s", "convert", ifelse(loop, "-loop 0", "-loop 1"), paste(files, collapse=" "), shQuote(output))
	}
	
	system(command)
	
	if (clean) {
		for (file in files) {
			unlink(file)
		}
	}
}

#' Computes the sensitivities of the decision variables.
#' 
#' Using Plischke's delta-moment sensitivity analysis method, this function
#' computes the sensitivities using the given data.  As such, this method does
#' not need to evaluate any new data points, it works with the provided data.
#' 
#' If \code{objective} is a marking, then this computes the sensitivities that
#' cause a point to be included in the marking.  This functionality is still
#' experimental.
#' 
#' @param data the data set
#' @param objective the objective index, column name, function, or marking
#' @param index if data is a time series, controls which entries to display
#'        (see \code{\link{mordm.getset}} for details) 
#' @param all if \code{TRUE}, include all points from all entries in the time
#'        series; otherwise, only the last entry is included
#' @param ... additional options for Plischke's method
#' @export
mordm.sensitivity <- function(data, objective, index=-1, all=FALSE, ...) {
	set <- mordm.getset(data, index)
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	names <- colnames(set)
	varargs <- list(...)
	
	if (all) {
		if (length(data) > 1) {
			for (i in seq(length(data)-1, 1, -1)) {
				set <- rbind(set, mordm.getset(data, i))
			}
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
		
		x <- set[,1:nvars]
	} else if (is.character(objective)) {
		y <- set[,objective]
		
		if (nvars == 0) {
			x <- set[,-which(colnames(set) %in% objective)]
		} else {
			x <- set[,1:nvars]
		}
	} else {
		y <- set[,nvars+objective]
		
		if (nvars == 0) {
			x <- set[,-(nvars+objective)]
		} else {
			x <- set[,1:nvars]
		}
	}
	
	do.call(deltamim, c(list(x, y), varargs))
}

#' Computes robustness under uncertainty.
#' 
#' Adds Gaussian noise to the decision variables and resamples the model output.
#' Then computes one or more robustness metrics.
#' 
#' This method is equivalent to \code{\link{mordm.uncertainty}} using a single
#' model.
#' 
#' @param data the data set
#' @param sd scalar or vector specifying the standard deviation for each
#'        decision variable
#' @param nsamples the number of samples to generate for each point
#' @param problem the problem formulation
#' @param method the robustness metric or a list of metrics to use (see
#'        \code{\link{check.robustness}} for available options)
#' @param verbose display additional information
#' @export
mordm.robustness <- function(data, sd, nsamples, problem, method="default", verbose=TRUE) {
	set <- mordm.getset(data)
	set <- set[,1:problem$nvars,drop=FALSE]
	
	t(sapply(1:nrow(set), function(i) {
		if (verbose && nrow(set) > 1) {
			cat("\r")
			cat(i)
			cat(" of ")
			cat(nrow(set))
		}
		
		samples <- nsample(set[i,], sd, nsamples, problem)
		
		sapply(unlist(list(method)), function(m) {
			check.robustness(samples, problem, verbose=FALSE, method=m)
		})
	}))
}

#' Computes robustness under deep uncertainty.
#' 
#' Adds Gaussian noise to the decision variables and resamples the model output.
#' The samples are distributed across one or more different models for the
#' problem.  The result from this method should be passed to 
#' \code{mordm.uncertainty.evaluate} to compute the robustness metrics.
#' 
#' If multiple models are provided, it is assumed that all models have the same
#' inputs and outputs; they would only differ in the internal calculcations
#' within the model.
#' 
#' @param data the data set
#' @param nsamples the number of samples to generate for each point
#' @param models the problem formulations created using \code{setup}
#' @param sd scalar or vector specifying the standard deviation for each
#'        decision variable
#' @param verbose display additional information
#' @export
mordm.uncertainty.sample <- function(data, nsamples, models, sd=0, verbose=TRUE) {
	if (!is.list(models)) {
		models <- list(models)
	}
	
	# sanity check to ensure all models are valid
	if (length(models) == 0) {
		stop("At least one model must be provided")
	}
	
	for (j in 1:length(models)) {
		if (j == 1) {
			nvars <- models[[j]]$nvars
			nobjs <- models[[j]]$nobjs
			nconstrs <- models[[j]]$nconstrs
		} else {
			if (nvars != models[[j]]$nvars) {
				stop("All models must have the same number of decision variables")
			}
			
			if (nobjs != models[[j]]$nobjs) {
				stop("All models must have the same number of objectives")
			}
			
			if (nconstrs != models[[j]]$nconstrs) {
				stop("All models must have the same number of constraints")
			}
		}
	}
	
	set.orig <- mordm.getset(data)
	set <- set.orig[,1:nvars,drop=FALSE]
	
	# simple way to figure how many samples to draw from each model
	indices <- (0:(nsamples-1) %% length(models))+1

	result <- lapply(1:nrow(set), function(i) {
		if (verbose && nrow(set) > 1) {
			cat("\r")
			cat(i)
			cat(" of ")
			cat(nrow(set))
		}
			
		for (j in 1:length(models)) {
			qty <- sum(indices == j)
			temp <- nsample(set[i,], sd, qty, models[[j]])
				
			if (j == 1) {
				samples <- temp
			} else {
				samples$vars <- rbind(samples$vars, temp$vars)
				samples$objs <- rbind(samples$objs, temp$objs)
					
				if (!is.null(samples$constrs)) {
					samples$constrs <- rbind(samples$constrs, temp$constrs)
				}
			}
		}
		
		samples
	})
	
	class(result) <- "uncertainty.samples"
	attr(result, "nsamples") <- nsamples
	attr(result, "models") <- models
	attr(result, "nvars") <- nvars
	attr(result, "nobjs") <- nobjs
	attr(result, "nconstrs") <- nconstrs
	attr(result, "set.orig") <- set.orig
}

#' Computes robustness under deep uncertainty.
#' 
#' @param samples the samples generated by \code{mordm.uncertainty.sample}
#' @param satisficing.fcn the satisficing function for computing the two
#'        satisficing robustness metrics
#' @export
mordm.uncertainty.evaluate <- function(samples, satisficing.fcn=NULL) {
	set.orig <- attr(samples, "set.orig")
	
	regret.type1 <- function(original.point, samples) {
		mean(sapply(1:nrow(samples$objs), function(i) {
			norm(samples$objs[i,] - original.point[(nvars+1):(nvars+nobjs)], "2") + 10*sum(samples$constrs[i,])
		}))
	}
	
	regret.type2 <- function(original.point, samples) {
		mean(sapply(1:nrow(factors), function(i) {
			best <- c(
				min(sapply(1:length(result.uncertainty.raw), function(j) result.uncertainty.raw[[j]]$objs[1,])),
				max(sapply(1:length(result.uncertainty.raw), function(j) result.uncertainty.raw[[j]]$objs[2,])),
				max(sapply(1:length(result.uncertainty.raw), function(j) result.uncertainty.raw[[j]]$objs[3,])),
				max(sapply(1:length(result.uncertainty.raw), function(j) result.uncertainty.raw[[j]]$objs[4,])))
			norm(samples$objs[i,] - best, "2")
		}))
	}
	
	satisficing.type1 <- function(original.point, samples, satisficing.fcn) {
		mean(sapply(1:nrow(samples$objs), function(i) {
			satisficing.fcn(list(vars=samples$vars[i,], objs=samples$objs[i,], constrs=samples$constrs[i,]))
		}))
	}
	
	satisficing.type2 <- function(original.point, samples, satisficing.fcn) {
		min(sapply(1:nrow(samples$objs), function(i) {
			if (satisficing.fcn(list(vars=samples$vars[i,], objs=samples$objs[i,], constrs=samples$constrs[i,]))) {
				NA
			} else {
				norm(factors[i,]-baseline_factors, "2")
			}
		}), na.rm=TRUE)
	}
	
	result.regret.type1 <- sapply(1:length(samples), function(i) {
		regret.type1(set.orig[i,], samples[[i]])
	})
	
	result.regret.type2 <- sapply(1:length(samples), function(i) {
		regret.type2(set.orig[i,], samples[[i]])
	})
	
	if (!is.null(satisficing.fcn)) {
		result.satisficing.type1 <- sapply(1:length(samples), function(i) {
			satisficing.type1(set.orig[i,], samples[[i]], satisficing.fcn)
		})
		
		result.satisficing.type2 <- sapply(1:length(samples), function(i) {
			satisficing.type2(set.orig[i,], samples[[i]], satisficing.fcn)
		})
	}
	
	result <- cbind(result.regret.type1, result.regret.type2)
	
	if (!is.null(satisficing.fcn)) {
		result <- cbind(result, result.satisficing.type1, result.satisficing.type2)
	}
	
	colnames(result) <- c("Regret Type I", "Regret Type II", "Satisficing Type I", "Satisficing Type II")
	result
}

#' Computes robustness under deep uncertainty.
#' 
#' Adds Gaussian noise to the decision variables and resamples the model output.
#' The samples are distributed across one or more different models for the
#' problem.  The result from this method should be passed to 
#' \code{mordm.uncertainty.evaluate} to compute the robustness metrics.
#' 
#' If multiple models are provided, it is assumed that all models have the same
#' inputs and outputs; they would only differ in the internal calculcations
#' within the model.
#' 
#' @param data the data set
#' @param nsamples the number of samples to generate for each point
#' @param models the problem formulations created using \code{setup}
#' @param sd scalar or vector specifying the standard deviation for each
#'        decision variable
#' @param verbose display additional information
#' @param satisficing.fcn the satisficing function for computing the two
#'        satisficing robustness metrics
#' @export
uncertainty <- function(data, nsamples, models, sd=0, verbose=TRUE, satisficing.fcn=NULL) {
	samples <- mordm.uncertainty.sample(data, nsamples, models, sd=sd, verbose=verbose)
	mordm.uncertainty.evaluate(samples, satisficing.fcn=satisficing.fcn)
}

#' This is the old uncertainty function, no longer used.
#' @keywords internal
mordm.uncertainty <- function(data, nsamples, models, sd=0, base.model=NULL, method="default", verbose=TRUE) {
	if (!is.list(models)) {
		models <- list(models)
	}
	
	# sanity check to ensure all models are valid
	if (length(models) == 0) {
		stop("At least one model must be provided")
	}
	
	for (j in 1:length(models)) {
		if (j == 1) {
			nvars <- models[[j]]$nvars
			nobjs <- models[[j]]$nobjs
			nconstrs <- models[[j]]$nconstrs
		} else {
			if (nvars != models[[j]]$nvars) {
				stop("All models must have the same number of decision variables")
			}
			
			if (nobjs != models[[j]]$nobjs) {
				stop("All models must have the same number of objectives")
			}
			
			if (nconstrs != models[[j]]$nconstrs) {
				stop("All models must have the same number of constraints")
			}
		}
	}
	
	if (is.null(base.model)) {
		base.model <- models[[1]]
	}
	
	set.orig <- mordm.getset(data)
	set <- set.orig[,1:base.model$nvars,drop=FALSE]
	
	# simple way to figure how many samples to draw from each model
	indices <- (0:(nsamples-1) %% length(models))+1
	
	if (is.null(method)) {
		# if no robustsness method specified, return raw data
		lapply(1:nrow(set), function(i) {
			if (verbose && nrow(set) > 1) {
				cat("\r")
				cat(i)
				cat(" of ")
				cat(nrow(set))
			}
			
			for (j in 1:length(models)) {
				qty <- sum(indices == j)
				temp <- nsample(set[i,], sd, qty, models[[j]])
				
				if (j == 1) {
					samples <- temp
				} else {
					samples$vars <- rbind(samples$vars, temp$vars)
					samples$objs <- rbind(samples$objs, temp$objs)
					
					if (!is.null(samples$constrs)) {
						samples$constrs <- rbind(samples$constrs, temp$constrs)
					}
				}
			}
			
			samples
		})
	} else {
		# otherwise, calculate the robustness for each set
		t(sapply(1:nrow(set), function(i) {
			if (verbose && nrow(set) > 1) {
				cat("\r")
				cat(i)
				cat(" of ")
				cat(nrow(set))
			}
			
			for (j in 1:length(models)) {
				qty <- sum(indices == j)
				temp <- nsample(set[i,], sd, qty, models[[j]])
				
				if (j == 1) {
					samples <- temp
				} else {
					samples$vars <- rbind(samples$vars, temp$vars)
					samples$objs <- rbind(samples$objs, temp$objs)
					
					if (!is.null(samples$constrs)) {
						samples$constrs <- rbind(samples$constrs, temp$constrs)
					}
				}
			}
			
			original.point <- list()
			original.point$vars <- set.orig[i,1:base.model$nvars,drop=FALSE]
			original.point$objs <- set.orig[i,(base.model$nvars+1):(base.model$nvars+base.model$nobjs),drop=FALSE]
			
			if (base.model$nconstrs > 0) {
				original.point$constrs <- matrix(0, nrow=1, ncol=base.model$nconstrs)
				#original.point$constrs <- set.orig[i,(base.model$nvars+base.model$nobjs+1):(base.model$nvars+base.model$nobjs+base.model$nconstrs),drop=FALSE]
			}
			
			sapply(unlist(list(method)), function(m) {
				check.robustness(samples, base.model, verbose=FALSE, method=m, original.point=original.point)
			})
		}))
	}
}

#' Computes a vector of weighted preferences
#' 
#' @param data the data
#' @param weights the vector of weights
#' @export
mordm.weight <- function(data, weights) {
	set <- mordm.getset(data)
	nvars <- attr(set, "nvars")
	nobjs <- attr(set, "nobjs")
	min.obj <- apply(set, 2, function(x) min(x))
	max.obj <- apply(set, 2, function(x) max(x))
	weights <- weights / sum(weights)
	
	apply(set, 1, function(x) sum((x[(nvars+1):(nvars+nobjs)]-min.obj[(nvars+1):(nvars+nobjs)])/(max.obj[(nvars+1):(nvars+nobjs)]-min.obj[(nvars+1):(nvars+nobjs)])*weights))
}
