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
library("shiny")
library("shinyRGL")
library("rgl")
library("scales")
library("functional")
source("OpenMORDM.R")
source("config.R")

# Setup and load the data
cat("Loading data, this may take several minutes...\n")
data <- mordm.read(filename, nvars, nobjs, nconstrs, bounds=bounds,
				   names=objectives, maximize=maximize, digits=5)
cat("Finished loading data!\n")

min.nfe <- attr(data[[1]], "NFE")
max.nfe <- attr(data[[length(data)]], "NFE")
step.nfe <- max.nfe / length(data)
colors <- list("Rainbow (Red to Blue)", "Inv. Rainbow (Blue to Red)", "Heat (White to Red)")

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
	if (name == colors[1]) {
		palette <- rainbow(100, start=0, end=0.66)
	} else if (name == colors[2]) {
		palette <- rev(rainbow(100, start=0, end=0.66))
	} else if (name == colors[3]) {
		palette <- rev(heat.colors(100))
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
plot.colorbar <- function(show.color="None", index=-1, colormap="Rainbow (Blue ot Red)") {
	oldmai <- par()$mai
	par(mai=c(1, 2, 0.5, 2))
	set <- mordm.getset(data, index)
	
	if (show.color == "None" || show.color == "Selected Point" || show.color == "Constant") {
		plot.new()
	} else {
		if (plot.toobj(show.color) > 0) {
			crange <- limits[,plot.toobj(show.color)]
			palette <- to.palette(colormap)
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
	
	mordm.currentset <<- set
	mordm.currentcolors <<- alpha(original.colors, transparency)
	
	# generate the parallel coordinates plot
	mordm.plotpar(alpha=NA, label.size=input$parallel.cex, line.width=input$parallel.lwd)
	
	# restore the original settings
	mordm.currentset <<- original.set
	mordm.currentcolors <<- original.colors
}

do.plotOps <- function(input) {
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
	
	pairs(set, col=alpha(original.colors, transparency), cex.labels=input$scatter.label, cex=input$scatter.point, pch=20)
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

do.sensitivity <- function(input) {
	objective <- plot.toobj(input$sensitivity.response)-nvars
	result <- mordm.sensitivity(data, objective,
								all=input$sensitivity.all,
								kd.estimator=input$sensitivity.kd.estimator)
	
	indices <- result$Si
	
	if (input$sensitivity.order) {
		indices <- indices[,result$rank,drop=FALSE]
	}
	
	barplot(indices, ylim=c(0,1), ylab="Sensitivity Indices")
}

shinyServer(
	function(input, output, session) {
		output$plot3d <- renderWebGL({
			do.plot3d(input)
		})
		
		output$colorbar <- renderPlot({
			args <- list()
			args$show.color <- input$color
			args$colormap <- input$colormap
			
			if (is.null(input$nfe) || is.na(input$nfe)) {
				args$index = length(data)
			} else {
				args$index = input$nfe / step.nfe
			}
			
			do.call(plot.colorbar, args)
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
	}
)
