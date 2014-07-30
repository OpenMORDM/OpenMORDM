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

# Setup and load the CDICE data
objectives <- c("Expected Utility", "Reliability of Temperature Goal", "Climate Damages", "Abatement Costs")
colors <- list("Rainbow (Red to Blue)", "Inv. Rainbow (Blue to Red)", "Heat (White to Red)")
reduced <- TRUE

if (reduced) {
	filename <- "runtime_util_ptemp_pvdam_pvabate_100cs_short_10k.txt"
} else {
	filename <- "runtime_util_ptemp_pvdam_pvabate_100cs_short_parsed.txt"	
}

cat("Loading data, this may take several minutes...\n")
data <- mordm.read(filename, 0, 4,
				   names=objectives,
				   maximize=objectives[1:2])
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
			limits <- apply(temp.set[,1:4], 2, range)
		} else {
			limits <- apply(rbind(temp.set[,1:4], limits), 2, range)
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
		which(objectives == name)
	} else if (name == "Constant") {
		5
	} else {
		0
	}
}

plot.limits <- function(brush.reliability=NULL, brush.damages=NULL, brush.cost=NULL, brush.utility=NULL) {
	if (is.null(brush.utility)) {
		brush.utility <- limits[,1]
	}
	
	if (is.null(brush.reliability)) {
		brush.reliability <- limits[,2]
	}
	
	if (is.null(brush.damages)) {
		brush.damages <- limits[,3]
	}
	
	if (is.null(brush.cost)) {
		brush.cost <- limits[,4]
	}
	
	brush.limits <- cbind(brush.utility, brush.reliability, brush.damages, brush.cost, c(0,0))
	
	if (all(brush.limits == limits)) {
		NULL
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

get.palette <- function(name) {
	if (name == colors[1]) {
		palette <- rainbow(100, start=0, end=0.66)
	} else if (name == colors[2]) {
		palette <- rev(rainbow(100, start=0, end=0.66))
	} else if (name == colors[3]) {
		palette <- rev(heat.colors(100))
	}
}

do.plot <- function(input) {
	args <- list()
	args$show.x <- input$x
	args$show.y <- input$y
	args$show.z <- input$z
	args$show.size <- "Constant"
	args$show.color <- input$color
	args$show.label <- input$label
	args$show.ideal <- input$ideal
	args$colormap <- input$colormap
	args$brush.reliability <- input$brush.reliability
	args$brush.damages <- input$brush.damages
	args$brush.cost <- input$brush.cost
	args$brush.utility <- input$brush.utility
	args$slider.transparency <- input$slider.transparency
	args$fontsize <- input$fontsize
	args$tick.size <- input$tick.size
	args$label.size <- input$label.size
	args$label.line <- input$label.line
	args$radius.scale <- input$radius.scale
	
	if (is.null(input$nfe) || is.na(input$nfe)) {
		args$index = length(data)
	} else {
		args$index = input$nfe / step.nfe
	}
	
	do.call(plot.snake, args)
}

# Generates the 3D plot.
plot.snake <- function(selection=NULL, index=-1, show.x="None", show.y="None", show.z="None", show.color="None", show.size="None", show.ideal=TRUE, show.label=FALSE, colormap="Rainbow (Blue ot Red)", brush.reliability=NULL, brush.damages=NULL, brush.cost=NULL, brush.utility=NULL, slider.transparency=0.01, fontsize=12, tick.size=1, label.size=1.2, label.line=1, radius.scale=1, window=NULL) {
	set <- mordm.getset(data, index)
	
	objectives <- vector()
	names <- colnames(set)
	
	# append a list of 0's in case the constant (column 5) option is selected
	set <- cbind(set, rep(0, nrow(set)))
	names <- c(names, "Constant")
	
	# brush the set
	brush.limits <- plot.limits(brush.reliability, brush.damages, brush.cost, brush.utility)
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
		#slim <- limits[,plot.toobj(show.size)]
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
	palette <- get.palette(colormap)
	
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
			palette <- get.palette(colormap)
			image(seq(crange[1], crange[2], (crange[2]-crange[1])/100), 0, matrix(seq(0, 1, 0.01), ncol=1), col=palette, axes=FALSE, xlab=show.color, ylab="")
			box("plot")
			axis(1)
		} else {
			plot.new()
		}
	}
	
	par(mai=oldmai)
}

shinyServer(
	function(input, output, session) {
		output$plot3d <- renderWebGL({
			do.plot(input)
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
		
		output$slider.reliability <- renderUI({
			index <- 2
			min <- floor(limits[1,index])
			max <- ceil(limits[2,index])
			sliderInput("brush.reliability", objectives[index], min=min, max=max, value=c(min, max))
		})
		
		output$slider.damages <- renderUI({
			index <- 3
			min <- floor(limits[1,index])
			max <- ceil(limits[2,index])
			sliderInput("brush.damages", objectives[index], min=min, max=max, value=c(min, max))
		})
		
		output$slider.cost <- renderUI({
			index <- 4
			min <- floor(limits[1,index])
			max <- ceil(limits[2,index])
			sliderInput("brush.cost", objectives[index], min=min, max=max, value=c(min, max))
		})
		
		output$slider.utility <- renderUI({
			index <- 1
			min <- floor(limits[1,index])
			max <- ceil(limits[2,index])
			sliderInput("brush.utility", objectives[index], min=min, max=max, value=c(min, max))
		})
		
		output$slider.nfe <- renderUI({
			sliderInput("nfe", "Current NFE", min=min.nfe, max=max.nfe, value=max.nfe, step=step.nfe, animate=TRUE)
		})
		
		output$download.png <- downloadHandler(
			filename = "snapshot.png",
			content = function(file) {
				open3d(useNULL=FALSE, windowRect=c(0, 0, 600, 600))
				do.plot(input)
				
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
		
		output$download.pdf <- downloadHandler(
			filename = "snapshot.ps",
			content = function(file) {
				open3d(useNULL=FALSE, windowRect=c(0, 0, 600, 600))
				do.plot(input)
				
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
				
				rgl.postscript(file, fmt="ps")
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
				brush.limits <- plot.limits(input$brush.reliability, input$brush.damages, input$brush.cost, input$brush.utility)
				alpha <- plot.brush(set, brush.limits[,1:4], 0.0)
				write.csv(set[alpha==1,,drop=FALSE], file)
			})
	}
)
