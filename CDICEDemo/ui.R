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
source("config.R")

options <- c(objectives, "Constant")
colors <- list("Rainbow (Red to Blue)", "Inv. Rainbow (Blue to Red)", "Heat (White to Red)")

shinyUI(fluidPage(navbarPage("OpenMORDM", id="main",
	tabPanel("3D Plot",
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
					 			selectInput("color", "Color", options, ifelse(length(objectives) < 4, "Constant", options[order[4]])),
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
			 			tabPanel("Animate",
			 				div(
				 				h3("Animation Controls"),
				 				helpText("User the slider below to show the set at different times during optimization."),
				 				uiOutput("slider.nfe"),
				 				style="height: 550px")),
			 			tabPanel("Download",
			 				div(
				 				h3("Image Download"),
				 				helpText("Download images of the 3D plot.  SVG/EPS generation may take a while."),
				 				downloadButton("download.plot3d.png", "PNG Image"),
				 				downloadButton("download.plot3d.svg", "SVG Image"),
				 				downloadButton("download.plot3d.eps", "EPS File"),
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
				 				selectInput("colormap", "Color Scheme", colors),
				 				sliderInput("tick.size", "Tick Size", min=0.1, max=2, value=1, step=0.1),
				 				sliderInput("label.size", "Label Size", min=0.1, max=2, value=1.2, step=0.1),
				 				sliderInput("label.line", "Label Offset", min=1, max=5, value=2, step=0.25),
				 				sliderInput("radius.scale", "Sphere Radius", min=0.1, max=2, value=1, step=0.1),
				 				style="height: 550px")))),
				 mainPanel(
				 	conditionalPanel("!output.plot3d", p("Loading data, please wait...")),
				 	div(webGLOutput("plot3d", width="100%", height="600px"), style="overflow: hidden"),
				 	plotOutput("colorbar", height="150px")))),
	tabPanel("2D Plots",
			 tabsetPanel(
			 	tabPanel("Parallel Coords.",
			 			 sidebarLayout(
			 			 	sidebarPanel(
			 			 		h3("Plotting Options"),
			 			 		sliderInput("parallel.lwd", "Line Width", min=0.5, max=4, value=1, step=0.5),
			 			 		sliderInput("parallel.transparency", "Transparency", min=0.1, max=1, value=1, step=0.1),
			 			 		sliderInput("parallel.cex", "Label Size", min=0.1, max=2, value=1, step=0.1),
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
			 			 		plotOutput("plot2d.parallel")))),
			 	tabPanel("Scatter Plots",
			 			 sidebarLayout(
			 			 	sidebarPanel(
			 			 		h3("Plotting Options"),
			 			 		sliderInput("scatter.point", "Point Size", min=0.2, max=4, value=1, step=0.2),
			 			 		sliderInput("scatter.transparency", "Point Transparency", min=0.1, max=1, value=1, step=0.1),
			 			 		sliderInput("scatter.label", "Label Size", min=0.1, max=2, value=1, step=0.1),
			 			 		br(),
			 			 		br(),
			 			 		h4("Download"),
			 			 		downloadButton("download.scatter.png", "PNG Image"),
			 			 		downloadButton("download.scatter.svg", "SVG Image"),
			 			 		downloadButton("download.scatter.eps", "EPS File"),
			 			 		br(),
			 			 		br(),
			 			 		helpText("Note: EPS export does not support transparency.  Transparent lines will not appear.")),
			 			 	mainPanel(
			 			 		plotOutput("plot2d.scatter")))),
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
			 			 		plotOutput("plot2d.operators")))))),
	tabPanel("Analyze",
			 tabsetPanel(
			 	tabPanel("Correlations",
			 			 pre(textOutput("correlations"))),
			 	tabPanel("Sensitivity",
			 			 sidebarLayout(
			 			 	sidebarPanel(
			 			 		h3("Sensitivity Options"),
			 			 		selectInput("sensitivity.response", "Response", objectives),
			 			 		selectInput("sensitivity.kd.estimator", "Kernel Density Estimator", c("cheap", "stats", "diffusion", "hist")),
			 			 		checkboxInput("sensitivity.all", "Use all time series data", value=FALSE),
			 			 		checkboxInput("sensitivity.order", "Rank factors in plot", value=FALSE),
			 			 		br(),
			 			 		br(),
			 			 		h4("Download"),
			 			 		downloadButton("download.sensitivity.png", "PNG Image"),
			 			 		downloadButton("download.sensitivity.svg", "SVG Image"),
			 			 		downloadButton("download.sensitivity.eps", "EPS File")),
			 			 	mainPanel(
			 			 		plotOutput("sensitivity")))))),
	tabPanel("Raw Data",
			 dataTableOutput("raw.data")),
	tabPanel("Options",
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
			 h4("Downloaded Image Size"),
			 helpText("By default, downloaded images are sized similarly to what you see in the browser.  Check the option below to customize the image size."),
			 div(
				 checkboxInput("custom_image", "Customize downloaded image size", value=FALSE),
				 conditionalPanel("input.custom_image == true",
				 	numericInput("image.width", "Width (in)", 8, min=1, step=0.5),
				 	numericInput("image.height", "Height (in)", 6, min=1, step=0.5)),
				 style="margin-left: 25px"))
)))
