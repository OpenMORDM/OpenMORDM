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

objectives <- list("Reliability of Temperature Goal", "Climate Damages", "Abatement Costs", "Expected Utility", "Constant")
colors <- list("Rainbow (Red to Blue)", "Inv. Rainbow (Blue to Red)", "Heat (White to Red)")
reduced <- TRUE

shinyUI(fluidPage(navbarPage("OpenMORDM", id="main",
	tabPanel("Climate Snake",
			 sidebarLayout(
			 	sidebarPanel(
			 		tabsetPanel(
			 			tabPanel("Plot",
			 				div(
			 					h3("Plotting Controls"),
			 					helpText("Use the controls below to explore the multidimensional tradeoffs."),
					 			selectInput("x", "X-Axis", objectives, objectives[1]),
					 			selectInput("y", "Y-Axis", objectives, objectives[2]),
					 			selectInput("z", "Z-Axis", objectives, objectives[3]),
					 			selectInput("color", "Color", objectives, objectives[4]),
			 					style="height: 550px")),
			 			tabPanel("Brush",
			 				div(
				 				h3("Brushing Controls"),
				 				helpText("Brushing selects a subset of the data for further analysis."),
				 				uiOutput("slider.reliability"),
				 				uiOutput("slider.damages"),
				 				uiOutput("slider.cost"),
				 				uiOutput("slider.utility"),
				 				br(),
				 				br(),
				 				h4("Additional Options"),
				 				sliderInput("slider.transparency", "Brush Transparency", min=0, max=0.01, value=0.005),
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
				 				helpText("Download images / PDF files of the data."),
				 				downloadButton("download.png", "PNG Image"),
				 				downloadButton("download.pdf", "PDF File"),
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
				 	div(webGLOutput("plot3d", width="100%", height="600px"), style="overflow: hidden"),
				 	plotOutput("colorbar", height="150px"))))
)))
