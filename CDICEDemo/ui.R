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
			 h1("The Climate Snake"),
			 sidebarLayout(
			 	sidebarPanel(
			 		h3("Controls"),
			 		helpText("Use the controls below to explore the multidimensional tradeoffs."),
			 		selectInput("snake.x", "X-Axis", objectives, objectives[1]),
			 		selectInput("snake.y", "Y-Axis", objectives, objectives[2]),
			 		selectInput("snake.z", "Z-Axis", objectives, objectives[3]),
			 		selectInput("snake.color", "Color", objectives, objectives[4]),
			 		br(),
			 		br(),
			 		h4("Additional Options"),
			 		checkboxInput("snake.label", "Show Full Labels", value=FALSE),
			 		checkboxInput("snake.ideal", "Show Ideal Point", value=TRUE),
			 		selectInput("snake.colormap", "Color Scheme", colors),
			 		sliderInput("snake.nfe", "Current NFE", min=ifelse(reduced, 10000, 1000), max=1000000, value=1000000, step=ifelse(reduced, 10000, 1000), animate=TRUE)),
			 	mainPanel(
			 		div(webGLOutput("snake", width="100%", height="600px"), style="overflow: hidden"),
			 		plotOutput("snake.colorbar", height="150px"))))
)))
