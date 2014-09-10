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

#' Runs the exploration tool on a 5-objective problem.
#' 
#' @export
runVisDemo <- function() {
	addResourcePath("LakeProblem", system.file("LakeProblem", package="OpenMORDM"))
	
	filename <- system.file("LakeProblem/lake5obj.txt", package="OpenMORDM")
	names <- c("Phosphorus in Lake", "Bentham's Utility", "Today's Utility", "Future Utility", "Reliability")
	nvars <- 20
	nobjs <- length(names)
	nconstrs <- 1
	bounds <- matrix(rep(range(0.0, 0.1), 20), nrow=2)
	maximize <- names[2:5]
	order <- 1:5
	visible.variables <- FALSE
	
	welcome.panel <- function(input, output, session) {
		renderUI({
			list(h1("The Lake Problem"),
				 p("The lake problem is a classic decision making problem presented
				   by Carpenter et al. (1999).  It describes a hypothetical town
				   situated near a lake.  Over time, the town releases a certain
				   amount of anthropogenic pollution into the lake in the form of
				   phosphorus.  Additionally, there is some amount of uncontrolled
				   natural inflow into the lake.  The lake, over time, is able to
				   remove part of this pollution."),
				 div(img(src=ifelse(input$colormap.black, "LakeProblem/lake_model_simple_inverted.png", "LakeProblem/lake_model_simple.png")), style="text-align: center"),
				 p("The inhabitants of the town have to decide on the amount of
				   allowable pollution that can be emitted into the lake for a
				   given planning horizon (100 years).  Allowing more pollution
				   increases the economic, industrial, and agricultural output
				   (thus increasing the utility).  However, the increased pollution
				   degrades the quality of the lake, causing algal blooms and
				   hurting local fisheries and/or tourism."),
				 p("Singh et al. (In Review) have generated the optimal solutions
				   (the Pareto optimal set) for this problem using five objectives: pollution in
				   the lake, Bentham's utility, today's utility, future utility,
				   and reliability.  This website provides several visualizations
				   of the optimal solutions to the lake problem, and includes
				   several analysis routines for exploring the data in depth."),
				 p("[Figures and text modified with permission from Singh et al. (In Review).]", style="font-style: italic; text-align: center"),
				 h3("Quick Tutorial"),
				 p("Please refer to the Visualizing Data with OpenMORDM tutorial
				   for instructions on using this software, available as a",
				   a("PowerPoint (PPTX) presentation", href="LakeProblem/Visualizing_Data_with_OpenMORDM.pptx", target="_blank"),
				   "or a",
				   a("PDF file", href="LakeProblem/Visualizing_Data_with_OpenMORDM.pdf", target="_blank"), "."),
				 h3("Compatability Note"),
				 p(HTML("This website uses WebGL, a 3D library supported only by the
				 	   most recent web browsers.  For the best experience, please
				 	   ensure you are using the latest versions of
				 	   <a href=\"http://www.mozilla.org/firefox\">Firefox</a>,
				 	   <a href=\"http://www.google.com/chrome/browser/\">Chrome</a>,
				 	   <a href=\"http://www.opera.com\">Opera 22+</a>,
				 	   Safari 8, or Internet Explorer 11.")))
		})
	}
	
	selection.panel <- function(data, input, output, session) {
		max.nfe <- attr(data[[length(data)]], "NFE")
		step.nfe <- max.nfe / length(data)
		
		to.index <- function(input) {
			if (is.null(input$nfe) || is.na(input$nfe)) {
				length(data)
			} else {
				input$nfe / step.nfe
			}
		}
		
		observe({
			do.custom <- function(input) {
				index <- to.index(input)
				set <- mordm.getset(data, index)
				point <- set[input$selection,,drop=FALSE]
				
				names <- vector()
				year <- as.numeric(format(Sys.time(), "%Y"))
				
				for (i in 1:20) {
					names <- c(names, sprintf("%d - %d", year+5*(i-1), year+5*(i-1)+4))
				}
				
				oldmai <- par()$mai
				par(mai=c(oldmai[1]+1,oldmai[2],oldmai[3],oldmai[4]))
				barplot(point[1:20], xlab="", ylab="Industrial Pollution", 
						ylim=range(0, 0.1), cex.lab=1.3, las=2, names.arg=names,
						main="Pollution Control Strategy for the Selected Solution")
				par(mai=oldmai)
			}
			
			output$custom.view <- renderPlot({
				if (exists("default.par", mordm.globals)) par(get("default.par", mordm.globals))
				
				if (input$colormap.black) {
					par(bg="black", fg="white", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
				} else {
					par(bg="white", fg="black", col="black", col.axis="black", col.lab="black", col.main="black", col.sub="black")
				}
				
				assign("default.par", par(no.readonly=TRUE), mordm.globals)
				do.custom(input)
			})
			
			to.image.size <- function(session, id, dpi=72) {
				if (session$input$custom_image) {
					list(width=session$input$image.width, height=session$input$image.height)
				} else {
					list(width=session$clientData[[paste("output_", id, "_width", sep="")]]/dpi,
						 height=session$clientData[[paste("output_", id, "_height", sep="")]]/dpi)
				}
			}
			
			output$download.custom.png <- downloadHandler(
				filename = "custom.png",
				content = function(file) {
					size <- to.image.size(session, "custom.view")
					png(file, height=size$height, width=size$width, units="in", res=72)
					do.custom(input)
					dev.off()
				})
			
			output$download.custom.svg <- downloadHandler(
				filename = "custom.svg",
				content = function(file) {
					size <- to.image.size(session, "custom.view")
					svg(file, height=size$height, width=size$width)
					do.custom(input)
					dev.off()
				})
			
			output$download.custom.eps <- downloadHandler(
				filename = "custom.eps",
				content = function(file) {
					size <- to.image.size(session, "custom.view")
					postscript(file, height=size$height, width=size$width)
					do.custom(input)
					dev.off()
				})
		})
		
		renderUI({
			index <- to.index(input)
			set <- mordm.getset(data, index)
			
			if (is.null(input$selection) || is.na(input$selection) || input$selection <= 0 || input$selection > nrow(set)) {
				p("Select a point on the 3D Plot page to view details.")
			} else {
				sidebarLayout(
					sidebarPanel(
						p("You can provide a custom Shiny interface that will display details for the currently selected point.  In this example, the figure to the right shows the pollution control strategy for the lake problem."),
						br(),
						br(),
						h4("Download"),
						downloadButton("download.custom.png", "PNG Image"),
						downloadButton("download.custom.svg", "SVG Image"),
						downloadButton("download.custom.eps", "EPS File")),
					mainPanel(plotOutput("custom.view")))
			}
		})
	}
	
	explore(filename, nvars, nobjs, nconstrs, names, bounds, maximize, order,
			welcome.panel=welcome.panel, selection.panel=selection.panel)
}