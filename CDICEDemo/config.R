model <- "lake"

if (model == "lake") {
	filename <- "../demo/data/lake5obj.txt"
	objectives <- c("Phosphorus in Lake", "Bentham's Utility", "Today's Utility", "Future Utility", "Reliability")
	nvars <- 20
	nobjs <- length(objectives)
	nconstrs <- 1
	bounds <- matrix(rep(range(0.0, 0.1), 20), nrow=2)
	maximize <- objectives[2:5]
	order <- 1:5
	selectable <- TRUE
	selection.panel <- function(data, input, output, session) {
		do.custom <- function(input) {
			if (is.null(input$nfe) || is.na(input$nfe)) {
				index <- length(data)
			} else {
				index <- input$nfe / step.nfe
			}
			
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
		
		renderUI({
			if (is.null(input$nfe) || is.na(input$nfe)) {
				index <- length(data)
			} else {
				index <- input$nfe / step.nfe
			}
			
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
} else if (model == "cdice") {
	filename <- "runtime_util_ptemp_pvdam_pvabate_100cs_short_10k.txt"
	objectives <- c("Expected Utility", "Reliability of Temperature Goal", "Climate Damages", "Abatement Costs")
	nvars <- 0
	nobjs <- 4
	nconstrs <- 0
	bounds <- NULL
	maximize <- objectives[1:2]
	order <- c(2,3,4,1)
	selectable <- TRUE
	selection.panel <- NULL
}

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
