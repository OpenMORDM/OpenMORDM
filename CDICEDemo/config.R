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
	selection.panel <- function(data, input, output, session) {
		output$custom.view <- renderPlot({
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
				plotOutput("custom.view")
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
