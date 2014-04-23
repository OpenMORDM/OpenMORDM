library(grid)
library(prim)
library(plot3D)

mordm.defaultnames <- function(nvars, nobjs) {
	names <- vector()

	for (i in 1:nvars) {
		names <- append(names, paste("Var", as.character(i), sep=""))
	}

	for (i in 1:nobjs) {
		names <- append(names, paste("Obj", as.character(i), sep=""))
	}

	return(names)
}

mordm.read <- function(file, nvars, nobjs, nconstrs=0) {
	text <- readLines(file)
	solutions <- vector()
	attributes <- vector()
	result <- list()
	names <- mordm.defaultnames(nvars, nobjs)

	for (line in text) {
		if (substr(line, 1, 1) == "#") {
			if (length(solutions) > 0 | length(attributes) > 0) {
				entry <- matrix(nrow=length(solutions), ncol=nvars+nobjs)

				for (i in 1:length(solutions)) {
					tokens <- unlist(strsplit(solutions[i], " ", fixed=TRUE))

					for (j in 1:length(tokens)) {
						entry[i,j] <- as.double(tokens[j])
					}
				}

				colnames(entry) <- names

				for (line in attributes) {
					tokens <- unlist(strsplit(line, "=", fixed=TRUE))

					if (length(tokens) == 1) {
						attr(entry, tokens[1]) <- TRUE
					} else {
						attr(entry, tokens[1]) <- as.double(tokens[2])
					}
				}

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

	attr(result, "nvars") <- nvars
	attr(result, "nobjs") <- nobjs
	attr(result, "nconstrs") <- nconstrs
	class(result) <- "mordm"

	return(result)
}

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

mordm.plothist <- function(data) {
	last <- data[[length(data)]]
	names <- colnames(last)

	pairs(last)
#, xlim=range(0,1), ylim=range(0,1))

	#par(mfrow=c(1,length(names)))

	#for (i in 1:length(names)) {
	#	hist(last[,i], main=names[i], xlim=range(0,1))
	#}
}

mordm.plotset <- function(data) {
	last <- data[[length(data)]]
	nvars <- attr(data, "nvars")
	nobjs <- attr(data, "nobjs")
	
	if (nobjs == 2) {
		plot(last[,(nvars+1):(nvars+2)])
	} else if (nobjs == 3) {
		plot3d(last[,(nvars+1):(nvars+3)], type="s")
	} else if (nobjs == 5) {
		scatter3D(x=last[,nvars+1],
			y=last[,nvars+2],
			z=last[,nvars+3],
			colvar=last[,nvars+4],
			pch=16, cex=1.5,
			xlab=colnames(last)[nvars+1],
			ylab=colnames(last)[nvars+2],
			zlab=colnames(last)[nvars+3],
			clab=colnames(last)[nvars+4],
			ticktype="detailed",
			theta=10, d=2)
	}
}

mordm.plotops <- function(data, time=FALSE, improvements=FALSE, log=FALSE) {
	names <- c("SBX", "DE", "PCX", "SPX", "UNDX", "UM")
	colors <- c("cyan", "red", "blue", "green", "orange", "purple")
	attributes <- mordm.attributes(data)

	par(xpd=TRUE, mar=c(10,5,2,ifelse(improvements, 5, 2))+0.1)

	if (improvements) {
		improve.diff <- c(attributes[1,"Improvements"], diff(attributes[,"Improvements"]))

		if (time) {
			time.diff <- c(attributes[1,"ElapsedTime"], diff(attributes[,"ElapsedTime"]))
			improve.per <- improve.diff / (time.diff)
		} else {
			nfe.diff <- c(attributes[1,"NFE"], diff(attributes[,"NFE"]))
			improve.per <- improve.diff / (nfe.diff/1000)
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

		axis(4, at=improve.at, label=improve.label, las="1", col="gray50", col.axis="gray50")
		mtext(paste("Pareto Improvements per ", ifelse(time, "Second", "1000 NFE"), sep=""), side=4, line=3, col="gray50")

		par(new=TRUE)
	}

	yrange <- range(0:100)
	xrange <- range(attributes[,ifelse(time, "ElapsedTime", "NFE")])

	plot(xrange, yrange, type="n", log=ifelse(log, "x", ""),
		xlab=ifelse(time, "Elapsed Time (seconds)", "NFE"),
		ylab="Operator Probability", bty="L")

	for (i in 1:length(names)) {
		lines(attributes[,ifelse(time, "ElapsedTime", "NFE")], attributes[,names[i]]*100, lwd=4, col=colors[i])
	}

	legend(x=grconvertX(unit(ifelse(improvements, 0.5, 0.45), "npc"), from="npc", to="user"),
		y=grconvertY(unit(-0.3, "npc"), from="npc", to="user"),
		legend=names, col=colors, lwd=4, bty="o", cex=1.0, horiz=TRUE, xjust=0.5)
}

data <- mordm.read("E:/Git/openmordm/largeoutput.txt", 20, 5, 1)
#hypervolume(data[[length(data)]], bandwidth=1, repsperpoint=10)
mordm.plotops(data, time=FALSE, improvements=TRUE)
#mordm.plothist(data)

#last <- data[[length(data)]]
#pairs(last)
#last.prim <- prim.box(x=last[,1:11], y=last[,12], threshold.type=0, threshold=c(0.75, 0.25))
#summary(last.prim)
#plot(last.prim)
