nvars <- 11
nobjs <- 2

DTLZ2 <- function(x) {
	g <- sum((x[nobjs:nvars] - 0.5*rep(1,nvars-nobjs+1))^2)
	objs <- rep(1+g, nobjs)

	for (i in 1:nobjs) {
		if (nobjs > i) {
			for (j in 1:(nobjs-i)) {
				objs[i] <- objs[i] * cos(0.5 * pi * x[j])
			}
		}

		if (i > 1) {
			objs[i] <- objs[i] * sin(0.5 * pi * x[nobjs-i+1])
		}
	}

	return(objs)
}
