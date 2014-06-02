# Estimation of Borgonovo's Delta Moment Independent Measure
#
# This code is a direct translation of Elmar Plischke's original MATLAB code
# into R.  Dr. Plischke's original MATLAB code is available for download from
# <http://www.immr.tu-clausthal.de/~epl/papers/papers.html>.  Permission to
# distribute this version under the MIT License was granted by Dr. Plischke on
# 5/15/2014 via e-mail.
# 
# Please refer to the following paper for details on this method:
#     Plischke, Borgonovo, Smith: "Global sensitivity measures from given data",
#     European Journal of Operational Research 226(3):536-550, 2013.
#
#
# MIT License:
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

library("pracma")

kernel.normal <- function(x) exp(-x^2/2)/sqrt(2*pi)
kernel.triangle <- function(x) pmax(1-abs(x/sqrt(6)), 0)/sqrt(6)
kernel.epanechnikov <- function(x) 3/(4*sqrt(5))*pmax(1-(x^2/5), 0)
kernel.uniform <- function(x) (1-abs(x/sqrt(3)) > 0)/(2*sqrt(3))
kernel.biweight <- function(x) 15/(16*sqrt(7))*pmax(1-(x^2/7), 0)^2

kernel.lookup <- function(kd.shape="epanechnikov") {
	if (is.function(kd.shape)) {
		kernel <- kd.shape
	} else if (is.character(kd.shape)) {
		kd.shape <- tolower(kd.shape)
		
		if (kd.shape == "normal") {
			kernel <- kernel.normal
		} else if (kd.shape == "triangle") {
			kernel <- kernel.triangle
		} else if (kd.shape == "epanechnikov") {
			kernel <- kernel.epanechnikov
		} else if (kd.shape == "box" | kd.shape == "uniform") {
			kernel <- kernel.uniform
		} else if (kd.shape == "biweight" | kd.shape == "biquadratic") {
			kernel <- kernel.biweight
		} else {
			stop("Unsupported kernel")
		}	
	} else {
		stop("Unsupported kernel")
	}
}

kolmog <- function(x,y) (x<4)*sqrt(2*pi)/x*sum(exp(-seq(1,35,2)^2*pi^2/(8*x^2)))+(x>=4)*1.0-y

critical.value <- function(x, y, ks.level=0.95, complement=FALSE) {
	k <- ncol(x)
	
	if (ks.level != 0) {
		if (complement) {
			ks.level <- abs(ks.level)
		}
		
		if (ks.level < 0) {
			ks.level <- -ks.level
			ks.test <- function(s,y,f) max(abs(cumtrapz(y,f)))
		} else {
			ks.test <- function(s,y,f) s*0.5
		}
		
		ks.crit <- sapply(1:length(ks.level), function(i) fzero(function(x) kolmog(x, ks.level[i]), c(0.001,2))$x)
	} else {
		ks.crit <- 0
	}
	
	if (length(ks.crit) == 1) {
		ks.crit <- ks.crit*rep(1,k)
	}
	
	list(ks.test=ks.test, ks.crit=ks.crit)
}

transform.output <- function(y, output.trafo="off", quadrature.points=110) {
	if (is.function(output.trafo)) {
		y <- output.trafo(y)
		output.trafo <- "off"
	}
	
	if (is.character(output.trafo)) {
		output.trafo <- tolower(output.trafo)
	}
	
	indx <- order(y)
	ys <- y[indx]
	
	if (output.trafo == "off") {
		ymaxmin <- range(ys)
		rangey <- diff(ymaxmin)
		ysupp <- ymaxmin+0.06*rangey*matrix(c(-1,1))
		yy <- linspace(ymaxmin[1]-0.04*rangey, ymaxmin[2]+0.04*rangey, quadrature.points)
	} else if (output.trafo == "cdf") {
		ys <- empcdf(ys)
		y[indx] <- ys
		ysupp <- matrix(c(-0.06, 1.06))
		yy <- linspace(-0.04, 1.04, quadrature.points)
	} else if (output.trafo == "normal") {
		ncdf <- -sqrt(2)*erfinv(1-2*empcdf(ys))
		y[indx] <- ncdf
		ys <- ncdf
		ymaxmin <- ys[c(1,length(ys))]
		rangey <- diff(ymaxmin)
		ysupp <- ymaxmin+0.06*rangey*matrix(c(-1,1))
		yy <- linspace(ymaxmin[1]-0.04*rangey, ymaxmin[2]+0.04*rangey, quadrature.points)
	} else if (output.trafo == "interpol") {
		yy <- interp1((2*(1:n)-1)/(2*n), ys, linspace(0,1,quadrature.points),"spline")
	} else if (output.trafo == "cdf-tight") {
		ys <- empcdf(ys)
		y[indx] <- ys
		ysupp <- matrix(c(-0.02, 1.02))
		yy <- linspace(-0.01, 1.01, quadrature.points)
	} else if (output.trafo == "cdf-loose") {
		ys <- empcdf(ys)
		y[indx] <- ys
		ysupp <- matrix(c(-0.1, 1.1))
		yy <- linspace(-0.08, 1.08, quadrature.points)
	} else {
		stop("Unsupported output transformation")
	}
	
	list(indx=indx, ys=ys, y=y, ysupp=ysupp, yy=yy)
}

rank.sort <- function(x, indx) {
	n <- nrow(x)
	k <- ncol(x)
	consticator <- zeros(1, k)
	xr <- zeros(n, k)
	
	for (i in 1:k) {
		indxx <- order(x[,i])
		xx <- x[indxx,i]
		consticator[i] <- xx[length(xx)] == xx[1]
		xx[indxx] <- empcdf(xx)
		xr[,i] <- xx[indx]
	}
	
	list(consticator=consticator, xr=xr)
}

smooth.pdf <- function(y, ys, yy, ysupp2, quadrature.points=110, kd.shape="epanechnikov", kd.estimator="cheap", kd.width="auto") {
	if (is.character(kd.estimator)) {
		kd.estimator <- tolower(kd.estimator)
	}
	
	if (is.character(kd.width)) {
		alfa <- 0
	} else {
		alfa <- kd.width
	}
	
	if (kd.estimator == "cheap") {
		kdest.result <- kdest(y, yy, alfa, kernel.lookup(kd.shape))
		f <- kdest.result$est
		alfa <- kdest.result$h
	} else if (kd.estimator == "stats") {
		if (alfa == 0) {
			f <- density(y, kernel=kd.shape, n=length(yy), from=ysupp2[1], to=ysupp2[2])$y
		} else {
			f <- density(y, kernel=kd.shape, n=length(yy), from=ysupp2[1], to=ysupp2[2], width=alfa)$y
		}
	} else if (kd.estimator == "diffusion") {
		if (alfa == 0) {
			kde.result <- kde(ys, quadrature.points, ysupp2[1], ysupp2[2])
		} else {
			kde.result <- kde(ys, quadrature.points, ysupp2[1], ysupp2[2], alfa)
		}
		
		f <- kde.result$density
		f[f<0] <- 0
	} else if (kd.estimator == "hist") {
		f0 <- histc(ys, yy)$cnt
		f <- f0/trapz(yy, f0)
	} else {
		stop("Unknown kernel density estimator")
	}
	
	list(f=f, alfa=alfa)
}

deltamim <- function(x,
					 y,
					 partition.size=min(ceiling(nrow(x)^(2/(7+tanh((1500-nrow(x))/500)))), 48),
					 quadrature.points=110,
					 ks.level=0.95,
					 zero.crossing="on",
					 kd.estimator="cheap",
					 kd.width="auto",
					 complement=FALSE,
					 plot.enabled=FALSE,
					 plot.cols=min(ncol(x), 4),
					 output.trafo="off",
					 kd.shape="epanechnikov",
					 ...) {
	n <- nrow(x)
	k <- ncol(x)
	parameter.names <- colnames(x)
	
	if (is.null(parameter.names)) {
		parameter.names = sprintf("x%d", 1:k)
	}

	if (tolower(kd.estimator) == "diffusion") {
		quadrature.points=2^nextpow2(quadrature.points)
	}
	
	# compute critical value for KS statistics
	ks.result <- critical.value(x, y, ks.level, complement)
	ks.test <- ks.result$ks.test
	ks.crit <- ks.result$ks.crit
	
	# transform the output
	trafo.result <- transform.output(y, output.trafo, quadrature.points)
	indx <- trafo.result$indx
	ys <- trafo.result$ys
	y <- trafo.result$y
	ysupp <- trafo.result$ysupp
	yy <- trafo.result$yy
	
	# create a backup copy
	ysupp2 <- range(yy)
	yy_ <- yy
	
	# transform x into ranks and sort
	rank.result <- rank.sort(x, indx)
	consticator <- rank.result$consticator
	xr <- rank.result$xr
	
	# smoothed empirical pdf
	smoothed.pdf <- smooth.pdf(y, ys, yy, ysupp2, quadrature.points, kd.shape, kd.estimator, kd.width)
	f1 <- smoothed.pdf$f
	alfa <- smoothed.pdf$alfa
	
	# initialize the plotting data
	if (plot.enabled) {
		plot.f1 <- f1
		plot.f2 <- rep(list(list()), k)
		plot.nr <- list()
		plot.Sr <- list()
	}
	
	segs <- linspace(0, 1, partition.size+1)
	delta <- zeros(1, k)
	Sr <- zeros(1, partition.size)
	nr <- zeros(partition.size, 1)
	Si <- zeros(1, k)
	Vyc <- zeros(1, partition.size)
	Ey <- mean(y)
	Vy <- var(y)
	Kr <- zeros(1, partition.size)
	acceptL <- zeros(3, k)
	
	for (i in 1:k) {		
		for (m in 1:partition.size) {
			if (complement) {
				yx <- ys[xr[,i]<segs[m] | xr[,i]>=segs[m+1]]
			} else {
				yx <- ys[xr[,i]>=segs[m] & xr[,i]<segs[m+1]]
			}
			
			nx <- length(yx)
			
			if (nx > 0) {
				# contribution to the variance of the conditional expectation
				if (complement) {
					Vyc[m] <- nx^2/(n-nx)*(mean(yx)-Ey)^2
				} else {
					Vyc[m] <- nx*(mean(yx)-Ey)^2
				}
				
				if (is.character(kd.width)) {
					# recompute alfa if kd.width is "auto"
					alfa <- kd.width
				}
				
				smoothed.pdf <- smooth.pdf(yx, yx, yy, ysupp, quadrature.points, kd.shape, kd.estimator, alfa)
				f2 <- smoothed.pdf$f
				alfa <- smoothed.pdf$alfa
				
				# collect data for plotting
				if (plot.enabled) {
					if (complement) {
						plot.f2[[i]] <- append(plot.f2[[i]], list(pmax(1/(n-nx)*(n*f1-nx*f2),0)))
					} else {
						plot.f2[[i]] <- append(plot.f2[[i]], list(f2))
					}
				}
				
				# compute differences
				fff <- f1-f2
				
				#detect zero crossings (no impressive effect)
				if (zero.crossing == "on") {
					ff <- abs(fff)
					deltafactor <- 0.5
					indx <- which(fff[-length(fff)]*fff[-1] < 0)
					
					if (length(indx) > 0) {
						yz <- yy[indx] + (yy[indx+1]-yy[indx])*fff[indx]/(fff[indx]-fff[indx+1])
						
						for (j in seq(length(indx), 1, -1)) {
							l <- indx[j]
							yy <- c(yy[1:l], yz[j], yy[(l+1):length(yy)])
							ff <- c(ff[1:l], 0, ff[(l+1):length(ff)])
							fff <- c(fff[1:l], 0, fff[(l+1):length(fff)])
						}
					}
				} else if (zero.crossing == "positive") {
					ff <- pmax(fff, 0)
					deltafactor <- 1
				} else if (zero.crossing == "negative") {
					ff <- -pmin(fff, 0)
					deltafactor <- 1
				} else if (zero.crossing == "off") {
					ff <- abs(fff)
					deltafactor <- 0.5
				} else if (zero.crossing == "test") {
					ff <- c(pmax(fff, 0), -pmin(fff, 0))
					deltafactor <- 0.5
					yy <- c(yy, yy-yy[1]+yy[length(yy)])
				} else {
					stop("Unknown zero crossing method")
				}
				
				if (complement) {
					ff <- nx/(n-nx)*ff
					nr[m]=n-nx
					nx=n-nx
				} else {
					nr[m]=nx
				}
				
				# integrate using trapzoidal rule
				S <- 2*deltafactor*trapz(yy,ff)
				
				# test for Kolmogorov-Smirnov (p=95%) (asymptotics for nx>30)
				if (ks.crit[i] & ks.test(S,yy,fff) < sqrt(1/n+1/nx)*ks.crit[i]) {
					Sr[m] <- 0
					Vyc[m] <- 0
				} else {
					Sr[m] <- S
				}
				
				# save KS for endogeneous threshold computation
				Kr[m] <- max(abs(cumtrapz(yy,fff)))
				
				yy <- yy_
			}
		}
		
		# collect data for plotting
		if (plot.enabled) {
			plot.nr <- append(plot.nr, list(nr))
			plot.Sr <- append(plot.Sr, list(Sr))
		}
		
		#predict rejection level
		thres1 <- max(0.5*Sr/sqrt(1/n+1/t(nr)))
		thres2 <- max(Kr/sqrt(1/n+1/t(nr)))
		thres3 <- min(Kr/sqrt(1/n+1/t(nr)))
		
		if (thres1 > 0) {
			acceptL[1,i] <- kolmog(thres1, 0)
		}
		
		if (thres2 > 0) {
			acceptL[2,i] <- kolmog(thres2, 0)
		}
		
		if (thres3 > 0) {
			acceptL[3,i] <- kolmog(thres3, 0)
		}
		
		# expectation over all partition segments
		delta[i] <- 0.5*(Sr %*% nr)/n
		
		# first order sensitivity index (biased estimator)
		Si[i] <- sum(Vyc)/Vy/(n-1)
	}
	
	# render the plots
	if (plot.enabled) {
		plot.new()
		cols <- rainbow(partition.size)
		nf <- layout(matrix(c(1:plot.cols, rep(plot.cols+1, plot.cols)), nrow=2, byrow=TRUE))
		layout.show(nf)
		
		for (i in 1:plot.cols) {
			plot(yy, plot.f1, type='l', lwd=2, ylab="Density function",
				 xlab=paste("F(x) given ", parameter.names[i], sep=""),
				 ylim=range(plot.f1, plot.f2[[i]]), xlim=range(yy))
			
			for (m in 1:partition.size) {
				lines(yy, plot.f2[[i]][[m]], col=cols[m])
			}
		}
		
		# plot the conditional densities
		cols <- rainbow(k)
		plot(cumsum(plot.nr[[1]])/n-1/(2*partition.size),
			 plot.Sr[[1]],
			 ylab=expression("S"["r"]),
			 xlab="Empirical CDF of Inputs",
			 main="Separation of Conditional Densities",
			 col=cols[1],
			 type='l',
			 ylim=range(0, plot.Sr))
		
		for (i in 2:k) {
			lines(cumsum(plot.nr[[i]])/n-1/(2*partition.size), plot.Sr[[i]], col=cols[i])
		}
		
		lines(c(0,1), ks.crit[i]*c(1,1)*sqrt((partition.size+1)/n), lty=2)
		legend("topright", legend=parameter.names, lty=1, lwd=1, col=cols, inset=0.02, bty="n")
	}
	
	# provide ranking of sensitivity indices
	rank <- rev(order(Si))
	
	# add parameter names
	if (!is.null(parameter.names)) {
		colnames(delta) <- parameter.names
		colnames(Si) <- parameter.names
		colnames(acceptL) <- parameter.names
	}
	
	list(delta=delta, Si=Si, rank=rank, acceptL=acceptL)
}

empcdf <- function(xs) {
	n <- length(xs)
	xr <- matrix(1:n)
	tie_loc <- c(which(diff(xs)==0), n+2)
	tie_next <- diff(tie_loc)
	maxt <- numel(tie_loc)
	i <- 1
	
	while (i < maxt) {
		run <- tie_loc[i]
		len <- 1
		
		while (tie_next[i] == 1) {
			i <- i+1
			len <- len+1
		}
		
		xr[run:(run+len)] <- run+len/2
		i <- i+1
	}
	
	xr <- (xr - 0.5)/n
}

kdest <- function(y, z, h=0, kernel=kernel.epanechnikov) {
	n <- length(y)
	
	if (h == 0) {
		m <- median(y)
		s <- min(std(y, flag=1), median(abs(m-y))/0.675)
		h <- s/(((3*n)/4)^(1/5))
		
		if (h == 0) {
			stop("Unable to estimate the kernel density with the cheap method, try another kd.estimator")
		}
	}
	
	k <- length(z)
	W <- matrix(z, nrow=n, ncol=k, byrow=TRUE) - matrix(y, nrow=n, ncol=k)
	
	if (n == 1) {
		est <- mean(kernel(W/h))/h
	} else {
		est <- colMeans(kernel(W/h))/h
	}
	
	list(est=est, h=h)
}

kde <- function(data,
				n=2^14,
				MIN=min(data)-(max(data)-min(data))/10,
				MAX=max(data)+(max(data)-min(data))/10,
				bandwidth_in=NULL) {
	data <- c(data)
	n <- 2^ceil(log2(n))
	R <- MAX-MIN
	dx <- R/(n-1)
	xmesh <- MIN+seq(0, R, dx)
	N=length(data)
	initial_data <- histc(data, xmesh)$cnt/N
	a <- dct1d(initial_data)
	
	if (is.null(bandwidth_in)) {
		I <- (1:(n-1))^2
		a2 <- (a[2:length(a)]/2)^2
		t_star <- fzero(function(t) fixed_point(t,N,I,a2), c(0, 0.1))$x
	} else {
		t_star <- (bandwidth_in/R)^2
	}
	
	a_t <- a*exp(-seq(0, n-1)^2*pi^2*t_star/2)
	density <- idct1d(a_t)/R
	bandwidth <- sqrt(t_star)*R
	
	list(bandwidth=bandwidth, density=density, xmesh=xmesh)
}

fixed_point <- function(t,N,I,a2) {
	l <- 7
	f <- 2*pi^(2*l)*sum(I^l*a2*exp(-I*pi^2*t))
	
	for (s in seq(l-1,2,-1)) {
		K0 <- prod(seq(1,2*s-1,2))/sqrt(2*pi)
		const <- (1+(1/2)^(s+1/2))/3
		time <- (2*const*K0/N/f)^(2/(3+2*s))
		f <- 2*pi^(2*s)*sum(I^s*a2*exp(-I*pi^2*time))
	}
	
	t-(2*N*sqrt(pi)*f)^(-2/5)
}

idct1d <- function(x) {
	nrows <- length(x)
	weights <- nrows*exp(1i*(0:(nrows-1))*pi/(2*nrows))
	x <- Re(ifft(c(weights * x)))
	
	result <- zeros(nrows, 1)
	result[seq(1,nrows,2)] <- x[1:(nrows %/% 2)]
	result[seq(2,nrows,2)] <- x[seq(nrows, nrows %/% 2 + 1, -1)]
	result
}

dct1d <- function(x) {
	nrows <- length(x)
	weight <- c(1, 2*(exp(-1i*(1:(nrows-1))*pi/(2*nrows))))
	x <- c(x[seq(1,nrows,2)], x[seq(nrows,2,-2)])
	Re(weight * fft(x))
}

deltafast <- function(x, y, M=24) {
	n <- nrow(x)
	k <- ncol(x)
	kernel <- kernel.epanechnikov
	
	# numerical noise cutoff (simple Kolmogorov-Smirnov)
	cutoff <- 0.7
	
	# output stats
	miny <- min(y)
	maxy <- max(y)
	L <- miny - 0.04*(maxy-miny)
	U <- maxy + 0.04*(maxy-miny)
	
	# transform to unbounded support, the default is no transformation
	stretch <- function(y,l,u) y
	squeeze <- function(z,l,u) z
	ty <- stretch(y,L,U)
	
	# work with transformed data
	medy <- median(ty)
	iqry <- median(abs(medy-ty)) # interquartile range estimator
	
	# bandwidth estimate
	stdy <- min(std(ty, flag=1), iqry/0.675)
	h <- stdy*((4/(3*n))^(1/5))
	
	# construct interpolation points
	z1 <- linspace(min(ty)-2*h, medy-iqry, 25)
	z2 <- linspace(medy-iqry, medy+iqry, 52)
	z3 <- linspace(medy+iqry, max(ty)+2*h, 25)
	z <- c(z1, z2[2:(length(z2)-1)], z3)
	l <- length(z)
	
	# back-trafo interpolatin points
	tz <- squeeze(z,L,U)
	
	# kernel density matrix
	W <- kernel(bsxfun("-", repmat(z, n, 1), repmat(matrix(ty, nrow=n, ncol=1), 1, l))/h)/h
	
	# unconditional density
	densy <- apply(W, 2, mean)
	
	# conditional densities for partitioned data
	Sm <- zeros(k,M)
	Tm <- zeros(k,M)
	nm <- zeros(k,M)
	
	# keep only W from the partition
	indxx <- apply(x, 2, order)
	xr <- zeros(n, k)
	
	for (i in 1:k) {
		xr[,i] <- x[indxx[,i],i]
		xr[indxx[,i],i] <- 1:n # ranks (no ties)
	}
	
	for (j in 1:M) {
		indx <- ((j-1)*n/M < xr) & (xr <= j*n/M)
		nm[,j] <- apply(indx, 2, sum) # no ties, always same nbr of realizations
		
		for (i in 1:k) {
			densc <- apply(W[indx[,i],], 2, mean) # conditional density
			Sm[i,j] <- trapz(z, pmax(densy-densc,0)) # L1 separation of densities
			
			#Kullback Leibler
			tt <- densc*(log(densc)-log(densy))
			tt[densc==0] <- 0
			Tm[i,j] <- trapz(z, tt)
		}

		Sm[Sm<cutoff*sqrt(1/n+1/nm)] <- 0
		d <- apply(Sm*nm, 1, sum)/n
		s <- apply(Tm*nm, 1, sum)/n
	}

	list(delta=d, theta=s)
}
