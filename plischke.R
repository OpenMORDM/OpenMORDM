library("pracma")

rosenbrock <- function(x) {
	c(x[1])
}

kernel.normal <- function(x) exp(-x^2/2)/sqrt(2*pi)
kernel.triangle <- function(x) pmax(1-abs(x/sqrt(6)), 0)/sqrt(6)
kernel.epanechnikov <- function(x) 3/(4*sqrt(5))*pmax(1-(x^2/5), 0)
kernel.uniform <- function(x) (1-abs(x/sqrt(3)) > 0)/(2*sqrt(3))
kernel.biweight <- function(x) 15/(16*sqrt(7))*pmax(1-(x^2/7), 0)^2

kernel.lookup <- function(kd.shape="epanechnikov") {
	if (is.function(kd.shape)) {
		kernel <- kd.shape
	} else if (is.character(kd.shape)) {
		kd.shape = tolower(kd.shape)
		
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
			ks.level = -ks.level
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
	
	indx <- order(y)
	ys <- y[indx]
	
	if (output.trafo == "off") {
		ymaxmin <- range(ys)
		rangey <- diff(ymaxmin)
		ysupp <- ymaxmin+0.06*rangey*matrix(c(-1,1))
		yy <- linspace(ymaxmin[1]-0.04*rangey, ymaxmin[2]+0.04*rangey, quadrature.points)
	} else if (output.trafo == "cdf") {
		ys = empcdf(ys)
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
	consticator <- matrix(0, nrow=1, ncol=k)
	xr = matrix(0, nrow=n, ncol=k)
	
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
	if (is.character(kd.width)) {
		alfa <- 0
	} else {
		alfa <- kd.width
	}
	
	if (kd.estimator == "cheap") {
		kdest.result <- kdest(y, yy, alfa, kernel.lookup(kd.shape))
		f1 <- kdest.result$est
		alfa <- kdest.result$h
	} else if (kd.estimator == "stats") {
		if (alfa == 0) {
			f1 <- density(y, kernel=kd.shape, n=length(yy), from=ysupp2[1], to=ysupp2[2])$y
		} else {
			f1 <- density(y, kernel=kd.shape, n=length(yy), from=ysupp2[1], to=ysupp2[2], width=alfa)$y
		}
	} else if (kd.estimator == "diffusion") {
		if (alfa == 0) {
			kde.result <- kde(ys, quadrature.points, ysupp2[1], ysupp2[2])
		} else {
			kde.result <- kde(ys, quadrature.points, ysupp2[1], ysupp2[2], alfa)
		}
		
		f1 <- kde.result$density
		f1[f1<0] = 0
	} else if (kd.estimator == "hist") {
		f0 <- histc(ys, yy)$cnt
		f1 <- f0/trapz(yy, f0)
	} else {
		stop("Unknown kernel density estimator")
	}
	
	list(f1=f1, alfa=alfa)
}

plischke <- function(x,
					 y,
					 partition.size=min(ceiling(nrow(x)^(2/(7+tanh((1500-nrow(x))/500)))), 48),
					 quadrature.points=110,
					 ks.level=0.95,
					 zero.crossing="on",
					 parameter.names=NULL,
					 kd.estimator="cheap",
					 kd.width="auto",
					 complement=FALSE,
					 switch.xy=FALSE,
					 output.trafo="off",
					 kd.shape="epanechnikov") {
	n <- nrow(x)
	k <- ncol(x)
	
	kd.estimator <- tolower(kd.estimator)
	kd.shape <- tolower(kd.shape)
	
	if (is.character(output.trafo)) {
		output.trafo <- tolower(output.trafo)
	}

	if (kd.estimator == "diffusion") {
		quadrature.points=2^nextpow2(quadrature.points)
	}
	
	# parse the kernel shape argument
	kernel <- kernel.lookup(kd.shape)
	
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
	ysupp2 <- range(yy) #yy[c(1, length(yy))]
	yy_ <- yy
	
	# transform x into ranks and sort
	rank.result <- rank.sort(x, indx)
	consticator <- rank.result$consticator
	xr <- rank.result$xr
	
	# smoothed empirical pdf
	smoothed.pdf <- smooth.pdf(y, ys, yy, ysupp2, quadrature.points, kd.shape, kd.estimator, kd.width)
	f1 <- smoothed.pdf$f1
	alfa <- smoothed.pdf$alfa
	
	segs = linspace(0, 1, partition.size+1)
	delta = matrix(0, nrow=1, ncol=k)
	Sr = matrix(0, nrow=1, ncol=partition.size)
	nr = matrix(0, nrow=partition.size, ncol=1)
	Si = matrix(0, nrow=1, ncol=k)
	Vyc = matrix(0, nrow=1, ncol=partition.size)
	Ey = mean(y)
	Vy = var(y)
	Kr <- matrix(0, nrow=1, ncol=partition.size)
	acceptL <- matrix(0, nrow=3, ncol=k)
	
	for (i in 1:k) {
		for (m in 1:partition.size) {
			if (complement) {
				yx <- ys[xr[,i]<segs[m] | xr[,i]>=segs[m+1]]
			} else {
				yx <- ys[xr[,i]>=segs[m] & xr[,i]<segs[m+1]]
			}
			
			nx = length(yx)
			
			if (nx > 0) {
				# contribution to the variance of the conditional expectation
				if (complement) {
					Vyc[m] <- nx^2/(n-nx)*(mean(yx)-Ey)^2
				} else {
					Vyc[m] <- nx*(mean(yx)-Ey)^2
				}
				
				if (kd.width == "auto") {
					alfa <- 0
				}
				
				smoothed.pdf <- smooth.pdf(yx, yx, yy, ysupp, quadrature.points, kd.shape, kd.estimator, alfa)
				f2 <- smoothed.pdf$f1
				alfa <- smoothed.pdf$alfa
				
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
					Sr[m] = S
				}
				
				# save KS for endogeneous threshold computation
				Kr[m] <- max(abs(cumtrapz(yy,fff)))
				
				yy <- yy_
			}
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
		
		delta[i] <- 0.5*(Sr %*% nr)/n
		Si[i] <- sum(Vyc)/Vy/(n-1)
	}
	
	list(delta=delta, Si=Si, acceptL=acceptL)
}

empcdf <- function(xs) {
	n <- length(xs)
	xr <- matrix(1:n)
	tie_loc <- c(which(diff(xs)==0), n+2)
	tie_next = diff(tie_loc)
	maxt <- numel(tie_loc)
	i <- 1
	
	while (i < maxt) {
		run <- tie_loc[i]
		len <- 1
		
		while (tie_next[i] == 1) {
			i <- i+1
			len <- len+1
		}
		
		xr[run:(run+len)] = run+len/2
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
	
	result <- matrix(0, nrow=nrows, ncol=1)
	result[seq(1,nrows,2)] <- x[1:(nrows %/% 2)]
	result[seq(2,nrows,2)] <- x[seq(nrows, nrows %/% 2 + 1, -1)]
	result
}

dct1d <- function(x) {
	nrows = length(x)
	weight <- c(1, 2*(exp(-1i*(1:(nrows-1))*pi/(2*nrows))))
	x <- c(x[seq(1,nrows,2)], x[seq(nrows,2,-2)])
	Re(weight * fft(x))
}

#X <- matrix(c(linspace(0, 1, 1000), linspace(0, 1, 1000)), nrow=1000, ncol=2)
X <- matrix(runif(2000), nrow=1000)
Y <- sapply(1:1000, function(i) rosenbrock(X[i,]))
print(plischke(X, Y))