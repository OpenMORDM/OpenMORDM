library(OpenMORDM)

rosenbrock <- function(x) {
	n <- length(x)
	as.vector(sum(100*(x[2:n]-x[1:(n-1)]^2)^2 + (x[1:(n-1)]-1)^2))
}

n <- 2
problem <- setup(rosenbrock, n, 1, bounds=matrix(c(rep(-1, n), rep(1, n)), byrow=TRUE, nrow=2))
#result <- borg.optimize(problem, 10000)
result <- lhsample(10000, problem)
explore(result, nobjs=n+1)