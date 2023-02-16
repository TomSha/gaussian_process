

SE_function <- function(Xi,Xj, rho = 1, alpha = 1) alpha^2 * exp(-1/(2*rho^2) *(Xi - Xj) ^ 2)
covC_function <- function(X, Y, rho = 1, alpha = 1) outer(X, Y, SE_function, rho, alpha)




#### plot functions

plot_multi_norm <- function(Sig, y, mu = c(0, 0)){
	x_range <- seq(-2.5, 2.5, 0.1)

	dens <- function(x1, x2) dmnorm(cbind(x1, x2), mu, Sig)
	z <- outer(x_range, x_range, dens)

	contour(x_range, x_range, z, xlab = "f(x)", ylab = "f(x')", drawlabels = F)
	points(y, col = colourscale[1:3], pch = 19, cex = 1)
}

plot_output <- function(x, y){
	x_mat <- matrix(rep(x, nrow(y)), nrow = 3, byrow = T)

	plot(x_mat, y, pch = 19, col = colourscale[1 : 3], xlim = c(-0.3, 2.5), ylim = c(-1.5, 2), xlab = "x", ylab = "f(x)")

	n <- ncol(x_mat) - 1
	for(i in 1:n){
		segments(x_mat[,i], y[,i], x_mat[,i + 1], y[,i + 1], col = colourscale[1 : 3])
	}
}
