Risk_Efficient_Portfolio <- function(mu, S, mu_f, len=1001){
	##
	var_name <- names(mu)
	if(is.null(var_name)){
		n_var <- length(mu)
		var_name <- rep("V", n_var)
		for(i in 1:n_var) var_name[i] <- paste(var_name[i], i, sep="")
	}
	
	## calculate the efficient frontier
	p <- length(mu)
	O_vec <- rep(1,p) 
	
	mu_vec <- seq(min(mu), max(mu), len=len)
	Sinv <- solve(S)
	
	A <- c(t(mu)%*%Sinv%*%O_vec)
	B <- c(t(mu)%*%Sinv%*%mu)
	C <- c(t(O_vec)%*%Sinv%*%O_vec)
	D <- c(B*C - A*A)
	
	X1 <- Sinv%*%O_vec/D 
	X2 <- Sinv%*%mu/D
	g <- X1*B - X2*A
	h <- X2*C - X1*A
	
	omega_P <- t(mu_vec)%x%h + g%x%t(rep(1,len))
	sigma_P <- sapply(1:len, function(i) sqrt(t(omega_P[,i])%*%S%*%omega_P[,i]))
	
	## minimum risk portfolio
	sel <- which(sigma_P == min(sigma_P))
	omega_min_risk <- omega_P[,sel]
	mu_min_risk <- t(mu)%*%omega_min_risk
	s_min_risk <- sigma_P[sel]
	
	v_min_risk <- c(mu_min_risk, s_min_risk, omega_min_risk)
	names(v_min_risk) <- c("mu", "s", var_name)
	
	## tangency portfolio
	tangent <- (mu_vec-mu_f)/sigma_P
	sel <- which(tangent == max(tangent))
	omega_tangency <- omega_P[,sel]
	mu_tangency <- t(mu)%*%omega_tangency
	s_tangency <- sigma_P[sel]
	
	v_tangency <- c(mu_tangency, s_tangency, omega_tangency)
	names(v_min_risk) <- c("mu", "s", var_name)
	
	return(list(mu=mu, S=S, mu_f=mu_f, mu_P=mu_vec, s_P=sigma_P, 
		min_risk =v_min_risk, tangency =v_tangency))	
}


print.REP <- function(obj){
	x <- obj$s_P
	y <- obj$mu_P
	
	plot(c(0, x), c(0, y), type="n", 
		xlab="Standard Deviation of Retrun", ylab="Expected Return")
	lines(x, y)
	
	F <- c(0, obj$mu_f)
	M <- c(obj$min_risk[2], obj$min_risk[1])
	T <- c(obj$tangency[2], obj$tangency[1])
	
	points(F[1], F[2], pch=19); text(F[1], F[2], "Fixed Risk", pos=4)
	points(M[1], M[2], pch=19); text(M[1], M[2], "Minimum Risk", pos=4)
	points(T[1], T[2], pch=19); text(T[1], T[2], "Tangency Risk", pos=2)
	lines(c(F[1], T[1]), c(F[2], T[2]), col=2, lty=2)
}

summary.REP <- function(obj){
	X <- rbind(obj$min_risk, obj$tangency)
	rownames(X) <- c("min_risk", "tangency")
	print(round(X, digit=3))
}
	
n <- 200; p <- 5	
X <- matrix(rnorm(n*p), n, p)
X <- X + rep(1, 200)%*%t(c(1:p)/20)	
mu <- apply(X, 2, mean)
S <- var(X)
mu_f <- .01	
my_port <- Risk_Efficient_Portfolio(mu, S, mu_f)
print.REP(my_port)
summary.REP(my_port)


### How to get stock prices from yahoo
# 2330 is the ID of TSMC

# install.packages("quantmod")
library("quantmod")
tw2330 <- getSymbols("2330.TW", auto.assign=FALSE)
dr_tw2330 <- dailyReturn(tw2330)