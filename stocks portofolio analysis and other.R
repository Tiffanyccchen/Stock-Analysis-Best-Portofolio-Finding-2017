##GET STOCK PRICE RETURN DATA--How to get stock prices from yahoo-----------------------

# 2330 is the ID of TSMC
tw2330 <- getSymbols("2330.TW", auto.assign=FALSE)
dr_tw2330 <- dailyReturn(tw2330)

# install.packages("quantmod")
library("quantmod")
return<- dailyReturn(tw50)
drtw_50<-as.numeric(drtw_50)
k <- 200
n_days <- nrow(tw2330)
length(drtw_50)
sel <- (n_days-k+1):n_days
drtw_50<-drtw_50[sel,]
drtw_50

## Read Data-------------------------------------------------------

tw50<-read.csv("C:/Users/user/Downloads/0050.csv",header=T,row.names=NULL,sep=",",dec=".")
dim(tw50)
tw50<-tw50[,5]
tw50<-as.numeric(tw50)
returntw50<-c()
for(i in 1:length(tw50)){
returntw50[i]<-(tw50[i+1]-tw50[i])/tw50[i]
}
tw50<-cbind(tw50,returntw50)
dim(tw50)
tw50<-tw50[1:200,]

tw2049<-read.csv("C:/Users/user/Downloads/2049.csv",header=T,row.names=NULL,sep=",",dec=".")
dim(tw2049)
tw2049<-tw2049[,5]
tw2049<-as.numeric(tw2049)
returntw2049<-c()
for(i in 1:length(tw2049)){
returntw2049[i]<-(tw2049[i+1]-tw2049[i])/tw2049[i]
}
tw2049<-cbind(tw2049,returntw2049)
tw2049<-tw2049[1:200,]

tw2383<-read.csv("C:/Users/user/Downloads/2383.csv",header=T,row.names=NULL,sep=",",dec=".")
dim(tw2383)
tw2383<-tw2383[,5]
tw2383<-as.numeric(tw2383)
returntw2383<-c()
for(i in 1:length(tw2383)){
returntw2383[i]<-(tw2383[i+1]-tw2383[i])/tw2383[i]
}
tw2383<-cbind(tw2383,returntw2383)
dim(tw2383)
tw2383<-tw2383[1:200,]

tw2439<-read.csv("C:/Users/user/Downloads/2439.csv",header=T,row.names=NULL,sep=",",dec=".")
dim(tw2439)
tw2439<-tw2439[,5]
tw2439<-as.numeric(tw2439)
returntw2439<-c()
for(i in 1:length(tw2439)){
returntw2439[i]<-(tw2439[i+1]-tw2439[i])/tw2439[i]
}
tw2439<-cbind(tw2439,returntw2439)
dim(tw2439)
tw2439<-tw2439[1:200,]

plot(tw50[,1],xaxt="n",ylab="TW50 stock price",type="o")
plot(tw50[,2],xaxt="n",ylab="TW50 return",type="o",ylim=c(-0.05,0.05))

plot(tw2049[,1],xaxt="n",ylab="TW2049 stock price",type="o")
plot(tw2049[,2],xaxt="n",ylab="TW50 return",type="o")

plot(tw2383[,1],xaxt="n",ylab="TW2383 stock price",type="o")
plot(tw2383[,2],xaxt="n",ylab="TW2383 return",type="o",ylim=c(-0.05,0.1))

plot(tw2439[,1],xaxt="n",ylab="TW2439 stock price",type="o")
plot(tw2439[,2],xaxt="n",ylab="TW2439 stock price",type="o")

##Fit Spline--------------------------------------

# cature the trend
k<-200
par(mfrow=c(1,1))
day <- 1:k
day <- day/k

cs <- smooth.spline(day, tw2049[,2]) # auto tuning by GCV
plot(1:k, fitted.values(cs), type="l", col=2,ylab="fitted.values",xlab="")
points(1:k, tw2049[,2])
a1<-tw2049[,2]-fitted.values(cs)

cs_df_2 <- smooth.spline(day,tw2383[,2])
plot(1:k, fitted.values(cs_df_2), type="l", col=2,ylab="fitted.values",xlab="")
points(1:k, tw2383[,2])
a2<-tw2383[,2]-fitted.values(cs_df_2)

cs_df_8 <- smooth.spline(day, tw2439[,2])
plot(1:k, fitted.values(cs_df_8), type="l", col=2,ylab="fitted.values",xlab="")
points(1:k, tw2439[,2])
a<-tw2439[,2]-fitted.values(cs_df_8)

residuals <- function(y, yhat) y-yhat
plot(1:k, residuals(tw2049[,2], fitted.values(cs)), type="l",ylab="residuals",xlab="")
sum(residuals(tw2049[,2], fitted.values(cs)))/k
plot(1:k, residuals(tw2383[,2], fitted.values(cs_df_2)), type="l",ylab="residuals",xlab="")
sum(residuals(tw2383[,2], fitted.values(cs_df_2)))/k
plot(1:k, residuals(tw2439[,2], fitted.values(cs_df_8)), type="l",ylab="residuals",xlab="")
sum(residuals(tw2439[,2], fitted.values(cs_df_8)))/k

##CALCULATE MEAN AND VARIANCE AFTER FITTING SPLINE-------------------------------------------------------

X<-cbind(tw50[,2],tw2049[,2],tw2383[,2],tw2439[,2]) #not removing spline's stock return rate matrix
X1<-cbind(tw50[,2],a1,a2,a) #after removing spline's stock return rate matrix

a1<-mean(tw50[,2])
a2<-sd(tw50[,2])
b1<-mean(tw2049[,2])
b2<-sd(tw2049[,2])
c1<-mean(tw2383[,2])
c2<-sd(tw2383[,2])
d1<-mean(tw2439[,2])
d2<-sd(tw2439[,2])

# mean /var. matrix
mean.sd<-matrix(c(a1,b1,c1,d1,a2,b2,c2,d2),4,2)
row.names(mean.sd)<-c("tw50","�W��","�x���q","����")
colnames(mean.sd)<-c("mean","sd")

mu <- apply(X, 2, mean)
S <- var(X1)
mu_f <- .0001	

##GET THE MINIMUM RISK AND TANGENCY PORTOFOLIO EFFICIENT FRONTIER PLOT--------------------------	
	
# calculate the efficient frontier
p <- length(mu)
O_vec <- rep(1,p) 

mu_vec <- seq(min(mu), max(mu), len=1001)
Sinv <- solve(S)

A <- c(t(mu)%*%Sinv%*%O_vec)
B <- c(t(mu)%*%Sinv%*%mu)
C <- c(t(O_vec)%*%Sinv%*%O_vec)
D <- c(B*C - A*A)

X1 <- Sinv%*%O_vec/D 
X2 <- Sinv%*%mu/D
g <- X1*B - X2*A
h <- X2*C - X1*A

len=1001

# wmup
omega_P <- t(mu_vec)%x%h + g%x%t(rep(1,len))
# var(wtR)
sigma_P <- sapply(1:len, function(i) sqrt(t(omega_P[,i])%*%S%*%omega_P[,i]))

# minimum risk portfolio
sel <- which(sigma_P == min(sigma_P))
omega_min_risk <- omega_P[,sel]
mu_min_risk <- t(mu)%*%omega_min_risk
s_min_risk <- sigma_P[sel]

v_min_risk <- c(mu_min_risk, s_min_risk, omega_min_risk)
names(v_min_risk) <- c("mu", "s", "var_name")

# tangency portfolio
tangent <- (mu_vec-mu_f)/sigma_P
sel <- which(tangent == max(tangent))
omega_tangency <- omega_P[,sel]
mu_tangency <- t(mu)%*%omega_tangency
s_tangency <- sigma_P[sel]

v_tangency_risk<- c(mu_tangency, s_tangency, omega_tangency)
names(v_tangency_risk) <- c("mu", "s","var_name")

##Optimal or efficient portofolio: tangency portfolio mixed with risk-free asset-------------------

	alpha<-matrix((mu_tangency-mu_f)/(mu-mu_f*O_vec)%*%Sinv%*%(mu-mu_f*O_vec))

	#new adjusted wmup for considering fixed asset (p13)
	omegafinal<-alpha%*%(t(O_vec)%*%(Sinv%*%(matrix(mu-mu_f))))*omega_tangency
	omegafinal<-as.vector(omegafinal)

	v_tangency_fixed<-c(omegafinal,1-sum(omegafinal)) #1-sum(omegafinal): percentage invested in fixed asset

	names(v_tangency_fixed)<-c("var_name")
	list(mu=mu, S=S, mu_f=mu_f, min_risk = v_min_risk, tangency =v_tangency_risk)	

##Plot of the efficient frontier-----------------------------------------------------------------------

	x <-sigma_P
	y <-mu_vec
	plot(c(0, x), c(0, y), type="n", xlab="Standard Deviation of Retrun", ylab="Expected Return")
	lines(x, y)

	#fixed risk/ min risk / tangency risk point on efficient frontier
	F <- c(0, mu_f)
	M <- c(v_min_risk[2], v_min_risk[1])
	T <- c(v_tangency_risk[2],v_tangency_risk[1])
	points(F[1], F[2], pch=19); text(F[1], F[2], "Fixed Risk", pos=4)
	points(M[1], M[2], pch=19); text(M[1], M[2], "Minimum Risk", pos=4)
	points(T[1], T[2], pch=19); text(T[1], T[2], "Tangency Risk", pos=2)

	lines(c(F[1], T[1]), c(F[2], T[2]), col=2, lty=2)

	X <- rbind(v_min_risk,v_tangency_risk)
	rownames(X) <- c("min_risk", "tangency")
	print(round(X, digit = 5))
	round(v_tangency_fixed,digit = 5)

##TEST CAPM ASSUMPTION FOR EACH STOCK-----------------------------------

market<-read.csv("C:/Users/user/Downloads/market.csv",row.names=NULL,sep=",",dec=".")
market<-market[,1]

returnmarket<-c()
for(i in 1:199){
returnmarket[i]<-(market[i+1]-market[i])/market[i]
}
returnmarket[200]<-1.414306e-03

market<-cbind(market,returnmarket)
plot(market[,1],xaxt="n",ylab="market stock price",type="o")
plot(market[,2],xaxt="n",ylab="market return",type="o")

r50<-tw50[,2]-0.0001
r2049<-a1-0.0001
r2383<-a2-0.0001
r2439<-a-0.0001
rmarket<-market[,2]-0.0001

capmr50<-lm(r50~rmarket)
summary(capmr50)

capmr2049<-lm(r2049~rmarket)
summary(capmr2049)

capmr2383<-lm(r2383~rmarket)
summary(capmr2383)

capmr2439<-lm(r2439~rmarket)
summary(capmr2439)

##CALCULATE VALUE AT RISK--------------------------------------------

qnorm(0.001)
a<-c(seq(0.001,0.02,by=0.002))

VAR<-c()
for(i in 1:10){
	VAR[i]<--(0.004538+qnorm(a[i])*0.0226)*100000000 #VAR (under normality asumption) : (mu+std*norminverse(a)) * S
}
VAR

plot(VAR,type="o",xaxt="n")
axis(1, at=seq(1,10), labels = a, lwd = 0.3,lty = 0.3)





