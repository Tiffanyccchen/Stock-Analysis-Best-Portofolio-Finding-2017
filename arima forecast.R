library(forecast)

## prepare data
setwd("~/dropbox/consulting/Posho/forever78920/")
A_Train <- read.csv("BD_Epi2000to2009_forC.csv",header=TRUE)
A_Test <- read.csv("BD_Epi2010to2011_forC.csv",header=TRUE)
A <- rbind(A_Train, A_Test)

epi <- 0
YM <- substr(A[,"dd_calendar_date"], start=1, stop=6)
YM_group <- rownames(as.matrix(table(YM)))

n <- length(YM_group)
Data <- matrix(0, n*9, 6)
count <- 1
for(i_time in 1:n){
	sel_time <- YM == YM_group[i_time]
	flag_time <- ifelse(sel_time, 1, 0)
	temperature <- A[sel_time,"mean_TX01"]; temperature <- temperature[1]	
	for(i_episode in 0:2){
		flag_episode <- ifelse(A[,"episodes_type"] == i_episode, 1, 0)
		for(i_type in 1:3){
			flag_type <- ifelse(A[,"group"] == i_type, 1, 0)
			sel <- which(flag_time + flag_episode + flag_type == 3)
			response <- ifelse(is.na(length(sel)), 0, length(sel))
			Data[count,] <- c(response, i_episode, i_type, YM_group[i_time], temperature, i_time)	
			count <- count + 1
		}
	}	
}

colnames(Data) <- c("counts", "epi_type", "group", "YM", "temp", "time")
Data <- data.frame(Data)
year <- substr(Data[,"YM"], start=1, stop=4)
month <- substr(Data[,"YM"], start=5, stop=6)
Data2 <- Data
Data <- data.frame(counts=as.numeric(as.character(Data$counts)), 
	epi_type=factor(Data$epi_type), 
	group=factor(Data$group), YM=Data$YM, 
	year=year, 
	month=as.numeric(month),
	temp=as.numeric(as.character(Data$temp)), 
	time=as.numeric(as.character(Data$time)))

# if 2004.1~2012.12 are omitted		
#K_G2_E0 <- Data[Data$time > 36 & Data$group == 2 & Data$epi_type == 0,]
#K_G3_E0 <- Data[Data$time > 36 & Data$group == 3 & Data$epi_type == 0,]
#K_G3_E1 <- Data[Data$time > 36 & Data$group == 3 & Data$epi_type == 1,]
#K_G3_E2 <- Data[Data$time > 36 & Data$group == 3 & Data$epi_type == 2,]
#seq_train <- 1:96 # first 8 years
#seq_test <- 97:108 # last 1 year

# if 2001.1~2012.12 are used
K_G2_E0 <- Data[Data$group == 2 & Data$epi_type == 0,]
K_G3_E0 <- Data[Data$group == 3 & Data$epi_type == 0,]
K_G3_E1 <- Data[Data$group == 3 & Data$epi_type == 1,]
K_G3_E2 <- Data[Data$group == 3 & Data$epi_type == 2,]
seq_train <- 1:132 # first 11 years
seq_test <- 132:144 # last 1 year

d_Counts <- cbind(K_G2_E0[,"counts"], K_G3_E0[,"counts"], K_G3_E1[,"counts"], K_G3_E2[,"counts"])



###############################
# The process begin here.
# Step 0: choose the group 
# Step 1: try the order of arima(p,d,q)
# Step 2: use the result of Step 1 to check whether season is important
# Step 3: determine the final model and get forecasts

## Setup 0
group_index <- 2

if(group_index == 1) M <- K_G2_E0
if(group_index == 2) M <- K_G3_E0
if(group_index == 3) M <- K_G3_E1
if(group_index == 4) M <- K_G3_E2
y <- ts(M[,"counts"], start=c(2000,1), frequency=12)

m2 <- as.numeric(M[,"month"])
ss <- rep("Winter", length(m2))
ss <- ifelse(m2 >= 3 & m2 <= 5, "Spring", ss)
ss <- ifelse(m2 >= 6 & m2 <= 8, "Summer", ss)
ss <- ifelse(m2 >= 9 & m2 <= 11, "Fall",ss)

X <- model.matrix(~-1+I(ss)); X <- X[,-1] # fall is the baseline
colnames(X) <- c("Spring", "Summer", "Winter")
#X <- cbind(X, temperature, age)

## Step 1: read the output to determine the order of ARIMA
## choices:
##  1. order=c(p,d,q)
##  2. xreg=X
auto.arima(y[1:108], max.p=3, max.d=2, max.q=3, start.p=0, start.q=0, ic="aic")



## Step 2: check whether season is important
## regression with correlated error
summary(Arima(y, order=c(2,1,0), include.mean=TRUE, xreg=X)) 
## Please trial and error to choose best order and to determine whethere X is needed.
## Then Answer the following question



## Step 3: determine the model and get forecasts
## Which order do you prefer?
## order_vector <- c(p,d,q) , p = order of ar, d = order of diff, q = order of ma
order_vector <- c(2,1,0)
## Do you want to use season as predictor?
## opt = 1: yes
## opt = 0: no
opt <- 1

if(opt == 1){
	out <- Arima(y[seq_train], order=order_vector, xreg=X[seq_train,])
	summary(out)
	plot(forecast(out, xreg=X[seq_test,], h=12))
	points(seq_test, y[seq_test], pch="*")
}else{
	out <- Arima(y[seq_train], order=order_vector)	
	summary(out)
	plot(forecast(out, h=12))
	points(seq_test, y[seq_test], pch="*")
}




