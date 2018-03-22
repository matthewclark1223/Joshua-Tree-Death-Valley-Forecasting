## The following code is for running a negbinomial glm on the park 
## visitation data with a various combinations of variables
## and using leave one out cross validation (LOOCV)
## for model selection AND to assess model fit

## This is for JOTR only and looks at total yearly visitation. This is 
## intended to be used as a framework for later analysis of DEVA
## and JOTR visitation stratified by season and with social media data added

## Import the simplified dataset

library(readr)
scaff <- read_csv("~/ParkBreak/Scaffold data - Sheet1.csv")
View(scaff)

## There are some NA's in there, lets remove those
scaff<-na.omit(scaff)

## For this framework, lets look at 5 variables
## CCI, LA POP, Cochella Google searches,
## gas prices, and burning man attendance

## We need to standardize these variables 
## so they are all on the same scale and the slopes are comparable 
## Create a function for standardizing variables
stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}

scaff$stdcci<-stdize(scaff$CCI_USA)
scaff$stdlapop<-stdize(scaff$LA_pop)
scaff$stdcochella<-stdize(scaff$GoogleSearch_Coachella)
scaff$stdgas<-stdize(scaff$GasPrices)
scaff$stdburningman<-stdize(scaff$BurningMan)

## Now all our parameter estimetes will be comparable to each other
## Lets make our model using the different paramater combinations

library(MASS)
## M1 we will use all the variables
M1<-glm.nb(JOTR_all~stdcci+stdlapop+stdcochella+stdgas+stdburningman, data=scaff)
## M2 let's remove the google search variables
M2<-glm.nb(JOTR_all~stdcci+stdlapop+stdgas, data=scaff)
## M3 lets use ONLY the google search terms
M3<-glm.nb(JOTR_all~stdcochella+stdburningman, data=scaff)
## M4 lets use ONLY the economic variables
M4<-glm.nb(JOTR_all~stdcci+stdgas, data=scaff)

## Let's use LOOCV to evaluate each of the models
## First create an empty vector to fill with using a loop
leftout<-rep(NA,times=length(scaff$JOTR_all))
## Now let's fill it by removing one row at a time and
## running our model on the other datapoints, trying to predct
## the row we removed

for(i in 1:length(scaff$JOTR_all)){
  sub_dat<-scaff[-i,]
  m_sub<-glm.nb(JOTR_all~stdcci+stdlapop+stdcochella+stdgas+stdburningman, data=sub_dat)
  leftout[i]=predict(m_sub,newdat=scaff[i,]) 
}

## The test statistics for LOOCV are RMSE and R^2
library('Metrics')
## Make an R^2 function
R2 <- function (x, y) cor(x, y) ^ 2
R2M1<-R2(leftout,scaff$JOTR_all)
rmseM1<-rmse(leftout,scaff$JOTR_all)

## Now that we have successfully dont this, let's 
## do it for the rest of the variables and compare them

## M2 
leftout2<-rep(NA,times=length(scaff$JOTR_all))

for(i in 1:length(scaff$JOTR_all)){
  sub_dat2<-scaff[-i,]
  m_sub2<-glm.nb(JOTR_all~stdcci+stdlapop+stdgas, data=sub_dat2)
  leftout2[i]=predict(m_sub2,newdat=scaff[i,]) 
}

R2M2<-R2(leftout2,scaff$JOTR_all)
rmseM2<-rmse(leftout2,scaff$JOTR_all)

## M3
leftout3<-rep(NA,times=length(scaff$JOTR_all))

for(i in 1:length(scaff$JOTR_all)){
  sub_dat3<-scaff[-i,]
  m_sub3<-glm.nb(JOTR_all~stdcochella+stdburningman, data=sub_dat3)
  leftout3[i]=predict(m_sub3,newdat=scaff[i,]) 
}

R2M3<-R2(leftout3,scaff$JOTR_all)
rmseM3<-rmse(leftout3,scaff$JOTR_all)

## M4
leftout4<-rep(NA,times=length(scaff$JOTR_all))

for(i in 1:length(scaff$JOTR_all)){
  sub_dat4<-scaff[-i,]
  m_sub4<-glm.nb(JOTR_all~stdcci+stdgas, data=sub_dat4)
  leftout4[i]=predict(m_sub4,newdat=scaff[i,]) 
}

R2M4<-R2(leftout4,scaff$JOTR_all)
rmseM4<-rmse(leftout4,scaff$JOTR_all)

## Create a table to compare values
 modcomp <- matrix(c(rmseM1,R2M1,rmseM2,R2M2,rmseM3,R2M3,rmseM4,R2M4),ncol=2,byrow=TRUE)
 colnames(modcomp) <- c("Root Mean Square Error","R^2")
 rownames(modcomp) <- c("M1","M2","M3","M4")
 modcomp <- as.table(modcomp)
 modcomp

 ## Lets do one more looking at the
 ## most significant variables
 M5<-glm.nb(JOTR_all~stdcci+stdcochella+stdgas, data=scaff)
 
 leftout5<-rep(NA,times=length(scaff$JOTR_all))
 
 for(i in 1:length(scaff$JOTR_all)){
   sub_dat5<-scaff[-i,]
   m_sub5<-glm.nb(JOTR_all~stdcci+stdcochella+stdgas, data=sub_dat5)
   leftout5[i]=predict(m_sub5,newdat=scaff[i,]) 
 }
 
 R2M5<-R2(leftout5,scaff$JOTR_all)
 rmseM5<-rmse(leftout5,scaff$JOTR_all)

 modcomp <- matrix(c(rmseM1,R2M1,rmseM2,R2M2,rmseM3,R2M3,rmseM4,R2M4,rmseM5,R2M5),ncol=2,byrow=TRUE)
 colnames(modcomp) <- c("Root Mean Square Error","R^2")
 rownames(modcomp) <- c("M1","M2","M3","M4","M5")
 modcomp <- as.table(modcomp)
 modcomp
