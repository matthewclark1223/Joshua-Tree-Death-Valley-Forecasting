##Load in Data and a variety of datasets
library(readr)
Data <- read_csv("~/ParkBreak/Seasonal.csv")
View(Data)
library(rethinking)
library(shiny)
library(rstanarm)
library(rstan)
library(MASS)
library(ggplot2)

##Create STAN Model
fcmod=stan_glm.nb(SeasVis~MedCCI+MedGas+GoogStag+Fall+Spring+Winter,data=Data)
summary(fcmod)


##Check out the posteriors
bayesplot::color_scheme_set("brightblue")
plot(fcmod, "areas",prob = 0.5, prob_outer = 0.9)

plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(fcmod, "hist") + plot_title

plot(fcmod, "dens_overlay", pars = "(Intercept)"
) + plot_title

##Evaluate the model using LOOCV
stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}
Data$stdcci<-stdize(Data$MedCCI)
Data$stdgas<-stdize(Data$MedGas)
Data$stdgoog<-stdize(Data$GoogStag)
Data$stdwint<-stdize(Data$Winter)
Data$stdfall<-stdize(Data$Fall)
Data$stdspring<-stdize(Data$Spring)

fcmod=stan_glm.nb(SeasVis~MedCCI+MedGas+GoogStag+Fall+Spring+Winter,data=Data)

fcmodstd=stan_glm.nb(SeasVis~stdcci+stdgas+stdgoog+stdfall+stdspring+stdwint,data=Data)


leftout<-rep(NA,times=length(Data$SeasVis))
for(i in 1:length(Data$SeasVis)){
  sub_dat<-Data[-i,]
  m_sub<-glm.nb(SeasVis~stdcci+stdgas+stdgoog+stdfall+stdspring+stdwint,data=sub_dat)
  leftout[i]=predict(m_sub,newdat=Data[i,]) 
}

R2 <- function (x, y) cor(x, y) ^ 2
R2mod<-R2(exp(leftout),Data$SeasVis)
library('Metrics')
rmsemod<-rmse(leftout,Data$SeasVis)
R2mod

rmsemod

plot(exp(leftout),Data$SeasVis)

#test posterior predict
nd<-data.frame(MedCCI=98,GoogStag=60,MedGas=3.56,Winter=0,Fall=1,Spring=0)
post <- posterior_predict(fcmod,nd,draws=4000)
plot(post)

#Test the glm on overnight predictions 
glmon=glm.nb(Overnight~stdcci+stdgas+stdgoog+stdfall+stdspring+stdwint,data=Data)

leftout2<-rep(NA,times=length(Data$Overnight))
for(i in 1:length(Data$Overnight)){
  sub_dat<-Data[-i,]
  m_sub<-glm.nb(Overnight~stdcci+stdgas+stdgoog+stdfall+stdspring+stdwint,data=sub_dat)
  leftout2[i]=predict(m_sub,newdat=Data[i,]) 
}

R2mod2<-R2(exp(leftout2),Data$Overnight)
rmsemod2<-rmse(leftout2,Data$Overnight)
R2mod2

rmsemod2

plot(exp(leftout2),Data$Overnight)

#do a second stan model

fcmodON=stan_glm.nb(Overnight~MedCCI+MedGas+GoogStag+Fall+Spring+Winter,data=Data)

fcmodstdON=stan_glm.nb(Overnight~stdcci+stdgas+stdgoog+stdfall+stdspring+stdwint,data=Data)

###############################################
par(mfrow=c(1,2))

plot(exp(leftout)~Data$Year,col="red", pch=2, ylab="Visitation",xlab="Year",
     main="Predicted vs Observed Visitation by Season")
legend("topleft", pch=c(2,1), col=c("red", "blue"), c("Predicted", "Observed"))
points(SeasVis~Year,data=Data, pch=1, col="blue")

plot(leftout,log(Data$SeasVis),main="Leave-One-Out Cross Validation: Predicted vs Observed",
     xlab="Log of Predicted Visitation",ylab="Log of Observed Visitation")
abline(lm(leftout~log(Data$SeasVis)),col="red")
legend("topleft", bty="n", legend=paste("R2 is", format(R2mod, digits=4)))
                                                     