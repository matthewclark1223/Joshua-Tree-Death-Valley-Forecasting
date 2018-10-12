Parks<-read_csv("~/ParkBreak/Block cross model-code-data/Parks_Monthly - Sheet1 (3).csv")
Parks$Year<-as.numeric(format(Parks$Date,"%Y"))
df<-Parks %>%
  group_by(Park,Year) %>%
  summarize(Visitation=sum(Visitation),Google=sum(Google),
            VisLag1=sum(VisLag1),VisLag2=sum(VisLag2),Lag12G=sum(Lag12G))
df$Year<-as.numeric(df$Year)


#Create seperate data frames for each year. We will use these as our inout for Vis 
# prediction later and to get the number of rows for out container data frames later
Dat2013<-df[df$Year==2013,]      
Dat2014<-df[df$Year==2014,]   
Dat2015<-df[df$Year==2015,]      
Dat2016<-df[df$Year==2016,] 
Dat2017<-df[df$Year==2017,] 

#Create Models
options(mc.cores=4)

#Google First
GLMall<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df)
GLM2013<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df[df$Year != 2013:2017,])
GLM2014<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df[df$Year != 2014:2017,])
GLM2015<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df[df$Year != 2015:2017,])
GLM2016<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df[df$Year != 2016:2017,])
GLM2017<-stan_glmer.nb(Visitation~Lag12G+(Lag12G|Park),chains=8,data=df[df$Year != 2017,])
save(GLMall,file= "GLMall.rda")
save(GLM2013,file= "GLM2013.rda")
save(GLM2014,file= "GLM2014.rda")
save(GLM2015,file= "GLM2015.rda")
save(GLM2016,file= "GLM2016.rda")
save(GLM2017,file= "GLM2017.rda")

#Create containers for the results for each year
GLMRes2013<-data.frame(matrix(nrow=nrow(df[df$Year == 2013,]),ncol=8))
GLMRes2014<-data.frame(matrix(nrow=nrow(df[df$Year == 2014,]),ncol=8))
GLMRes2015<-data.frame(matrix(nrow=nrow(df[df$Year == 2015,]),ncol=8))
GLMRes2016<-data.frame(matrix(nrow=nrow(df[df$Year == 2016,]),ncol=8))
GLMRes2017<-data.frame(matrix(nrow=nrow(df[df$Year == 2017,]),ncol=8))

#Name the collumns in the containers
colnames(GLMRes2013) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(GLMRes2014) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(GLMRes2015) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(GLMRes2016) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(GLMRes2017) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")


load(file="./GLM2013.rda")
load(file="./GLM2014.rda")
load(file="./GLM2015.rda")
load(file="./GLM2016.rda")
load(file="./GLM2017.rda")
GLMpost2013=posterior_predict(GLM2013,Dat2013, draws=2000) 
for(i in 1:ncol(GLMpost2013)){
  GLMRes2013[i,1]=quantile(GLMpost2013[,i],0.5)
  GLMRes2013[i,2]=quantile(GLMpost2013[,i],0.375)
  GLMRes2013[i,3]=quantile(GLMpost2013[,i],0.25)
  GLMRes2013[i,4]=quantile(GLMpost2013[,i],0.625)
  GLMRes2013[i,5]=quantile(GLMpost2013[,i],0.75)
  GLMRes2013[i,6]=Dat2013[i,]$Visitation
  GLMRes2013[i,7]=Dat2013[i,]$Year
  GLMRes2013[i,8]=Dat2013[i,]$Park}


GLMpost2014=posterior_predict(GLM2014,Dat2014, draws=3000) 
for(i in 1:ncol(GLMpost2014)){
  GLMRes2014[i,1]=quantile(GLMpost2014[,i],0.5)
  GLMRes2014[i,2]=quantile(GLMpost2014[,i],0.375)
  GLMRes2014[i,3]=quantile(GLMpost2014[,i],0.25)
  GLMRes2014[i,4]=quantile(GLMpost2014[,i],0.625)
  GLMRes2014[i,5]=quantile(GLMpost2014[,i],0.75)
  GLMRes2014[i,6]=Dat2014[i,]$Visitation
  GLMRes2014[i,7]=Dat2014[i,]$Year
  GLMRes2014[i,8]=Dat2014[i,]$Park}

GLMpost2015=posterior_predict(GLM2015,Dat2015, draws=2000) 
for(i in 1:ncol(GLMpost2015)){
  GLMRes2015[i,1]=quantile(GLMpost2015[,i],0.5)
  GLMRes2015[i,2]=quantile(GLMpost2015[,i],0.375)
  GLMRes2015[i,3]=quantile(GLMpost2015[,i],0.25)
  GLMRes2015[i,4]=quantile(GLMpost2015[,i],0.625)
  GLMRes2015[i,5]=quantile(GLMpost2015[,i],0.75)
  GLMRes2015[i,6]=Dat2015[i,]$Visitation
  GLMRes2015[i,7]=Dat2015[i,]$Year
  GLMRes2015[i,8]=Dat2015[i,]$Park}

GLMpost2016=posterior_predict(GLM2016,Dat2016, draws=2000) 
for(i in 1:ncol(GLMpost2016)){
  GLMRes2016[i,1]=quantile(GLMpost2016[,i],0.5)
  GLMRes2016[i,2]=quantile(GLMpost2016[,i],0.375)
  GLMRes2016[i,3]=quantile(GLMpost2016[,i],0.25)
  GLMRes2016[i,4]=quantile(GLMpost2016[,i],0.625)
  GLMRes2016[i,5]=quantile(GLMpost2016[,i],0.75)
  GLMRes2016[i,6]=Dat2016[i,]$Visitation
  GLMRes2016[i,7]=Dat2016[i,]$Year
  GLMRes2016[i,8]=Dat2016[i,]$Park}

GLMpost2017=posterior_predict(GLM2017,Dat2017, draws=2000) 
for(i in 1:ncol(GLMpost2017)){
  GLMRes2017[i,1]=quantile(GLMpost2017[,i],0.5)
  GLMRes2017[i,2]=quantile(GLMpost2017[,i],0.375)
  GLMRes2017[i,3]=quantile(GLMpost2017[,i],0.25)
  GLMRes2017[i,4]=quantile(GLMpost2017[,i],0.625)
  GLMRes2017[i,5]=quantile(GLMpost2017[,i],0.75)
  GLMRes2017[i,6]=Dat2017[i,]$Visitation
  GLMRes2017[i,7]=Dat2017[i,]$Year
  GLMRes2017[i,8]=Dat2017[i,]$Park}

Full_GL_Results<-rbind(GLMRes2013,GLMRes2014,GLMRes2015,GLMRes2016,GLMRes2017)
write.csv(Full_GL_Results, file = "Full_GL_Results.csv")

####Last one 5 year autoregressive####
Parks5y<-read_csv("~/ParkBreak/Yearly Visitation Data - Sheet1.csv")
df=Parks5y[Parks5y$Park != "NPSA",]
options(mc.cores=4)
#Lets do the Autoregressive alone first
AR5ALL<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df)
AR52013<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df[df$Year != 2013:2017,])
AR52014<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df[df$Year != 2014:2017,])
AR52015<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df[df$Year != 2015:2017,])
AR52016<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df[df$Year != 2016:2017,])
AR52017<-stan_glmer.nb(Visitation~VisLag1+VisLag2+VisLag3+VisLag4+VisLag5+(1|Park),chains=4,QR=TRUE,data=df[df$Year != 2017,])

#Save the models so we don't have to wait for them to run again
save(AR5ALL,file= "AR5ALL.rda")
save(AR52013,file= "AR52013.rda")
save(AR52014,file= "AR52014.rda")
save(AR52015,file= "AR52015.rda")
save(AR52016,file= "AR52016.rda")
save(AR52017,file= "AR52017.rda")
#------Load if not already-------
load(file="./AR5ALL.rda")
load(file="./AR52013.rda")
load(file="./AR52014.rda")
load(file="./AR52015.rda")
load(file="./AR52016.rda")
load(file="./AR52017.rda")
#Create seperate data frames for each year. We will use these as our inout for Vis 
# prediction later and to get the number of rows for out container data frames later
Dat2013<-df[df$Year==2013,]      
Dat2014<-df[df$Year==2014,]   
Dat2015<-df[df$Year==2015,]      
Dat2016<-df[df$Year==2016,] 
Dat2017<-df[df$Year==2017,] 

#Create containers for the results for each year
AR5Res2013<-data.frame(matrix(nrow=nrow(df[df$Year == 2013,]),ncol=8))
AR5Res2014<-data.frame(matrix(nrow=nrow(df[df$Year == 2014,]),ncol=8))
AR5Res2015<-data.frame(matrix(nrow=nrow(df[df$Year == 2015,]),ncol=8))
AR5Res2016<-data.frame(matrix(nrow=nrow(df[df$Year == 2016,]),ncol=8))
AR5Res2017<-data.frame(matrix(nrow=nrow(df[df$Year == 2017,]),ncol=8))

#Name the collumns in the containers
colnames(AR5Res2013) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(AR5Res2014) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(AR5Res2015) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(AR5Res2016) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
colnames(AR5Res2017) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")

#Predict the year we left out based on data from previous years. 
#Fill the containers with our results and some basic data from the original df
AR5post2013=posterior_predict(AR52013,Dat2013, draws=3000) 
for(i in 1:ncol(AR5post2013)){
  AR5Res2013[i,1]=quantile(AR5post2013[,i],0.5)
  AR5Res2013[i,2]=quantile(AR5post2013[,i],0.375)
  AR5Res2013[i,3]=quantile(AR5post2013[,i],0.25)
  AR5Res2013[i,4]=quantile(AR5post2013[,i],0.625)
  AR5Res2013[i,5]=quantile(AR5post2013[,i],0.75)
  AR5Res2013[i,6]=Dat2013[i,]$Visitation
  AR5Res2013[i,7]=Dat2013[i,]$Year
  AR5Res2013[i,8]=Dat2013[i,]$Park}


AR5post2014=posterior_predict(AR52014,Dat2014, draws=3000) 
for(i in 1:ncol(AR5post2014)){
  AR5Res2014[i,1]=quantile(AR5post2014[,i],0.5)
  AR5Res2014[i,2]=quantile(AR5post2014[,i],0.375)
  AR5Res2014[i,3]=quantile(AR5post2014[,i],0.25)
  AR5Res2014[i,4]=quantile(AR5post2014[,i],0.625)
  AR5Res2014[i,5]=quantile(AR5post2014[,i],0.75)
  AR5Res2014[i,6]=Dat2014[i,]$Visitation
  AR5Res2014[i,7]=Dat2014[i,]$Year
  AR5Res2014[i,8]=Dat2014[i,]$Park}

AR5post2015=posterior_predict(AR52015,Dat2015, draws=3000) 
for(i in 1:ncol(AR5post2015)){
  AR5Res2015[i,1]=quantile(AR5post2015[,i],0.5)
  AR5Res2015[i,2]=quantile(AR5post2015[,i],0.375)
  AR5Res2015[i,3]=quantile(AR5post2015[,i],0.25)
  AR5Res2015[i,4]=quantile(AR5post2015[,i],0.625)
  AR5Res2015[i,5]=quantile(AR5post2015[,i],0.75)
  AR5Res2015[i,6]=Dat2015[i,]$Visitation
  AR5Res2015[i,7]=Dat2015[i,]$Year
  AR5Res2015[i,8]=Dat2015[i,]$Park}

AR5post2016=posterior_predict(AR52016,Dat2016, draws=3000) 
for(i in 1:ncol(AR5post2016)){
  AR5Res2016[i,1]=quantile(AR5post2016[,i],0.5)
  AR5Res2016[i,2]=quantile(AR5post2016[,i],0.375)
  AR5Res2016[i,3]=quantile(AR5post2016[,i],0.25)
  AR5Res2016[i,4]=quantile(AR5post2016[,i],0.625)
  AR5Res2016[i,5]=quantile(AR5post2016[,i],0.75)
  AR5Res2016[i,6]=Dat2016[i,]$Visitation
  AR5Res2016[i,7]=Dat2016[i,]$Year
  AR5Res2016[i,8]=Dat2016[i,]$Park}

AR5post2017=posterior_predict(AR52017,Dat2017, draws=3000) 
for(i in 1:ncol(AR5post2017)){
  AR5Res2017[i,1]=quantile(AR5post2017[,i],0.5)
  AR5Res2017[i,2]=quantile(AR5post2017[,i],0.375)
  AR5Res2017[i,3]=quantile(AR5post2017[,i],0.25)
  AR5Res2017[i,4]=quantile(AR5post2017[,i],0.625)
  AR5Res2017[i,5]=quantile(AR5post2017[,i],0.75)
  AR5Res2017[i,6]=Dat2017[i,]$Visitation
  AR5Res2017[i,7]=Dat2017[i,]$Year
  AR5Res2017[i,8]=Dat2017[i,]$Park}

#combine all the results into one df
Full_AR5_Results<-rbind(AR5Res2013,AR5Res2014,AR5Res2015,AR5Res2016,AR5Res2017)
write.csv(Full_AR5_Results, file = "Full_AR5_Results.csv")


#Now let's compare some overall metrics
R2 <- function (x, y) cor(x, y) ^ 2
library(Metrics)

GL_errors<- Full_GL_Results %>% 
  group_by(Park)%>%
  filter(Park != "NPSA")%>%
  summarize(R2GL=R2(Median,Visitation),
            RMSEGL=rmse(Median,Visitation),
            MAEGL=mae(Median,Visitation))


AR_errors<- Full_AR5_Results %>% 
  group_by(Park)%>%
  summarize(R2AR=R2(Median,Visitation),
            RMSEAR=rmse(Median,Visitation),
            MAEAR=mae(Median,Visitation))

AR_errors$Park<-NULL
Error_Metrics<-cbind(GL_errors,AR_errors)


