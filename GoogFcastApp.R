library(shiny)
library(shinythemes)
library(rstan)
library(rstanarm)
library(rethinking)
library(readr)
options(scipen=5)
Data <- read_csv("./Seasonal.csv")
Data<-na.omit(Data)   
fcmod=stan_glm.nb(SeasVis~CCIStag+GoogStag+Fall+Spring+Winter+Summer,data=Data)
fcmodON=stan_glm.nb(Overnight~CCIStag+GoogStag+Fall+Spring+Winter+Summer,data=Data)

ui <- fluidPage(
  
  titlePanel("Visitor Use Forecasting Tool"),
  theme=shinytheme("flatly"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "season",label= "The NEXT upcoming season is?",choices=c("Winter (Dec-Feb)","Spring (March-May)",
                                                                                      "Summer (June-Aug)","Fall (Sept-Nov)")),
      selectInput(inputId = "Type", label = "Which Type of Visitation are You Predicting For?",choices=c("Total Seasonal Visitation","Overnight Seasonal Visitation")),
      sliderInput(inputId ="ConsumerConfidenceIndex", label = "CURRENT Consumer Confidence Index", min = 80, max = 120,step=1, value = 99),
      sliderInput(inputId ="GoogStag", label = " What is the CURRENT Google Search Term Index for JOTR", min = 1, step=1,max = 100, value = 50)
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot",plotOutput(outputId = "myplot", width = "100%", height = "600px"), br(),
                           p("This plot shows the Bayesian posterior predictions of Joshua Tree National Park visitation 
                             based on an rstanarm model fit to an negative binomial distribution.
                             This model estimates expected visitation based on the slider inputs 4000 times and displays these 
                             estimates on the X-axis. The Y-axis represents the probability of any one of the 4000 estimates." ,style = "font-family: 'times'; font-si24pt"),
                           strong("Note: This tool accurately predicts visitation ONE season ahead. All inputs are for the 
                                  current season and outputs represent the expected visitation for the following season only. ")),
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Contact", h3("Contact information for the author of this application"),
                           br(),
                           p("This visitor use forecasting tool and the Bayesian model that is used to produce the predictions
                             were constructed by Matt Clark, please direct any questions to  Matthewclark989@u.boisestate.edu "),
                           p("Special thanks to Charlie Becker Charliebecker@u.boisestate.edu for help with RShiny app development"))
                  )
      
                           )
      )
  )




server <- function(input, output) {
  output$myplot <- renderPlot({
    n1 <- 5 
    n2<-95
    color=rgb(0,0,0,alpha=0.3)
    mod1<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GoogStag=input$GoogStag,
                       Winter=1,Spring=0,Fall=0,Summer=0),draws=4000))
    
    h1 = hist(mod1,breaks=50,plot=FALSE) 
    h1$density = h1$counts/sum(h1$counts)*100
    
    mod1ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GoogStag=input$GoogStag,
                         Winter=1,Spring=0,Fall=0,Summer=0),draws=4000))
    
    h1ON = hist(mod1ON,breaks=50,plot=FALSE) 
    h1ON$density = h1ON$counts/sum(h1ON$counts)*100
    
    mod2<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GoogStag=input$GoogStag,
                       Winter=0,Spring=1,Fall=0,Summer=0),draws=4000))
    
    h2 = hist(mod2,breaks=50,plot=FALSE) 
    h2$density = h2$counts/sum(h2$counts)*100
    
    mod2ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GoogStag=input$GoogStag,
                         Winter=0,Spring=1,Fall=0,Summer=0),draws=4000))
    
    h2ON = hist(mod2ON,breaks=50,plot=FALSE) 
    h2ON$density = h2ON$counts/sum(h2ON$counts)*100
    
    mod3<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GoogStag=input$GoogStag,
                       Winter=0,Spring=0,Fall=1,Summer=0),draws=4000))
    
    h3 = hist(mod3,breaks=50,plot=FALSE) 
    h3$density = h3$counts/sum(h3$counts)*100
    
    mod3ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GoogStag=input$GoogStag,
                         Winter=0,Spring=0,Fall=1,Summer=0),draws=4000))
    
    h3ON = hist(mod3ON,breaks=50,plot=FALSE) 
    h3ON$density = h3ON$counts/sum(h3ON$counts)*100
    
    mod4<-posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GoogStag=input$GoogStag,
                       Winter=0,Spring=0,Fall=0,Summer=1),draws=4000)
    
    h4 = hist(mod4,breaks=50,plot=FALSE) 
    h4$density = h4$counts/sum(h4$counts)*100
    
    mod4ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GoogStag=input$GoogStag,
                         Winter=0,Spring=0,Fall=0,Summer=1),draws=4000))
    
    h4ON = hist(mod4ON,breaks=50,plot=FALSE) 
    h4ON$density = h4ON$counts/sum(h4ON$counts)*100
    
    med1<-round(median(mod1),2)
    med1ON<-round(median(mod1ON),2)
    med2<-round(median(mod2),2)
    med2ON<-round(median(mod2ON),2)
    med3<-round(median(mod3),2)
    med3ON<-round(median(mod3ON),2)
    med4<-round(median(mod4),2)
    med4ON<-round(median(mod4ON),2)
    
    if (input$season == "Winter (Dec-Feb)") {
      if (input$Type == "Total Seasonal Visitation") {
        plot(h1, main=paste("Median Visitation Estimate = ", format(med1,big.mark = ",")),
             xlab="Number of Total Visitors", xlim=c(5000, 2000000),ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod1),
               col = "black",
               lwd = 3)
        
        upbound1<-min(mod1[mod1 > quantile(mod1,prob=1-n1/100),])
        
        abline(v=upbound1,col=color,lty=2,lwd=3)
        
        lwrbound1<-max(mod1[mod1<quantile(mod1,prob=1-n2/100),])
        abline(v=lwrbound1,col=color,lty=2,lwd=3)
        
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        plot(h1ON, main=paste("Median Visitation Estimate = ",  format(med1ON,big.mark = ",")),
             xlab="Number of Overnight Visitors", xlim=c(5000, 200000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod1ON),
               col = "red",
               lwd = 5)
        
        upbound1ON<-min(mod1ON[mod1ON > quantile(mod1ON,prob=1-n1/100),])
        abline(v=upbound1ON,col=color,lty=2,lwd=3)
        
        lwrbound1ON<-max(mod1ON[mod1ON<quantile(mod1ON,prob=1-n2/100),])
        abline(v=lwrbound1ON,col=color,lty=2,lwd=3)
        
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound1ON,big.mark=","), "-" ,format(upbound1ON,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}}
    
    if (input$season == "Spring (March-May)") {
      if (input$Type == "Total Seasonal Visitation") {
        plot(h2,main=paste("Median Visitation Estimate = ",  format(med2,big.mark = ",")),
             xlab="Number of Total Visitors",xlim=c(5000, 2000000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod2),
               col = "black",
               lwd = 5)
        
        upbound2<-min(mod2[mod2 > quantile(mod2,prob=1-n1/100),])
        abline(v=upbound2,col=color,lty=2,lwd=3)
        
        lwrbound2<-max(mod2[mod2<quantile(mod2,prob=1-n2/100),])
        abline(v=lwrbound2,col=color,lty=2,lwd=3)
        
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound2,big.mark=","), "-" ,format(upbound2,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        plot(h2ON, main=paste("Median Visitation Estimate = ",   format(med2ON,big.mark = ",")),
             xlab="Number of Overnight Visitors", xlim=c(5000, 200000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod2ON),
               col = "black",
               lwd = 5)
        upbound2ON<-min(mod2ON[mod2ON > quantile(mod2ON,prob=1-n1/100),])
        abline(v=upbound2ON,col=color,lty=2,lwd=3)
        
        lwrbound2ON<-max(mod2ON[mod2ON<quantile(mod2ON,prob=1-n2/100),])
        abline(v=lwrbound2ON,col=color,lty=2,lwd=3)
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound2ON,big.mark=","), "-" ,format(upbound2ON,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}}
    
    
    if (input$season == "Fall (Sept-Nov)") {
      if (input$Type == "Total Seasonal Visitation") {
        plot(h3,main=paste("Median Visitation Estimate = ",  format(med3,big.mark = ",")),
             xlab="Number of Total Visitors", xlim=c(5000, 2000000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod3),
               col = "black",
               lwd = 5)
        upbound3<-min(mod3[mod3 > quantile(mod3,prob=1-n1/100),])
        abline(v=upbound3,col=color,lty=2,lwd=3)
        
        lwrbound3<-max(mod3[mod3<quantile(mod3,prob=1-n2/100),])
        abline(v=lwrbound3,col=color,lty=2,lwd=3)
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound3,big.mark=","), "-" ,format(upbound3,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        plot(h3ON, main=paste("Median Visitation Estimate = ",   format(med3ON,big.mark = ",")),
             xlab="Number of Overnight Visitors", xlim=c(5000, 200000), ylab="Probability",col="deepskyblue",freq=FALSE)
        abline(v = median(mod3ON),
               col = "black",
               lwd = 5)
        upbound3ON<-min(mod3ON[mod3ON > quantile(mod3ON,prob=1-n1/100),])
        abline(v=upbound3ON,col=color,lty=2,lwd=3)
        
        lwrbound3ON<-max(mod3ON[mod3ON<quantile(mod3ON,prob=1-n2/100),])
        abline(v=lwrbound3ON,col=color,lty=2,lwd=3)
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound3ON,big.mark=","), "-" ,format(upbound3ON,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}}
    
    else if (input$season == "Summer (June-Aug)") {
      if (input$Type == "Total Seasonal Visitation") {
        plot(h4, main=paste("Median Visitation Estimate = ",   format(med4,big.mark = ",")),
             xlab="Number of Total Visitors", xlim=c(5000, 2000000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod4),
               col = "black",
               lwd = 5)
        upbound4<-min(mod4[mod4 > quantile(mod4,prob=1-n1/100),])
        abline(v=upbound4,col=color,lty=2,lwd=3)
        
        lwrbound4<-max(mod4[mod4<quantile(mod4,prob=1-n2/100),])
        abline(v=lwrbound4,col=color,lty=2,lwd=3)
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound4,big.mark=","), "-" ,format(upbound4,big.mark=","))),
               col =  c("black",color),lty=c(1,2),
               lwd = c(2,2))}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        plot(h4ON, main=paste("Median Visitation Estimate = ",format(med4ON,big.mark = ",")),
             xlab="Number of Overnight Visitors", xlim=c(5000, 200000), ylab="Percentage of Samples",col="deepskyblue",freq=FALSE)
        abline(v = median(mod4ON),
               col = "black",
               lwd = 5)
        upbound4ON<-min(mod4ON[mod4ON > quantile(mod4ON,prob=1-n1/100),])
        abline(v=upbound4ON,col=color,lty=2,lwd=3)
        
        lwrbound4ON<-max(mod4ON[mod4ON<quantile(mod4ON,prob=1-n2/100),])
        abline(v=lwrbound4ON,col=color,lty=2,lwd=3)
        
        legend(x = "topright", 
               c("Median",paste("90% Credibility Interval:",format(lwrbound4ON,big.mark=","), "-" ,format(upbound4ON,big.mark=","))),
               col =  c("black",color),lty=c(1,2), 
               lwd = c(2,2))}}
  }
  )
  
  
  
  output$table <- renderTable({
    Data
  }) }

shinyApp(ui = ui, server = server)
