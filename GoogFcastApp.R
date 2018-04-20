library(shiny)
library(shinythemes)
library(rstan)
library(rstanarm)
library(rethinking)
library(readr)
options(scipen=5)
Data <- read_csv("./Seasonal.csv")
Data<-na.omit(Data)   
fcmod=stan_glm.nb(SeasVis~CCIStag+GasStag+GoogStag+Fall+Spring+Winter,data=Data)
fcmodON=stan_glm.nb(Overnight~CCIStag+GasStag+GoogStag+Fall+Spring+Winter,data=Data)

ui <- fluidPage(
  
  titlePanel("Visitor Use Forecasting Tool"),
  theme = shinytheme("flatly"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "season",label= "The NEXT upcoming season is?",choices=c("Winter (Dec-Feb)","Spring (March-May)",
                                                                                      "Summer (June-Aug)","Fall (Sept-Nov)")),
      selectInput(inputId = "Type", label = "Which Type of Visitation are You Predicting For?",choices=c("Total Seasonal Visitation","Overnight Seasonal Visitation")),
      sliderInput(inputId ="ConsumerConfidenceIndex", label = "CURRENT Consumer Confidence Index", min = 80, max = 120,step=1, value = 99),
      sliderInput(inputId ="GoogStag", label = " What is the CURRENT Google Search Term Index for JOTR", min = 1, step=1,max = 100, value = 50),
      sliderInput(inputId ="GasPrice", label = "CURRENT Gas Price ($/Gallon)", min = 1, max = 5, step = 0.05, value = 3)
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot",plotOutput(outputId = "myplot", width = "100%", height = "600px"), br(),
                           p("This plot shows the Bayesian posterior predictions of Joshua Tree National Park visitation 
                             based on an rstanarm model fit to a negative binomial distribution.
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
    
    mod1<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GasStag=input$GasPrice,GoogStag=input$GoogStag,
                       Winter=1,Spring=0,Fall=0),draws=4000))
    
    mod1ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GasStag=input$GasPrice,GoogStag=input$GoogStag,
                         Winter=1,Spring=0,Fall=0),draws=4000))
    
    mod2<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GasStag=input$GasPrice,GoogStag=input$GoogStag,
                       Winter=0,Spring=1,Fall=0),draws=4000))
    mod2ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GasStag=input$GasPrice,GoogStag=input$GoogStag,
                         Winter=1,Spring=0,Fall=0),draws=4000))
    
    mod3<-(posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GasStag=input$GasPrice,GoogStag=input$GoogStag,
                       Winter=0,Spring=0,Fall=1),draws=4000))
    
    mod3ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GasStag=input$GasPrice,GoogStag=input$GoogStag,
                         Winter=1,Spring=0,Fall=0),draws=4000))
    
    mod4<-posterior_predict(
      fcmod,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                       GasStag=input$GasPrice,GoogStag=input$GoogStag,
                       Winter=0,Spring=0,Fall=0),draws=4000)
    
    mod4ON<-(posterior_predict(
      fcmodON,data.frame(CCIStag=input$ConsumerConfidenceIndex,
                         GasStag=input$GasPrice,GoogStag=input$GoogStag,
                         Winter=1,Spring=0,Fall=0),draws=4000))
    
    
    if (input$season == "Winter (Dec-Feb)") {
      if (input$Type == "Total Seasonal Visitation") {
        hist(mod1, main=paste("Median Visitation Estimate= " , round(median(mod1),2)),
             xlab="Number of Total Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob=T)
        abline(v = median(mod1),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        hist(mod1ON, main=paste("Median Visitation Estimate= ",  round(median(mod1ON),2)),
             xlab="Number of Overnight Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob=T)
        abline(v = median(mod1ON),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}}
    
    
    if (input$season == "Spring (March-May)") {
      if (input$Type == "Total Seasonal Visitation") {
        hist(mod2,main=paste("Median Visitation Estimate= ",  round(median(mod1),2)),
             xlab="Number of Total Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob = T)
        abline(v = median(mod2),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        hist(mod2ON, main=paste("Median Visitation Estimate= ",  round(median(mod2ON),2)),
             xlab="Number of Overnight Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob=T)
        abline(v = median(mod1ON),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}}
    
    
    if (input$season == "Fall (Sept-Nov)") {
      if (input$Type == "Total Seasonal Visitation") {
        hist(mod3,main=paste("Median Visitation Estimate= ",  round(median(mod1),2)),
             xlab="Number of Total Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob = T)
        abline(v = median(mod3),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        hist(mod3ON, main=paste("Median Visitation Estimate= ",  round(median(mod3ON),2)),
             xlab="Number of Overnight Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob=T)
        abline(v = median(mod1ON),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}}
    
    else if (input$season == "Summer (June-Aug)") {
      if (input$Type == "Total Seasonal Visitation") {
        hist(mod4, main=paste("Median Visitation Estimate= ",  round(median(mod1),2)),
             xlab="Number of Total Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob = T)
        abline(v = median(mod4),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}
      
      else if (input$Type == "Overnight Seasonal Visitation") {
        hist(mod4ON, main=paste("Median Visitation Estimate= ",  round(median(mod4ON),2)),
             xlab="Number of Overnight Visitors", ylab="Probability",col="deepskyblue",breaks=100,prob=T)
        abline(v = median(mod1ON),
               col = "red",
               lwd = 5)
        legend(x = "topright", 
               "Median",
               col =  "red",
               lwd = 2)}}
  }
  )
  
  
  
  output$table <- renderTable({
    Data
  }) }

shinyApp(ui = ui, server = server)


