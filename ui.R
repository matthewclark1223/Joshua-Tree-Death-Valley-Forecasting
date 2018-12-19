library(shiny)
library(gtable)
library(grid)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(markdown)
library(knitr)
library(shinythemes)
library(plotly)
library(readr)
library(gridExtra)
library(rstan)
library(shinystan)
library(rstanarm)

# Set up the application ui
shinyUI(navbarPage("NPS Visitation Forecast Explorer",
                       theme = shinytheme("flatly"),
                       
                       # define the tabs to be used in the app ----------------------------------------
                       # introduction splash
                       tabPanel("Intro",
                                h1("NPS Visitation Forecast Explorer"),
                                br(),
                                h3("This application is designed to be coupled with DESCRIPTION/LINK TO OUR PAPER"),
                                p("This application allows users to compare two different types of predictive models for visitation forecasting in US
                                  National Parks. One model is informed by Google Trends values alone and the other is informed by previous visitation alone
                                  (auoregressive model). Users can examine the accuracies of both of these models for specific parks and explore how varying levels of 
                                  visitation and Google Trends values affect model projections."),
                                br(),
                                h2("Overview"),
                                p("The plots below show the accuracy of all models for all years. These are designed to display the total accuracy for each model.
                                  Hover your cursor over the points to explore for which park and which year you are looking at. X and Y values for each point are also displayed."),
                                br(),
                                fluidRow(
                                  
                                  column(6,plotlyOutput("glR2")),
                                  column(6,plotlyOutput("a5R2"))
                                ),
                                br(),
                                h2("Authorship and contact"),
                                p("This application was created by Matt Clark, please direct all contact to: matthewclark989@u.boisestate.edu"),
                                br(),
                                p("This project was initiated as part of the Park Break program of the George Wright Society.  Special thanks to Jane Rogers at Joshua Tree National Park for her support.
                                "),
                                hr()),
                       
                       # Model Validation
                       tabPanel("Model validation",
                                # plot the map
                                fluidRow(column(12,
                                                h1("Model validation"),
                                                p("Tool for comparing accuracy of all model types for each park. Credibility intervals (C.I.) represent the inner 25th and 50th percentiles of 2000 visitation estimates for each park."),
                                                br(),
                                                strong("Visitation estimates are informed only by data prior each estimated year."))),
                                
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      
                                                      helpText("Which park do you want to look at?"),
                                                      selectInput(inputId = "Type", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                    "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" , "NOCA", "OLYM",
                                                                                                                    "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION"),selected = "CUVA")),

                                         mainPanel(plotOutput("myplot",width="800px",height = "600px")))
                       ),
                       
                       
                       #width="1200px",height = "550px"
                       
                       # forecasting tool
                       tabPanel("Forecast explorer",
                                fluidRow(column(12,
                                                h1("Forecast explorer"),
                                                p("This tool is designed to let users explore how varying inputs affect the visitation estimates produced by the models described in the 
                                                  paper which accompanies this application. We provide this interface as a mechanism to increase understanding of our study only, not as a
                                                  real world tool for park managers. All visitation projections should be taken with a hefty dose of skepticism. With that being said: start forecasting!"),
                                                br(),
                                                h4("Instructions"),
                                                p("Use the control panel on the left to chose a park and inform your predictions." ))),
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      h4("Model type"),
                                                      helpText("Chose which type of model you want to inform your predictions."),
                                                      selectInput(inputId = "ModelType",label = "Model Type",choices=c("Autoregressive Forecast", "Google Trends Forecast"),selected="Google Trends Forecast"),
                                                      selectInput(inputId = "Park", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                    "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" , "NOCA", "OLYM",
                                                                                                                    "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION")), 
                                                      conditionalPanel(condition="input.ModelType == 'Google Trends Forecast'",
                                                                       sliderInput(inputId ="Lag12G", label = "Yearly total Google Trends value (sum of all months) last calendar year", min = 100, step=50,max = 1000, value = 300)),
                                                      conditionalPanel(condition="input.ModelType == 'Autoregressive Forecast'",
                                                                       numericInput(inputId = "a5Vis1", label = "Last year's total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis2", label = "Two years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis3", label = "Three years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis4", label = "Four years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis5", label = "Five years ago total visitation", value = 1000000))),
                                         mainPanel(plotOutput("Forecast", height = 500)))
                       ),
                       
                       
                       # simple data table output
                       tabPanel("Error metrics by park",
                                column(12,
                                       h1("Error Metrics by Park"),
                                       p("This tabs shows the R^2, mean percent difference, and mean absolute error (MAE) for the median visitation estimate for each park and the observed visitation for each park. The overall error estimates for each model are also displayed"
                                       ),
                                       column(10, dataTableOutput("table", height = "100%")))),
                       
                       
                       #Raw data
                       tabPanel("Data",
                                column(12,
                                       h1("Data"),
                                       p("This is the complete dataframe used to inform these models. All park visitation data was collected from the National Park Visitor Use Statistics Portal."),
                                       
                                       column(10, dataTableOutput("data", height = "100%")))),
                   #Key and pop
                   tabPanel("Unit code key & population data",
                            column(12,
                                   h1("Unit Code Key & Population Data"),
                                   p("Key for park unit codes used in this application and the accompanying paper. We also show the population within 50 miles surrounding each park. These data are used in an exploratory analysis in the accompanying paper, but were not used to develop this application. "),
                                   
                                   column(10, dataTableOutput("Codes", height = "100%"))))
                       
                       # close the UI definition
                       ))