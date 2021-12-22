# Library for shiny
library(shiny)
library(shinythemes)
library(shinyvalidate)
library(shinyjs)
library(shinyalert)
library(shinydashboard)

# Libraries for survival analysis
library(tidyverse)
library(knitr)
library(survminer)
library(survival)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme
    theme = shinytheme("darkly"),
    useShinyjs(),  
    # Application title
    titlePanel(
        h1("Survival Analysis", align = "center")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Inputs"),
            br(),
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            br(),
            uiOutput("time"),
            uiOutput("status"),
            uiOutput("factor"),
            uiOutput("with"),
            uiOutput("against"),
            div(style="",
                disabled(actionButton("submit", label = "Submit", 
                                      style="background-color: #337ab7;")),
                disabled(actionButton("reset",label = "Reset",
                                      style="background-color: #337ab7;"))),
            # disabled(actionButton("submit", label = "Submit", 
            #              style="background-color: #337ab7;"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("contents"),
            box(
                title = "Data Table", 
                status = "warning", 
                solidHeader = TRUE,
                width = 6,
                height = 142,
                verbatimTextOutput("survivalSummary")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Displaying data in a table
    output$contents <- renderDataTable({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
                enable("submit")
                enable("reset")
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        return(df)
    })
    
    # Select input for time
    output$time <- renderUI({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
                dataFields <- c("Select a field")
                dataFields <- append(dataFields,colnames(df))
                selectInput("time",
                                   label = h3("Time"),
                                   choices = dataFields)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
    })
    
    # Select input for status
    output$status <- renderUI({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
                dataFields <- c("Select a field")
                dataFields <- append(dataFields,colnames(df))
                selectInput("status",
                            label = h3("Status"),
                            choices = dataFields)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
    })
    
    # Select input for factors
    output$factor <- renderUI({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
                dataFields <- c("Select a field")
                dataFields <- append(dataFields,colnames(df))
                selectInput("factor",
                            label = h3("Factor"),
                            choices = dataFields)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
    })
    
    # # Reset action
    # observeEvent(input$reset,{
    #     output$contents = renderDataTable(NULL)
    #     reset("file1")
    #     reset("time")
    #     hide("time")
    #     reset("status")
    #     hide("status")
    #     reset("factor")
    #     hide("factor")
    #     reset("with")
    #     hide("with")
    #     reset("against")
    #     hide("against")
    #     disable("submit")
    #     disable("reset")
    # })
    
    # Submit action
    observeEvent(input$submit,{
        if(input$time == "Select a field")
        {
          shinyalert("Please choose an option for 'Time'", type = "error")
        }
        else if(input$status == "Select a field")
        {
          shinyalert("Please choose an option for 'Status'", type = "error")
        }
        else if(input$factor == "Select a field")
        {
          shinyalert("Please choose an option for 'Factor'", type = "error")
        }
        else if(input$with == "Select a field")
        {
          shinyalert("Please choose an option for 'With'", type = "error")
        }
        else if(input$against == "Select a field")
        {
          shinyalert("Please choose an option for 'Against'", type = "error")
        }
        else
        {
          # Reading and getting individual columns.
          df <- read.csv(input$file1$datapath)
          time = input$time
          status = input$status
          factors = input$factor
          
          time_data = df[,time]
          status_data = as.factor(df[,status])
          factors_data = as.factor(df[,factors])
          
          final_data = data.frame(Months = time_data,
                                  Patient_survival_status = status_data,
                                  KRAS_mutations = factors_data)
          
          # factors_with = df[df[input$factor] == input$with,c(input$factor,input$time,input$status)]
          # factors_against = df[df[input$factor] == input$against,c(input$factor,input$time,input$status)]
          # final_data = do.call("rbind",list(factors_with,factors_against))
          # final_data$status = as.factor(final_data$status)
          # final_data$factors = as.factor(final_data$factors)
          print(filter(final_data, input$factor == input$with))
          # final_data_with = filter(final_data, input$factor == input$with)
          
          # final_data_against = final_data[final_data$factors == input$against,]
          # finalized_data = do.call("rbind",list(final_data_with,final_data_against))
          # print(str(finalized_data))
          
          # Validating selected data datatype
          # if(!all(sapply(finalized_data[,time],is.numeric)))
          # {
          #    shinyalert("Please choose an numeric column for 'Time'", type = "error")
          # }
          # else if(!all(sapply(finalized_data[,status],is.factor)))
          # {
          #     shinyalert("Please choose a nominal column for 'Status'", type = "error")
          # }
          # else if(!all(sapply(finalized_data[,factors],is.factor)))
          # {
          #     shinyalert("Please choose a nominal column for 'Factor'", type = "error")
          # }
          # else
          # {
          #     print("Inside else")
          #     survival <- Surv(time = finalized_data[,time], event = finalized_data[,status])
          #     Survfit <- survfit(survival ~ finalized_data[,factors], data = finalized_data)
          #     output$survivalSummary <- renderPrint({summary(Survfit)})
          # }
        }
    })
    
    # Comparing mutations
    observeEvent(input$factor,{
        if(input$factor[1] != "Select a field")
        {
            output$with <- renderUI({
                req(input$file1)
                tryCatch(
                    {
                        df <- read.csv(input$file1$datapath)
                        dataFields <- c("Select a field", "All")
                        dataFields <- append(dataFields,df[,input$factor[1]])
                        selectInput("with",
                                    label = h4("With"),
                                    choices = dataFields)
                    },
                    error = function(e){
                        stop(safeError(e))
                    }
                )
            })
        }
    })
    
    ## Select input for "Against"
    observeEvent(input$with,{
        if(input$with[1] != "Select a field")
        {
            output$against <- renderUI({
                req(input$file1)
                tryCatch(
                    {
                        df <- read.csv(input$file1$datapath)
                        dataFields <- c("Select a field", "All")
                        dataFields <- append(dataFields,df[,input$factor[1]])
                        dataFields <- dataFields[!dataFields %in% c(input$with)]
                        selectInput("against",
                                    label = h4("Against"),
                                    choices = dataFields)
                    },
                    error = function(e){
                        stop(safeError(e))
                    }
                )
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
