# Library for shiny
library(shiny)
library(shinythemes)
library(shinyvalidate)

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
            actionButton("submit", label = "Submit", 
                         style="background-color: #337ab7;")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("contents")
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
                df$Months = round(df$Months,0)
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
    
    # Submit action
    observeEvent(input$submit,{
        
        # Validating fields
        iv <- InputValidator$new()
        iv$add_rule("time", function(value){
            if(value == "Select a field")
            {
                "Please select an option"
            }
        })
        iv$add_rule("status", function(value){
            if(value == "Select a field")
            {
                "Please select an option"
            }
        })
        iv$add_rule("factor", function(value){
            if(value == "Select a field")
            {
                "Please select an option"
            }
        })
        iv$add_rule("with", function(value){
            if(value == "Select a field")
            {
                "Please select an option"
            }
        })
        iv$add_rule("against", function(value){
            if(value == "Select a field")
            {
                "Please select an option"
            }
        })
        iv$enable()
        
        if(InputValidator$is_valid())
        {
            print("Hi")
        }
        else
        {
            Print("No")
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
