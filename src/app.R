library(shiny)
library(shinythemes)

library(tidyverse)
library(knitr)
library(survminer)
library(survival)

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
        print(input$time)
        print(input$status)
        #print(input$factor)
    })
    
    # Comparing mutations
    observeEvent(input$factor,{
        if(input$factor[1] != "Select a field")
        {
            print(input$factor[1])
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
