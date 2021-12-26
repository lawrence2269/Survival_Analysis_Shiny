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
library(broom)
library(ggplot2)


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
            div(id = "form",
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
                uiOutput("against")),
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
            hidden(dataTableOutput("contents")),
            br(),
            uiOutput("summary_table_heading"),
            br(),
            dataTableOutput("survivalSummary"),
            br(),
            plotOutput("kaplan_meier_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    uiValues <- reactiveValues(km_plot = NULL)
  
    observe({
        if(!is.null(input$file1$datapath))
        {
            enable("submit")
            enable("reset")
        }
    })
    
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
          time <- as.character(input$time)
          status <- as.character(input$status)
          factors <- as.character(input$factor)
          
          time_data <- df[,time]
          # status_data = as.factor(df[,status])
          # factors_data = as.factor(df[,factors])
          
          status_data <- df[,status]
          factors_data <- df[,factors]
          
          final_data <- data.frame(time = time_data,
                                  status = status_data,
                                  factors = factors_data)
          
          finalized_data <- final_data %>% select(time,
                                                 status,
                                                 factors) %>%
                 filter(factors %in% c(input$with,input$against))
          
          # Validating selected data datatype
          # if(!all(sapply(finalized_data$time,is.numeric)))
          # {
          #    shinyalert("Please choose an numeric column for 'Time'", type = "error")
          # }
          # else if(!all(sapply(finalized_data$status,is.factor)))
          # {
          #     shinyalert("Please choose a nominal column for 'Status'", type = "error")
          # }
          # else if(!all(sapply(finalized_data$factors,is.factor)))
          # {
          #     shinyalert("Please choose a nominal column for 'Factor'", type = "error")
          # }
          # else
          # {
              time_data <- finalized_data$time
              status_data <- finalized_data$status
              #factor_data = str_split(finalized_data$factors,"_",simplify = T)[,1]
              factor_data <- finalized_data$factors
              survival <- Surv(time = time_data, event = status_data)
              Survfit <- do.call(survfit,list(formula = Surv(time = time_data, event = status_data) ~ factor_data, data = finalized_data))
              #Survfit <- survfit(survival ~ factor_data, data = finalized_data)
              res <- summary(Survfit)
              cols <- lapply(c(2:10,15,16) , function(x) res[x])
              tbl <- do.call(data.frame, cols)
              head(tbl)
              tbl$strata <- str_split(as.character(tbl$strata),"=",simplify = T)[,2]
              
              output$summary_table_heading <- renderUI({
                h3("Survival analysis summary")
              })
              
              # output$case_table <- renderDataTable({
              #   return(caseTable)
              # })
              
              output$survivalSummary <- renderDataTable({
                return(tbl)
              })
              
              uiValues$km_plot <- ggsurvplot(fit = Survfit, 
                                          data = finalized_data,
                                          ####### Format Title #######
                                          title = "Overall Survival",
                                          subtitle = "Stratified By Mutations",
                                          pval = T,
                                          font.title = c(22, "bold", "black"),
                                          ggtheme = theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ # theme_classic will give a white background with no lines on the plot
                                            theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), 
                                          ####### Format Axes #######
                                          xlab="Months", # changes xlabel,
                                          ylab = "Survival Probability",
                                          font.x=c(18,"bold"), # changes x axis labels
                                          font.y=c(18,"bold"), # changes y axis labels
                                          font.xtickslab=c(14,"plain"), # changes the tick label on x axis
                                          font.ytickslab=c(14,"plain"),
                                          ####### Format Curve Lines #######
                                          palette = c("red","black"),
                                          ####### Censor Details ########
                                          censor = TRUE, # logical value. If TRUE, censors will be drawn,
                                          censor.shape="|",
                                          censor.size = 5,
                                          ####### Confidence Intervals ########
                                          conf.int = FALSE, # To Remove conf intervals use "FALSE"
                                          conf.int.fill = "purple", # fill color to be used for confidence interval
                                          surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
                                          ######## Format Legend #######
                                          legend = "top", # If you'd prefer more space for your plot, consider removing the legend
                                          #legend.title = "All Patients",
                                          legend.labs = c(input$with,input$against), # Change the Strata Legend
                                          ######## Risk Table #######
                                          risk.table = TRUE, # Adds Risk Table
                                          risk.table.height = 0.25 # Adjusts the height of the risk table (default is 0.25)
              )
              uiValues$km_plot$plot <- uiValues$km_plot$plot + scale_x_continuous(expand=c(0,0))
              
              output$kaplan_meier_plot <- renderPlot({
                if(is.null(uiValues$km_plot))
                {
                    return()
                }
                else
                {
                  uiValues$km_plot
                }
              })
              
              # Rendering Kaplan-Meier plot
              # output$kaplan_meier_plot = renderPlot({
              #   survival_plot <- ggsurvplot(fit = Survfit, 
              #                               data = finalized_data,
              #                               ####### Format Title #######
              #                               title = "Overall Survival",
              #                               subtitle = "Stratified By Mutations",
              #                               font.title = c(22, "bold", "black"),
              #                               ggtheme = theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ # theme_classic will give a white background with no lines on the plot
              #                                 theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), 
              #                               ####### Format Axes #######
              #                               xlab="Months", # changes xlabel,
              #                               ylab = "Survival Probability",
              #                               font.x=c(18,"bold"), # changes x axis labels
              #                               font.y=c(18,"bold"), # changes y axis labels
              #                               font.xtickslab=c(14,"plain"), # changes the tick label on x axis
              #                               font.ytickslab=c(14,"plain"),
              #                               ####### Format Curve Lines #######
              #                               palette = c("red","black"),
              #                               ####### Censor Details ########
              #                               censor = TRUE, # logical value. If TRUE, censors will be drawn,
              #                               censor.shape="|",
              #                               censor.size = 5,
              #                               ####### Confidence Intervals ########
              #                               conf.int = FALSE, # To Remove conf intervals use "FALSE"
              #                               conf.int.fill = "purple", # fill color to be used for confidence interval
              #                               surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
              #                               ######## Format Legend #######
              #                               legend = "top", # If you'd prefer more space for your plot, consider removing the legend
              #                               #legend.title = "All Patients",
              #                               legend.labs = c(input$with,input$against), # Change the Strata Legend
              #                               ######## Risk Table #######
              #                               risk.table = TRUE, # Adds Risk Table
              #                               risk.table.height = 0.25 # Adjusts the height of the risk table (default is 0.25)
              #   )
              #   survival_plot$plot <- survival_plot$plot + scale_x_continuous(expand=c(0,0))
              #   return(survival_plot)
              # })
              #reset("form")
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
                        dataFields <- c("Select a field")
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
                        dataFields <- c("Select a field")
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
