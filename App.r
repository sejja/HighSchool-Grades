library(shiny)
library(shinythemes)

source("DataPreparation.R")
load("datasets.RData")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(tags$style(
    HTML("hr {border-top: 1px solid #000000;}")
  )),
  navbarPage(
    "High School Grades",
    tabsetPanel(
      id = "tabsetPanel",
    tabPanel("Math", value = "math_df"),
    tabPanel("Portuguese", value = "portuguese_df"),
    tabPanel("Average", value = "both_df")
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selection_tag_1",
        label = "Select Parameter 1:",
        choices = names(math_df),
        selected = "grade_1"
      ),
      
      selectInput(
        inputId = "selection_tag_2",
        label = "Select Parameter 2:",
        choices = names(math_df),
        selected = "grade_2"
        
      ),
      
      sliderInput(
        inputId = "samplesize",
        label = "Sample Size:",
        min = 1,
        max = nrow(math_df),
        value = 30
      ),
      
      uiOutput("numericPanel"),
      actionButton('Update_Selection', 'Update Selection')
      
    ),
    
    mainPanel(plotOutput(outputId = "distPlot"),textOutput("datasetInfo")),
  ),
  
  
)
server <- function(input, output) {
  isNumericColumns <- function(){
    col1 <- input$selection_tag_1
    col2 <- input$selection_tag_2
    is.numeric(math_df[[col1]]) & is.numeric(math_df[[col2]])
  }
  
  selectedDataset <- reactive({
    switch(input$tabsetPanel, 
           "math_df" = math_df,
           "portuguese_df" = portuguese_df,
           "both_df" = both_df)
  })
  

  
  output$numericPanel <- renderUI({
    col1 <- input$selection_tag_1
    col2 <- input$selection_tag_2
    
    if (isNumericColumns()) {
      tagList(
        checkboxInput('calculate_kmeans', 'Calculate K-Means', value = FALSE),
        conditionalPanel(
          condition = "input.calculate_kmeans == true",
          sliderInput(
            inputId = "k",
            label = "Number of Clusters:",
            min = 1,
            max = 10,
            value = 3
          )
        ),
        checkboxInput('calculate_correlaction', 'Calculate Correlation', value = FALSE),
        conditionalPanel(
          condition = "input.calculate_correlaction == true",
          textOutput("correlation_result")
        ),
        checkboxInput('add_tentency_line', 'Tendency Line', value = FALSE),
      )
    } else {
      tagList()
    }
  })
  
  
  observeEvent(c(input$Update_Selection), ignoreInit = TRUE, {
    showNotification("Updated Plotting Parameters")
    
    output$distPlot <- renderPlot({
      x <- isolate(selectedDataset())[[isolate(input$selection_tag_1)]][1:isolate(input$samplesize)]
      y <- isolate(selectedDataset())[[isolate(input$selection_tag_2)]][1:isolate(input$samplesize)]
      plot(
        x,
        y,
        xlab = isolate(input$selection_tag_1),
        ylab = isolate(input$selection_tag_2),
        pch = 16
      )
      
      if (isolate(input$add_tentency_line) && isNumericColumns()) {
        lm_model <- lm(y ~ x)
        abline(lm_model, col = "red")
      }
    })
    
    if (isolate(input$calculate_kmeans) & isNumericColumns()) {
      data <- isolate(selectedDataset())[1:isolate(input$samplesize), c(input$selection_tag_1, input$selection_tag_2)]
      
      # Assuming 'k' is the number of clusters (you can adjust this as needed)
      k <- input$k
      
      # Perform K-Means clustering
      km <- kmeans(data, centers = k)
      
      output$distPlot <- renderPlot({
        # Plot data points with colors representing K-Means clusters
        plot(
          data[, 1],
          data[, 2],
          col = km$cluster,
          xlab = input$selection_tag_1,
          ylab = input$selection_tag_2,
          pch = 16
        )
        
        if (isolate(input$add_tentency_line) && isNumericColumns()) {
          x <- isolate(selectedDataset())[[isolate(input$selection_tag_1)]][1:isolate(input$samplesize)]
          y <- isolate(selectedDataset())[[isolate(input$selection_tag_2)]][1:isolate(input$samplesize)]
          lm_model <- lm(y ~ x)
          abline(lm_model, col = "red")
        }
      })
    }


    output$correlation_result <- renderText({
      paste("Correlation:", cor(isolate(selectedDataset())[1:isolate(input$samplesize), isolate(input$selection_tag_1)], 
                                isolate(selectedDataset())[1:isolate(input$samplesize), isolate(input$selection_tag_2)]))
    })
  })
  
}

shinyApp(ui = ui, server = server)

# You might also want to take a look to our previous version. We created multiplot functionality

# library(shiny)
# library(shinythemes)
# 
# source("DataPreparation.R")
# load("datasets.RData")
# 
# ui <- fluidPage(
#   theme = shinytheme("sandstone"),
#   tags$head(
#     tags$style(HTML("hr {border-top: 1px solid #000000;}"))
#   ),
#   navbarPage(
#     "High School Grades",
#     tabPanel("Math", uiOutput("panelMath")),
#     tabPanel("Portuguese", uiOutput("panelPortuguese")),
#     tabPanel("Average", uiOutput("panelAverage"))
#   ),
#   
#   selectizeInput(inputId = "selection_tags", 
#                  label = NULL, 
#                  choices = names(math_df), 
#                  selected = c("grade_1", "grade_2"), 
#                  multiple = T,
#                  options = NULL),
#   
#   actionButton('Update_Selection', 'Update Selection'),
#   hr(),
#   
#   titlePanel("High School Grades"),
#   fluidRow(
#     sidebarPanel(
#       sliderInput(inputId = "samplesize",
#                   label = "Sample Size:",
#                   min = 1,
#                   max = nrow(math_df),
#                   value = 30)
#       
#     ),
#     
#     column(2, 
#            uiOutput("plots")
#            
#     )
#   )
# )
# server <- function(input, output) {
#   
#   observeEvent(c(input$Update_Selection), ignoreInit = TRUE, {
#     showNotification("Updated Plotting Parameters")
#     
#     permutations <- combn(input$selection_tags, 2)
#     output$panelMath <- renderUI({
#       plot_output_list <- lapply(1:ncol(permutations), function(i) {
#         plotname <- paste0("plotm", i)
#         plotOutput(plotname, height = 512, width = 512)
#       })
#       
#       do.call(tagList, plot_output_list)
#     })
#     output$panelPortuguese <- renderUI({
#       plot_output_list <- lapply(1:ncol(permutations), function(i) {
#         plotname <- paste0("plotp", i)
#         plotOutput(plotname, height = 512, width = 512)
#       })
#       
#       do.call(tagList, plot_output_list)
#     })
#     
#     output$panelAverage <- renderUI({
#       plot_output_list <- lapply(1:ncol(permutations), function(i) {
#         plotname <- paste0("plota", i)
#         plotOutput(plotname, height = 512, width = 512)
#       })
#       
#       do.call(tagList, plot_output_list)
#     })
#     
#     for (i in 1:ncol(permutations)) {
#       local({
#         my_i <- i
#         plotname <- paste0("plotm", my_i)
#         
#         output[[plotname]] <- renderPlot({
#           plot(math_df[[permutations[1, my_i]]][1:isolate(input$samplesize)], math_df[[permutations[2, my_i]]][1:isolate(input$samplesize)],
#                xlab = permutations[1, my_i], 
#                ylab = permutations[2, my_i],
#                main = plotname
#           )
#         })
#       })
#     }
#     
#     for (i in 1:ncol(permutations)) {
#       local({
#         my_i <- i
#         plotname <- paste0("plotp", my_i)
#         
#         output[[plotname]] <- renderPlot({
#           plot(portuguese_df[[permutations[1, my_i]]][1:isolate(input$samplesize)], portuguese_df[[permutations[2, my_i]]][1:isolate(input$samplesize)],
#                xlab = permutations[1, my_i], 
#                ylab = permutations[2, my_i],
#                main = plotname
#           )
#         })
#       })
#     }
#     
#     for (i in 1:ncol(permutations)) {
#       local({
#         my_i <- i
#         plotname <- paste0("plota", my_i)
#         
#         output[[plotname]] <- renderPlot({
#           plot(both_df[[permutations[1, my_i]]][1:isolate(input$samplesize)], both_df[[permutations[2, my_i]]][1:isolate(input$samplesize)],
#                xlab = permutations[1, my_i], 
#                ylab = permutations[2, my_i],
#                main = plotname
#           )
#         })
#       })
#     }
#   })
# }
# 
# # Create Shiny app ----
# shinyApp(ui = ui, server = server)

