library(shiny)
library(shinythemes)
source("DataPreparation.R")

Surveillance <- prepare_data("dataset/student_math_clean.csv")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  navbarPage(
    "High School Grades",
    tabPanel("Math"),
    tabPanel("Portuguese"),
    tabPanel("Average")
  ),
  
  selectizeInput(inputId = "selection_tags", 
                 label = NULL, 
                 choices = names(Surveillance), 
                 selected = c("grade_1", "grade_2"), 
                 multiple = T,
                 options = NULL),
  
  actionButton('Update_Selection', 'Update Selection'),
  hr(),
  
  titlePanel("High School Grades"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "samplesize",
                  label = "Sample Size:",
                  min = 1,
                  max = nrow(Surveillance),
                  value = 30)
      
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
      
    )
  )
)
server <- function(input, output) {
  
  observeEvent(c(input$Update_Selection), ignoreInit = TRUE, {
    showNotification("Updated Plotting Parameters")
    
    output$distPlot <- renderPlot({
      if(length(input$selection_tags) > 1) {
        plot(Surveillance[[input$selection_tags[1]]][1:input$samplesize], 
             Surveillance[[input$selection_tags[2]]][1:input$samplesize], 
             xlab = input$selection_tags[1], 
             ylab = input$selection_tags[2])
      }
    })
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)