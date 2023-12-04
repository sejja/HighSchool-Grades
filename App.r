library(shiny)
library(shinythemes)

Surveillance <- read.csv("student_math_clean.csv")

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
  
  selectizeInput(inputId = "selection_tags", label = NULL, choices = names(Surveillance), selected = NULL, multiple = T,
                 options = NULL),
  
  hr(),
  
  titlePanel("High School Grades"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "children",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
      
    )
  )
)
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    grades <- seq(min(x), max(x), length.out = input$children + 1)
    
    hist(x, breaks = grades,
         xlab = "Grades",
         main = "High School Grade Population")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)