library(shiny)
library(shinythemes)

load("datsets.RData")

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
                 choices = names(math_df), 
                 selected = c("grade_1", "grade_2"), 
                 multiple = T,
                 options = NULL),
  
  actionButton('Update_Selection', 'Update Selection'),
  hr(),
  
  titlePanel("High School Grades"),
  fluidRow(
    sidebarPanel(
      sliderInput(inputId = "samplesize",
                  label = "Sample Size:",
                  min = 1,
                  max = nrow(math_df),
                  value = 30)
      
    ),
    
    column(2, 
      uiOutput("plots")
      
    )
  )
)
server <- function(input, output) {
  
  observeEvent(c(input$Update_Selection), ignoreInit = TRUE, {
    showNotification("Updated Plotting Parameters")
    
    permutations <- combn(input$selection_tags, 2)
    output$plots <- renderUI({
    plot_output_list <- lapply(1:ncol(permutations), function(i) {
      plotname <- paste0("plot", i)
      plotOutput(plotname, height = 512, width = 512)
    })

    do.call(tagList, plot_output_list)
  })

  for (i in 1:ncol(permutations)) {
    local({
      my_i <- i
      plotname <- paste0("plot", my_i)
      
      output[[plotname]] <- renderPlot({
        plot(math_df[[permutations[1, my_i]]][1:input$samplesize], math_df[[permutations[2, my_i]]][1:input$samplesize],
             xlab = permutations[1, my_i], 
             ylab = permutations[2, my_i],
             main = plotname
        )
      })
      })
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)