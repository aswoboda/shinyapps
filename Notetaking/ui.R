library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("How much time should I spend taking notes?"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    wellPanel(
      p("Learning from Notes = a*n^2 + b*n + c"),
      numericInput("a", "a:", -.02),
      numericInput("b", "b:", 4),
      numericInput("c", "c:", 0)
      ),
    
    wellPanel(
      p("Learning from other time = d*o^2 + e*o + f"),
      numericInput("d", "d:", -.02),
      numericInput("e", "e:", 4),
      numericInput("f", "f:", 0)
      )
    ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(
      tabPanel("Plot 1", plotOutput("notePlot1", height = "100%")),
      tabPanel("Plot 2", plotOutput("notePlot2", height = "100%")),
      tabPanel("Plot 3", plotOutput("notePlot3", height = "100%")),
      tabPanel("Plot 4", plotOutput("notePlot4", height = "100%")),
      tabPanel("Plot 5", plotOutput("notePlot5", height = "100%"))
    )
  )
  
))