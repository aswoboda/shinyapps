library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Marginal Excess Tax Burden"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    numericInput("P0", "Initial Price:", 10),
    numericInput("Q0", "Initial Quantity:", 100),
    numericInput("Ed", "Elasticity of Demand:", -1),
    numericInput("Es", "Elasticity of Supply:", 1),
    selectInput("AllTax", "Choose a set of taxes:", choices = c("case1", "case2", "case3"))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("marketPlot")),
      tabPanel("Tax Q", plotOutput("TaxQPlot")),
      tabPanel("Tax Revenue", plotOutput("TaxRevPlot")),
      tabPanel("Burden", plotOutput("ExcessTaxBurdenPlot")),
      tabPanel("Summary", plotOutput("SummaryPlot"))
      )
  )

))