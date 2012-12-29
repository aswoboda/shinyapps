library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  #  Application title
  headerPanel("CBA of Hybrid Purchase"),
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    wellPanel(
      p(strong("Hybrid Vehicle")),
      sliderInput(inputId = "Cost.hybrid",
                  label = "Cost ($)",
                  min = 0, max = 50000, step = 500, value = 25000),  
      sliderInput(inputId = "MPG.hybrid",
                  label = "Fuel Efficiency (MPG)",
                  min = 1, max = 100, step = 1, value = 50)
    ),
    
    wellPanel(
      p(strong("Non-Hybrid Vehicle")),
      sliderInput(inputId = "Cost.other",
                  label = "Cost ($)",
                  min = 0, max = 50000, step = 500, value = 20000),
      sliderInput(inputId = "MPG.other",
                  label = "Fuel Efficiency (MPG)",
                  min = 1, max = 50, step = 1, value = 25)
      ),
    
    wellPanel(
      p(strong("Other Parameters")),
      sliderInput(inputId = "Miles.driven.other",
                  label = "Annual Miles Driven with non-Hybrid",
                  min = 5000, max = 25000, step = 500, value = 12000),
      sliderInput(inputId = "Driving.elasticity",
                  label = "Miles Driven Elasticity",
                  min = -2, max = 0, step = .05, value = -.2),
      sliderInput(inputId = "Price.gas.0",
                  label = "Price of Gasoline ($ per gallon)",
                  min = 1, max = 10, step = .1, value = 3),     
      sliderInput(inputId = "Gas.growth.rate",
                  label = "Annual Gasoline Price Growth Rate (.1 = 10%)",
                  min = 0, max = .5, step = .01, value = .1),
      sliderInput(inputId = "Years.analysis",
                  label = "Years in Analysis",
                  min = 1, max = 15, step = 1, value = 10),
      sliderInput(inputId = "Discount.rate",
                  label = "Discount Rate",
                  min = 0, max = .2, step = .01, value = .05),
      sliderInput(inputId = "PollutionCostPerGallon",
                  label = "Pollution Costs ($ per Gallon of Gasoline)",
                  min = 0, max = 10, step = .1, value = 2.1)
      )
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
      plotOutput("hybridPlot")
  )
  
))