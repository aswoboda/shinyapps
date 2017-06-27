library(shiny)

ui <- fluidPage(
  numericInput(inputId = "a",
               label = "Demand Curve Intercept",
               value = 100),
  numericInput(inputId = "b",
               label = "Demand Curve Slope",
               value = 2),
  uiOutput("int_slider"),
  checkboxInput(inputId = "show_q",
                label = "Show selected q",
                value = FALSE),
  checkboxInput(inputId = "show_TR",
                label = "Show TR",
                value = FALSE),
  checkboxInput(inputId = "show_MR",
                label = "Show MR",
                value = FALSE),
  plotOutput(outputId = "demand_plot",
             height = "400px")
)

server <- function(input, output) {
  inv_demand <- function(q) input$a-input$b*q
  demand <- function(p) (input$a-p)/input$b
  TR <- function(q) inv_demand(q)*q
  MR <- function(q) input$a-2*input$b*q
  
  out <- reactive({
    ot = list()
    ot$max_p <- input$a
    ot$max_q <- demand(0)
    ot
  })
  
  output$int_slider <- renderUI({
    sliderInput(inputId = "q", 
              label = "My q",
              min = 1, max = out()$max_q, value = out()$max_q/4)
    })
  
  output$demand_plot <- renderPlot({
    plot(0:out()$max_q, inv_demand((0:out()$max_q)), axes = FALSE,
         ylim = c(min(0, MR(input$q)), input$a),
         type = "l", lwd = 3,
         yaxs = "i", xaxs = "i",
         xlab = "quantity",
         ylab = "price",
         main = "Demand")
    axis(1); axis(2, las = 1)
    
    if(input$show_TR) {
      polygon(c(0, input$q, input$q, 0),
              c(0, 0, inv_demand(input$q), inv_demand(input$q)),
              col = rgb(0, 0, 1, alpha = .15),
              border = FALSE)
    }
    
    if(input$show_q) {
      points(input$q, inv_demand(input$q), 
             pch = 16, col = "red")
      lines(c(input$q, input$q, 0),
            c(0, inv_demand(input$q), inv_demand(input$q)),
            lty = 3)
    }
    
    if(input$show_MR) {
      lines(c(input$q-1, input$q-1, 0),
            c(0, inv_demand(input$q-1), inv_demand(input$q-1)),
            lty = 2)
      abline(input$a, -2*input$b)
      text(input$q, inv_demand(input$q), 
           paste("MR =", round(MR(input$q))),
           pos = 4)
    }
  })
}

shinyApp(ui = ui, server = server)