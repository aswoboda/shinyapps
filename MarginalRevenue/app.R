library(shiny)

ui <- fluidPage(
  numericInput(inputId = "a",
               label = "Demand Curve Intercept",
               value = 100),
  numericInput(inputId = "b",
               label = "Demand Curve Slope",
               value = 2),
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
  output$demand_plot <- renderPlot({
    out = list()
    inv_demand <- function(q) input$a-input$b*q
    demand <- function(p) (input$a-p)/input$b
    TR <- function(q) inv_demand(q)*q
    MR <- function(q) input$a-2*input$b*q
    
    out$max_p <- input$a
    out$max_q <- demand(0)
    out$input_q <- out$max_q/4
    
    plot(0:out$max_q, inv_demand((0:out$max_q)), axes = FALSE,
         type = "l", lwd = 3,
         yaxs = "i", xaxs = "i",
         xlab = "quantity",
         ylab = "price",
         main = "Demand")
    axis(1); axis(2, las = 1)
    
    if(input$show_TR) {
      polygon(c(0, out$input_q, out$input_q, 0),
              c(0, 0, inv_demand(out$input_q), inv_demand(out$input_q)),
              col = rgb(0, 0, 1, alpha = .15),
              border = FALSE)
    }
    
    if(input$show_q) {
      points(out$input_q, inv_demand(out$input_q), 
             pch = 16, col = "red")
      lines(c(out$input_q, out$input_q, 0),
            c(0, inv_demand(out$input_q), inv_demand(out$input_q)),
            lty = 3)
    }
    
    if(input$show_MR) {
      lines(c(out$input_q-1, out$input_q-1, 0),
            c(0, inv_demand(out$input_q-1), inv_demand(out$input_q-1)),
            lty = 2)
      abline(input$a, -2*input$b)
      text(out$input_q, MR(out$input_q), 
           paste("MR =", round(MR(out$input_q))),
           pos = 4)
    }
  })
}

shinyApp(ui = ui, server = server)