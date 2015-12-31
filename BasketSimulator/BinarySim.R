BinaryMCsim <- function() {
  
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 2px;",
                            column(4, 
                                   sliderInput("p", label = "Shooter Proportion:",
                                                value = 0.5, min = 0, max = 1, step = 0.05),
                                   br(),
                                   numericInput("n", 
                                                   label = "Number of Shots:",
                                                   value = 20, min = 0),
                                   br(),
                                   numericInput("M", 
                                                label = "Number of Replications:",
                                                value = 40),
                                   br(),
                                   selectInput("graph", "",
                                               c("Shots Made" = "shots", "Proportion Made" = "prop")),
                                   br(),
                                   checkboxInput("stats", "Show Summary Stats"),
                                   br(),
                                   actionButton("goButton", "Run Another Simulation!")
                          
                            ),
                            column(8, plotOutput('mygraph')
                            )
                     )
    ),
    
    server = function(input, output, session) {
      
      Mysums <- reactive({
        input$goButton
        temp = matrix(sample(x = c(1, 0), size = input$n*input$M, replace = TRUE, prob = c(input$p, 1-input$p)), 
                      nrow = input$M)
        out = rowSums(temp)
        out
      })
      
      output$mygraph <- renderPlot(height = 400, {
        par(mar = c(5, 5, 4, 1))
        if (input$graph == "shots") {
          hist(Mysums(), 
               xlab = paste("# shots made out of", input$n),
               main = "Simulation Results",
               xlim = c(0, input$n),
               col = "grey85",
               breaks = seq(-0.5, input$n+0.5, by = 1),
               las = 1)
          if(input$stats) legend("topright", legend = c(paste("Mean: ", round(mean(Mysums()), 1)), 
                                                        paste("St. Dev: ", round(sd(Mysums()), 1))))
        }
        
        if (input$graph == "prop") {
          hist(Mysums()/input$n, 
               xlab = "proportion of shots made",
               main = "Simulation Results",
               xlim = c(0, 1),
               col = "grey85",
               breaks = seq(-0.5, input$n+0.5, by = 1)/input$n,
               las = 1)
          if(input$stats) legend("topright", legend = c(paste("Mean: ", round(mean(Mysums()/input$n), 2)), 
                                                        paste("St. Dev: ", round(sd(Mysums()/input$n), 2))))
        }

      })
    },
    
    options = list(height = 600)
  )
}