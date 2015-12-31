MarketPlot <- function() {
  
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 2px;",
                            column(3, numericInput("Di", label = "Demand Intercept:",
                                                   value = 25, max = 30, min = 0),
                                   br(),
                                   numericInput("Ds", label = "Demand Slope:",
                                                   value = -1, max = 0, step = 0.1)
                                   ),
                            column(3, numericInput("Si", label = "Supply Intercept:",
                                                   value = 0),
                                   br(),
                                   numericInput("Ss", label = "Supply Slope:",
                                                value = 1, min = 0, step = 0.1)
                                   ), 
                            column(3, numericInput("MEBi", 
                                                   label = "Marginal External Benefit Int.:",
                                                   value = 0, min = 0),
                                   br(),
                                   numericInput("MEBs", 
                                                   label = "Marginal External Benefit Slope:",
                                                   value = 0, step = 0.1)
                                   ),
                            column(3, numericInput("MECi", label = "Marginal External Cost Int.:",
                                                   value = 0, min = 0),
                                   br(),
                                   numericInput("MECs", label = "Marginal External Cost Slope:",
                                                   value = 0, step = 0.1)
                                   )
                     ),
                   fluidRow(
                     column(3, 
                            numericInput("tax", label = "tax",
                                            value = 0),
                            br(),
                            checkboxGroupInput("checkGroup", 
                                               label = "Show...", 
                                               choices = list("Demand" = "D",
                                                              "Supply" = "S",
                                                              "Marginal External Benefit" = "MEB", 
                                                              "Marginal External Cost" = "MEC",
                                                              "Social Marginal Benefit" = "SMB",
                                                              "Social Marginal Cost" = "SMC",
                                                              "Equilibrium Quantity" = "QTAX",
                                                              "Efficient Quantity" = "QSTAR",
                                                              "Social Deadweight Loss" = "SDWL"),
                                               selected = c("D", "S")
                            )
                     ),
                     column(9, plotOutput('mygraph', height = "400px")
                            )
                            
                   )
    ),
    
    server = function(input, output, session) {
      
      # Combine the selected variables into a new data frame
      qstar <- reactive({
        ( (input$Di + input$MEBi) - (input$Si + input$MECi) )/( (input$Ss + input$MECs) - (input$Ds + input$MEBs))
      })
      
      qc <- reactive({
        (input$Di - input$Si)/(input$Ss - input$Ds)
      })
      
      qt <- reactive({
        (input$Di - input$Si - input$tax)/(input$Ss - input$Ds)
      })
      
      output$mygraph <- renderPlot(height = 400, {
        par(mar = c(5, 5, 4, 1))
        plot(0, 0, type = "n", 
             xaxs = "i", yaxs = "i", las  = 1,
             ylim = c(0, 1.1*input$Di), 
             xlim = c(0, 1.3*max(qstar(), qt(), qc())), 
             xlab = "Quantity", ylab = "Price", 
             main = "",
             cex.main = 2, cex.lab = 1.75, cex.axis = 2)
        if(input$tax != 0) {lines(c(qt(), qt()), 
                                  c((input$Di + input$Ds*qt()), (input$Si + input$Ss*qt())),
                                  col = "black", lwd = 6)
        }
        
        if (!is.null(input$checkGroup)) {
          if("S" %in% input$checkGroup) {
            abline(input$Si, input$Ss, col = "blue", lwd = 2)
          }
          if("D" %in% input$checkGroup) {
            abline(input$Di, input$Ds, col = "black", lwd = 2)
          }
          if("MEB" %in% input$checkGroup) {
            abline(input$MEBi, input$MEBs, col = "red", lwd = 4, lty = 3)
          }
          if("MEC" %in% input$checkGroup) {
            abline(input$MECi, input$MECs, col = "red", lwd = 4, lty = 3)
          }
          if("SMB" %in% input$checkGroup) {
            abline(input$Di+input$MEBi, input$MEBs + input$Ds, col = "purple", lwd = 4)
          }
          if("SMC" %in% input$checkGroup) {
            abline(input$Si+input$MECi, input$MECs + input$Ss, col = "purple", lwd = 4)
          }
          if("QSTAR" %in% input$checkGroup) {
            abline(v = qstar())
            text(qstar(), 2, "Q*", cex = 2, pos = 3)
          }
          if("QTAX" %in% input$checkGroup) {
            abline(v = qt())
            text(qt(), 2, expression(Q[t]), cex = 2, pos = 3)
          }
          if("SDWL" %in% input$checkGroup) {
            Qstar = qstar()
            Qt = qt()
            
            SMBstar = (input$Di + input$MEBi) + Qstar*(input$Ds + input$MEBs)
            SMBt = (input$Di + input$MEBi) + Qt*(input$Ds + input$MEBs)
            SMCt = (input$Si + input$MECi) + Qt*(input$Ss + input$MECs)
            
            polygon(c(Qstar, Qt, Qt),
                    c(SMBstar, SMBt, SMCt),
                    density = 10, angle = 90)
          }
        }
      })
    },
    
    options = list(height = 800)
  )
}