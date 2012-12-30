library(shiny)

# write my basic functions
Learning.Notes = function(a, b, c, x) {a*x^2 + b*x + c }
Learning.Other = function(d, e, f, y) {d*y^2 + e*y + f }
Learning = function(a, b, c, d, e, f, x) {Learning.Notes(a, b, c, x) + Learning.Other(d, e, f, (100-x))}

MB = function(a, b, x) {2*a*x + b}
MC = function(d, e, y) {2*d*y + e}
OptimumX = function(a, b, d, e) {(e - b + 200*d)/2/(a+d)}

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Calculate the Learning from Time
  LearningData <- reactive(function() {
    a = input$a
    b = input$b
    c = input$c
    d = input$d
    e = input$e
    f = input$f
    percent = 0:100
    
    data.frame(percent = percent,
               LearningNotes = Learning.Notes(a, b, c, percent),
               LearningOther = Learning.Other(d, e, f, percent),
               LearningTotal = Learning(a, b, c, d, e, f, percent))
  })
  
  # Make a plot of the market supply and demand curves
  output$notePlot1 <- reactivePlot(function() {
    temp = LearningData()
    par(mar = c(5, 5, 4, 1))
    plot(temp$percent, temp$LearningNotes, type = "l", lwd = 2, col = "blue",
         main = "Learning from Notes", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2)
    }, width = 800, height = 800)
  
  output$notePlot2 <- reactivePlot(function() {
    par(mfrow = c(2, 1))
    par(mar = c(5, 5, 4, 0))
    temp = LearningData()
    plot(temp$percent, temp$LearningNotes, type = "l", lwd = 2, col = "blue",
         main = "Learning from Notes", cex.main = 1.5,
         xlab = "Time taking notes", cex.axis = 1.5,
         ylab = "Learning", cex.lab = 1.5)
    plot(temp$percent, temp$LearningOther, type = "l", lwd = 2, col = "red",
         main = "Learning from Other", cex.main = 1.5,
         xlab = "Time doing other", cex.axis = 1.5,
         ylab = "Learning", cex.lab = 1.5)
    
  }, width = 500, height = 900)
  
  output$notePlot3 <- reactivePlot(function() {
    par(mfcol = c(2, 2))
    par(mar = c(5, 5, 4, 0))
    temp = LearningData()
    plot(temp$percent, temp$LearningNotes, type = "l", lwd = 2,
         main = "Learning from Notes", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "blue")
    plot(temp$percent, temp$LearningOther, type = "l", lwd = 2,
         main = "Learning from Other", cex.main = 2,
         xlab = "Time doing other", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "red")
    plot(temp$percent, temp$LearningTotal, type = "l", lwd = 2,
         ylim = c(0, max(temp$LearningTotal)),
         main = "Total Learning", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2)
    lines(temp$percent, Learning.Notes(input$a, input$b, input$c, temp$percent), 
          col = "blue", lwd = 2)
    lines(temp$percent, Learning.Other(input$d, input$e, input$f, (100-temp$percent)), 
          col = "red", lwd = 2)   
  }, width = 900, height = 900)

  output$notePlot4 <- reactivePlot(function() {
    par(mfcol = c(2, 2))
    par(mar = c(5, 5, 4, 2))
    temp = LearningData()
    plot(temp$percent, temp$LearningNotes, type = "l", lwd = 2,
         main = "Learning from Notes", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "blue")
    plot(temp$percent, temp$LearningOther, type = "l", lwd = 2,
         main = "Learning from Other", cex.main = 2,
         xlab = "Time doing other", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "red")
    plot(temp$percent, temp$LearningTotal, type = "l", lwd = 2,
         ylim = c(0, max(temp$LearningTotal)),
         main = "Total Learning", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2)
    lines(temp$percent, Learning.Notes(input$a, input$b, input$c, temp$percent), 
          col = "blue", lwd = 2)
    lines(temp$percent, Learning.Other(input$d, input$e, input$f, (100-temp$percent)), 
          col = "red", lwd = 2)  
    
    MBs = MB(input$a, input$b, temp$percent)
    MCs = MC(input$d, input$e, (100-temp$percent))
    plot(c(0, 100), range(c(MBs, MCs)), type = "n", 
         main = "Marginal Benefit and Cost of Note Taking", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning / Time taking notes", cex.lab = 2
    )
    lines(temp$percent, MBs, col = "blue", lwd = 2)
    lines(temp$percent, MCs, col = "red", lwd = 2)
  }, width = 900, height = 900)
  
  output$notePlot5 <- reactivePlot(function() {
    par(mfcol = c(2, 2))
    par(mar = c(5, 5, 4, 2))
    temp = LearningData()
    plot(temp$percent, temp$LearningNotes, type = "l", lwd = 2,
         main = "Learning from Notes", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "blue")
    plot(temp$percent, temp$LearningOther, type = "l", lwd = 2,
         main = "Learning from Other", cex.main = 2,
         xlab = "Time doing other", cex.axis = 2,
         ylab = "Learning", cex.lab = 2, col = "red")
    plot(temp$percent, temp$LearningTotal, type = "l", lwd = 2,
         ylim = c(0, max(temp$LearningTotal)),
         main = "Total Learning", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning", cex.lab = 2)
    lines(temp$percent, Learning.Notes(input$a, input$b, input$c, temp$percent), 
          col = "blue", lwd = 2)
    lines(temp$percent, Learning.Other(input$d, input$e, input$f, (100-temp$percent)), 
          col = "red", lwd = 2)  
    xstar = (input$e - input$b + 200*input$d)/2/(input$a+input$d)
    lines(c(xstar, xstar), 
          c(0, max(Learning(input$a, input$b, input$c, input$d, input$e, input$f, xstar))))
    
    MBs = MB(input$a, input$b, temp$percent)
    MCs = MC(input$d, input$e, (100-temp$percent))
    plot(c(0, 100), range(c(MBs, MCs)), type = "n", 
         main = "Marginal Benefit and Cost of Note Taking", cex.main = 2,
         xlab = "Time taking notes", cex.axis = 2,
         ylab = "Learning / Time taking notes", cex.lab = 2
    )
    lines(temp$percent, MBs, col = "blue", lwd = 2)
    lines(temp$percent, MCs, col = "red", lwd = 2)
    lines(c(xstar, xstar), 
          c(min(c(MBs, MCs)), max(c(MBs, MCs))))
    
  }, width = 900, height = 900)
  
})