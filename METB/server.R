library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive function to compose a data frame containing all of the values
  # I'm going to try and ignore this function for now
  Inputs <- reactive(function() {
    
    # Compose data frame
    data.frame(
      Name = c("Initial Price", 
               "Initial Quantity",
               "Elasticity of Demand",
               "Elasticity of Supply"),
      Value = c(input$P0,
                input$Q0,
                input$Ed,
                input$Es), 
      stringsAsFactors=FALSE,
      row.names = c("P0", "Q0", "Ed", "Es"))
  }) 
  
  # Show the values using an HTML table
  output$parameters <- reactiveTable(function() {
    temp = Inputs()
    #rownames(temp) = NULL
    temp$Value = as.character(temp$Value)
    temp
  })
  
  # Calculate the Linearized Demand and Supply Curves
  LinearValues <- reactive(function() {
    # Elasticity of Demand and Supply
    Ed = input$Ed    
    Es = input$Es
    # Initial Price and Quantity
    P0 = input$P0
    Q0 = input$Q0
    
    # Solve for linear demand and supply
    Dslope = P0/Q0/Ed
    Dintercept = P0 - Dslope*Q0
    
    Sslope = P0/Q0/Es
    Sintercept = P0 - Sslope*Q0
    
    data.frame(
      Name = c("Demand Intercept", 
               "Demand Slope",
               "Supply Intercept",
               "Supply Slope"),
      Value = c(Dintercept,
                Dslope,
                Sintercept,
                Sslope), 
      stringsAsFactors=FALSE,
      row.names = c("Di", "Ds", "Si", "Ss"))
    
  })
  
  # Show the values in an HTML table
  output$linearEquations <- reactiveTable(function() {
    temp = LinearValues()
    #rownames(temp) = NULL
    #temp$Value = as.character(temp$Value)
    temp
  })
  
  # Make a plot of the market supply and demand curves
  output$marketPlot <- reactivePlot(function() {
    temp = rbind(LinearValues(), Inputs())
    Values = temp$Value
    names(Values) = rownames(temp)
    Qmax = 1.2*Values["Q0"]
    plot(0:Qmax, Values["Di"] + Values["Ds"]*(0:Qmax),
         type = "l", lwd = 3,
         ylim = c(0, Values["Di"]),
         main = "Market Conditions",
         xlab = "Quantity of Good",
         ylab = "Price of Good ($/Q)",
         col = "blue")
    lines(0:Qmax, Values["Si"] + Values["Ss"]*(0:Qmax),
          lwd = 3,
          col = "red")
    text(c(Qmax, Qmax),
         c(Values["Di"] + Values["Ds"]*Qmax, Values["Si"] + Values["Ss"]*Qmax),
         c("Demand", "Supply"),
         col = c("blue", "red"),
         pos = 1
         )
  })
  
  TaxFunctions = reactive(function() {
    temp = rbind(LinearValues(), Inputs())
    Values = temp$Value
    names(Values) = rownames(temp)
    if (input$AllTax == "case1") myTaxes = 0:((Values["Di"] - Values["Si"])/4) else {
      if (input$AllTax == "case2") myTaxes = 0:((Values["Di"] - Values["Si"])/2) else {
        myTaxes = 0:(Values["Di"] - Values["Si"])
      }
    }
    
    qTax = (myTaxes + Values["Si"] - Values["Di"])/(Values["Ds"] - Values["Ss"])
    TaxRev = myTaxes*qTax
    Burden = TaxRev + myTaxes*(Values["Q0"]-qTax)/2
    data.frame(tax = myTaxes,
               Qtax = qTax,
               Revenue = TaxRev,
               LostPSCS = Burden)
  })
  
  # Taxes vs. New Q
  output$TaxQPlot <- reactivePlot(function() {
    TaxData = TaxFunctions()
    plot(TaxData$tax, TaxData$Qtax, type = "l",
         ylim = c(0, max(TaxData$Qtax)),
         main = "The Impact of Taxes on Equilibrium Quantity",
         xlab = "Per Unit Tax",
         ylab = "Equilibrium Market Quantity")
  })
  
  # Taxes vs Tax Revenue
  output$TaxRevPlot <- reactivePlot(function() {
    TaxData = TaxFunctions()
    plot(TaxData$tax, TaxData$Revenue, type = "l")
  })
  
  # Tax Revenue vs. Total Burden
  output$TaxBurdenPlot <- reactivePlot(function() {
    TaxData = TaxFunctions()
    plot(TaxData$Revenue, TaxData$LostPSCS, type = "l")
  })
  
  output$SummaryPlot <- reactivePlot(function() {
    par(mfrow = c(2, 2))
    TaxData = TaxFunctions()
    temp = rbind(LinearValues(), Inputs())
    Values = temp$Value
    names(Values) = rownames(temp)
    Qmax = 1.2*Values["Q0"]
    plot(0:Qmax, Values["Di"] + Values["Ds"]*(0:Qmax),
         type = "l", lwd = 3,
         ylim = c(0, Values["Di"]),
         main = "Market Conditions",
         xlab = "Quantity of Good",
         ylab = "Price of Good ($/Q)",
         col = "blue")
    lines(0:Qmax, Values["Si"] + Values["Ss"]*(0:Qmax),
          lwd = 3,
          col = "red")
    text(c(Qmax, Qmax),
         c(Values["Di"] + Values["Ds"]*Qmax, Values["Si"] + Values["Ss"]*Qmax),
         c("Demand", "Supply"),
         col = c("blue", "red"),
         pos = 1
    )
    plot(TaxData$tax, TaxData$Qtax, type = "l",
         ylim = c(0, max(TaxData$Qtax)),
         main = "The Impact of Taxes on Equilibrium Quantity",
         xlab = "Per Unit Tax",
         ylab = "Equilibrium Market Quantity")
    plot(TaxData$tax, TaxData$Revenue, type = "l")
    plot(TaxData$Revenue, TaxData$LostPSCS, type = "l")
  })
  
})