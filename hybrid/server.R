library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Calculate the Linearized Demand and Supply Curves
  HybridCalcs <- reactive(function() {
    Cost.hybrid = input$Cost.hybrid
    Cost.other = input$Cost.other
    
    Miles.driven.other = input$Miles.driven.other
    Driving.elasticity = input$Driving.elasticity
    MPG.other = input$MPG.other
    MPG.hybrid = input$MPG.hybrid
    
    Price.gas.0 = input$Price.gas.0
    Gas.growth.rate = input$Gas.growth.rate
    
    Years.analysis = input$Years.analysis
    Discount.rate = input$Discount.rate
    
    PollutionCostPerGallon = input$PollutionCostPerGallon
    
    # the calculations
    Price.gas = Price.gas.0 * (1 + Gas.growth.rate)^(1:Years.analysis)
    Price.per.mile.other = Price.gas/MPG.other
    Price.per.mile.hybrid = Price.gas/MPG.hybrid
    
    Change.miles.driven = (Price.per.mile.hybrid - Price.per.mile.other)/Price.per.mile.other * Driving.elasticity * Miles.driven.other
    Miles.driven.hybrid = Miles.driven.other + Change.miles.driven
    
    ConsumerSurplusChange = (Price.per.mile.other - Price.per.mile.hybrid) * (Miles.driven.other + Miles.driven.hybrid)/2
    CS.PV = ConsumerSurplusChange / (1 + Discount.rate)^(1:Years.analysis)
    
    GasGallonsChange = Miles.driven.hybrid/MPG.hybrid - Miles.driven.other/MPG.other
    PollutionCostChange = PollutionCostPerGallon * GasGallonsChange
    PollutionChangePV = PollutionCostChange / (1 + Discount.rate)^(1:Years.analysis)
    
    NPVcumulative = Cost.other - Cost.hybrid + cumsum(CS.PV - PollutionChangePV)
    
    data.frame(years = 1:Years.analysis, NPV = NPVcumulative)
  })
  
  # Show the values in an HTML table
  output$hybridPlot <- reactivePlot(function() {
    temp = HybridCalcs()
    par(mar = c(6, 5, 5, 1))
    par(oma = rep(0, 4))
    plot(temp$years, temp$NPV/1000, type = "l", lwd = 3,
         main = "Cost Benefit Analysis of Hybrid Purchase",
         las = 1,
         axes = F, xaxs="i", #yaxs = "i",
         xlim = c(0, max(temp$years)),
         #ylim = c(floor(min(temp$NPV/1000)), ceiling(max(temp$NPV/1000))),
         xlab = "", ylab = "", cex.main = 2)
    axis(1, cex.axis = 2, padj = .5)
    mtext("Years of Ownership", side=1, line = 4, cex = 2)
    axis(2, las = 1, cex.axis = 2)
    mtext("NPV (in $1000s)", side = 2, line = 3, cex = 2)
    abline(h = 0, col = "gray90")
  })
  
})