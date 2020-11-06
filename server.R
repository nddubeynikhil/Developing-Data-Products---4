library(shiny)
library(datasets)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
  
  frmlaTxt <- reactive({
    paste("mpg ~", input$variable)
  })
  
  frmlaTxtPnt <- reactive({
    paste("mpg ~", "as.integer(", input$variable, ")")
  })
  
  fit <- reactive({
    lm(as.formula(frmlaTxtPnt()), data=mpgData)
  })
  
  output$caption <- renderText({
    frmlaTxt()
  })
  
  output$mpgBoxPlot <- renderPlot({
    boxplot(as.formula(frmlaTxt()), 
            data = mpgData,
            outline = input$outliers)
  })
  
  output$fit <- renderPrint({
    summary(fit())
  })
  
  output$mpgPlot <- renderPlot({
    with(mpgData, {
      plot(as.formula(frmlaTxtPnt()))
      abline(fit(), col=2)
    })
  })
  
})