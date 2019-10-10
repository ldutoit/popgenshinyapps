require(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Genetic drift"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "N",
                  label = "Population size",
                  min = 2,
                  max = 10000,
                  value = 100)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h6("This is a simple model of genetic drift starting with 10 alleles at frequency = 0.5. Experience how the size of the population limits the loss of genetic diversity by limiting the effect of random sampling."),
    
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )  
    )
  )


server <- function(input, output) {
  

  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change

  output$distPlot <- renderPlot({
    
    N<-input$N

    plot(1,0, type="n", xlim=c(1,100), ylim=c(0,1), xlab="Generations", ylab="frequency")
    for (Nallele in 1:10){
      alleles <- c(rep("A",N/2), rep("a",N/2))
      traj<-rep(NA,100)
    for(i in 1:100){
      alleles <- sample(alleles, N, replace=TRUE)
      traj[i]<-length(alleles[alleles=="A"])
    }
      lines(1:100,traj/N, pch=19, col=rainbow(10)[Nallele],cex=0.5,type="b")
      
    }
    
  })
  
}
shinyApp(ui = ui, server = server)
