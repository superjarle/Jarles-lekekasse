#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define the function outside of the ui and server
C <- function(a, b, w, r, q, total){
  (((a/b)^(b/total) + (a/b)^(-a/total)) * w^(a/total) * r^(b/total) * q^(1/total))
}

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Cobb-Douglas Cost Functions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("Cost Function Parameters"),
      p("Set the parameters below for the production function"),
      helpText("$$q=l^{\alpha}k^{\beta}$$"),
      # Input: Slider for f ----
      sliderInput(inputId = "alpha",
                  label = "Fixed Costs, alpha",
                  min = 0,
                  max = 2,
                  value = 0.5, step=0.25),
      
      # Input: Slider for a ----
      sliderInput(inputId = "beta",
                  label = "Scale Parameter, beta",
                  min = 0,
                  max = 2,
                  value = 0.5, step=0.25),
      
      # Input: Slider for b ----
      sliderInput(inputId = "w",
                  label = "Price of labor, w",
                  min = 0.0,
                  max = 2.5,
                  value = 1.5, step=0.5),
      
      # Input: Slider for b ----
      sliderInput(inputId = "r",
                  label = "Price of capital, r",
                  min = 0.0,
                  max = 2.5,
                  value = 1.5, step=0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot"),
      p("Wage", uiOutput("wage"), align="center"),
      p("Alpha and Beta", uiOutput("alpha_beta"), align="center"),
      p("This model is coded by",
        a("Jarle Kvile",
          href="https://github.com/superjarle"))
    )
  )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Calculate total here
  total <- reactive({
    input$alpha + input$beta
  })
  
  output$plot <- renderPlotly({
    
    # total cost function
    C_func <- function(q){C(input$alpha, input$beta, input$w, input$r, q, total())}
    
    df <- data.frame(x = seq(0,10, length.out = 1000))
    df$y <- C_func(df$x)
    
    plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Cost Function", 
             xaxis = list(title = "Output (q)"),
             yaxis = list(title = "Costs"))
  })
  
  output$wage<-renderUI({
    w <- reactive(input$w)
    FC <- "$$wage=%.0f$$"
    text <- sprintf(FC, w())
    withMathJax(  
      tags$p(text)
    )
  })
  
  output$alpha_beta <- renderUI({
    alpha <- reactive(input$alpha)
    beta <- reactive(input$beta)
    text <- sprintf("$$\\alpha=%.2f, \\beta=%.2f$$", alpha(), beta())
    withMathJax(
      tags$p(text)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
