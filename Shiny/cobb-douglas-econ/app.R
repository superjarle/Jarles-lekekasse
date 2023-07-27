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
library(Deriv)

C <- function(alpha, beta, w, r, q){
  (((alpha/beta)^(beta/alpha) + (alpha/beta)^(-alpha/beta)) * w^(alpha/beta) * r^(beta/alpha) * q^(1/(alpha+beta)))
}

# Define the Cobb-Douglas production function
Cobb_Douglas <- function(A, alpha, beta, L, K){
  A * L^alpha * K^beta
}

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Cobb-Douglas Cost and Production Functions"),
  
  # Define tabs for different sections
  tabsetPanel(
    tabPanel("Cost Function", 
             sidebarLayout(
               sidebarPanel(
                 h4("Cost Function Parameters"),
                 p("Set the parameters below for the production function"),
                 helpText("$$q=l^{\alpha}k^{\beta}$$"),
                 sliderInput(inputId = "alpha_cost",
                             label = "Fixed Costs, alpha",
                             min = 0,
                             max = 2,
                             value = 0.5, step=0.25),
                 sliderInput(inputId = "beta_cost",
                             label = "Scale Parameter, beta",
                             min = 0,
                             max = 2,
                             value = 0.5, step=0.25),
                 sliderInput(inputId = "w",
                             label = "Price of labor, w",
                             min = 0.0,
                             max = 2.5,
                             value = 1.5, step=0.5),
                 sliderInput(inputId = "r",
                             label = "Price of capital, r",
                             min = 0.0,
                             max = 2.5,
                             value = 1.5, step=0.5),
                 sliderInput(inputId = "q",
                             label = "Output, q",
                             min = 0,
                             max = 100,
                             value = 50, step=5)
               ),
               mainPanel(
                 plotlyOutput("cost_plot"),
                 p("Cost for q", uiOutput("cost_q"), align="center"),
                 p("This model is coded by",
                   a("Jarle Kvile",
                     href="https://github.com/superjarle"))
               )
             )),
    tabPanel("Production Function",
             sidebarLayout(
               sidebarPanel(
                 h4("Production Function Parameters"),
                 p("Set the parameters below for the production function"),
                 helpText("$$Y=A*L^{\alpha}*K^{\beta}$$"),
                 sliderInput(inputId = "A",
                             label = "Total factor productivity, A",
                             min = 0,
                             max = 2,
                             value = 1, step=0.25),
                 sliderInput(inputId = "alpha_prod",
                             label = "Output elasticity of labour, alpha",
                             min = 0,
                             max = 1,
                             value = 0.5, step=0.1),
                 sliderInput(inputId = "beta_prod",
                             label = "Output elasticity of capital, beta",
                             min = 0,
                             max = 1,
                             value = 0.5, step=0.1),
                 sliderInput(inputId = "L",
                             label = "Labour input, L",
                             min = 0,
                             max = 100,
                             value = 50, step=5),
                 sliderInput(inputId = "K",
                             label = "Capital input, K",
                             min = 0,
                             max = 100,
                             value = 50, step=5)
               ),
               mainPanel(
                 plotlyOutput("prodity_plot"),
                 p("Total Production (Y)", uiOutput("total_production"), align="center"),
                 p("Alpha and Beta for Production Function", uiOutput("alpha_beta_prod"), align="center")
               )
             ))
  )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$cost_plot <- renderPlotly({
    C_func <- function(q){C(input$alpha_cost, input$beta_cost, input$w, input$r, q)}
    df <- data.frame(x = seq(0,10, length.out = 1000))
    df$y <- C_func(df$x)
    plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Cost Function", 
             xaxis = list(title = "Output (q)"),
             yaxis = list(title = "Costs"))
  })
  
  output$prodity_plot <- renderPlotly({
    Cobb_func <- function(L){Cobb_Douglas(input$A, input$alpha_prod, input$beta_prod, L, input$K)}
    df <- data.frame(x = seq(0,10, length.out = 1000))
    df$y <- Cobb_func(df$x)
    plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Production Function", 
             xaxis = list(title = "Labour Input (L)"),
             yaxis = list(title = "Total Production (Y)"))
  })
  
  output$cost_q <- renderUI({
    cost_val = C(input$alpha_cost, input$beta_cost, input$w, input$r, input$q)
    cost_text <- sprintf("Cost for q: %.2f", cost_val)
    withMathJax(tags$p(cost_text))
  })
  
  output$alpha_beta_prod <- renderUI({
    text <- sprintf("$$\\alpha_{prod}=%.2f, \\beta_{prod}=%.2f$$", input$alpha_prod, input$beta_prod)
    withMathJax(
      tags$p(text)
    )
  })
  
  output$total_production <- renderUI({
    total_prod = input$A * (input$L^input$alpha_prod) * (input$K^input$beta_prod)
    
    total_prod_text <- sprintf("Total Production (Y): %.2f", total_prod)
    
    withMathJax(tags$p(total_prod_text))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

