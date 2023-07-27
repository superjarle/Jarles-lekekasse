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
library(shinythemes)

C <- function(alpha, beta, w, r, q){
  total = alpha + beta
  (((alpha/beta)^(beta/total) + (alpha/beta)^(-alpha/total)) * w^(alpha/total) * r^(beta/total) * q^(1/total))
}

# Define the Cobb-Douglas production function
Cobb_Douglas <- function(A, alpha, beta, L, K){
  A * L^alpha * K^beta
}

# Define UI
ui <- fluidPage(
  
  theme = shinytheme("united"), # Add a theme
  
  # Application title
  titlePanel("Cobb-Douglas Cost and Production Functions"),
  
  # Define tabs for different sections
  tabsetPanel(
    tabPanel("Cost Function", 
             sidebarLayout(
               sidebarPanel(
                 h4("Cost Function Parameters", align = "center"),
                 p("Set the parameters below for the production function", align = "center"),
                 helpText("$$q=l^{\alpha}k^{\beta}$$", align = "center"),
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
                             label = "Output quantity, q",
                             min = 0,
                             max = 100,
                             value = 50, step=5)
               ),
               mainPanel(
                 plotlyOutput("cost_plot"),
                 p("Cost for q", uiOutput("cost_q"), align="center"),
                 p("Alpha and Beta", uiOutput("alpha_beta_cost"), align="center"),
                 p("This model is coded by",
                   a("Jarle Kvile",
                     href="https://github.com/superjarle"), align="center")
               )
             )),
    tabPanel("Production Function",
             sidebarLayout(
               sidebarPanel(
                 h4("Production Function Parameters", align = "center"),
                 p("Set the parameters below for the production function", align = "center"),
                 helpText("$$Y=A*L^{\alpha}*K^{\beta}$$", align = "center"),
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
                 plotlyOutput("utility_plot"),
                 p("Total Production (Y)", uiOutput("total_production"), align="center"),
                 p("Returns to scale", uiOutput("returns_to_scale"), align="center"),
                 p("Alpha and Beta for Production Function", uiOutput("alpha_beta_prod"), align="center")
               )
             ))
  )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  total_cost <- reactive({
    input$alpha_cost + input$beta_cost
  })
  
  total_prod <- reactive({
    input$alpha_prod + input$beta_prod
  })
  
  output$cost_plot <- renderPlotly({
    C_func <- function(q){C(input$alpha_cost, input$beta_cost, input$w, input$r, q)}
    df <- data.frame(x = seq(0,10, length.out = 1000))
    df$y <- C_func(df$x)
    plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Cost Function", 
             xaxis = list(title = "Output (q)"),
             yaxis = list(title = "Costs"))
  })
  
  output$utility_plot <- renderPlotly({
    Cobb_func <- function(L){Cobb_Douglas(input$A, input$alpha_prod, input$beta_prod, L, input$K)}
    df <- data.frame(x = seq(0,10, length.out = 1000))
    df$y <- Cobb_func(df$x)
    plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Production Function", 
             xaxis = list(title = "Labour Input (L)"),
             yaxis = list(title = "Total Production (Y)"))
  })
  
  output$cost_q <- renderUI({
    # get reactive values
    alpha <- input$alpha_cost
    beta <- input$beta_cost
    w <- input$w
    r <- input$r
    q <- input$q
    # calculate cost
    cost_q = C(alpha, beta, w, r, q)
    cost_text <- sprintf("Cost for q=%.2f: %.2f", q, cost_q)
    withMathJax(tags$p(cost_text))
  })
  
  output$alpha_beta_cost <- renderUI({
    text <- sprintf("$$\\alpha_{cost}=%.2f, \\beta_{cost}=%.2f$$", input$alpha_cost, input$beta_cost)
    withMathJax(tags$p(text))
  })
  
  output$total_production <- renderUI({
    # get reactive values
    A <- input$A
    alpha <- input$alpha_prod
    beta <- input$beta_prod
    L <- input$L
    K <- input$K
    # calculate total production
    total_production = Cobb_Douglas(A, alpha, beta, L, K)
    total_production_text <- sprintf("Total Production: %.2f", total_production)
    withMathJax(tags$p(total_production_text))
  })
  
  output$alpha_beta_prod <- renderUI({
    text <- sprintf("$$\\alpha_{util}=%.2f, \\beta_{util}=%.2f$$", input$alpha_prod, input$beta_prod)
    withMathJax(tags$p(text))
  })
  
  # Compute the returns to scale
  output$returns_to_scale <- renderUI({
    if (total_prod() > 1) {
      text <- "Increasing returns to scale"
    } else if (total_prod() < 1) {
      text <- "Decreasing returns to scale"
    } else {
      text <- "Constant returns to scale"
    }
    withMathJax(tags$p(text))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
