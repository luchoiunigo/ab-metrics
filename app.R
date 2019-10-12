library(shiny)
library(shinydashboard)
library(markdown)
library(utf8)

# UDF
# calulate 
zeta <- function(p1,p2,n1,n2,z){
  
  p = (p1*n1 + p2*n2)/(n1+n2)
  z_est = (p2-p1)/sqrt(p*(1-p)*((1/n1) + (1/n2))) - z
  
  return(z_est)
  
}


# UI
ui <- dashboardPage(
  dashboardHeader(title = "AB Metrics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PRE Experimeto", tabName = "pre_test"),
      menuItem("POST Experimento", tabName = "post_test")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("pre_test",
              fluidRow(
                column(width = 11, offset = 1,
                  h2("Ayuda en la definicion del experimento"),
                  p("Este apartado sirve para ayudar a definir lo siguiente antes de realizar el experimento:"),
                  tags$li("Dados los dos tamaños de las muestras, cual es el mimino cvr esperado del experimento para considerarlo como estadisticamente exitoso, bajo un nivel de confianza elegido"),
                  tags$li("Definido una diferencia entre cvr a la que se quiere llegar, cual es el n de la muestra del experimento que lo hace posible, bajo un nivel de confianza elegido"),
                  
                  h3("Behind the scenes"),
                  p("Se esta haciendo el siguiente test de hipotesis de proporciones:"),
                  tags$li('Ho ---> p1 = p2'),
                  tags$li('Ha ---> p1 < p2')
                  
                ),
                column(width = 12, offset = 0
                       
                       #withMathJax(),
                       #uiOutput("ab_h0_formula"),
                       #uiOutput("ab_h1_formula")
                )
              ),
              br(),
              fluidRow(
                shinydashboard::box(
                  width = 6, status = "info", solidHeader = FALSE,
                  title = "Calcular minimo cvr para que el experimento sea estadisticamente exitoso",
                  numericInput(inputId='cvr_n1', label="n1", value=1000),
                  numericInput(inputId='cvr_n2', label="n2", value=1000),
                  numericInput(inputId='cvr_p1', label="p1", value=0.5),
                  numericInput(inputId='cvr_confidence', label="Confiabilidad", value=0.95),
                  actionButton(inputId = 'cvr_calculate', label = "Calcular")
                ),
                shinydashboard::box(
                  width = 6, status = "info", solidHeader = FALSE,
                  title = "Calcular el n para garantizar una diferencia entre cvr estadisticamente exitosa",
                  numericInput(inputId='n_n1', label="n1", value=1000),
                  numericInput(inputId='n_p1', label="p1", value=0.5),
                  numericInput(inputId='n_p2', label="p2", value=0.5),
                  numericInput(inputId='n_confidence', label="Confiabilidad", value=0.95),
                  actionButton(inputId = 'n_calculate', label = "Calcular")
                )                
              ),
              fluidRow(
                shinydashboard::box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Resultado",
                  valueBoxOutput("cvr_result")
                ),
                shinydashboard::box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Resultado",
                  valueBoxOutput("n_result")
                )
              )
      ),
      tabItem("post_test",
              fluidRow(
                column(width = 11, offset = 1,
                       h2("Experimento finalizado"),
                       p("Este apartado sirve para entender los resultados del experimento:"),
                       tags$li("Dados los dos tamaños de las muestras, los dos resultados de conversion de cada muestra y el nivel de confianza elegido, se calcula si los resultados son estadisticamente significativos para garantizar que el cvr del experimento es mayor al cvr del grupo de control")
                )
              ),
              br(),
              fluidRow(
                shinydashboard::box(
                  width = 12, status = "info", solidHeader = FALSE,
                  title = "Calcular si la diferencia de cvr es estadisticamente significativa",
                  numericInput(inputId='ab_n1', label="n1", value=1000),
                  numericInput(inputId='ab_n2', label="n2", value=1000),
                  numericInput(inputId='ab_p1', label="p1", value=0.5),
                  numericInput(inputId='ab_p2', label="p2", value=0.5),
                  numericInput(inputId='ab_confidence', label="Confiabilidad", value=0.95),
                  actionButton(inputId = 'ab_calculate', label = "Calcular")
                ),
                shinydashboard::box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Resultado",
                  valueBoxOutput("ab_result")
                )
      )
    )
  )
)
)


# SERVER
server <- function(input,output,session){
  
  # disply Hypothesis Test formula
  output$ab_h0_formula <- renderUI({
    #withMathJax("$$\\{H}_{\\small{\\textrm{0}}} = {H}_{\\small{\\textrm{1}}} $$")
    print("$$H_{\\small{\\textrm{0}}} = H_{\\small{\\textrm{1}}}$$")
  })
  
  output$ab_h1_formula <- renderUI({
    #withMathJax("$$\\{H}_{\\small{\\textrm{0}}} = {H}_{\\small{\\textrm{1}}} $$")
    print("$$H_{\\small{\\textrm{0}}} < H_{\\small{\\textrm{1}}}$$")
  })
  
  # CVR calculation
  cvr_values <- eventReactive(input$cvr_calculate,{
    data = data.frame(n1=c(input$cvr_n1), n2=c(input$cvr_n2), p1=c(input$cvr_p1), confidence=c(input$cvr_confidence))
    return(data)
  })
  
  # CVR Result
  output$cvr_result <- renderValueBox({
    
    data <- cvr_values()
    
    n1 <- as.numeric(data$n1)
    n2 <- as.numeric(data$n2)
    p1 <- as.numeric(data$p1)
    confidence <- as.numeric(data$confidence)
    z <- qnorm(confidence)
    
    root <- round(uniroot(function(x) zeta(p1,x,n1,n2,z), interval = c(0,1))$root,5)
    subtitle <- "Minimo CVR para que el experimento sea exitoso"
    
    valueBox(root, subtitle, icon = NULL, color = "aqua", width = NULL, href = NULL)
    
  })
  
  # n calculation
  n_values <- eventReactive(input$n_calculate,{
    data = data.frame(n1=c(input$n_n1), p1=c(input$n_p1), p2=c(input$n_p2), confidence=c(input$n_confidence))
    return(data)
  })
  
  # n Result
  output$n_result <- renderValueBox({
    
    data <- n_values()
    
    n1 <- as.numeric(data$n1)
    p1 <- as.numeric(data$p1)
    p2 <- as.numeric(data$p2)
    confidence <- as.numeric(data$confidence)
    z <- qnorm(confidence)
    
    root <- round(uniroot(function(x) zeta(p1,p2,n1,x,z), interval = c(0,n1))$root,0)
    subtitle <- "Minimo n para garantizar la diferencia de cvr"
    
    valueBox(root, subtitle, icon = NULL, color = "aqua", width = NULL, href = NULL)
    
  })
  

  # AB calculation
  ab_values <- eventReactive(input$ab_calculate,{
    data = data.frame(n1=c(input$ab_n1), n2=c(input$ab_n2), p1=c(input$ab_p1), p2=c(input$ab_p2), confidence=c(input$ab_confidence))
    return(data)
  })
  
  # AB Result
  output$ab_result <- renderValueBox({
    
    data <- ab_values()
    
    n1 <- as.numeric(data$n1)
    n2 <- as.numeric(data$n2)
    p1 <- as.numeric(data$p1)
    p2 <- as.numeric(data$p2)
    confidence <- as.numeric(data$confidence)
    z <- qnorm(confidence)
    
    root <- zeta(p1,p2,n1,n2,z)
    
    if (root < 0){
      
      color = 'red'
      value <- "El experimento fallo   \U00002620"
      subtitle <- "Los datos no son suficientes para sacar una conclusion posotiva"
      
    }else {
      
      color = 'green'
      value <- utf8_print("Experimento exitoso   \U0001f64c")
      subtitle <- " "
      
    }
    
    valueBox(value, subtitle, icon = NULL, color = color, width = NULL, href = NULL)
    
  })
  

}


shinyApp(ui, server)
