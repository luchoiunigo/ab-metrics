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
      menuItem("POST Experimento", tabName = "post_test",
               menuSubItem("Frecuentista", tabName = "frec_test"),
               menuSubItem("Bayes", tabName = 'bayes_test'))
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
                  p("El problema que se quiere resolver es el del siguiente tipo: "),
                  br()
                ),
                column(width=5, offset = 3,
                       htmlOutput("freq_formula_pre_test"),
                       br()
                ),
                column(width = 11, offset = 1,
                       p("donde CONTROL es la data que no esta expuesta al cambio, y EXPERIMENTO es la data que si tuvo modificaciones"),
                       
                       br(),
                       p("Primero se debe elegir que tipo de problema se quiere resolver, luego completar con los datos indicados:")
                       #tags$li('Ho ---> p1 = p2'),
                       #tags$li('Ha ---> p1 < p2')
                       
                ),
                column(width = 12, offset = 0
                       
                       #withMathJax(),
                       #uiOutput("ab_h0_formula"),
                       #uiOutput("ab_h1_formula")
                )
              ),
              br(),
              fluidRow(box(#title = "Elegir el tipo de problema",
                           width = 12,
                           status = "warning",
                           solidHeader = FALSE,
                           radioButtons(inputId = "problem_type_pre",
                                        label = "Elegir tipo de problema",
                                        inline = FALSE,
                                        choices = c(
                                               "P(control) < P(experimento)" = "greater",
                                               "P(control) > P(experimento)" = "lower"
                                                )
                                        )
                           )
                       ),
              #br(),
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
      tabItem("frec_test",
              fluidRow(
                column(width = 11, offset = 1,
                       h2("Experimento finalizado"),
                       p("Este apartado sirve para entender los resultados del experimento desde un enfoque frecuentista:"),
                       tags$li("Dados los dos tamaños de las muestras, los dos resultados de conversion de cada muestra y el nivel de confianza elegido, se calcula si los resultados son estadisticamente significativos para garantizar que el cvr del experimento es mayor  (o menor) al cvr del grupo de control"),
                       h3("Behind the scenes"),
                       p("El problema que se quiere resolver es el del siguiente tipo: "),
                       br()
                ),
                column(width=5, offset = 3,
                       htmlOutput("freq_formula_post_test"),
                       br()
                ),
                column(width = 11, offset = 1,
                       p("donde CONTROL es la data que no esta expuesta al cambio, y EXPERIMENTO es la data que si tuvo modificaciones"),
                       
                       br(),
                       p("Primero se debe elegir que tipo de problema se quiere resolver, luego completar con los datos indicados:")
                       #tags$li('Ho ---> p1 = p2'),
                       #tags$li('Ha ---> p1 < p2')
                       
                )
              ),
              br(),
              fluidRow(box(#title = "Elegir el tipo de problema",
                width = 12,
                status = "warning",
                solidHeader = FALSE,
                radioButtons(inputId = "problem_type_freq_post",
                             label = "Elegir tipo de problema",
                             inline = FALSE,
                             choices = c(
                               "P(control) < P(experimento)" = "greater",
                               "P(control) > P(experimento)" = "lower"
                             )
                )
              )
              ),
             # br(),
              fluidRow(
                shinydashboard::box(
                  width = 12, status = "info", solidHeader = FALSE,
                  title = "Calcular si la diferencia de cvr es estadisticamente significativa",
                  numericInput(inputId='freq_ab_n1', label="n1", value=1000),
                  numericInput(inputId='freq_ab_n2', label="n2", value=1000),
                  numericInput(inputId='freq_ab_p1', label="p1", value=0.5),
                  numericInput(inputId='freq_ab_p2', label="p2", value=0.5),
                  numericInput(inputId='freq_ab_confidence', label="Confiabilidad", value=0.95),
                  actionButton(inputId = 'freq_ab_calculate', label = "Calcular")
                ),
                shinydashboard::box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Resultado",
                  valueBoxOutput("freq_ab_result")
                )
      )
    ),
    tabItem("bayes_test",
            fluidRow(
              column(width = 11, offset = 1,
                     h2("Experimento finalizado"),
                     p("Este apartado sirve para entender los resultados del experimento desde un enfoque frecuentista:"),
                     tags$li("Dados los dos tamaños de las muestras, los dos resultados de conversion de cada muestra y el nivel de confianza elegido, se calcula si los resultados son estadisticamente significativos para garantizar que el cvr del experimento es mayor  (o menor) al cvr del grupo de control"),
                     h3("Behind the scenes"),
                     p("El problema que se quiere resolver es el del siguiente tipo: "),
                     br()
              ),
              column(width=11, offset = 1,
                     htmlOutput("bayes_formula_post_test"),
                     br()
              ),
              column(width = 11, offset = 1,
                     p("donde CONTROL es la data que no esta expuesta al cambio, y EXPERIMENTO es la data que si tuvo modificaciones y B(a,b) es la distribucion Beta. Lo que retorna este calculo es la probabilidad de que la media del experimento sea mayor a la media de control"),
                     
                     br(),
                     p("Primero se debe elegir que tipo de problema se quiere resolver, luego completar con los datos indicados:")
                     #tags$li('Ho ---> p1 = p2'),
                     #tags$li('Ha ---> p1 < p2')
                     
              )
            ),
            br(),
            fluidRow(box(#title = "Elegir el tipo de problema",
              width = 12,
              status = "warning",
              solidHeader = FALSE,
              radioButtons(inputId = "problem_type_bayes_post",
                           label = "Elegir tipo de problema",
                           inline = FALSE,
                           choices = c(
                             "P(control) < P(experimento)" = "greater",
                             "P(control) > P(experimento)" = "lower"
                           )
              )
            )
            ),
            # br(),
            fluidRow(
              shinydashboard::box(
                width = 12, status = "info", solidHeader = FALSE,
                title = "Calcular si la diferencia de cvr es estadisticamente significativa",
                numericInput(inputId='bayes_ab_n1', label="n1", value=1000),
                numericInput(inputId='bayes_ab_n2', label="n2", value=1000),
                numericInput(inputId='bayes_ab_p1', label="p1", value=0.5),
                numericInput(inputId='bayes_ab_p2', label="p2", value=0.5),
                numericInput(inputId='bayes_ab_confidence', label="Confiabilidad", value=0.95),
                actionButton(inputId = 'bayes_ab_calculate', label = "Calcular")
              ),
              shinydashboard::box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Resultado",
                valueBoxOutput("bayes_ab_result")
              )
            )
    ),
    tabItem("debug",
            textOutput(outputId = "text_debug"))
    
  )
)
)


# SERVER
server <- function(input,output,session){
  
  # display Hypothesis Test formula
  output$ab_h0_formula <- renderUI({
    #withMathJax("$$\\{H}_{\\small{\\textrm{0}}} = {H}_{\\small{\\textrm{1}}} $$")
    print("$$H_{\\small{\\textrm{0}}} = H_{\\small{\\textrm{1}}}$$")
  })
  
  output$ab_h1_formula <- renderUI({
    #withMathJax("$$\\{H}_{\\small{\\textrm{0}}} = {H}_{\\small{\\textrm{1}}} $$")
    print("$$H_{\\small{\\textrm{0}}} < H_{\\small{\\textrm{1}}}$$")
  })
  
  getPage<-function(page) {
    return(includeHTML(page))
  }
  output$freq_formula_pre_test<-renderUI({getPage("formulas/freq_formula.html")})
  output$freq_formula_post_test<-renderUI({getPage("formulas/freq_formula.html")})
  output$bayes_formula_post_test<-renderUI({getPage("formulas/bayes_formula.html")})
  
  
  # CVR calculation
  cvr_values <- eventReactive(input$cvr_calculate,{
    
    if(input$problem_type_pre == "greater"){
      
      cvr_confidence <- input$cvr_confidence
      
    }else if(input$problem_type_pre == "lower"){
      
      cvr_confidence = 1 - input$cvr_confidence
      
    }
    
    data = data.frame(n1=c(input$cvr_n1), n2=c(input$cvr_n2), p1=c(input$cvr_p1), confidence=c(cvr_confidence))
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
    
    if(input$problem_type_pre == "greater"){
      
      n_confidence <- input$n_confidence
      
    }else if(input$problem_type_pre == "lower"){
      
      n_confidence = 1 - input$n_confidence
      
    }    
    
    data = data.frame(n1=c(input$n_n1), p1=c(input$n_p1), p2=c(input$n_p2), confidence=c(n_confidence))
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
  

  # Freq AB calculation
  freq_ab_values <- eventReactive(input$freq_ab_calculate,{
    
    if(input$problem_type_freq_post == "greater"){
      
      ab_confidence <- input$freq_ab_confidence
      
    }else if(input$problem_type_freq_post == "lower"){
      
      ab_confidence = 1 - input$freq_ab_confidence
      
    }        
    
    data = data.frame(n1=c(input$freq_ab_n1), n2=c(input$freq_ab_n2), p1=c(input$freq_ab_p1), p2=c(input$freq_ab_p2), confidence=c(ab_confidence))
    
    return(data)
    
  })
  
  # Freq AB Result
  output$freq_ab_result <- renderValueBox({
    
    data <- freq_ab_values()
    
    n1 <- as.numeric(data$n1)
    n2 <- as.numeric(data$n2)
    p1 <- as.numeric(data$p1)
    p2 <- as.numeric(data$p2)
    confidence <- as.numeric(data$confidence)
    z <- qnorm(confidence)
    
    root <- zeta(p1,p2,n1,n2,z)
    
    if (input$problem_type_freq_post == 'greater'){
      if (root < 0){
        
        color = 'red'
        value <- "El experimento fallo   \U00002620"
        subtitle <- "Los datos no son suficientes para sacar una conclusion positiva"
        
      }else {
        
        color = 'green'
        value <- utf8_print("Experimento exitoso   \U0001f64c")
        subtitle <- " "
        
      }
    }else if (input$problem_type_freq_post == 'lower'){
      if (root > 0){
        
        color = 'red'
        value <- "El experimento fallo   \U00002620"
        subtitle <- "Los datos no son suficientes para sacar una conclusion positiva"
        
      }else {
        
        color = 'green'
        value <- utf8_print("Experimento exitoso   \U0001f64c")
        subtitle <- " "
        
      }     
      
    }
    
    valueBox(value, subtitle, icon = NULL, color = color, width = NULL, href = NULL)
    
  })
  
  # Bayes AB calculation
  bayes_ab_values <- eventReactive(input$bayes_ab_calculate,{
    
    data = data.frame(n1=c(input$bayes_ab_n1), n2=c(input$bayes_ab_n2), p1=c(input$bayes_ab_p1), p2=c(input$bayes_ab_p2), confidence=c(input$bayes_ab_confidence))
    
    return(data)
    
  })
  
  # Bayes AB Result
  output$bayes_ab_result <- renderValueBox({
    
    data <- bayes_ab_values()
    
    n1 <- as.numeric(data$n1)
    n2 <- as.numeric(data$n2)
    p1 <- as.numeric(data$p1)
    p2 <- as.numeric(data$p2)
    confidence <- as.numeric(data$confidence)
    
    if (input$problem_type_bayes_post == 'greater'){
      a_A <- round(p1*n1,0) + 1
      b_A <- n1 - round(p1*n1,0) + 1
      a_B <- round(p2*n2,0) + 1
      b_B <- n2 - round(p2*n2,0) + 1
      
    } else if (input$problem_type_bayes_post == 'lower'){
      a_B <- round(p1*n1,0) + 1
      b_B <- n1 - round(p1*n1,0) + 1
      a_A <- round(p2*n2,0) + 1
      b_A <- n2 - round(p2*n2,0) + 1
      
      
    }
    
    total <- 0
    
    for (i in 0:(a_B-1) ) {
      total <- total + exp(lbeta(a_A+i, b_B+b_A)
                           - log(b_B+i)
                           - lbeta(1+i, b_B)
                           - lbeta(a_A, b_A))
      
    }
    print(a_A)
    print(b_A)
    print(a_B)
    print(b_B)
    print(total)
    print(confidence)
    print(1-total)
    
    
    if (input$problem_type_bayes_post == 'greater'){
      if (total < confidence){
        
        color = 'red'
        value <- "El experimento fallo   \U00002620"
        subtitle <- "Los datos no son suficientes para sacar una conclusion positiva"
        
      }else {
        
        color = 'green'
        value <- utf8_print("Experimento exitoso   \U0001f64c")
        subtitle <- " "
        
      }
    }else if (input$problem_type_bayes_post == 'lower'){
      if (1-total > confidence){
        
        color = 'red'
        value <- "El experimento fallo   \U00002620"
        subtitle <- "Los datos no son suficientes para sacar una conclusion positiva"
        
      }else {
        
        color = 'green'
        value <- utf8_print("Experimento exitoso   \U0001f64c")
        subtitle <- " "
        
      }     
      
    }
    
    valueBox(value, subtitle, icon = NULL, color = color, width = NULL, href = NULL)
    
  })

}


shinyApp(ui, server)
