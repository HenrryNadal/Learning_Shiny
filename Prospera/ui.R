library(shiny)


# Define UI for application that draws a histogram
shinyUI(navbarPage(img(src="prospera.png", height =35, width =100),
                        tabPanel("Con Corresponsabilidad", 
                                 fluidPage(
                                         
                                         plotOutput("map"),
                                         
                                         column(3,selectInput("select1", label=h4("Que desea ver?"),
                                                     choices = list("Apoyos Emitidos"=1, "Familias"=2,
                                                                    "Municipios"=3, "Localidades"=4,
                                                                    "Integrantes"=5, "Becarios"=6,
                                                                    "Adultos Mayores"=7, "Infantil"=8 ))),
                                         
                                         column(3,radioButtons("radio", label = h4("Nivel de desagrecacin"),
                                                      choices = list("Nacional"=1, "Estatal"=2, "Municipal"=3, "Localidad"=4),
                                                      selected = 2)), 
                                         
                                         column(12,fluidRow("Tabla",
                                                  DT::dataTableOutput("table"))),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         
                                         column(12,fluidRow("Grafica", plotOutput("plot1")))
                                         
                                         
                                         
                                          
                                         
                                         
                                 )), 
                        tabPanel("Sin Corresponsabilidad"), 
                        tabPanel("Transito"),
                   
                        br(),
                        p("Designed by Henrry Nadal")
                   
                        )
        )

        
  
        