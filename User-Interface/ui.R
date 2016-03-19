library(shiny)


shinyUI(fluidPage(
        titlePanel("My Shiny App"),
        sidebarLayout(
                
                sidebarPanel(h1("Installation"),
                             p("Shiny is available on CRAN, so you can install it
                               in the usual way from R console:"),
                             br(),
                             code('install.packages("shiny")'),
                             br(),
                             br(),
                             p(img(src="bigorb.png", height = 50, width = 50),
                               "shiny is a product of", span("Rstudio", style = "color:blue"))        
                ),
                

                mainPanel(h1(strong("Indroduction Shiny")),
                          
                          p("Shiny is a new package from Rstudio that makes it",
                            em("incredibly easy"), "to build interactive web applications
                            with R."),
                            br(),
                            "For an introduction and live examples, visit the", a(href="http://shiny.rstudio.com", "Shiny homepage."),
                            br(),
                            br(),
                            h2(strong("Features")),
                            br(),
                            tags$div(
                                  tags$li("Build useful web applications with only a few of code - no JavaScript required."),
                                  tags$li('Shiny applications are automatically "live" in the same way that', strong("spreadsheets"),
                                  "are live. Outputs change instantly as users modify inputs, without requering a reload of the browser.")
                                  

                          )
                            
                          )
                
                        )
                )
)
        