
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Ramona Andalucia Brew Farre"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  "Variable:",
                  c('Attempt',
                    'Caca',
                    #'Feed from pump',
                    'Breastfeed',
                    'Pipi',
                    'Pump no feed'))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      #h3('Everything'),
      #htmlOutput("timeline"),
      h3('Daily summary'),
      tableOutput('table2'),
      h3('Detailed summary'),
      dataTableOutput("table1")
    )
  )
))
