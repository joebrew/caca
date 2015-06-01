
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source('read_in.R')
shinyServer(function(input, output) {
  
  selected <- reactive({
    df[which(df$type == input$var),]
  })

  
  output$timeline <- renderGvis({
    
    z <- df
    z$duration <- as.character(z$Duration)
    z$duration[which(z$duration == '0')] <- ''
    x <- gvisTimeline(data = z,
                 rowlabel = 'Type',
                 barlabel = 'duration',
                 start = 'start',
                 end = 'end',
                 options=list(#timeline="{groupByRowLabel:false}",
                              backgroundColor='#ffd', 
                              height=350,
                              colors="['#cbb69d', '#603913', '#c69c6e']"))
    
  })
  
  output$table1 <- renderDataTable({

    temp <- selected()
    temp$Day <- temp$day
    temp <- temp[,c('Type', 
                   'Day',  
                   'Time', 
                   'Duration', 
                   'Comment', 
                   'Pump')]
    temp

  })
  
  output$table2 <- renderTable({
    temp <- selected()
    temp$Day <- temp$day
    temp <- temp[,c('Type',
                    'type',
                    'Day',  
                    'Time', 
                    'Duration', 
                    'Comment', 
                    'Pump')]
    temp <- temp %>%
      group_by(Day, type) %>%
      summarise(Minutes = sum(Duration, na.rm = TRUE),
                Times = n())
    temp$Day <- as.character(temp$Day)
    temp
  }, include.rownames = FALSE)

})
