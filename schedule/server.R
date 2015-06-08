
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
    z$lab <- paste0(z$Type, ' ',
                    ifelse(z$Duration > 0,
                           paste(z$Duration, 'minutes'),
                           ''))
    z$duration <- as.character(z$Duration)
    z$duration[which(z$duration == '0')] <- ''
    gvisTimeline(data = z,
                 rowlabel = 'type',
                 barlabel = 'lab',
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
    
    # Breastfeed times
    if(input$var == 'Breastfeed'){
      temp$apart <- NA
      for (i in 2:nrow(temp)){
        temp$apart[i] <- 
          temp$time3[i] - temp$time3[(i-1)]
      }
      
      # remove those less less than one hour
      temp <- temp[which(temp$apart >= 1 | is.na(temp$apart)),]

      
    }
    
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
