
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RImagePalette)
library(png);library(jpeg)
library(scales)

shinyServer(function(input, output, session) {
  #set up boolean to keep default image, and delete temp image
  del_bool <- FALSE
  
  output$imgPlot <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_imgPlot_width
    height <- session$clientData$output_imgPlot_height
    pixelratio <- session$clientData$pixelratio
    
    if(is.null(input$image_file)){
      image_file <- "testImage/heStain.JPG"
    } else {
      image_file <- input$image_file$datapath
      del_bool <- TRUE
    }
    
    message(del_bool)
    list(src = image_file,
         contentType = 'image/jpeg',
         width = width
         )
  }, deleteFile = del_bool)
  
  
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    

  })



})
