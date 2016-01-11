
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
  #---------Show Image-------------------#
  #set up boolean to keep default image, and delete temp image
  del_bool <- FALSE
    output$imgPlot <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_imgPlot_width
    height <- session$clientData$output_imgPlot_height
    pixelratio <- session$clientData$pixelratio
    
    if(is.null(input$image_file)){
      image_file <- "testImage/heStain.jpg"
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
  
  #--------Show Palette-----------#
  image_jpeg <- reactive({
      image_in <- input$image_file
      if (is.null(image_in)){
        
        image_in <- readJPEG("testImage/heStain.jpg")
      } else {
        message(image_in)
        image_in <- readJPEG(image_in$datapath)
        
      }
      image_in
  })  
    
  output$palettePlot <- renderImage({
      width  <- session$clientData$output_palettePlot_width
      height <- session$clientData$output_palettePlot_height
      pixelratio <- session$clientData$pixelratio
      
      #get palette for image
      image_jpg <- image_jpeg()
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.png')
      
      if(is.null(image_jpg)){
        # Generate the PNG
        png(outfile, width=width, height=height)
        scales::show_col(rainbow(input$bins))
        #scales::show_col(RImagePalette::image_palette(image_jpeg(), n=input$bins))
        dev.off()
      } else {
        png(outfile, width=width, height=height)
        #scales::show_col(rainbow(input$bins))
        scales::show_col(RImagePalette::image_palette(image_jpg, n=input$bins))
        dev.off()
      }
      message(list.files())
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = width
           )
    }, deleteFile = TRUE)  
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    

  })



})
