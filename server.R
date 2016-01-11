
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RImagePalette)
library(png);library(jpeg)
library(scales);library(stringr)

shinyServer(function(input, output, session) {
  #Function to read image from input as JPEG or PNG
  image_read <- reactive({
    image_in <- input$image_file
    if(!is.null(image_in)){
      if(str_detect(image_in$type, "jpg")){
        return(readJPEG(image_in$datapath))
      } else if(str_detect(image_in$type, "png")) {
        return(readPNG(image_in$datapath))
      }
    }
    return(readJPEG("testImage/heStain.jpg"))
  })  
  
  #Who could believe there isn't a mode function in R?
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #---------Show Image-------------------#
  #set up boolean to keep default image, and delete temp image
  del_bool <- FALSE
    output$imgPlot <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_imgPlot_width
    height <- session$clientData$output_imgPlot_height
    pixelratio <- session$clientData$pixelratio
    
    if(!is.null(input$image_file)){
      if(str_detect(input$image_file$type, "png|jpg")){
        image_file <- input$image_file$datapath
        del_bool <- FALSE
      } else {
        image_file <- "testImage/heStain.jpg"
      }
    } else {
      image_file <- "testImage/heStain.jpg"
    }
    
    list(src = image_file,
         contentType = 'image/png',
         width = width
         )
  }, deleteFile = del_bool)
  
  #--------Show Palette-----------#
  output$palettePlot <- renderImage({
      width  <- session$clientData$output_palettePlot_width
      height <- session$clientData$output_palettePlot_height
      pixelratio <- session$clientData$pixelratio
      
      #get palette for image
      image_read <- image_read()
      
      #Get function type from radiobuttons
      if(input$choice == "mean"){
        choice <- mean
      } else if (input$choice == "median"){
        choice = median
      } else {
        choice = Mode
      }
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.png')
      
      
      png(outfile, width=width, height=height)
      show_col(sort(image_palette(image_read,
                             n=input$bins,
                             volume=input$vol_bool,
                             choice=choice)))
      dev.off()
      
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
