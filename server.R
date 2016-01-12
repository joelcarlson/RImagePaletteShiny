
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RImagePalette)
library(png);library(jpeg)
library(ggplot2);library(stringr)
source("scripts/show_colors.R")

shinyServer(function(input, output, session) {
  #Function to read image from input as JPEG or PNG
  image_read <- reactive({
    image_in <- input$image_file
    if(!is.null(image_in)){
      if(str_detect(image_in$type, "jpg|JPG|jpeg|JPEG")){
        return(readJPEG(image_in$datapath))
      } else if(str_detect(image_in$type, "png|PNG")) {
        return(readPNG(image_in$datapath))
      }
    }
    return(readJPEG("defaultImage/celery.jpg"))
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
      image_file <- input$image_file$datapath
    } else {
      image_file <- "defaultImage/celery.jpg"
    }
    
    list(src = image_file,
         contentType = 'image/png',
         width = width
         )
  }, deleteFile = del_bool)
  
  #--------Show Palette-----------#
  output$palPlot <- renderPlot({
      image_read <- image_read()
      #Get function type from radiobuttons
      if(input$choice == "mean"){
        choice <- mean
      } else if (input$choice == "median"){
        choice = median
      } else {
        choice = Mode
      }
      
      #Set parameters and seed
      if(!is.na(input$set_seed)) set.seed(input$set_seed) 
      volume <- ifelse("volume" %in% input$checkboxes, TRUE, FALSE)
      label <- ifelse("label" %in% input$checkboxes, TRUE, FALSE)
        
      show_colors(image_palette(image_read,
                                n=input$bins,
                                volume=volume,
                                choice=choice), label)
    })

})
