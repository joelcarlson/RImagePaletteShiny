
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(shinythemes)

shinyUI(fluidPage(theme=shinytheme("flatly"),

  # Application title
  titlePanel("RImagePalette Package Demo"),
  

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("image_file", label=h6("Upload Image (*.jpeg or *.png)"), multiple = FALSE, accept = c("image/jpeg", "image/png")),
      sliderInput("bins",
                  "Number of Colors to Extract:",
                  min = 1,
                  max = 50,
                  value = 9),
      selectInput("choice", "Color Extraction Function",
                  c("Mean" = "mean",
                    "Median" = "median",
                    "Mode" = "mode"),
                  selected="mode"),
      checkboxGroupInput("checkboxes", label = "", 
                         choices = list("Volume" = "volume", "Labels" = "label"), inline=TRUE),
      
      numericInput("set_seed", label = "Set Seed", value = 1, step=1)
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width=6,
         plotOutput("imgPlot")
         ),
        column(width=6,
         plotOutput("palPlot")
        )
      )
      
    )
  )
))
