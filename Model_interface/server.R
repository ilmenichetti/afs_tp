#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    addClass(selector = "body", class = "sidebar-collapse") #to collapse the sidebar as default
    
    load("workspace.RData")
    countryData = list("Region"=c(cardinael_data_subset$Region))
    
    output$RegionChoice <- renderUI ({ 
        selectInput(inputId = "RegionChoice",label = "Region",
                    choices = unique(cardinael_data_subset$Region))
    })
    
    output$AFChoice <- renderUI ({ 
        selectInput(inputId = "AFChoice",label = "Agroforestry classification",
                    choices = unique(cardinael_data_subset$Agroforestry_classification))
    })
    
    output$PreviousLucChoice <- renderUI ({ 
        selectInput(inputId = "PreviousLucChoice",label = "Previous land use",
                    choices = unique(cardinael_data_subset$Previous_land_use))
    })
    
    output$IPCC_climateChoice <- renderUI ({ 
        selectInput(inputId = "IPCC_climate",label = "Climate (IPCC)",
                    choices = unique(cardinael_data_subset$IPCC_Climate))
    })
    output$Soil_typeChoice <- renderUI ({ 
        selectInput(inputId = "Soil_type",label = "Soil Type",
                    choices = unique(cardinael_data_subset$Soil_type))
    })
    
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
