#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "RF Agroforestry"),
    dashboardSidebar(),
    dashboardBody(
        
        useShinyjs(),
        
        fluidRow(
            tabBox(
                id = "tabset1", height = "550px",  type="pills",
                tabPanel("Geographical variables",
                         uiOutput("RegionChoice"),
                         numericInput("Longitude", 
                                       ("Longitude"), 
                                         value = 1),
                         numericInput("Latitude",
                                      ("Latitude"),
                                      value = 1),
                         sliderInput("Altitude_masl",
                                     ("Altitude masl"),
                                     min = 1,
                                     max = 50,
                                     value = 30)
                         ), #end of first tab
                tabPanel("Edaphic variables", 
                         numericInput("Initial_C_stocks",
                                      ("Initial C stocks"),
                                      value = 1),
                         sliderInput("Mean_annual_rainfall",
                                     ("Mean annual rainfall"),
                                     min = 1,
                                     max = 50,
                                     value = 30),
                         sliderInput("Mean_annual_temperature",
                                     ("Mean annual temperature"),
                                     min = 1,
                                     max = 50,
                                     value = 30),
                         uiOutput("IPCC_climateChoice"),
                         uiOutput("Soil_typeChoice")
                         ),#end opf second tab
                tabPanel("Agronomical variables", 
                         uiOutput("AFChoice"),
                         sliderInput("Total_tree_density",
                                     ("Total tree density"),
                                     min = 1,
                                     max = 50,
                                     value = 30
                                     ),
                         uiOutput("PreviousLucChoice")
                         
            )#end of third tab
            
        ), #end of tabbox 
        
        box(
            title = "Predictions",
            "Here plot with C stocks predictions"
        )
        
        ), #end ot fluidrow

        
        fluidRow(
            # Clicking this will increment the progress amount
            box(width = 6,
                actionButton("count", "Run predictions", width=300))
        ),

    )
)
    
    
    
    
#     shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30),
#             numericInput("Longitude",
#                          ("Longitude"),
#                          value = 1),
#             numericInput("Latitude",
#                          ("Latitude"),
#                          value = 1),
#             numericInput("Initial_C_stocks",
#                          ("Initial C stocks"),
#                          value = 1),
#             sliderInput("Mean_annual_rainfall",
#                      ("Mean annual rainfall"),
#                      min = 1,
#                      max = 50,
#                      value = 30),
#             sliderInput("Mean_annual_temperature",
#                      ("Mean annual temperature"),
#                      min = 1,
#                      max = 50,
#                      value = 30),
#             sliderInput("Altitude_masl",
#                         ("Altitude masl"),
#                         min = 1,
#                         max = 50,
#                         value = 30),
#             sliderInput("Total_tree_density",
#                         ("Total tree density"),
#                         min = 1,
#                         max = 50,
#                         value = 30),
#         uiOutput("RegionChoice"),
#         uiOutput("AFChoice"),
#         uiOutput("PreviousLucChoice"),
#         uiOutput("IPCC_climateChoice"),
#         uiOutput("Soil_typeChoice")
# 
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# ))
