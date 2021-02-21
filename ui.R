library(shiny)
library(shinydashboard)
library("tidyverse")
library("readxl")
library("lubridate")
library("leaflet")
library("leaflet.extras")
library("rnaturalearthdata")
library("sf")
library("shinycustomloader")



header <-  dashboardHeader(title = "SCMS Shipping")
sidebar <-  dashboardSidebar(
    conditionalPanel(condition="input.tabselected == 1",
                     fluidRow(
                       selectInput("selected_continent",
                                   "Select a continent",
                                   choices = NULL,
                                   width = "80%")
                     ),
                     fluidRow(
                       selectInput("selected_country",
                                   "Select a country",
                                   choices = NULL,
                                   width = "80%")
                     ),
                     fluidRow(
                       selectInput("select_year",
                                   "Select Year",
                                   choices = NULL,
                                   width = "80%")
                     ),
                     actionButton("update_chart",label="Update Chart & Metrics",width="80%")
    ),
    conditionalPanel(condition="input.tabselected==2",
                     
                     selectInput("select_metric",
                                 label="Choose a Metric:",
                                 choices=c("Line Item Quantity",
                                           "Weight Kilograms (kg)",
                                           "Line Item Value")),
                     actionButton("update_chart_1",label="Update Metrics Map",width="80%")),
    conditionalPanel(condition = "input.tabselected==3",
                     selectInput("select_continent_1",
                                 "Select a continent",
                                 choices = NULL,
                                 width = "80%"),
                     selectInput("select_country_1",
                                 label = "Select a Country",
                                 choices = NULL,
                                 width = "80%",
                                 selectize = TRUE),
                     actionButton("update_chart_2",label ="Update Country",width="80%"))
    )
 body <-  dashboardBody(
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
   ),
   mainPanel(
     tabsetPanel(id = "tabselected",
       tabPanel("Country Metrics", 
                value=1,
                fluidRow(
                valueBox(
                  value = textOutput("line_item_quantity"),
                  subtitle = "Line Item Quantity",
                  icon = icon("gear")
                ),
                valueBox(
                  value = textOutput("line_item_value"),
                  subtitle = "Line Item Value",
                  icon = icon("dollar-sign"),
                  color = "yellow"
                ),
                valueBox(
                  value = textOutput("weight_kilograms"),
                  subtitle = "Average Weight (kg)",
                  icon = icon("star"),
                  color = "green"
                )
                ),
                box(width = 12,
                  title = "Revenue per Account"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                ,plotOutput("wdi_indicator_chart"))
                # htmlTemplate("template1.html",
                #       wellpanel2 = plotOutput("wdi_indicator_chart"),
                #       wellpanel1 = valueBox(width=3,
                #                             value = "Image2",
                #                             subtitle = "This is where the image goes",
                #                             icon = icon("star")),
                #       wellpanel3 = wellPanel(p("Column width 2"))
       ),
       tabPanel("Metrics Map", 
                  value=2,
                  withLoader(leafletOutput("highlighted_world_map",width = "1250px",height = "650px"))
                ),
       tabPanel("Shipment Mode",
                value = 3,
                withLoader(plotOutput("violin_plot")))
       )
    )
  )
ui <- dashboardPage(header,sidebar,body, skin="red")
