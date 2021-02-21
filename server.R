library("tidyverse")
library("readxl")
library("lubridate")
library("leaflet")
library("leaflet.extras")
library("rnaturalearthdata")
library("sf")

source(file = "data_processing.R")

function(input, output, session) {

 
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(scms_countries_df$continent))
  
  observeEvent(input$selected_continent,
               {
                 scms_countries_df <- scms_countries_df %>%
                   filter(continent == input$selected_continent) %>%
                   pull(country)
                 
                 updateSelectInput(session,
                                   "selected_country",
                                   choices = scms_countries_df)
               })
  
  updateSelectInput(session,
                    "select_continent_1",
                    choices = unique(scms_countries_df$continent))
  
  observeEvent(input$select_continent_1,
               {
                 scms_countries_df <- scms_countries_df %>%
                   filter(continent == input$select_continent_1) %>%
                   pull(country)
                 
                 updateSelectInput(session,
                                   "select_country_1",
                                    choices = scms_countries_df)
               })
  
  observeEvent(input$selected_country,
               {
                 scms_countries_df <- scms_countries_df %>%
                  filter(country == input$selected_country) %>%
                   pull(year)
                 
                 updateSelectInput(session,
                                   "select_year",
                                   choices = unique(scms_countries_df))
               })
  
  
  update_valuebox1 <- eventReactive(input$update_chart,{
    data<-   scms_countries_df %>%
      group_by(country,year) %>%
      filter(country == input$selected_country) %>%
      mutate(mean_line_item = mean(line_item_quantity)) %>% 
      group_by(year) %>% 
      select(year,mean_line_item) %>%
      summarise(
        mean_line_item = mean(mean_line_item)
        ,.groups='drop') %>% 
      filter(year == input$select_year) %>%
      pull(mean_line_item)
  })
  
  update_valuebox2 <- eventReactive(input$update_chart,{
    data<-   scms_countries_df %>%
      group_by(country,year) %>%
      filter(country == input$selected_country) %>%
      mutate(mean_line_value = mean(line_item_value)) %>% 
      group_by(year) %>% 
      select(year,mean_line_value) %>%
      summarise(
        mean_line_value = mean(mean_line_value)
        ,.groups='drop') %>% 
      filter(year == input$select_year) %>%
      pull(mean_line_value)
  })
  
  update_valuebox3 <- eventReactive(input$update_chart,{
    data<-   scms_countries_df %>%
      group_by(country,year) %>%
      filter(country == input$selected_country) %>%
      mutate(mean_weight = mean(weight_kilograms_final)) %>% 
      group_by(year) %>% 
      select(year,mean_weight) %>%
      summarise(
        mean_weight = mean(mean_weight)
        ,.groups='drop') %>% 
      filter(year == input$select_year) %>%
      pull(mean_weight)
  })
  output$line_item_quantity <- renderText({
        toString(floor(update_valuebox1()))
  })
  
  output$line_item_value <- renderText({
         print(toString(floor(update_valuebox2())))
  })
  
  output$weight_kilograms <- renderText({
       print(toString(floor(update_valuebox3())))
  })
  
  
  output$wdi_indicator_chart <- renderPlot({
    if(input$update_chart == 0){
      return()
    }

    scms_countries_df %>%
      group_by(country,year) %>%
      filter(country == isolate(input$selected_country)) %>%
      #filter(vendor == isolate(input$selected_indicator)) %>%
      filter(!is.na(line_item_quantity)) %>%
      filter(year == isolate(input$select_year)) %>%
      ggplot() +
      geom_boxplot(aes(x = scheduled_date, 
                       y = line_item_quantity,color=product_group)) +          facet_wrap(~product_group)+
      labs(
          title = paste("Line Item Quantity", "in", isolate(input$selected_country)),
          subtitle = "SCMS Data"
       )
    # mtcars %>%
    #   ggplot() + geom_boxplot(aes(x=gear,y=mpg,color=factor(gear)))
  })
  
  
  update_map <- eventReactive(input$update_chart_1,{
    
    if(input$select_metric == "Line Item Quantity"){
    scms_countries_df <- scms_countries_df %>%
      group_by(country) %>%
      mutate(avg_line_item = mean(line_item_quantity))
    col_val = "Reds"
    str_val = "Avg Line Item Quantity:"
    } else if(input$select_metric == "Weight Kilograms (kg)"){
      print(names(scms_countries_df))
      scms_countries_df <- scms_countries_df %>%
        group_by(country) %>%
        mutate(avg_line_item = mean(weight_kilograms_final))
      col_val = "Greens"
      str_val = "Avg Weight (Kg):"
    } else{
      scms_countries_df <- scms_countries_df %>%
        group_by(country) %>%
        mutate(avg_line_item = mean(line_item_value))
        col_val = "Blues"
        str_val = "Average Line Item Value:"
    }
    
    qpal <- colorNumeric(col_val, scms_countries_df$avg_line_item, na.color ="#808080")
    
    scms_countries_df %>%
      rename(geounit = country) %>%
      full_join(countries_sf,by= "geounit") %>%
      st_as_sf()%>% 
      leaflet() %>%
      addPolygons(
        fillColor = "grey",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      addPolygons(
        data = scms_countries_df %>% 
          rename(geounit = country) %>%
          full_join(countries_sf,by= "geounit") %>%
          st_as_sf(),#%>% 
        fillOpacity = 1,
        color = ~qpal(avg_line_item),
        label = ~geounit,
        weight = 1,
        popup = ~ paste("Country:", geounit, "<br/>", str_val, floor(avg_line_item))
      ) %>%
      addLegend(pal = qpal, values = ~avg_line_item, opacity = 1,
                title = paste0(str_val,"<br/>",max(scms_countries_df$year, na.rm = TRUE), ")"))
  })
  
  output$highlighted_world_map <- renderLeaflet({
    update_map()
  })
  
  plotviolin <- eventReactive(input$update_chart_2,{
    scms_countries_df %>% 
      filter(country == input$select_country_1) %>%
      ggplot() +
      geom_violin(aes(x = shipment_mode,y=line_item_value,fill = shipment_mode )) +
      facet_wrap(~year) 
  })
  
  output$violin_plot <- renderPlot({
    plotviolin()
  })
}

