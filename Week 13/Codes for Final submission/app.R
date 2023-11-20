library(tidyverse)
#library(shiny)
library(plotly)
library(shinyWidgets)
library(sf)
library(spData)
library(leaflet)
library(shinythemes)
library(bslib)
library(purrr)
library(readr)
library(dplyr)


#Dataset for Choropleth
mapData<-world[c(2,11)]
inflation <-read.csv("Inflation_world.csv")

#Dataset for ASEAN-5 Linegraph
ASEAN5 <- read.csv("Asean_5_IMF.csv")

#Dataset for CPI Linegraph
CPI<-read.csv("CPI_goods_singstat.csv")

#Dataset for CPI Weights Pie Chart
weights<-read.csv("cpi_weights.csv")
categories <- unique(weights$Main_Goods)








ui <- page_fixed(theme = bslib::bs_theme(bootswatch = "minty", base_font="Roboto"  
),
#World Inflation Rate
card(class="card border-secondary mb-3",
     height = 500,
     full_screen = TRUE,
     card_header(
       style = "font-family: 'Roboto'; font-size: 18px; font-weight: bold; color: #F99192;",
       "World Inflation Rate 2023"),
     card_body(
       class = "p-2",
       leafletOutput("map"),
       fill = TRUE, gap = 0,
       
       p(style = "font-family: 'arial'; font-size: 16px;", 'This Choropleth map above provides a visual representation of global inflation rates using colors to depict varying levels. A majority of the countries have an average inflation rate falling within the 5-10% range, reiterating the widespread nature of the issue that needs to be tackled.')
       
     ),
     card_footer(
       class = "fs-9",
       "Source: International Monetary Fund, 2023"
     )
),  

#ASEAN 5 Inflation
card(class="card border-secondary mb-3",
     
     full_screen = TRUE,
     card_header( style = "font-family: 'Roboto'; font-size: 18px; font-weight: bold; color: #F99192;","Comparison of Inflation Rates in ASEAN-5 Countries"),
     layout_sidebar(
       fillable = FALSE,
       sidebar = sidebar(
         #Slider (To choose the year interval)
         sliderTextInput(
           inputId = "Year1",
           label = "Range:",
           choices = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
           selected = c("2001", "2023"),
           width = "90%"
           
         ),
         #Select Input (To choose the countries that user wants to compare)
         selectInput(
           inputId = "cities",
           label = "Select countries",
           choices = unique(ASEAN5$Country),
           selected = "Singapore",
           multiple = TRUE
           
         )),
       
       
       plotlyOutput("AS5graph"),
       card_body(
         fill = FALSE, gap = 0,
         p(style = "font-family: 'arial'; font-size: 16px;","This line graph illustrates a comparison of inflation rates among the 5 ASEAN countries. All countries experienced a surge in their inflation starting levels from the year 2020. In 2023, Singapore is placed 2nd in terms of inflation among the ASEAN nations."))
       
     ), card_footer(
       class = "fs-9",
       "Source: International Monetary Fund, 2023"),
),

# Write-up 1
card(class="card text-white bg-secondary mb-3", 
     
     style = "width: 300; height: 300px; border-radius: 30px;",
     p(style = "font-family: 'Courier New'; font-size: 21px; font-weight: bold;","Why address Inflation?"),
     
     p(style = "font-family: 'arial'; font-size: 16px;","These 2 plots illustrate the importance for addressing inflation in Singapore as  inflationary pressures have become a pervasive issue affecting countries worldwide, and Singapore stands out among its neighboring nations as one of the most significantly impacted."),
     
     p(style = "font-family: 'arial'; font-size: 16px;","Inflation can led to the decline in the purchasing power of families, particularly impacting the standards of living for low-income families who are facing difficulty in meeting basic living expenses, thereby exerting adverse effects on their mental and physical well-being. Therefore, it is important to identify the specific goods or services that will make a huge impact on households in Singapore, so that policymakers can formulate targeted solutions and strategies, aiming to rectify the issue and restore inflation rates to more sustainable levels."
     )),


#CPI of Goods and Services
card(class="card border-primary mb-3",
     
     full_screen = TRUE,
     card_header(style = "font-family: 'Roboto'; font-size: 18px; font-weight: bold; color:#3EB488;","CPI of Goods and Services in Singapore"),
     layout_sidebar(
       fillable = TRUE,
       sidebar = sidebar(
         #Slider (To choose the year interval)
         sliderTextInput(
           inputId = "date",
           label = "Range:",
           choices = c("2021Q2", "2021Q3", "2021Q4", "2022Q1", "2022Q2", "2022Q3", "2022Q4", "2023Q1", "2023Q2"),
           selected = c("2021Q2", "2023Q2"),
           width = "90%"
         ),
         #Select Input (To choose the goods and services that user wants to compare)
         selectInput(
           inputId = "gs",
           label = "Select Goods & Services",
           choices = unique(CPI$Goods),
           multiple = TRUE
         )),
       plotlyOutput("CPIgraph"), 
       card_body(
         fill = FALSE, gap = 0,
         p(style = "font-family: 'arial'; font-size: 16px;", "This line graph depicts a comparison of CPI among the goods and services. Within this category, Transport experienced the most significant surge in prices, followed by Food excluding food serving services and Food serving services."))
     ),
     card_footer(
       class = "fs-9",
       "Source: Department of Statistics Singapore, 2023"),
),

#CPI Weights
card(class="card border-primary mb-3",
     height = 500,
     full_screen = TRUE,
     card_header(
       style = "font-family: 'Roboto'; font-size: 18px; font-weight: bold; color:#3EB488;","CPI Weighing pattern 2022"),
     card_body(
       class = "p-2",
       plotlyOutput("pie"), uiOutput("back"),
       fill = TRUE, gap = 0,
       p(style = "font-family: 'arial'; font-size: 16px;", "This pie chart represents the distribution of household expenditures on various goods and services. It illustrates the relative importance of each goods and service. Housing utilities emerged as the highest ranked followed by Transport and Food serving services. Private transport and Hawker food emerged as the highest among the sub-categories for Transport and Food Serving Services respectively.")),
     card_footer(
       class = "fs-9",
       "Source: Department of Statistics Singapore, 2023"
     )
),
# Write-up 2
card(class="card text-white bg-primary mb-3", 
     p(style = "font-family: 'Courier New'; font-size: 21px; font-weight: bold;","Conclusion"),
     p(style = "font-family: 'arial'; font-size: 16px;","Based on all the data given, we are able to justify that Transport and Food serving services will make the most significant impact on households during inflation as both are ranked one of the highest in terms of CPI and household expenditures."))




)



server <- function(input, output, ...){
  thematic::thematic_shiny()
  #Choropleth World Inflation Rate 2023
  output$map <- renderLeaflet({
    
    countries <- left_join(inflation, mapData, c("Country" = "name_long"))
    
    bins <- c(-1, 3, 5, 10, 20, 50, 100, 300, 400)
    pal <- colorBin(palette = "Reds", domain = countries$Inflation_rate, bins=bins
    )
    map_labels <- paste(
      countries$Country, 
      "'s Inflation:", 
      round(countries$Inflation_rate, 1))
    # Generate basemap
    map <- leaflet(options = leafletOptions(dragging=TRUE)) %>%
      addTiles() %>% 
      setView(0, 0, 1)
    
    
    
    map %>% addPolygons(data = countries$geom,
                        fillColor = pal(countries$Inflation_rate), 
                        fillOpacity = .7,
                        color = "grey",
                        weight = 1,
                        label = map_labels,
                        labelOptions = labelOptions(textsize = "12px")) %>% 
      #Legends 
      addLegend(pal = pal, 
                values = (countries$Inflation_rate),
                position = "bottomleft",
                title= "Inflation rate (%)")
  })
  #CPI Line graph
  output$CPIgraph <- renderPlotly({
    changed_data <- subset(CPI, Quarterly <= input$date[2] & Quarterly >= input$date[1])
    plot2 <- plot_ly(data = changed_data,
                     x = ~Quarterly, 
                     y = ~C_P_I,
                     hoverinfo= 'text') %>%
      filter(Goods %in% input$gs) %>%
      group_by(Goods) %>%
      add_trace(type = "scatter", 
                mode = "markers+lines",
                color = ~Goods,
                text = ~paste('</br>',Goods, 
                              '</br> CPI: ', C_P_I,
                              '</br> Year: ', Quarterly))
    
    
    plot2 <- plot2 %>% layout(
      xaxis = list(title = 'Year'),
      yaxis = list (title = 'CPI'))
    
    # Update the x-axis range based on the selected slider range
    plot2 <- layout(plot2, xaxis = list(range = c(input$date[1], input$date[2])))
    
  })
  
  #ASEAN-5 Line graph
  output$AS5graph <- renderPlotly({
    filtered_data <- subset(ASEAN5, Year <= input$Year1[2] & Year >= input$Year1[1])
    plot <- plot_ly(data = filtered_data, x = ~Year, y = ~Inflation_Rate, hoverinfo='text') %>%
      filter(Country %in% input$cities) %>%
      group_by(Country) %>% 
      add_trace(type = "scatter", mode = "markers+lines",color = ~Country, text = ~paste('</br>', Country, '</br> Inflation Rate: ', Inflation_Rate,
                                                                                         '</br> Year: ', Year))
    
    plot <- plot %>% layout(
      xaxis = list(title = 'Year'),
      yaxis = list (title = 'Inflation Rate (Annual Percentage Change)'))
    
    
    
    # Update the x-axis range based on the selected slider range
    plot <- layout(plot, xaxis = list(range = c(input$Year1[1], input$Year1[2])))
  })
  
  
  #Drill-Down Piechart
  current_category <- reactiveVal()
  
  weights_data <- reactive({
    if (!length(current_category())) {
      return(weights %>% group_by(Main_Goods) %>% summarise(values = sum(weights, na.rm = TRUE)))
    }
    weights %>%
      filter(Main_Goods %in% current_category()) %>%
      group_by(Sub_goods) %>%
      summarise(values = sum(weights, na.rm = TRUE))
    
    
  })
  output$pie <- renderPlotly({
    drilldownpie <- setNames(weights_data(), c("labels", "values"))
    plot_ly(drilldownpie) %>%
      add_pie(
        labels = ~labels,
        values = ~values, 
        customdata = ~labels,
        textposition = 'inside',
        textinfo = 'percent',
        insidetextfont = list(color = '#FFFFFF'), 
        marker = list(line = list(color = '#FFFFFF', width = 1)
        )) %>%
      layout(title = current_category() %||% "CPI weights")
  })
  #To click and drill down to the sub categories
  observe({
    clicked_category <- event_data("plotly_click")$customdata[[1]]
    if (isTRUE(clicked_category %in% categories)) current_category(clicked_category)
  })
  #Back Button (To return to main categories)
  output$back <- renderUI({
    if (length(current_category())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  observeEvent(input$clear, current_category(NULL))
}










shinyApp(ui, server)






