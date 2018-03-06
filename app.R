library(shiny)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(data.table)
library(DescTools)
library(lubridate)
library(leaflet)
library(formattable)
library(scales)



df_residential <- as.data.table(readRDS("residential_small_03052018.rds"))
#preprocesses groupbys so less work later for the reactive scripts
rneighborhoods <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')

nyc_neighborhoods <- readOGR(content(rneighborhoods,'text'), 'OGRGeoJSON', verbose = F)
drop(rneighborhoods)

ui <- fluidPage(
  
  
  
  
  fluidRow(
    column(10, align="center",
           titlePanel("NYC Median Residential Home Sales Price in 2017$")
    ),
    column(2, align="center",
           selectInput("Year", 
                       
                       label = "Choose a year to display",
                       choices = sort(unique(df_residential$year_of_sale), decreasing = TRUE)),
           
           selectizeInput("Neighborhood", 
                       
                       label = "Choose neighborhoods to filter by",
                       selected = c("Park Slope", "Financial District", "Tribeca","SoHo","Long Island city","Flushing","Upper East Side","Upper West Side","Downtown Brooklyn","Boerum Hill"),
                       choices = sort(unique(df_residential$neighborhood)),
                       multiple = TRUE)
    )
  ),
  
  
  fluidRow(
    column(7, align="center",
           leafletOutput("map")
    ),
    column(5, align="center",
           tableOutput("table")
    )
  )
  
)


# Server logic ----
server <- function(input, output) {
  
  df <- reactive({
  
    
    df0 <- df_residential[year_of_sale %in% input$Year]
    df0 <- df0[, list(median_price = round(median(sale_price),0), mean_price = round(mean(sale_price),0), number_of_sales =.N ), list(neighborhood)]
    #df0 <- df0[number_of_sales >30]
    
    finaloutput <- geo_join(nyc_neighborhoods, df0, "neighborhood", "neighborhood")
  
  return(finaloutput)
  })
  
  
  
  df_table<- reactive({
    df <- df()
    df2 <- as.data.table(df@data)
    
    df_median_sale_price_by_neighborhood <- df2[neighborhood %in% input$Neighborhood, .(borough, neighborhood, median_price, mean_price, number_of_sales)]
    #df_median_sale_price_by_neighborhood <- df_median_sale_price_by_neighborhood[number_of_sales >10] 
    df_median_sale_price_by_neighborhood <- df_median_sale_price_by_neighborhood[order(-median_price)]
    df_median_sale_price_by_neighborhood[,median_price:= dollar(median_price)]
    df_median_sale_price_by_neighborhood[,mean_price:= dollar(mean_price)]
    return(df_median_sale_price_by_neighborhood)
  })
  
  
  
  output$map <- renderLeaflet({
    map_data <- df()
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Median:$%s<br/>Number of Sales:%g",
      map_data@data$neighborhood, map_data@data$median_price, map_data@data$number_of_sales
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorBin(palette = "viridis",
                        #domain = range(map_data@data$median_price, na.rm=T)
                        domain = c(0,3000000),
                        na.color = "white"
                        )
    
    leaflet(map_data) %>%
      setView(-73.94, 40.72, zoom = 12) %>%
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(median_price),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~median_price, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
    
  }
  )
  
  output$table <- renderTable(df_table())
}


# Run app ----
shinyApp(ui, server)
