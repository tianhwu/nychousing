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
#library(lubridate)
library(leaflet)
#library(formattable)
library(scales)


df_residential <-
  as.data.table(readRDS("residential_small_03052018.rds"))

#drop out broad channel and jamaica bay from dataset due to glitch on geojoin
df_residential <- df_residential[!neighborhood %in% c("Broad Channel","Jamaica Bay")]

#loads neighborhoods from nyc public dataset, kudos to the guy who made this tutorial that I learned from https://rpubs.com/jhofman/nycmaps
rneighborhoods <-
  GET(
    'http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson'
  )

#not 100% sure what readOGR does and shapefiles still seem like magic to me, need to dive deeper at another date
nyc_neighborhoods <-
  readOGR(content(rneighborhoods, 'text'), 'OGRGeoJSON', verbose = F)
drop(rneighborhoods)

#Shiny app starts here

#ui portion of our shiny app
ui <- fluidPage(fluidRow(
  column(
    8,
    align = "center",
    titlePanel("NYC Housing Prices by Year & Neighborhood"),
    textOutput("text"),
    leafletOutput("map")
  ),
  column(
    4,
    align = "center",
    selectInput(
      "Year",
      
      label = "Choose a year to investigate (2003 to 2017)",
      selected = c(2013),
      choices = sort(unique(df_residential$year_of_sale), decreasing = TRUE)
    ),
    
    selectizeInput(
      "Neighborhood",
      
      label = "Choose neighborhoods to display additional info for and download raw data, delete and rechoose selections using backspace",
      
      #select a few neighborhoods I like to display more info in a table
      selected = c(
        "Park Slope",
        "Financial District",
        "Tribeca",
        "SoHo",
        "Long Island city",
        "Flushing",
        "Upper East Side",
        "Upper West Side",
        "Downtown Brooklyn",
        "Boerum Hill",
        "Bushwick",
        "Sunnyside",
        "Long Island City",
        "Williamsburg"
        
      ),
      choices = sort(unique(df_residential$neighborhood)),
      multiple = TRUE
    ),
    downloadButton("downloadData", "Download Raw Data")
  ),
  tableOutput("table"))
)



# Server logic ----
server <- function(input, output) {
  df_main <- reactive({
    df <- df_residential[year_of_sale %in% input$Year]
    df <-
      df[, list(
        median_price = round(median(sale_price), 0),
        mean_price = round(mean(sale_price), 0),
        number_of_sales = .N
      ), list(neighborhood)]
    #df0 <- df0[number_of_sales >30]
    
    finaloutput <-
      geo_join(nyc_neighborhoods, df, "neighborhood", "neighborhood")
    
    return(finaloutput)
  })
  
  
  #renders our leaflet map
  output$map <- renderLeaflet({
    map_data <- df_main()
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Median:$%s<br/>Number of Sales:%g",
      map_data@data$neighborhood,
      map_data@data$median_price,
      map_data@data$number_of_sales
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorBin(
      palette = "viridis",
      #domain = range(map_data@data$median_price, na.rm=T)
      domain = c(0, 3000000),
      na.color = "white"
    )
    
    leaflet(map_data) %>%
      setView(-73.94, 40.72, zoom = 12) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(median_price),
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
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ median_price,
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
    
    
    
  })
  #creates our table
  df_table <- reactive({
    df <- df_main()
    df <- as.data.table(df@data)
    
    df <-
      df[neighborhood %in% input$Neighborhood, .(borough,
                                                 neighborhood,
                                                 median_price,
                                                 mean_price,
                                                 number_of_sales)]
    #df_median_sale_price_by_neighborhood <- df_median_sale_price_by_neighborhood[number_of_sales >10]
    df <- df[order(-median_price)]
    df[, median_price := dollar(median_price)]
    df[, mean_price := dollar(mean_price)]
    return(df)
  })
  
  df_download <- reactive({
    df <- df_residential
    df <-df[neighborhood %in% input$Neighborhood & year_of_sale %in% input$Year, .(
      neighborhood,
      zip_code,
      address,
      sale_price,
      year_of_sale,
      year_built)]
    df <- left_join(df, df_table())
    return(df)
  })
  
  
  #renders our table
  output$table <- renderTable(df_table())
  
  
  output$downloadData <- downloadHandler(
    filename = "nyc_residentialdata.csv",
    content = function(file) {
      write.csv(df_download(), file, row.names = FALSE)
    }
  )
  
  
  output$text <- renderText({ 
      paste(input$Year, " Median Residential Home Sales Price in 2017$")
    })

  
  
}





# Run app ----
shinyApp(ui, server)
