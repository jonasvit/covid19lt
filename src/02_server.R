library(shiny) # makes it easy to build interactive web apps straight from R.
library(leaflet) # one of the most popular open-source JavaScript libraries for interactive maps


# We have to build the UI of our interface
ui <- bootstrapPage(
  tags$style("
        #controls {
          background-color: #ddd;
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 1;
        }
         {text/css", "html, body {width:100%;height:100%}}",
        type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 10, left = "auto", right = 10, bottom = "auto",
                              width = 500, height = "auto",
                              
                sliderInput("range", "Covid-19 deaths", min(LT$V3), max(LT$V3),
                            value = range(LT$V3), step = 1, width = 500
                            )
                ,plotOutput("plot1")
                ,plotOutput("plot2")
  )
)



# Now create a server function that allows us to create the interface
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    LT[LT$V3 >= input$range[1] & LT$V3 <= input$range[2],]
  })
  
  # Creating map coloring
  colorpal <- reactive({
    colorQuantile("YlOrRd", LT$V3)
  })
  
  
  # Creating the first plot to our control panel
  output$plot1 <- renderPlot({
    ggplot(plot1, aes(x = age_gr, y = deaths_cov1)) +
      ggtitle("Deaths per age group")+
      geom_bar(stat = "identity", fill = "#004980", alpha = 0.6) +
      stat_summary(aes(label = stat(y)), fun = 'sum', geom = 'text', col = 'black', vjust = -0.3) +
      scale_y_continuous(labels = function(l) {l = l / 1000; paste0(l, "K")})  +
      labs(
        title = "Deaths per age group",
        subtitle = "Lithuania",
        x = "Age groups",
        y="Covid19 deaths"
      )+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  
            plot.background = element_rect(fill = "transparent", colour = NA),
            plot.title = element_text(size = 25))
  }, bg="transparent")
  
  
  
  # Creating the fsecond plot to our control panel
  output$plot2 <- renderPlot({
    atveju_skaicius %>%
    ggplot(aes(Category, cases_7day)) +
    geom_line(color = "orange") +
    theme(legend.position = "none") +
    geom_line(aes(x = Category, y = deaths_7day * coeff), color = "red") +
    scale_y_continuous(
      labels = scales::comma,
      name = "Cases",
      sec.axis = sec_axis(deaths_7day ~ . / coeff,
                          name = "Deaths",
                          labels = scales::comma)) +
    theme(
      axis.title.y = element_text(color = "orange", size = 17),
      axis.title.y.right = element_text(color = "red", size = 17)) +
    labs(
      title = "Lithuania: Cases vs. Deaths",
      subtitle = "7-Day Average",
      x = "Date")+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  
            plot.background = element_rect(fill = "transparent", colour = NA),
            plot.title = element_text(size = 25))
    }, bg="transparent")
  
  
  # 
  output$map <- renderLeaflet({
    leaflet(LT) %>% addTiles() %>%
      setView(lng = lt_loc$lon, lat = lt_loc$lat, zoom = 8)
  })
  
  # Making our map look interactive: adding colors and other stuff
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  popup=~paste(
                               "<b>",Category,"</b>","<br/>",
                               "Population: ",population,"<br/>",
                               "Total deaths: ",V3,"<br/>",
                               "Vaccinated (at least one jab): ", pasiskiepije, "<br/>",
                               "Vaccinated population percentage: ", proc,"%", "<br/>",
                               "Zone in Lithuanian: ", map_colors, "<br/>"
                  ),
                  fillColor = ~pal(V3),
                  highlightOptions = highlightOptions(color = "white", weight = 4,
                                                      bringToFront = TRUE))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = LT)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    # if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal,
                          values = ~V3,
                          title = "Deaths quantiles"
      )
  })
}

shinyApp(ui, server)