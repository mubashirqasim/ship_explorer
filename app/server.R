library(dplyr)

server <- function(input, output, session) {
  
  ## A bit of pre-processing
  # library(readr)
  # df <- read_csv("data/ships.csv") %>% unique()
  # ships <- select(df, ship_type, SHIPNAME) %>% unique() %>% arrange(ship_type, SHIPNAME)
  # save.image("data/workingdata.RData", compress = TRUE)
  
  ## Loading data
  # load("data/workingdata.RData")
  load("data/workingdata.RData")
  ship_types <- sort(unique(ships$ship_type))
  
  # Dynamic UI ------------------------------------------------------------
  
  # User inputs sidebar
  output$inputs_sidebar <- renderUI({
    tagList(
      # Ship type
      hr(),
      h3("User inputs"),
      br(),
      selectInput(inputId = "ship_type", label = "Ship Type",
                  choices = ship_types, selected = "Cargo"),
      
      br(),
      # Ship name
      selectInput(inputId = "ship_name", label = "Ship Name",
                  choices = sort(subset(ships, ship_type == "Cargo")$SHIPNAME)),
      br(),br(),hr()
    )
  })
  
  # Update ship name from selected ship type
  
  ship_names <- reactive({
    ships %>%
      filter(ship_type == input$ship_type) %>%
      pull(SHIPNAME)
  })
  
  observeEvent(
    input$ship_type,
    updateSelectInput(session, "ship_name", "Ship Name",
                      choices = ship_names(), selected = ship_names()[1] )
  )
  
  # Distance traveled ----------------------------------------------------
  
  df_map <- reactive({
    
    if(is.null(input$ship_name) | is.null(input$ship_type)) { print("NULL INPUTS") } else {
      
      ## Distance traveled data.frame
      distance <- df %>%
        filter(ship_type == input$ship_type & SHIPNAME == input$ship_name) %>%
        #filter(ship_type == "Cargo" & SHIPNAME == "ADAMAS") %>%
        arrange(desc(DATETIME)) %>%
        unique() %>%
        select(DATETIME, LAT, LON) 
      
      ## Data transformation
      if(nrow(distance) >= 1 ) {
        distance <- transform(distance, DATETIME2 = c(DATETIME[-1], NA),
                              LAT2 = c(LAT[-1], NA), LON2 = c(LON[-1], NA))
        
        ## Calculate distance and dates traveled
        distance <- distance %>%
          rowwise() %>%
          mutate(distm = geosphere::distm(c(LON, LAT), c(LON2, LAT2),
                                          fun = geosphere::distHaversine)) %>%
          ungroup() %>%
          arrange(desc(distm, DATETIME))
        
        df_map <- distance[1,]
        return(df_map) } else {return(data.frame())}
    }
  })
  
  # Info sidebar ------------------------------------------------------------
  
  output$info_sidebar <- renderUI({
    if( length(df_map()) > 0 & !is.null(input$ship_name) & !is.null(input$ship_type)) {
      
      message_box(class = "info", header = "Summary",
                  content = HTML(paste0("<BR> The selected ",
                                        input$ship_name, " ", input$ship_type,
                                        " ship travelled the longest distances between ",
                                        df_map()$DATETIME, " and ", df_map()$DATETIME2,
                                        ". The ship traveled ",
                                        round(df_map()$distm,0), " meters during this period.")))
    }
  })
  
  # Map output --------------------------------------------------------------
  map <- reactiveValues(dat = 0)
  
  ## Basemap
  output$map <- renderLeaflet({
    map$dat <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite View") %>%
      
      # Layers control
      addLayersControl(
        baseGroups = c("Default", "Toner", "Toner Lite","Satellite View"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  })
  
  # Incremental changes to the map
  observe({
    
    if( length(df_map()) > 0 & !is.null(input$ship_name) & !is.null(input$ship_type)) {
      
      # Map icons
      icons <- list(
        start = awesomeIcons( icon = 'ship', library = "fa", markerColor = 'white', iconColor = "gray"),
        end = awesomeIcons( icon = 'ship', library = "fa", markerColor = 'lightblue', iconColor = "green"))
      
      # Update map
      leafletProxy("map", data = df_map()) %>%
        
        # Set boundaries
        fitBounds(lng1=df_map()$LON-0.005, lng2=df_map()$LON2+0.005,
                  lat1=df_map()$LAT-0.005, lat2=df_map()$LAT2+0.005) %>%
        
        # Clear previous markers
        clearMarkers() %>%
        clearAntpath() %>%
        
        # Add markers from current selection
        addAwesomeMarkers(lng=~LON, lat=~ LAT, icon = icons$start, label=~as.character(DATETIME)) %>%
        addAwesomeMarkers(lng=~LON2, lat=~ LAT2, icon = icons$end, label=~as.character(DATETIME2)) %>%
        addAntpath(lng = ~c(LON,LON2), lat = ~c(LAT,LAT2), weight = 3, opacity = 0.2, smoothFactor = 5)
    } 
  })
  
  # Show a pop-up at the given location
  showPopup <- function(id, lat, lng) {
    content <- as.character(tagList(
      tags$h4("Ship:",input$ship_name),
      tags$strong(HTML(sprintf("%s %s", input$ship_type, "Ship" ))), tags$br(),tags$br(),
      sprintf("Started at: %s", df_map()$DATETIME), tags$br(),
      sprintf("Ended at: %s", df_map()$DATETIME2), tags$br(), tags$br(),
      sprintf("Distance travelled (in meters): %s", round(df_map()$distm, 0)), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng = lng, lat = lat, popup = content)
  }
  
  # When map is clicked, show a pop-up with Ship info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })
  
}
