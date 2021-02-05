library(shinycssloaders)

semanticPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),),
  
  # Grid template
  grid(
    grid_template = grid_template(
      default = list(
        areas = rbind( c("title", "map"), c("inputs", "map"), c("info", "map")),
        cols_width = c("400px", "1fr"),
        rows_height = c("50px", "320px", "auto")
      )),
    
    # Style
    area_styles = list(title = "margin: 20px;", inputs = "margin: 20px;", info = "margin: 20px;"),
    
    # Individual components
    title = h2(class = "ui header", icon("ship"), div(class = "content", "Ship Explorer")),
    inputs = uiOutput("inputs_sidebar"),
    info = withSpinner(uiOutput("info_sidebar")),
    map = withSpinner(leafletOutput("map", height = '800'))
    
  )# End of grid
) # End of semanticPage
