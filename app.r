library(shiny)
library(igraph)
library(leaflet)
library(visNetwork)

# Predefined coordinates for cities (latitude, longitude)
default_coords <- data.frame(
  city = c("Zagreb", "Rijeka", "Split", "Osijek", "Pula"),
  lat = c(45.8150, 45.3271, 43.5081, 45.5511, 44.8666),
  lon = c(15.9819, 14.4422, 16.4402, 18.6955, 13.8496)
)

# UI
ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #f9f9f9; font-family: Arial, sans-serif; }
    .sidebar { background-color: #fff; padding: 15px; border-radius: 5px; }
    .btn { margin-top: 10px; }
  ")),
  titlePanel("Shortest path finder"),
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar",
          h3("Setup"),
          numericInput("numRelations", "Number of Relations:", value = 3, min = 1),
          uiOutput("relationsInput"),
          h4("City Options"),
          selectInput("start", "Start City:", choices = default_coords$city, selected = "Zagreb"),
          selectInput("end", "End City:", choices = default_coords$city, selected = "Osijek"),
          actionButton("solve", "Calculate Shortest Path", class = "btn btn-primary"),
          actionButton("addCity", "Add New City", class = "btn btn-secondary")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Results", verbatimTextOutput("result")),
        tabPanel("Error Log", verbatimTextOutput("error"))
      )
    )
  ),
  # Modal to add a new city
  modalDialog(
    id = "addCityModal",
    title = "Add New City",
    textInput("newCity", "City Name:", value = ""),
    numericInput("newLat", "Latitude:", value = 0),
    numericInput("newLon", "Longitude:", value = 0),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmAddCity", "Add City")
    ),
    easyClose = TRUE
  )
)

# Server
server <- function(input, output, session) {
  city_coords <- reactiveVal(default_coords)
  
  # Show modal dialog to add a new city
  observeEvent(input$addCity, {
    showModal(modalDialog(
      title = "Add New City",
      textInput("newCity", "City Name:", value = ""),
      numericInput("newLat", "Latitude:", value = 0),
      numericInput("newLon", "Longitude:", value = 0),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmAddCity", "Add City")
      ),
      easyClose = TRUE
    ))
  })
  
  # Add new city to city_coords
  observeEvent(input$confirmAddCity, {
    new_city <- data.frame(
      city = input$newCity,
      lat = input$newLat,
      lon = input$newLon
    )
    city_coords(rbind(city_coords(), new_city))
    updateSelectInput(session, "start", choices = city_coords()$city)
    updateSelectInput(session, "end", choices = city_coords()$city)
    removeModal()
  })
  
  # Generate dynamic fields for relations
  output$relationsInput <- renderUI({
    num <- input$numRelations
    inputs <- lapply(1:num, function(i) {
      fluidRow(
        column(4, selectInput(paste0("from", i), paste0("From (Relation ", i, "):"),
                              choices = city_coords()$city)),
        column(4, selectInput(paste0("to", i), paste0("To (Relation ", i, "):"),
                              choices = city_coords()$city)),
        column(4, numericInput(paste0("distance", i), paste0("Distance (Relation ", i, "):"),
                               value = 0, min = 0))
      )
    })
    do.call(tagList, inputs)
  })
  
  observeEvent(input$solve, {
    num <- input$numRelations
    edges <- c()
    weights <- c()
    error_message <- NULL
    
    for (i in 1:num) {
      from <- input[[paste0("from", i)]]
      to <- input[[paste0("to", i)]]
      distance <- input[[paste0("distance", i)]]
      
      if (is.null(from) || is.null(to) || distance <= 0) {
        error_message <- "Ensure all fields are filled correctly and distances are > 0."
        break
      }
      
      edges <- c(edges, from, to)
      weights <- c(weights, distance)
    }
    
    if (!is.null(error_message)) {
      output$error <- renderPrint({ error_message })
      return()
    }
    
    g <- graph(edges = edges, directed = FALSE)
    E(g)$weight <- weights
    
    if (!(input$start %in% V(g)$name) || !(input$end %in% V(g)$name)) {
      output$error <- renderPrint({ "Start or End city does not exist in the graph!" })
      return()
    }
    
    sp <- shortest_paths(g, from = input$start, to = input$end, weights = E(g)$weight)$vpath[[1]]
    if (length(sp) == 0) {
      output$result <- renderPrint({ "No path available between cities!" })
      return()
    }
    
    path_names <- V(g)[sp]$name
    path_weight <- sum(E(g, path = sp)$weight)
    
    output$result <- renderPrint({
      paste("Shortest Path:", paste(path_names, collapse = " -> "), 
            " Total Distance:", path_weight, "km")
    })
    
    path_coords <- city_coords()[match(path_names, city_coords()$city), ]
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          data = path_coords[1, , drop = FALSE],
          ~lon, ~lat, popup = ~city,
          icon = icons(iconUrl = "http://maps.google.com/mapfiles/ms/icons/green-dot.png")
        ) %>%
        addMarkers(
          data = path_coords[-c(1, nrow(path_coords)), , drop = FALSE],
          ~lon, ~lat, popup = ~city,
          icon = icons(iconUrl = "http://maps.google.com/mapfiles/ms/icons/blue-dot.png")
        ) %>%
        addMarkers(
          data = path_coords[nrow(path_coords), , drop = FALSE],
          ~lon, ~lat, popup = ~city,
          icon = icons(iconUrl = "http://maps.google.com/mapfiles/ms/icons/red-dot.png")
        ) %>%
        addPolylines(lng = path_coords$lon, lat = path_coords$lat, color = "blue", weight = 4)
    })
    
    
    edges_data <- data.frame(
      from = edges[seq(1, length(edges), by = 2)],
      to = edges[seq(2, length(edges), by = 2)],
      value = weights
    )
    nodes_data <- data.frame(id = V(g)$name)
    
    output$graph <- renderVisNetwork({
      visNetwork(nodes_data, edges_data) %>%
        visEdges(smooth = TRUE) %>%
        visNodes(shape = "dot", color = list(border = "black", background = "lightblue"))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
