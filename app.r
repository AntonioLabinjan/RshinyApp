library(shiny)
library(igraph)
library(leaflet)

# Predefinirane koordinate za gradove (latitude, longitude)
city_coords <- data.frame(
  city = c("Zagreb", "Rijeka", "Split", "Osijek"),
  lat = c(45.8150, 45.3271, 43.5081, 45.5511),
  lon = c(15.9819, 14.4422, 16.4402, 18.6955)
)

# UI aplikacije
ui <- fluidPage(
  titlePanel("Solver za najkraći put među gradovima"),
  sidebarLayout(
    sidebarPanel(
      textInput("cities", "Gradovi (odvojeni zarezom, npr. Zagreb,Rijeka,Split):", value = "Zagreb,Rijeka,Split,Osijek"),
      textAreaInput("distances", "Cestovne udaljenosti (npr. Zagreb->Rijeka,150; Rijeka-Split,250):", 
                    value = "Zagreb->Rijeka,150; Rijeka-Split,250; Split->Osijek,300; Zagreb-Osijek,400"),
      textInput("start", "Početni grad:", value = "Zagreb"),
      textInput("end", "Završni grad:", value = "Osijek"),
      actionButton("solve", "Izračunaj najkraći put")
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("result"),
      verbatimTextOutput("error")
    )
  )
)

# Server aplikacije
server <- function(input, output) {
  observeEvent(input$solve, {
    # Parsiraj gradove i cestovne udaljenosti
    cities <- unique(unlist(strsplit(gsub(" ", "", input$cities), ",")))  # Ukloni razmake
    distances_input <- unlist(strsplit(gsub(" ", "", input$distances), ";"))   # Ukloni razmake
    
    edges <- c()
    weights <- c()
    directed <- NULL  # Postavi na NULL kako bismo odlučili na temelju ulaza
    error_message <- NULL  # Za bilježenje grešaka
    
    for (distance in distances_input) {
      parts <- unlist(strsplit(distance, ","))
      if (length(parts) != 2) {
        error_message <- "Neispravan format cestovnih udaljenosti! Provjerite unos."
        break
      }
      
      connection <- NULL
      if (grepl("->", parts[1])) {
        directed <- TRUE
        connection <- unlist(strsplit(parts[1], "->"))
      } else if (grepl("-", parts[1])) {
        directed <- FALSE
        connection <- unlist(strsplit(parts[1], "-"))
      } else {
        error_message <- "Cestovne udaljenosti moraju sadržavati oznaku '-' ili '->'!"
        break
      }
      
      edges <- c(edges, connection)
      weights <- c(weights, as.numeric(parts[2]))
    }
    
    if (!is.null(error_message)) {
      # Ako postoji greška, prikaži je i prekini daljnje izvršavanje
      output$error <- renderPrint({ error_message })
      output$result <- renderPrint({ "" })
      output$map <- renderLeaflet({ leaflet() %>% addTiles() })  # Prikaži praznu kartu
      return()
    } else {
      output$error <- renderPrint({ "" })  # Očisti grešku
    }
    
    # Kreiraj graf
    g <- graph(edges = edges, directed = directed)
    E(g)$weight <- weights
    
    # Provjera valjanosti početnog i završnog grada
    if (!(input$start %in% V(g)$name) || !(input$end %in% V(g)$name)) {
      output$error <- renderPrint({ "Početni ili završni grad ne postoji u grafu!" })
      output$result <- renderPrint({ "" })
      output$map <- renderLeaflet({ leaflet() %>% addTiles() })  # Prikaži praznu kartu
      return()
    }
    
    # Izračunaj najkraći put
    path <- shortest_paths(g, from = input$start, to = input$end, weights = E(g)$weight)$vpath[[1]]
    if (length(path) == 0) {
      output$result <- renderPrint({ "Nema dostupnog puta između gradova!" })
      output$map <- renderLeaflet({
        leaflet() %>% addTiles()
      })
      return()
    }
    
    path_names <- V(g)[path]$name
    path_weight <- sum(E(g, path = path)$weight)
    
    # Prikaži rezultat
    output$result <- renderPrint({
      paste("Najkraći put:", paste(path_names, collapse = " -> "), 
            " Ukupna udaljenost:", path_weight, "km")
    })
    
    # Kreiraj mapu
    path_coords <- city_coords[city_coords$city %in% path_names, ]
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addMarkers(data = path_coords, ~lon, ~lat, label = ~city) %>%
        addPolylines(lng = path_coords$lon, lat = path_coords$lat, color = "blue")
    })
  })
}

# Pokretanje aplikacije
shinyApp(ui = ui, server = server)
