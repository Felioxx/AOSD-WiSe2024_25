library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(sp)
library(spdep)
library(shinyBS)

#appTitle <- "VoterGroupComparator"

# Read the shapefile once outside the server function to avoid re-reading it every time the app runs
shp_Bund <- st_read("Data/Bundestagswahlergebnisse_joined_Indikatoren.shp")
shp_Euro <- st_read("Data/Europawahlergebnisse_joined_Indikatoren.shp")

# Transforms the shapefiles into Web Mercator Projection
shp_Bund <- st_transform(shp_Bund, 4326)
shp_Euro <- st_transform(shp_Euro, 4326)



server <- function(input, output, session) {
  
  # infotext
  observeEvent(input$infoBtn, {
    toggleModal(session, "infoModal")
  })
  observeEvent(input$infoBtn2, {
    toggleModal(session, "infoModal2")
  })
  
  # initialisation of variable influence plot
  x <- shp_Euro$X2_Anteil.d
  for (i in seq_along(x)) {
    x[i] <- 100-x[i]
  }
  # Plot
  plot_obj <- reactive({
    x <- as.numeric(x)
    y <- as.numeric(shp_Euro$Volt2)
    plot(x, y, xlab = "employed adults", ylab = "Volt", main = "Influence of filters on election results of Volt")
    abline(lm(y ~ x), col = "red")
    })
  plot_obj2 <- reactive({
  })
  plot_obj3 <- reactive({
  })

  
  # initialisation of Moran I
  moran_I <- NULL
  
  #Leaflet calculation
  calculateLeaflet <- eventReactive(input$update_map, {
    if (input$selected_election == "European Election") {
      shp <- shp_Euro
      euro_or_bund <- "European Election"
    } else {
      shp <- shp_Bund
      euro_or_bund <- "Bundestag Election"
    }
    
    if (input$party_votes == "Union") {
      party_votes <- shp$Union2
    } else if(input$party_votes == "SPD") {
      party_votes <- shp$SPD2
    } else if(input$party_votes == "Grüne") {
      party_votes <- shp$Grüne2
    } else if(input$party_votes == "FDP") {
      party_votes <- shp$FDP2
    } else if(input$party_votes == "AFD") {
      party_votes <- shp$AFD2
    } else if(input$party_votes == "Volt") {
      party_votes <- shp$Volt2
    } else if(input$party_votes == "Linke") {
      party_votes <- shp$Linke2
    } else if(input$party_votes == "Die PARTEI") {
      party_votes <- shp$DiePartei2
    } else if(input$party_votes == "Tierschutzpartei") {
      party_votes <- shp$Tierschut2
    } else if(input$party_votes == "Familienpartei") {
      party_votes <- shp$Familie2
    } else if(input$party_votes == "Freie Wähler") {
      party_votes <- shp$FreiWäh2
    } else if(input$party_votes == "ÖDP") {
      party_votes <- shp$ÖDP2
    } else if(input$party_votes == "BSW") {
      party_votes <- shp$BSW2
    } else if(input$party_votes == "PdF") {
      party_votes <- shp$PdF2
    } else {
      party_votes <- NULL
    }

    # calculation of Moran I
    W <- spdep::poly2nb(shp) # create neighbor-structure
    W <- spdep::nb2listw(W) # convert neighbor-structure into list
    morans_I <- spdep::moran.test(party_votes, W)
    output$MoranI <- renderUI({
      HTML(paste(
        morans_I$method, br(), br(),
        "standard deviate =", morans_I$statistic, "; p-value <", morans_I$p.value, br(),
        "alternative hypothesis:", morans_I$alternative, br(), br(),
        "sample estimates", br(),
        "Moran I statistic:", morans_I$estimate[1], br(),
        "Expectation:", morans_I$estimate[2], br(),
        "Variance:", morans_I$estimate[3]))
    })
    

    
    migration <- ""
    single_parent <- ""
    migration_text <- ""
    migration_text2 <- ""
    single_parent_text <- ""
    single_parent_text2 <- ""
    
    if (input$gender == "total") {                                #both
      if (input$age == "Children (0-14 years)") {                     # children
        if (input$unemployed == TRUE) {                                   #unemployed
          filter <- shp$X12_Anteil
        }
        else {                                                            # employed
          for (i in seq_along(shp$X12_Anteil)) {
            shp$X12_Anteil[i] <- 100-shp$X12_Anteil[i]
          }
          filter <- shp$X12_Anteil
        }
        if (input$migration == TRUE) {                                    # migration background
          migration <- shp$X11_Anteil
          migration_text <- "migration background: "
          migration_text2 <- "%"
        }
        if (input$single_parent == TRUE) {                                # in single parent household
          single_parent <- shp$X10_Anteil
          single_parent_text <- "in a single-parent household: "
          single_parent_text2 <- "%"
        }
      } else {                                                       # adult
        if (input$unemployed == TRUE) {                                   #unemployed
          filter <- shp$X13_Anteil
        }
        else {                                                            #employed
          for (i in seq_along(shp$X13_Anteil)) {
            shp$X13_Anteil[i] <- 100-shp$X13_Anteil[i]
          }
          filter <- shp$X13_Anteil
        }
      }
    } else if (input$gender == "female"){                    # female
      if (input$age == "Children (0-14 years)") {               # children
        if (input$unemployed == TRUE) {                             #unemployed
          filter <- shp$X7_Anteil.d
        }
        else {
          for (i in seq_along(shp$X7_Anteil.d)) {
            shp$X7_Anteil.d[i] <- 100-shp$X7_Anteil.d[i]
          }
          filter <- shp$X7_Anteil.d
        }
        if (input$migration == TRUE) {                             # migration background
          migration <- shp$X6_Anteil.d
          migration_text <- "migration background: "
          migration_text2 <- "%"
        }
      } else {                                                 #adult
        if (input$unemployed == TRUE) {                            #unemployed
          filter <- shp$X8_Anteil.d
        }
        else {                                                     #employed
          for (i in seq_along(shp$X8_Anteil.d)) {
            shp$X8_Anteil.d[i] <- 100-shp$X8_Anteil.d[i]
          }
          filter <- shp$X8_Anteil.d
        }
      }
    } else {                                              # male
      if (input$age == "Children (0-14 years)") {            # children
        if (input$unemployed == TRUE) {                          #unemployed
          filter <- shp$X3_Anteil.d
        }
        else {                                                   #employed
          for (i in seq_along(shp$X3_Anteil.d)) {
            shp$X3_Anteil.d[i] <- 100-shp$X3_Anteil.d[i]
          }
          filter <- shp$X3_Anteil.d
        }
        if (input$migration == TRUE) {                           # migration background
          migration <- shp$X4_Anteil.d
          migration_text <- "migration background: "
          migration_text2 <- "%"
        }
      } else {                                              # adult
        if (input$unemployed == TRUE) {                         #unemployed
          filter <- shp$X2_Anteil.d
        }
        else {                                                  # employed
          for (i in seq_along(shp$X2_Anteil.d)) {
            shp$X2_Anteil.d[i] <- 100-shp$X2_Anteil.d[i]
          }
          filter <- shp$X2_Anteil.d
        }
      }
    }
    
    # variable influence plot
    output$scatterPlot <- renderPlot({
      input$update_map
      x1 <- as.numeric(filter)
      y <- as.numeric(party_votes)
      plot(x1, y, xlab = "filters", ylab = input$party_votes, main = paste("Influence of filters on election results of ", input$party_votes))
      abline(lm(y ~ x1), col = "red")
    })
    output$scatterPlot2 <- renderPlot({
      input$update_map
      x2 <- as.numeric(migration)
      y <- as.numeric(party_votes)
      if(migration[[1]] != "") {
        plot(x2, y, xlab = "migration", ylab = input$party_votes, main = paste("Influence of migration background on election results of ", input$party_votes))
        abline(lm(y ~ x2), col = "red")
      }
    })
    output$scatterPlot3 <- renderPlot({
      input$update_map
      x3 <- as.numeric(single_parent)
      y <- as.numeric(party_votes)
      if(single_parent[[1]] != "") {
        plot(x3, y, xlab = "single_parent", ylab = input$party_votes, main = paste("Influence of single parent household on election results of ", input$party_votes))
        abline(lm(y ~ x3), col = "red")
      }
    })
    
    if (!is.null(party_votes)) {
      # Compute relative quantity of votes
      vote_share <- party_votes / shp$Wähler
      vote_share <- round(vote_share * 100, 2)
      
      # Remove numbers from the area names (gebiet.nam)
      for (i in seq_along(shp$gebiet.nam)) {
        shp$gebiet.nam[i] <- substr(shp$gebiet.nam[i], 7, nchar(shp$gebiet.nam[i]))
      }
      
      # Define custom palette
      my_palette <- colorNumeric(
        palette = c("#FFFF00", "#F7FE2E", "#F4FA58", "#F2F5A9", "#FFEDA0", "#FCCC0A", "#FDBB63", "#FAA60A", "#F99D13", "#F78F18", "#F57E1D", "#F36C23", "#F15A29", "#EB273B", "#E91641", "#E70547"),
        domain = range(vote_share)
      )
      
      # Create the map object
      basemap <- leaflet() %>%
        # add OSM
        addProviderTiles(
          "OpenStreetMap",
          group = "OpenStreetMap"
        ) %>%
        # add shapefile
        addPolygons(
          data = shp,
          popup = paste("share of people according to filters: ", filter, "%", br(),
                        migration_text, migration, migration_text2, br(),
                        single_parent_text, single_parent, single_parent_text2),
          #popupOptions(maxHeight = 400),
          color = ~my_palette(vote_share),
          stroke = TRUE,
          weight = 1,
          opacity = 1,
          fillOpacity = 1,
          label = ~paste(gebiet.nam, ":", vote_share, "%"),
          group = euro_or_bund,
          highlight = highlightOptions(
            weight = 5,
            color = "blue",
            bringToFront = TRUE
          )
        ) %>%
        #addPopups(data = shp,
        #          lng = ~st_coordinates(st_centroid(shp$geometry))[,1],
        #          lat = ~st_coordinates(st_centroid(shp$geometry))[,2],
        #          popup = "Test")%>%
        # add Legend
        addLegend(pal = my_palette, values = vote_share,
                  title = "Second votes for selected party (in %)",
                  position = "bottomright") %>%
        # add a layers control
        addLayersControl(
          baseGroups = c("OpenStreetMap"),
          overlayGroups = (euro_or_bund),
          position = "topleft"
        )
      
      return(basemap)
    }
  })
  
  output$map <- renderLeaflet({
    calculateLeaflet()
  })
  output$moran_I <- renderText({
    return(moran_I)
  })
  output$scatterPlot <- renderPlot({
    plot_obj()
  })
  output$scatterPlot2 <- renderPlot({
    plot_obj2()
  })
  output$scatterPlot3 <- renderPlot({
    plot_obj3()
  })
}

# TODO: über GitHub hosten oder Docker Image bereitstellen oder shinyapps.io (hosten von Shiny apps)