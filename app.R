#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("leaflet")
# install.packages("tidygeocoder")
# install.packages("revgeo")
# install.packages("shinyjs")
# install.packages("shinyFiles")

library(shiny)
library(shinythemes)   # Ajout d'un theme moderne
library(shinyWidgets)  # Boutons et sliders am?lior?s
library(leaflet)         # Carte interactive
library(tidygeocoder)    # Géolocalisation
library(revgeo)          # Conversion coordonnées -> adresse
library(shinyjs)
library(shinyFiles)


# UI : Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Choix du thème
  titlePanel("Questionnaire - Agricole avec cartographie interactive"),
  
  # Mise en page centrée
  div(style = "max-width: 90%; margin: auto;",
      wellPanel(
        textInput("name", "Votre nom :", placeholder = "Entrez votre nom ici..."),
        
        radioButtons("gender", "Genre :", choices = c("Homme", "Femme", "Autre"), 
                     inline = TRUE),
        
        sliderInput("age", "Votre age :", min = 18, max = 100, value = 25),
        
        checkboxGroupButtons(
          inputId = "interests",
          label = "Vos Centres d'interêt :", 
          choices = c("Agriculture bio", "Circuit court", "Labels de qualité ", "Accompagnement au changement"),
          checkIcon = list(yes = icon("check"))
        ),
        
        selectInput("typeculture", "Quel type de culture ?", 
                    choices = c("Céréales", "Légumineuses", "Fruits", "Légumes")),
        selectInput("batimentagricole", "Avez-vous des bâtiments agricoles ?", 
                    choices = c("Oui", "Non")),
        
        textInput("fonction", "Fonction du bâtiment:", ""),
        
        # Section 2: Carte Interactive pour localiser les bâtiments et parcelles
        h3("Localisation des Bâtiments et Parcelles"),
        fluidRow(
          column(width = 10,
                 textInput("adresse", placeholder = "Recherchez une adresse :", label=NULL),
          ),
          column(width = 2, offset = 0,
                 actionButton("rechercher", "Aller à l'adresse")
                 # uiOutput("actionBut.out")
          )
        ),
        leafletOutput("map", width = "100%", height = "400px"),
        
        # Section 3: Joindre un document
        h3("Exemple pour ajouter un document"),
        fileInput("file1", "Télécharger un document (PDF, JPG, PNG)", accept = c(".pdf", ".jpg", ".png")),
        
        actionBttn("submit", "Soumettre", style = "fill", color = "primary")
      ),
      
      # Affichage des réponses
      wellPanel(
        h4("Réponses enregistrées :"),
        tableOutput("responses")
      )
  )
)

# Server : Logique de l'application
server <- function(input, output, session) {
  # Variables réactives pour les données du formulaire
  selected_marker <- reactiveVal(NULL)
  
  # Carte interactive Leaflet
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.2137, lat = 46.6033, zoom = 6) %>%
      addMarkers(lng = 2.2137, lat = 46.6033, popup = "Cliquez ici pour localiser votre bâtiment ou parcelle.")
  })
  
  # Capturer les clics sur la carte et mettre à jour le marqueur
  observeEvent(input$map_click, {
    coord <- input$map_click
    selected_marker(coord)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = coord$lng, lat = coord$lat, popup = "Votre bâtiment ou parcelle")
  })
  
  # Aller à l'adresse recherchée
  observeEvent(input$rechercher, {
    req(input$adresse)
    geocode_url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", 
                          URLencode(input$adresse))
    
    # Obtenir les coordonnées depuis OpenStreetMap
    result <- jsonlite::fromJSON(geocode_url)
    if (length(result) > 0) {
      lat <- as.numeric(result$lat[1])
      lng <- as.numeric(result$lon[1])
      
      selected_marker(list(lat = lat, lng = lng))
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = lng, lat = lat, zoom = 15) %>%
        addMarkers(lng = lng, lat = lat, popup = input$adresse)
    } else {
      showNotification("Adresse non trouvée.", type = "error")
    }
  })
  
  
  # Traitement
  responses <- reactiveVal(data.frame(Nom = character(), Genre = character(), 
                                      age = numeric(), Interets = character(),
                                      Culture = character(), "Batiment agricoles" = character(),
                                      Adresse = character(),
                                      Fonction = character(),
                                      Fichier = character(),
                                      stringsAsFactors = FALSE))
  
    renderText({
    if (is.null(input$file1)) {
      return("Aucun fichier téléchargé.")
    }
    paste("Nom du fichier téléchargé :", input$file1$name)
  })
  
  observeEvent(input$submit, {
    new_entry <- data.frame(
      Nom = input$name,
      Genre = input$gender,
      age = input$age,
      Interets = paste(input$interests, collapse = ", "),
      Culture = input$typeculture,
      "Batiment agricoles" = input$batimentagricole,
      Adresse = if (!is.null(selected_marker())) paste("Bâtiment/parcelle selectionné: Longitude =", round(selected_marker()$lng, 4)," Latitude =", round(selected_marker()$lat, 4)) else "Aucun bâtiment/parcelle selectionné sur la carte",
      Fonction = input$fonction,
      Fichier = if (is.null(input$file1)) "Aucun fichier téléchargé." else paste("Nom du fichier téléchargé :", input$file1$name),
      stringsAsFactors = FALSE
    )
    responses(rbind(responses(), new_entry))
    
    showModal(modalDialog(
      title = "Merci pour votre soumission",
      paste("Votre questionnaire a été soumis avec succès. Vous trouverez un résumé de votre questionnaire en bas de la page."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$responses <- renderTable({
    responses()
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
