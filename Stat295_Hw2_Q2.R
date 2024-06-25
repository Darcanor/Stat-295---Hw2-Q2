# Define required libraries
libraries <- c("ggplot2","shiny", "tidyverse", "magrittr",
               "leaflet", "dplyr", "rvest", "sqldf", "httr")

# Install packages
install.packages("libraries")

# Load required libraries
lapply(libraries, require, character.only = TRUE)

# Read the dataset
dataset <- read_csv("spotify_data.csv")
summary(dataset)

# Define the user interface
ui <- fluidPage(
  titlePanel(
    div(style = "color: #2c3e50; font-weight: bold; font-size: 36px;", "Track Genre Suggestion")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("First: Select Intervals", style = "color: #e74c3c; font-weight: bold;"),
      # Slider inputs for musical attributes
      sliderInput("danceability", "Danceability:", min = 1, max = 100, value = c(30, 70), 
                  pre = tags$b(), post = tags$b()),
      sliderInput("loudness", "Loudness:", min = 1, max = 100, value = c(30, 70), 
                  pre = tags$b(), post = tags$b()),
      sliderInput("valence", "Valence:", min = 1, max = 100, value = c(30, 70), 
                  pre = tags$b(), post = tags$b()),
      sliderInput("tempo", "Tempo:", min = 1, max = 100, value = c(30, 70), 
                  pre = tags$b(), post = tags$b()),
      # Button to suggest genres
      actionButton("suggest", "Suggest Genres", style = "background-color: #3498db; color: #fff; font-weight: bold;"),
      
      h3("Second: Select Genre", style = "color: #e74c3c; font-weight: bold;"),
      # Dynamic UI for genre selection
      uiOutput("genre_ui"),
      # Button to suggest tracks
      actionButton("suggest_tracks", "Suggest Tracks", style = "background-color: #3498db; color: #fff; font-weight: bold;")
    ),
    
    mainPanel(
      h3("Suggested Genres", style = "color: #2c3e50; font-weight: bold;"),
      tableOutput("suggested_genres"),
      h3("Suggested Tracks", style = "color: #2c3e50; font-weight: bold;"),
      tableOutput("suggested_tracks"),
      
      h3("How to Use the App", style = "color: #2c3e50; font-weight: bold;"),
      # Instructions for using the app
      p("Welcome to the Track Genre Suggestion App! This app helps you discover music genres and tracks based on your preferences for certain musical attributes. Follow the steps below to get started:"),
      # Step 1 instructions
      p(tags$b("Step 1: Select Intervals (Please try to change interval sizes and locations if the app cannot find any genre)")),
      p("On the left sidebar, you'll find sliders that allow you to set your preferred ranges for the following musical attributes:"),
      # Attribute descriptions
      p(tags$b("1. Danceability:"), " This measures how suitable a track is for dancing. It ranges from 1 to 100, where higher values indicate more danceable tracks."),
      p(tags$b("2. Loudness:"), " This represents the overall loudness of a track in decibels, normalized to a range of 1 to 100."),
      p(tags$b("3. Valence:"), " This measures the musical positiveness conveyed by a track. Higher values indicate more positive (happy) tracks."),
      p(tags$b("4. Tempo:"), " This represents the speed or pace of a given piece, measured in beats per minute (BPM), normalized to a range of 1 to 100."),
      p("Adjust the sliders to your desired ranges and click the ", tags$b("Suggest Genres"), " button. The app will suggest the top 5 music genres that match your criteria."),
      # Step 2 instructions
      p(tags$b("Step 2: Select a Genre (Please avoid clicking Suggest Tracks button before Suggest Genres)")),
      p("Once the suggested genres are displayed, you can select one of the genres from the dropdown menu that appears. This selection allows you to further explore tracks within your chosen genre."),
      # Step 3 instructions
      p(tags$b("Step 3: Get Track Suggestions")),
      p("Click the ", tags$b("Suggest Tracks"), " button to get a list of tracks from the selected genre. The tracks are sorted by popularity, showing you the most popular tracks first."),
      p("Enjoy exploring and discovering new music tailored to your preferences!")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Event handler for suggesting genres
  observeEvent(input$suggest, {
    # Normalize data
    normalized_data <- dataset %>%
      mutate(danceability = danceability * 100,
             loudness = scales::rescale(loudness, to = c(1, 100)),
             valence = valence * 100,
             tempo = scales::rescale(tempo, to = c(1, 100)))
    
    # Filter data based on user input
    filtered_data <- normalized_data %>%
      filter(danceability >= input$danceability[1], danceability <= input$danceability[2],
             loudness >= input$loudness[1], loudness <= input$loudness[2],
             valence >= input$valence[1], valence <= input$valence[2],
             tempo >= input$tempo[1], tempo <= input$tempo[2])
    
    # Compute suggested genres
    suggested_genres <- filtered_data %>%
      group_by(track_genre) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      mutate(track_genre = str_replace_all(track_genre, "_", " "))
    
    # Render suggested genres table
    output$suggested_genres <- renderTable({
      suggested_genres
    })
    
    # Render genre selection dropdown
    output$genre_ui <- renderUI({
      selectInput("selected_genre", "Select Genre:", choices = suggested_genres$track_genre)
    })
  })
  
  # Event handler for suggesting tracks
  observeEvent(input$suggest_tracks, {
    req(input$selected_genre)
    
    # Filter tracks based on selected genre
    filtered_tracks <- dataset %>%
      filter(track_genre == input$selected_genre) %>%
      arrange(desc(popularity))
    
    # Render suggested tracks table
    output$suggested_tracks <- renderTable({
      filtered_tracks %>%
        select(track_name, artists, popularity) %>%
        mutate(track_name = str_replace_all(track_name, "_", " "),
               artists = str_replace_all(artists, "_", " "))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)