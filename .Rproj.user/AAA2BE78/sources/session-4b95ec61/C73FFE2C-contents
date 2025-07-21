library(shiny)
library(DT)
library(readxl)
library(readr)

ui <- fluidPage(
  titlePanel("Bird Observation Entry Tool"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("site", "Site", ""),
      textInput("waypoint", "Waypoint", ""),
      textInput("field_date", "Field date", "10. júní"),
      numericInput("year", "Year", value = 2025, min = 2000, max = 2100),
      textInput("observer", "Observer", "Einar Ó. Þorleifsson"),
      textInput("time_begun", "Time begun", ""),
      textInput("time_ended", "Time ended", ""),
      
      textAreaInput("species", "Species (comma-separated)", ""),
      textAreaInput("total", "Total (comma-separated)", ""),
      textAreaInput("distance", "Distance (comma-separated)", ""),
      textAreaInput("activity", "Activity (comma-separated, use 'ekkert' for blank)", ""),
      actionButton("submit", "Submit Entry")
    ),
    
    mainPanel(
      h4("Preview and Edit Observations"),
      DTOutput("data_table"),
      br(),
      downloadButton("downloadData", "Download Current Batch (CSV)"),
      actionButton("newpoint", "Start New Point")
    )
  )
)

server <- function(input, output, session) {
  # Load species list for validation
  valid_species <- readLines("non_rare_species.txt", encoding = "UTF-8")
  
  # Helper function to clean and correct species
  correct_species <- function(species_vec) {
    purrr::map_chr(species_vec, function(x) {
      x <- trimws(x)
      if (x == "Lóa") {
        return("Heiðlóa")
      }
      exact <- valid_species[valid_species == x]
      if (length(exact) > 0) return(exact[1])
      matches <- agrep(x, valid_species, max.distance = 0.3, value = TRUE)
      if (length(matches) > 0) return(matches[1])
      # fallback
      if (x == "Bóðinsanni") return("Óðinshani")
      matches_loose <- agrep(x, valid_species, max.distance = 0.5, value = TRUE)
      if (length(matches_loose) > 0) return(matches_loose[1])
      return("")
    })
  }
  values <- reactiveValues(data = data.frame())
  
  observeEvent(input$submit, {
    species <- strsplit(input$species, ",")[[1]]
    total <- strsplit(input$total, ",")[[1]]
    distance <- strsplit(input$distance, ",")[[1]]
    activity <- strsplit(input$activity, ",")[[1]]
    
    # Clean whitespace
    clean <- function(x) trimws(x)
    species <- correct_species(clean(species))
    total <- as.integer(clean(total))
    distance <- as.integer(clean(distance))
    activity <- clean(activity)
    activity[activity == "ekkert"] <- ""
    
    n <- length(species)
    
    if (!(length(total) == n && length(distance) == n && length(activity) == n)) {
      showModal(modalDialog(
        title = "Input Length Mismatch",
        "All input fields must have the same number of comma-separated values.",
        easyClose = TRUE
      ))
    } else {
      new_entries <- data.frame(
        Site = rep(input$site, n),
        Waypoint = rep(input$waypoint, n),
        Field_date = rep(input$field_date, n),
        Year = rep(input$year, n),
        Observer = rep(input$observer, n),
        Time_begun = rep(input$time_begun, n),
        Time_ended = rep(input$time_ended, n),
        Species = species,
        Total = total,
        Distance = distance,
        Activity = activity,
        stringsAsFactors = FALSE
      )
      values$data <- rbind(values$data, new_entries)
    }
  })
  
  output$data_table <- renderDT({
    datatable(values$data, editable = TRUE, options = list(pageLength = 10))
  })
  
  observeEvent(input$newpoint, {
    updateTextInput(session, "waypoint", value = "")
    updateTextAreaInput(session, "species", value = "")
    updateTextAreaInput(session, "total", value = "")
    updateTextAreaInput(session, "distance", value = "")
    updateTextAreaInput(session, "activity", value = "")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("bird_data_batch_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(values$data, file)
    }
  )
}

shinyApp(ui, server)
