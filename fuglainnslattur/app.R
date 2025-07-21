library(shiny)
library(DT)
library(readxl)
library(readr)

ui <- fluidPage(
  titlePanel("Innsláttur mófuglar"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("site", "Staður", ""),
      textInput("waypoint", "Punktur", ""),
      textInput("field_date", "Dagsetning", "10. júní"),
      numericInput("year", "Ár", value = 2025, min = 2000, max = 2100),
      textInput("observer", "Rannsakandi", ""),
      textInput("time_begun", "Klukkan hvað", ""),
      
      textAreaInput("species", "Tegundir (komma á milli)", ""),
      textAreaInput("total", "Fjöldi (komma á milli)", ""),
      textAreaInput("distance", "Fjarlægð (komma á milli)", ""),
      textAreaInput("activity", "Atferli (komma á milli)", ""),
      actionButton("submit", "Senda inn")
    ),
    
    mainPanel(
      h4("Skoða töflu"),
      DTOutput("data_table"),
      br(),
      downloadButton("downloadData", "Hala niður (CSV)"),
      actionButton("newpoint", "Byrkja á nýjum punkti")
    )
  )
)

server <- function(input, output, session) {
  # Load species list for validation
  valid_species <- readLines("non_rare_species.txt", encoding = "UTF-8")
  
  # Load distance categories from dropdown.csv
  dropdown_data <- read_csv("../dropdown.csv", col_names = FALSE, show_col_types = FALSE)
  
  # Extract distance ranges (they start after "Distance range" and end before next blank line)
  distance_start <- which(dropdown_data$X1 == "Distance range ")
  distance_ranges <- c()
  if (length(distance_start) > 0) {
    for (i in (distance_start + 1):nrow(dropdown_data)) {
      if (is.na(dropdown_data$X1[i]) || dropdown_data$X1[i] == "") {
        break
      }
      if (!is.na(dropdown_data$X1[i]) && dropdown_data$X1[i] != "") {
        distance_ranges <- c(distance_ranges, dropdown_data$X1[i])
      }
    }
  }
  
  # Function to categorize distance input into predefined ranges
  categorize_distance <- function(distance_input) {
    # Handle non-numeric or empty input
    if (is.na(distance_input) || distance_input == "") {
      return("")
    }
    
    # Convert to numeric if it's a character
    dist_num <- as.numeric(distance_input)
    
    if (is.na(dist_num)) {
      return("")
    }
    
    # Categorize based on ranges
    if (dist_num >= 0 && dist_num <= 50) {
      return("0-50")
    } else if (dist_num >= 51 && dist_num <= 100) {
      return("51-100")
    } else if (dist_num >= 101 && dist_num <= 150) {
      return("101-150")
    } else if (dist_num >= 151 && dist_num <= 200) {
      return("151-200")
    } else if (dist_num >= 201 && dist_num <= 250) {
      return("201-250")
    } else if (dist_num >= 251 && dist_num <= 300) {
      return("251-300")
    } else if (dist_num >= 301 && dist_num <= 350) {
      return("301-350")
    } else if (dist_num >= 351 && dist_num <= 400) {
      return("351-400")
    } else if (dist_num >= 401 && dist_num <= 450) {
      return("401-450")
    } else if (dist_num >= 451 && dist_num <= 500) {
      return("451-500")
    } else if (dist_num > 500) {
      return(">200")  # Using >200 for distances over 500
    } else if (dist_num < 0) {
      return("")  # Invalid distance
    }
    
    return("")  # Fallback
  }
  
  # Function to convert activity abbreviations to full forms
  convert_activity <- function(activity_input) {
    # Handle empty input
    if (is.na(activity_input) || activity_input == "") {
      return("")
    }
    
    # Convert abbreviations to full forms
    activity_lower <- tolower(trimws(activity_input))
    
    if (activity_lower == "sf") {
      return("söngflug")
    } else if (activity_lower == "s") {
      return("syngur")
    } else if (activity_lower == "eltast") {
      return("eltingaleikur")
    } else if (activity_lower == "pungar") {
      return("par með unga")
    } else if (activity_lower == "fl") {
      return("á flugi")
    } else {
      # Return original input if no abbreviation match
      return(activity_input)
    }
  }
  
  # Helper function to clean and correct species
  correct_species <- function(species_vec) {
    purrr::map_chr(species_vec, function(x) {
      x <- trimws(x)
      
      # Case-insensitive check for Lóa
      if (tolower(x) == "lóa") {
        return("Heiðlóa")
      }
      
      # Check for exact match
      exact <- valid_species[valid_species == x]
      if (length(exact) > 0) return(exact[1])
      
      # Fuzzy matching with distance 0.3, sorted by actual edit distance
      matches <- agrep(x, valid_species, max.distance = 0.3, value = TRUE)
      if (length(matches) > 0) {
        distances <- adist(x, matches)
        best_match <- matches[which.min(distances)]
        return(best_match)
      }
      
      # Explicit fallback for known misspelling
      if (x == "Bóðinsanni") return("Óðinshani")
      
      # Fuzzy matching with distance 0.5, sorted by actual edit distance
      matches_loose <- agrep(x, valid_species, max.distance = 0.5, value = TRUE)
      if (length(matches_loose) > 0) {
        distances_loose <- adist(x, matches_loose)
        best_match_loose <- matches_loose[which.min(distances_loose)]
        return(best_match_loose)
      }
      
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
    
    # Categorize distances instead of converting to integer
    distance <- sapply(clean(distance), categorize_distance)
    
    activity <- sapply(clean(activity), convert_activity)
    
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
