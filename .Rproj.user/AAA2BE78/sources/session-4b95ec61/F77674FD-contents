library(pdftools)
library(tidyverse)

pdf_text <- pdf_text("islenskir-fuglar-2009.pdf")
all_text <- paste(pdf_text, collapse = "\n")
lines <- strsplit(all_text, "\n")[[1]] %>% str_trim() %>% .[. != ""]
df <- tibble(line = lines) %>% 
  separate(line, into = c("icelandic", "scientific", "english", "category", "rare_flag", "count"), sep = "\\s{3,}", extra = "merge", fill = "right") %>%
  filter(!is.na(category)) %>%
  filter(row_number() > 2) %>%
  slice(1:which(icelandic == "Snjótittlingur")[1])

df |> filter(is.na(rare_flag)) |> select(icelandic) |> write.table("non_rare_species.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



# Observed (dictated) species
dictated_species <- c(
  "Þúvutitlingur", "Þúvutitlingur", "Þúvutitlingur",
  "Stelkur", "Lóa", "Rjópa", "Þúvutitlingur", "Lóuðræll", "Lóðræll", "Lóðræll", "Lóðræll", "Þúvötillingur", "Spói", "Hrosaugaukur", "Þúvötillingur", "Bóðinsanni", "Þúvötillingur", "Lóa"
)

# Load reference species list from file
valid_species <- readLines("non_rare_species.txt", encoding = "UTF-8")

# Compare each dictated name to the closest match in valid_species
corrections <- map_dfr(dictated_species, ~ {
  if (.x == "Lóa") {
    closest_match <- "Heiðlóa"
  } else {
    # First: exact matching
    exact_match <- valid_species[valid_species == .x]
    if (length(exact_match) > 0) {
      closest_match <- exact_match[1]
    } else {
      # Second: fuzzy matching
      matches <- agrep(.x, valid_species, max.distance = 0.3, value = TRUE)
      closest_match <- if(length(matches) > 0) matches[1] else ""
    }
  }
  tibble(Dictated = .x, Closest_match = closest_match)
})

# Second pass: looser matching for empty results
corrections <- corrections %>%
  mutate(Closest_match = case_when(
    Dictated == "Bóðinsanni" ~ "Óðinshani",
    Closest_match == "" ~ {
      matches <- agrep(Dictated, valid_species, max.distance = 0.5, value = TRUE)
      if(length(matches) > 0) matches[1] else ""
    },
    TRUE ~ Closest_match
  ))

print(corrections)