library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_squads"
html <- read_html(url)

# Extract and parse player squad tables
players <- html %>%
  html_nodes("table.wikitable") %>%
  map_df(function(tbl) {
    df <- html_table(tbl)
    
    # Check if the table is a player squad table
    if (all(c("Player", "Date of birth (age)") %in% colnames(df))) {
      country <- tbl %>% 
        html_node(xpath = "preceding::h3[1]") %>% 
        html_text(trim = TRUE)
      
      df %>%
        mutate(
          Country = country,
          Position = str_remove(Pos., "^\\d+"),
          DOB = str_extract(`Date of birth (age)`, "\\d{4}-\\d{2}-\\d{2}"),
          Age = as.integer(str_extract(`Date of birth (age)`, "(?<=aged\\s)\\d+")),
          Club = str_trim(Club)
        ) %>%
        select(Player, Country, Position, DOB, Age, Club)
    }
  })

# Write to CSV
write_csv(players, "players.csv")
