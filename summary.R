library(dplyr)
library(stringr)

all_data <- read.csv("C://Users//creat//OneDrive - UW//winter 2023//INFO 201//Checkouts_by_Title.csv", stringsAsFactors = FALSE)

all_data <- all_data %>%
  filter(Creator == "Sarah J. Maas" | Creator == "Maas, Sarah J.")

# GENERAL SUMMARY VALUES
type_highest_avg_checkouts <- all_data %>%
  group_by(MaterialType) %>%
  summarize(average = sum(Checkouts) / length(unique(CheckoutYear))) %>%
  filter(average == max(average)) %>%
  pull(MaterialType)

type_lowest_avg_checkouts <- all_data %>%
  group_by(MaterialType) %>%
  summarize(average = sum(Checkouts) / length(unique(CheckoutYear))) %>%
  filter(average == min(average)) %>%
  pull(MaterialType)

year_most_checkouts <- all_data %>%
  #filter(MaterialType == "BOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == max(sum, na.rm = TRUE)) %>%
  pull(CheckoutYear)

year_least_checkouts <- all_data %>%
  group_by(CheckoutYear) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == min(sum, na.rm = TRUE)) %>%
  pull(CheckoutYear)

# SARAH J MAAS SPECIFIC SUMMARY VALUES

by_book <- all_data

by_book$Title[str_detect(tolower(by_book$Title), "a court of frost and starlight")] <- "A Court of Frost and Starlight"
by_book$Title[str_detect(tolower(by_book$Title), "a court of mist and fury")] <- "A Court of Mist and Fury"
by_book$Title[str_detect(tolower(by_book$Title), "a court of silver flames")] <- "A Court of Silver Flames"
by_book$Title[str_detect(tolower(by_book$Title), "a court of wings and ruin")] <- "A Court of Wings and Ruin"
by_book$Title[str_detect(tolower(by_book$Title), "a court of thorns and roses")] <- "A Court of Thorns and Roses"

by_book$Title[str_detect(tolower(by_book$Title), "throne of glass")] <- "Throne of Glass"
by_book$Title[str_detect(tolower(by_book$Title), "crown of midnight")] <- "Crown of Midnight"
by_book$Title[str_detect(tolower(by_book$Title), "empire of storms")] <- "Empire of Storms"
by_book$Title[str_detect(tolower(by_book$Title), "heir of fire")] <- "Heir of Fire"
by_book$Title[str_detect(tolower(by_book$Title), "kingdom of ash")] <- "Kingdom of Ash"
by_book$Title[str_detect(tolower(by_book$Title), "queen of shadows")] <- "Queen of Shadows"
by_book$Title[str_detect(tolower(by_book$Title), "tower of dawn")] <- "Tower of Dawn"

by_book$Title[str_detect(tolower(by_book$Title), "catwoman")] <- "Catwoman: Soulstealer"

by_book$Title[str_detect(tolower(by_book$Title), "house of sky and breath")] <- "House of Sky and Breath"
by_book$Title[str_detect(tolower(by_book$Title), "house of earth and blood")] <- "House of Earth and Blood"

books <- c("A Court of Frost and Starlight", "A Court of Mist and Fury", "A Court of Silver Flames", "A Court of Wings and Ruin", "A Court of Thorns and Roses", "Throne of Glass", "Crown of Midnight", "Empire of Storms", "Heir of Fire", "Kingdom of Ash", "Queen of Shadows", "Tower of Dawn", "Catwoman: Soulstealer", "House of Sky and Breath", "House of Earth and Blood")

book_most_checkouts <- by_book %>%
  group_by(Title) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == max(sum)) %>%
  pull(Title)

book_least_checkouts <- by_book %>%
  filter(Title == books) %>%
  group_by(Title) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == min(sum)) %>%
  pull(Title)

# - which series (throne of glass, court of thorns and roses, crescent city) is checked out more on average?
by_series <- all_data

by_series$Title[str_detect(tolower(by_series$Title), "a court of")] <- "A Court of Thorns and Roses Series"

by_series$Title[str_detect(tolower(by_series$Title), "throne of glass")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "crown of midnight")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "empire of storms")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "heir of fire")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "kingdom of ash")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "queen of shadows")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "tower of dawn")] <- "Throne of Glass Series"

#by_series$Title[str_detect(tolower(by_series$Title), "catwoman")] <- "Catwoman: Soulstealer"

by_series$Title[str_detect(tolower(by_series$Title), "crescent city")] <- "Crescent City Series"

by_series$Title[str_detect(tolower(by_series$Title), "house of earth and blood")] <- "Crescent City Series"
by_series$Title[str_detect(tolower(by_series$Title), "house of sky and breath")] <- "Crescent City Series"

series_most_checkouts <- by_series %>%
  group_by(Title) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == max(sum)) %>%
  pull(Title)

Series <- c("A Court of Thorns and Roses Series", "Crescent City Series", "Throne of Glass Series")

series_least_checkouts <- by_series %>%
  filter(Title == Series) %>%
  group_by(Title) %>%
  summarize(sum = sum(Checkouts)) %>%
  filter(sum == min(sum)) %>%
  pull(Title)
