library(ggplot2)
library(dplyr)
library(stringr)
library(lintr)
library(styler)

# stacked bar chart of each series 

all_data <- read.csv("C://Users//creat//OneDrive - UW//winter 2023//INFO 201//Checkouts_by_Title.csv", stringsAsFactors = FALSE)

all_data <- all_data %>%
  filter(Creator == "Sarah J. Maas" | Creator == "Maas, Sarah J.")

#by_book <- all_data

all_data$Title[str_detect(tolower(all_data$Title), "a court of")] <- "A Court of Thorns and Roses Series"

all_data$Title[str_detect(tolower(all_data$Title), "throne of glass")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "crown of midnight")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "empire of storms")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "heir of fire")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "kingdom of ash")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "queen of shadows")] <- "Throne of Glass Series"
all_data$Title[str_detect(tolower(all_data$Title), "tower of dawn")] <- "Throne of Glass Series"

all_data$Title[str_detect(tolower(all_data$Title), "crescent city")] <- "Crescent City Series"

all_data$Title[str_detect(tolower(all_data$Title), "house of earth and blood")] <- "Crescent City Series"
all_data$Title[str_detect(tolower(all_data$Title), "house of sky and breath")] <- "Crescent City Series"

Series <- c("A Court of Thorns and Roses Series", "Crescent City Series", "Soulstealer", "Throne of Glass Series")

all_data <- all_data %>%
  filter(Title == Series)

ggplot(data = all_data) +
  geom_bar(mapping = aes(fill = MaterialType, y = Checkouts, x = Title), position = "stack", stat = "identity")
