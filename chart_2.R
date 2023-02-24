library(dplyr)
library(stringr)
library(ggplot2)

all_data <- read.csv("C://Users//creat//OneDrive - UW//winter 2023//INFO 201//Checkouts_by_Title.csv", stringsAsFactors = FALSE)
by_series <- all_data %>%
  filter(Creator == "Sarah J. Maas" | Creator == "Maas, Sarah J.")

by_series$Title[str_detect(tolower(by_series$Title), "a court of")] <- "A Court of Thorns and Roses Series"

by_series$Title[str_detect(tolower(by_series$Title), "throne of glass")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "crown of midnight")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "empire of storms")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "heir of fire")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "kingdom of ash")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "queen of shadows")] <- "Throne of Glass Series"
by_series$Title[str_detect(tolower(by_series$Title), "tower of dawn")] <- "Throne of Glass Series"

by_series$Title[str_detect(tolower(by_series$Title), "catwoman")] <- "Catwoman: Soulstealer"

by_series$Title[str_detect(tolower(by_series$Title), "crescent city")] <- "Crescent City Series"

by_series$Title[str_detect(tolower(by_series$Title), "house of earth and blood")] <- "Crescent City Series"
by_series$Title[str_detect(tolower(by_series$Title), "house of sky and breath")] <- "Crescent City Series"

Series <- c("A Court of Thorns and Roses Series", "Crescent City Series", "Catwoman: Soulstealer", "Throne of Glass Series")

by_series <- by_series %>%
  filter(Title == Series, na.rm = TRUE) %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"), na.rm = TRUE) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(Title, date) %>%
  summarize(sum = sum(Checkouts))

ggplot(data = by_series) +
  geom_line(mapping = aes(x = date , y = sum, color = Title)) + 
  labs(title = "Checkouts over Time by Book Series", x = "Date", y = "Checkouts") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

