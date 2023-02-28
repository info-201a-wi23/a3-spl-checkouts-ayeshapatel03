library(dplyr)
library(stringr)
library(ggplot2)

all_data <- read.csv("C://Users//creat//OneDrive - UW//winter 2023//INFO 201//Checkouts_by_Title.csv", stringsAsFactors = FALSE)
by_book <- all_data %>%
  filter(Creator == "Sarah J. Maas" | Creator == "Maas, Sarah J.")

by_book$Title[str_detect(tolower(by_book$Title), "a court of frost and starlight")] <- "A Court of Frost and Starlight"
by_book$Title[str_detect(tolower(by_book$Title), "a court of mist and fury")] <- "A Court of Mist and Fury"
by_book$Title[str_detect(tolower(by_book$Title), "a court of silver flames")] <- "A Court of Silver Flames"
by_book$Title[str_detect(tolower(by_book$Title), "a court of wings and ruin")] <- "A Court of Wings and Ruin"
by_book$Title[str_detect(tolower(by_book$Title), "a court of thorns and roses")] <- "A Court of Thorns and Roses"

by_book <- by_book %>%
  mutate(Series = NULL)

by_book$Series[str_detect(tolower(by_book$Title), "a court of")] <- "A Court of Thorns and Roses Series"

by_book <- by_book %>%
  filter(Series == "A Court of Thorns and Roses Series") %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"), na.rm = TRUE) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(Title, date) %>%
  summarize(sum = sum(Checkouts))

ggplot(data = by_book) +
  geom_line(mapping = aes(x = date , y = sum, color = Title)) + 
  labs(title = "Checkouts over Time by Book Series", x = "Date", y = "Checkouts") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()