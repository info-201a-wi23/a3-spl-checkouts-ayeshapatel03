library(ggplot2)
library(dplyr)
library(stringr)

all_data <- read.csv("C://Users//creat//OneDrive - UW//winter 2023//INFO 201//Checkouts_by_Title.csv", stringsAsFactors = FALSE)

all_data <- all_data %>%
  filter(Creator == "Sarah J. Maas" | Creator == "Maas, Sarah J.")

by_type <- all_data %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(MaterialType, date) %>%
  summarize(sum = sum(Checkouts))
  
ggplot(data = by_type) +
  geom_line(
    mapping = aes(x = date, y = sum, color = MaterialType)) +
  labs(title = "Checkouts over Time by Material Type", x = "Date", y = "Checkouts") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

  
