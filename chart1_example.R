library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_df <- read.csv("C:/Users/ebaga/OneDrive/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

checkouts_by_year_df <- spl_df %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

ebook_checkouts_df <- spl_df %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

ggplot(checkouts_by_year_df) +
  geom_line(aes(x = CheckoutYear, y = checkouts, color = "Total Checkouts")) +
  geom_point(aes(x = CheckoutYear, y = checkouts, color = "Total Checkouts")) +
  geom_line(data = ebook_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Ebook")) +
  geom_point(data = ebook_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Ebook")) +
  labs(
    title = "Total Checkouts per Year from 2017-2023",
    x = "Year",
    y = "Total Checkouts",
    color = "Total Checkouts vs Ebooks"
  ) +
  scale_color_manual(values = c("orange", "blue"), labels = c("Ebook", "Total Checkouts")) +
  scale_x_continuous(breaks = checkouts_by_year_df$CheckoutYear) +
  scale_y_continuous(label = comma, breaks = seq(0, 4000000, 500000))



