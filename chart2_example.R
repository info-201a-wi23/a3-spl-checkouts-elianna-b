library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_df <- read.csv("C:/Users/ebaga/OneDrive/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

hp_checkouts_df <- spl_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Harry Potter"))

hp_checkouts_by_year_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Sorcerer's Stone
sorcerers_stone_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Sorcerer")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Chamber of Secrerts
chamber_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Chamber")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Prisoner of Azkaban
azkaban_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Azkaban")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Goblet of Fire
goblet_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Goblet")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Order of the Phoenix
phoenix_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Phoenix")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Half-Blood Prince
prince_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Prince")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

# Deathly Hollows
deathly_checkouts_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Deathly")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

ggplot() +
  # Add each book's checkouts as a separate layer with its own color
  geom_line(data = sorcerers_stone_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Sorcerer's Stone")) +
  geom_point(data = sorcerers_stone_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "red") +
  geom_line(data = chamber_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Chamber of Secrets")) +
  geom_point(data = chamber_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "orange") +
  geom_line(data = azkaban_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Prisoner of Azkaban")) +
  geom_point(data = azkaban_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "yellow") +
  geom_line(data = goblet_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Goblet of Fire")) +
  geom_point(data = goblet_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "green") +
  geom_line(data = phoenix_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Order of the Phoenix")) +
  geom_point(data = phoenix_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "blue") +
  geom_line(data = prince_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Half-Blood Prince")) +
  geom_point(data = prince_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "purple") +
  geom_line(data = deathly_checkouts_df, aes(x = CheckoutYear, y = checkouts, color = "Deathly Hallows")) +
  geom_point(data = deathly_checkouts_df, aes(x = CheckoutYear, y = checkouts), color = "black") +
  # Set chart title and axis labels
  labs(
    title = "Harry Potter Book Checkouts by Year",
    x = "Year",
    y = "Checkouts"
  ) +
  # Add legend with book titles
  scale_color_manual(
    values = c(
      "Sorcerer's Stone" = "red",
      "Chamber of Secrets" = "orange",
      "Prisoner of Azkaban" = "yellow",
      "Goblet of Fire" = "green",
      "Order of the Phoenix" = "blue",
      "Half-Blood Prince" = "purple",
      "Deathly Hallows" = "black"
    ),
    name = "Book Titles"
  ) +
  # Break x-axis by each year
  scale_x_continuous(breaks = seq(min(spl_df$CheckoutYear), max(spl_df$CheckoutYear), 1)) +
  # Set y-axis in increments of 1000
  scale_y_continuous(breaks = seq(0, ceiling(max(hp_checkouts_by_year_df$checkouts / 1000)) * 1000, 1000))
