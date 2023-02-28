# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Download and unzip one or more of the SPL datasets and load here from a file path
spl_df <- read.csv("C:/Users/ebaga/OneDrive/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Make a new column with checkout month, checkout year, and a default day value ("01") *pasted* together
# Then convert that column to a date value
spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# 5 values calculated with DPLYR
# Value 1: Checkouts per Material Type
checkouts_by_material_df <- spl_df %>%
  group_by(MaterialType) %>%
  summarize(checkouts = sum(Checkouts))

most_checkout_material <- checkouts_by_material_df %>%
  filter(checkouts == max(checkouts)) %>%
  pull(MaterialType)

# Value 2: Total Checkouts per Year
checkouts_by_year_df <- spl_df %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

lowest_year <- checkouts_by_year_df %>%
  filter(CheckoutYear != 2023) %>%
  filter(checkouts == min(checkouts)) %>%
  pull(CheckoutYear)

# Value 3: What's the year with the most checkouts for all Harry Potter books
hp_checkouts_df <- spl_df %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(Title, "Harry Potter"))

hp_checkouts_by_year_df <- hp_checkouts_df %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

most_hp_year <- hp_checkouts_by_year_df %>%
  filter(checkouts == max(checkouts)) %>%
  pull(CheckoutYear)

# Value 4: What is the year with the most/least checkouts for ebooks?
ebook_checkouts_df <- spl_df %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

least_ebook_checkout_year <- ebook_checkouts_df %>%
  filter(CheckoutYear != 2023) %>%
  filter(checkouts == min(checkouts)) %>%
  pull(CheckoutYear)

most_ebook_checkout_year <- ebook_checkouts_df %>%
  filter(checkouts == max(checkouts)) %>%
  pull(CheckoutYear)

# Value 5: Nonfiction Genre Popularity Over Time
nonfiction_checkouts_df <- spl_df %>%
  filter(str_detect(Subjects, "Nonfiction")) %>%
  group_by(CheckoutYear) %>%
  summarize(checkouts = sum(Checkouts))

most_nonfiction_checkout_year <- nonfiction_checkouts_df %>%
  filter(checkouts == max(checkouts)) %>%
  pull(CheckoutYear)
