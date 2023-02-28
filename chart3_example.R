library(dplyr)
library(ggplot2)
library(scales)

spl_df <- read.csv("C:/Users/ebaga/OneDrive/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

checkouts_by_material_df <- spl_df %>%
  group_by(MaterialType) %>%
  summarize(checkouts = sum(Checkouts)) %>%
  arrange(desc(checkouts))

# Filter top 6 materials
top_materials <- checkouts_by_material_df %>%
  filter(row_number() <= 6)

ggplot(top_materials) + 
  geom_bar(aes(x = reorder(MaterialType, checkouts), y = checkouts, fill = MaterialType), stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 6 Total Checkouts by Material Type",
    x = "Material Type",
    y = "Total Checkouts"
  ) +
  scale_y_continuous(label = comma, breaks = seq(0, max(top_materials$checkouts), 1000000))







