install.packages('skimr')
library(data.table)
library(skimr)
library(readr)

sb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-02/youtube.csv')
sb <- as.data.table(sb)
skim(sb)

superbowl_scraped <- read_csv("superbowl_scraped.csv")
superbowl_scraped <- as.data.table(superbowl_scraped)
View(superbowl_scraped)


dt <- merge(sb, superbowl_scraped, by = "year")
skim(dt)


sb[ , min(year)]
sb[ , .N, by = .(brand)][order(-N)]
dt <- dt[year >= 2000, ]


library(tidyr)
library(ggplot2)

# Convert logical variables to long format
logical_vars <- sb %>%
  pivot_longer(cols = where(is.logical), names_to = "Variable", values_to = "Value")

# Plot logical variables
ggplot(logical_vars, aes(x = Variable, fill = Value)) +
  geom_bar(position = "dodge") +
  ggtitle("Count of Logical Variables") +
  xlab("Logical Variables") +
  ylab("Count") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"))


#what attribute in sb ads make them more engaging? 
sb[, engagement_score := like_count + comment_count + favorite_count]
engaging_attributes <- sb[, .(Avg_Engagement = mean(engagement_score, na.rm = TRUE)), by = .(funny, patriotic, show_product_quickly, celebrity)]
engaging_attributes
#what brand consitently got higher likes & engagment? 

#what attributes resulted in higher negative likes, is it influenced more by brand or by the featuresof the ad? 

#is there a trend overtime in sb ads? features 

#what about trend swithonline engagment on yoututbe, do we see a decline in total likes that might signal the migration to toher sm like insta tiktok? 

