library(data.table)
library(skimr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

sb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-02/youtube.csv')
sb <- as.data.table(sb)
skim(sb)

superbowl_scraped <- read_csv("https://raw.githubusercontent.com/ghadena/C3-R/refs/heads/main/superbowl_scraped.csv")
superbowl_scraped <- as.data.table(superbowl_scraped)
superbowl_scraped <- superbowl_scraped[year >= 2000, ] #filtering for years after 2000 
skim(superbowl_scraped)

dt <- merge(sb, superbowl_scraped, by = "year")
skim(dt)

sb[ , min(year)]
sb[ , .N, by = .(brand)][order(-N)] #counting commercials by brand 


# Convert logical variables to long format
logical_vars <- sb %>%
  pivot_longer(cols = where(is.logical), names_to = "Variable", values_to = "Value")

# chart 1: Plot logical variables
ggplot(logical_vars, aes(x = Variable, fill = Value)) +
  geom_bar(position = "dodge") +
  ggtitle("Count of Logical Variables") +
  xlab("Logical Variables") +
  ylab("Count") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"))


## #############################################################################
## what attribute in superbowl ads make them more engaging? 
## #############################################################################
# Define the list of categorical columns
categories <- c("funny", "patriotic", "celebrity", "danger", "animals", "use_sex", "show_product_quickly")

# Initialize an empty data.table to store results
cat_agg <- data.table(category = character(), value = logical(), avg_likes = numeric(), avg_comments = numeric())

# Loop through each category and compute avg_likes and avg_comments
for (category in categories) {
  temp_dt <- dt[, .(avg_likes = mean(like_count, na.rm = TRUE), 
                    avg_comments = mean(comment_count, na.rm = TRUE)), 
                by = category]
  setnames(temp_dt, category, "value") # Rename the grouping column to a generic name "value"
  temp_dt[, category := category]  # Add a new column to indicate the category name
  cat_agg <- rbind(cat_agg, temp_dt, fill = TRUE)   # Bind the results to the main results_dt
}
setcolorder(cat_agg, c("category", "value", "avg_likes", "avg_comments")) # Reorder columns for clarity
print(cat_agg)

#chart 2 avg likes by cat 
ggplot(cat_agg, aes(x = category, y = avg_likes, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Likes by Category", x = "Category", y = "Average Likes", fill = "Value (TRUE/FALSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#chart 3 avg commnets by cat
ggplot(cat_agg, aes(x = category, y = avg_comments, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Comments by Category", x = "Category", y = "Average Comments", fill = "Value (TRUE/FALSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################## why does a funny ad get less likes and comments? 
dt[, .N, by = funny] # we have more funny ads 
dt[, .(avg_likes = mean(like_count, na.rm = TRUE), 
       sd_likes = sd(like_count, na.rm = TRUE)), 
   by = funny] # the sd of funny is lower than not funny 

dt[, .(avg_likes = mean(like_count, na.rm = TRUE)), by = .(year, funny)][order(year, -avg_likes)]
ggplot(dt[, .(avg_likes = mean(like_count, na.rm = TRUE)), by = .(year, funny)], 
       aes(x = year, y = avg_likes, color = funny, group = funny)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Trend of Average Likes Over Time (Funny vs. Not Funny Ads)", 
       x = "Year", y = "Average Likes", color = "Funny") +
  theme_minimal()

dt[, .(avg_likes = mean(like_count, na.rm = TRUE)), by = .(funny, celebrity, animals)][order(-avg_likes)] # funny interaction with other factors like celeb
dt[, .(avg_views = mean(view_count, na.rm = TRUE), avg_likes = mean(like_count, na.rm = TRUE)), by = funny] # funny vds ge more views but less interactions 
dt[, likes_per_view := like_count / view_count]
ggplot(dt[, .(avg_likes_per_view = mean(likes_per_view, na.rm = TRUE)), by = funny], 
       aes(x = as.factor(funny), y = avg_likes_per_view, fill = as.factor(funny))) +
  geom_bar(stat = "identity") +
  labs(title = "Likes per View: Funny vs. Not Funny Ads", 
       x = "Funny", y = "Average Likes per View") +
  theme_minimal() # not funny vids get more engagement  

# lets check the other enagament metircs for funny vids - like dislines , funny vids get less dislikes 
dt[, .(avg_dislikes = mean(dislike_count, na.rm = TRUE)), by = funny]

#lets check the top 20 vids by engagement: i wacthed some and found them funny eventhough they weent listed as funny in the dataset 
# eg the superbowl baby choir or the budwoser donkey 
# humor is subjective so how did the data set define it? 
##   Is the ad jokey, goofy, weird or silly? Funny commercials (or ones that are trying to be funny) are a clear “yes” here
# also i discovered that this data didnt have the complete records of all suberbowl comercials, only of the top 10 companies that always adveritse since the last centrury 
# data limited to: "E-Trade"   "Budweiser" "Bud Light" "Pepsi"     "Doritos"   "NFL"       "Toyota"    "Coca-Cola" "Hynudai"  "Kia". 
dt[order(-like_count)][1:20, .(year, funny, brand, title, like_count, view_count)]

############ box plot 3x choose 1 - log scale 
ggplot(dt, aes(x = as.factor(funny), y = like_count, fill = as.factor(funny))) +
  geom_boxplot() +
  labs(title = "Likes Distribution: Funny vs. Non-Funny Ads", 
       x = "Funny", y = "Like Count", fill = "Funny") +
  theme_minimal()

# chart 4 - box lot - funny vid dist Exclude top 5% extreme values
dt_filtered <- dt[like_count <= quantile(like_count, 0.95, na.rm = TRUE)]

ggplot(dt_filtered, aes(x = as.factor(funny), y = like_count, fill = as.factor(funny))) +
  geom_boxplot() +
  labs(title = "Likes Distribution (Excluding Top 5% Outliers): Funny vs. Non-Funny Ads", 
       x = "Funny", y = "Like Count", fill = "Funny") +
  theme_minimal()

# on log scale 
ggplot(dt, aes(x = as.factor(funny), y = like_count, fill = as.factor(funny))) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Likes Distribution (Log Scale): Funny vs. Non-Funny Ads", 
       x = "Funny", y = "Log10(Likes)", fill = "Funny") +
  theme_minimal()



## #############################################################################
## #what brand consistently got higher likes & engagment? and what can be the resason? 
## #############################################################################

# Calculate average likes, comments, dislikes, and ad counts for each brand
brand_engagement <- dt[, .(
  avg_likes = mean(like_count, na.rm = TRUE),
  avg_comments = mean(comment_count, na.rm = TRUE),
  avg_dislikes = mean(dislike_count, na.rm = TRUE),
  total_ads = .N
), by = brand][order(-avg_likes)]

print(brand_engagement)

# Calculate attribute contribution and average engagement (likes, comments, dislikes)
attribute_contribution <- dt[, .(
  funny_pct = mean(funny, na.rm = TRUE) * 100,
  celebrity_pct = mean(celebrity, na.rm = TRUE) * 100,
  animals_pct = mean(animals, na.rm = TRUE) * 100,
  product_pct = mean(show_product_quickly, na.rm = TRUE) * 100,s
  patriotic_pct = mean(patriotic, na.rm = TRUE) * 100,
  danger_pct = mean(danger, na.rm = TRUE) * 100,
  sex_pct = mean(use_sex, na.rm = TRUE) * 100,
  avg_likes = mean(like_count, na.rm = TRUE),
  avg_comments = mean(comment_count, na.rm = TRUE),
  avg_dislikes = mean(dislike_count, na.rm = TRUE), 
  avg_views = mean(view_count, na.rm = TRUE)  # Removed * 100
), by = brand][order(-avg_likes)]

print(attribute_contribution)

# visualizing results 

# Normalize attribute percentages to sum to 100% per brand
attribute_contribution[, total_pct := funny_pct + celebrity_pct + animals_pct + product_pct + patriotic_pct + danger_pct + sex_pct]
attribute_contribution[, ':=' (
  funny_pct = funny_pct / total_pct * 100,
  celebrity_pct = celebrity_pct / total_pct * 100,
  animals_pct = animals_pct / total_pct * 100,
  product_pct = product_pct / total_pct * 100,
  patriotic_pct = patriotic_pct / total_pct * 100,
  danger_pct = danger_pct / total_pct * 100,
  sex_pct = sex_pct / total_pct * 100
)]

# Convert data to long format for the stacked bar
attribute_long <- melt(attribute_contribution, id.vars = "brand",
                       measure.vars = c("funny_pct", "celebrity_pct", "animals_pct", "product_pct", "patriotic_pct", "danger_pct", "sex_pct"),
                       variable.name = "attribute", value.name = "percentage")

# Calculate % of total likes for each brand
total_likes <- sum(attribute_contribution$avg_likes, na.rm = TRUE)
attribute_contribution[, likes_pct := (avg_likes / total_likes) * 100]

#chart 5: avg likes per brand 
ggplot(attribute_contribution, aes(x = reorder(brand, -avg_likes), y = avg_likes, fill = brand)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Likes by Brand", x = "Brand", y = "Average Likes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# chart 6: Create a 100% stacked bar chart
ggplot(attribute_long, aes(x = reorder(brand, -percentage), y = percentage, fill = attribute)) +
  geom_bar(stat = "identity", position = "fill") +  # "fill" ensures each bar sums to 100%
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentages
  labs(title = "Normalized Attribute Usage by Brand (100% Stacked Bar Chart)", 
       x = "Brand", y = "Percentage of Ads", fill = "Attribute") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# chart 7: stacked and % of total likes 
ggplot() +
  # Stacked bar chart (attributes)
  geom_bar(data = attribute_long, 
           aes(x = brand, y = percentage / 100, fill = attribute), 
           stat = "identity", position = "fill") +
  
  # Line and points for % of total likes (on the same y-axis)
  geom_line(data = attribute_contribution, 
            aes(x = brand, y = likes_pct / 100, group = 1, color = "Likes % of Total"), 
            size = 1.5) + 
  geom_point(data = attribute_contribution, 
             aes(x = brand, y = likes_pct / 100, color = "Likes % of Total"), 
             size = 2) +  
  scale_color_manual(values = c("Likes % of Total" = "black"), name = "Metric") +  # Add to legend
  
  # Labels for % of total likes
  geom_text(data = attribute_contribution, 
            aes(x = brand, y = (likes_pct / 100) + 0.04, label = paste0(round(likes_pct, 1), "%")), 
            vjust = -0, color = "black", size = 2.5) +
  
  # Formatting
  scale_y_continuous(labels = percent_format(), 
                     breaks = seq(0, 1, by = 0.25),  # Set ticks at 0, 25, 50, 75, 100
                     expand = expansion(mult = c(0, 0.1))) +  # Add space for labels
  
  labs(title = "Super Bowl Ad Strategies: Attribute Breakdown and Engagement Comparison by Brand", 
       subtitle = "Analyzing the attribute composition of Super Bowl ads by top brands \nand how their content translates into audience engagement (measured by percentage of total likes).",
       x = "Brand", y = "Percentage", fill = "Attribute") +
  
  # Theme adjustments
  theme_minimal(base_size = 12) +  # Keep minimal theme
  theme(panel.grid.major = element_blank(),   # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, margin = margin( b = 10)),  # Add top margin
        plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(b = 5)),  # Add bottom margin
        plot.margin = margin(t = 20, r = 0, b = 10, l = 10)
        )  # Better x-axis rotation

## #############################################################################
## How does TV viewership relate to engagement metric (likes)?
## #############################################################################
#Some years might have had higher TV viewership, meaning more people saw the ads—does this mean those ads got more engagement on YouTube?
#Ad revenue is a measure of how much was spent on advertising—but does more revenue mean the ads performed better online?
#This could reveal whether traditional TV exposure still influences online engagement, or if some brands are just inherently better at making viral ads.

setnames(dt, "tv_views(millions)", "tv_views_million")

#chart 7 tv vs yt likes - dos tv engagment ranslate to online enegment too? 
ggplot(dt, aes(x = tv_views_million, y = like_count)) +
  geom_point(alpha = 0.6) +
  geom_point(data = dt[which.max(like_count)], aes(x = tv_views_million , y = like_count), 
             color = "red", size = 3) +
  scale_x_continuous(labels = scales::comma) +  # No scientific notation
    scale_y_continuous(labels = scales::comma) +  # No scientific notation
  labs(title = "YouTube Views vs. Likes",
       x = "TV Views (millions)", y = "Likes",
       subtitle = "Red point = Outlier (Doritos 2012 ad) with 275k Youtube Likes") +
  theme_minimal()


dt[which.max(view_count)]
dt[which.max(like_count)]

#i realised that the data for tv views in per yezr and the likes is per ad so this chart makes no sense, lets aggregate!

#############
# Aggregate data: Average likes per year
yearly_engagement <- dt[, .(avg_likes = mean(like_count, na.rm = TRUE)), by = .(year, tv_views_million)]

# Load necessary library for ColorBrewer palettes
display.brewer.all()  # Shows all available palettes

# Define a color palette for years
year_colors <- brewer.pal(n = 12, name = "RdPu")  # Using a ColorBrewer palette

# Scatter plot of TV views vs. avg likes per year with gradient color
ggplot(yearly_engagement, aes(x = tv_views_million, y = avg_likes, color = year)) +
  geom_point(size = 4) +  # Bigger points for visibility
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  scale_x_continuous(labels = scales::comma) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_color_gradientn(colors = year_colors) +  # Apply ColorBrewer gradient
  labs(
    title = "TV Views vs. Average YouTube Likes per Year",
    subtitle = "Analyzing whether years with more Super Bowl viewers generate more online engagement",
    x = "TV Views (millions)",
    y = "Average YouTube Likes",
    color = "Year",  # Legend label
    caption = "Each point represents a year. Trend line shows overall correlation."
  ) +
  theme_minimal()

# Identify the outliers
max_outlier <- yearly_engagement[which.max(avg_likes)]  # 2012 (doritos ad - 275k likes)
second_outlier <- yearly_engagement[order(-avg_likes)][2]  # 2020 
second_outlier

dt[order(-like_count)][2]  # (nfl LIV comercial - 175k likes)

# Scatter plot with annotations
ggplot(yearly_engagement, aes(x = tv_views_million, y = avg_likes, color = year)) +
  geom_point(size = 4) +  # Bigger points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  scale_x_continuous(labels = scales::comma) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "RdPu")) +  # Apply color scheme
  labs(
    title = "TV Views vs. Average YouTube Likes per Year",
    subtitle = "Analyzing whether years with more Super Bowl viewers generate more online engagement",
    x = "TV Views (millions)",
    y = "Average YouTube Likes",
    color = "Year",
    caption = "Each point represents a year. Trend line shows overall correlation."
  ) +
  theme_minimal() +
  
  # Annotation for Max Outlier (Doritos 2012 Ad)
  annotate("text", x = max_outlier$tv_views_million, y = max_outlier$avg_likes + 5000,
           label = "Doritos 2012 Ad\n~275k Likes", color = "black", size = 4, hjust = 0.5) +
  geom_segment(aes(x = max_outlier$tv_views_million, y = max_outlier$avg_likes - 20000,
                   xend = max_outlier$tv_views_million, yend = max_outlier$avg_likes - 5000),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  
  # Annotation for Second Outlier
  annotate("text", x = second_outlier$tv_views_million, y = second_outlier$avg_likes + 5000,
           label = paste0("NFL 2020 Ad\n~178k Likes"), 
           color = "black", size = 4, hjust = 0.5) +
  geom_segment(aes(x = second_outlier$tv_views_million, y = second_outlier$avg_likes - 20000,
                   xend = second_outlier$tv_views_million, yend = second_outlier$avg_likes - 5000),
               arrow = arrow(length = unit(0.2, "cm")), color = "black")

## #############################################################################
## notes
## #############################################################################
#weakness in data - engement is tn the only form of interaction or metir of success for an ad - we can hae enegemnt on othrt social sites, offline enganement, incresed sales.... it is hard to measure 
# but based on this data we cna see that ..... 

