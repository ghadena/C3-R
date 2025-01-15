ls()
rm(list = ls())

source("http://bit.ly/CEU-R-heights-2018")
readLines("http://bit.ly/CEU-R-heights-2018")

## TODO compute the mean of heights
mean(heights, na.rm = TRUE)

## TODO do some dataviz
hist(heights)
library(ggplot2)
ggplot(data.frame(heights), aes(heights)) + geom_histogram()

rm(list = ls())
ls(all.names = TRUE)
.secret
