library(data.table)
?fread #loads data instead of running r code 
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')


## TODO count the number of bookings below 100 EUR
bookings[price < 100,.(count=.N) ]
## TODO count the number of bookings below 100 EUR without an offer
bookings[price < 100 & offer == 0,.(count=.N) ]
## TODO compute the average price of the bookings below 100 EUR
bookings[price < 100,.(avg=mean(price))]
## TODO compute the average price of bookings on weekends
bookings[weekend == 1 ,.(avg=mean(price))]
## TODO compute the average price of bookings on weekdays
bookings[weekend == 0,.(avg=mean(price))]
## TODO include nnights, holiday and year as well in the aggregate variables
bookings[,.(avg=mean(price)), by = .(weekend, nnights, holiday, year)]

## TODO avg price per number of stars 
#must merge first 
?merge
merge(bookings, features)[,.(avg=mean(price)), by = stars][order (-stars)]
#the inner join resulted in 3 missing observations, why? 
bookings[is.na(hotel_id)]
features[is.na(hotel_id)]
features[duplicated(hotel_id)]
bookings[!hotel_id %in% features$hotel_id] #finds rows where there is no hotel id in the features data set
features[hotel_id ==2]

merge(bookings, features, all = TRUE) #includes even the 3 missing observations from before 
#counting number of bookings in hotels for each star 
merge(bookings, features)[,.(avg_price=mean(price), .N), by = stars][order (-stars)]
#investigating why 2.5 stars is less expensive than 1 star hotels
library(ggplot2)
ggplot(merge(bookings, features)[stars == 2.5], aes(price))+geom_histogram()
merge(bookings, features)[stars == 2.5][, mean(price), by =nnights]
#we need to loook at avg price per night 
merge(bookings, features)[,.(avg_price=mean(price/nnights), .N), by = stars][order (-stars)]

#create new merged dt and create new column 
dt <- merge(bookings, features)
dt$price_per_night <- dt$price / dt$nnights
dt[, price_per_night := price / nnights] # this is a better way 

dt[, mean(price_per_night), by = stars][order (stars)]
dt[, weighted.mean(price_per_night, nnights), by = stars][order (stars)]        

#TODO hotels dataset: features + avg price of a night 
hotels <- merge(features, bookings[, .(price_per_night = mean(price/nnights), bookings = .N), by = hotel_id])
hotels[, weighted.mean(price_per_night, bookings), by = stars][order (stars)]

#TODO data vison avg price per nights per stars
dta <- hotels[, weighted.mean(price_per_night, bookings), by = stars][order (stars)]
ggplot (dta, aes(stars, V1)) + geom_point()
ggplot (dta[!is.na(stars)], aes(factor(stars), V1)) + geom_point() +xlab ("Number of stars") +ylab("AVG price per night")
#TODO data vison avg price per nights per stars by country  
dta <- hotels[, weighted.mean(price_per_night, bookings), by = .(stars, country)][order (stars)]
ggplot (dta[!is.na(stars)], aes(factor(stars), V1)) + geom_point() +xlab ("Number of stars") +ylab("AVG price per night") + facet_wrap(~country, scales = 'free')

#TODO aggregate the dataset by country: av price, rating, stars 
dt_country <- hotels[, .(
  price = mean(price_per_night, na.rm = TRUE),
  ratings = mean (rating, na.rm = TRUE), 
  stars = mean(stars, na.rm = TRUE)
), by = country][order (stars)]


#TODO list countries with above average rating 
#avg_rating <- mean(dt_country$ratings, na.rm = TRUE)
dt_country[ratings > mean(dt_country$ratings, na.rm = TRUE)]

#creating categories using cut 
hotels[, pricecat := cut(price_per_night, c(0, 100, 250, Inf), labels = c("cheap", "avg", "expensive"))]
hotels [, .N, by = pricecat]

lower <- quantile(hotels$price_per_night, 1/3)
upper <- quantile(hotels$price_per_night, 2/3)
hotels[, pricecat := cut(price_per_night, c(0, lower, upper, Inf), labels = c("cheap", "avg", "expensive"))]
hotels[, pricecat := cut(price_per_night, c(0, quantile(price_per_night, c(1/3, 2/3)), Inf), labels = c("cheap", "avg", "expensive"))]
hotels[, .N, by = pricecat]

#now create these quartiles for each country 
hotels[, lower := quantile(price_per_night, 1/3), by = country]
hotels[, upper := quantile(price_per_night, 2/3), by = country]
#this wouldnt work 
hotels[, pricecat := cut(price_per_night, c(0, lower[1], upper[1], Inf), labels = c("cheap", "avg", "expensive")), by = country]
#lets debug 
hotels[, .(0, lower[1], upper[1], Inf), by = country] #this part is good

cut(hotels[country == "Netherlands", price_per_night], c(0, 136.78423, 190.70238, Inf ))
#issue is that some countries have less thana 3 observations so we cannot ctegorise that #filter so that upper != lower
hotels[, pricecat := NULL]
hotels[upper != lower, pricecat := cut(price_per_night, c(0, lower[1], upper[1], Inf), labels = c("cheap", "avg", "expensive")), by = country]


##japan flag example or smth 
#TODO data.tale with x (1:100), y (1:100), color columns(red/white)
points<- data.table(x = rep(1 :100, 100), y = rep(1:100, 100), col = 'white')
points[x == 50 & y == 50, col:= 'red']
plot(points$x,points$y,col = points$col, pch = 19 )
library (ggplot2)
ggplot(points, aes(x, y, color = col)) +geom_point() + theme_bw() + scale_color_manual(values = c("red", "white")) + theme_void()+ theme(legend.position = 'none')

install.packages('rpart')
library(rpart)
####ughhhh. i zoned out oops idk whas going on here at all #go look at professors notes 