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
ggplot (dta[!is.na(stars)], aes(factor(stars), V1)) + geom_point() +xlab ("Number of stars")
#TODO data vison avg price per nights per stars by country  







