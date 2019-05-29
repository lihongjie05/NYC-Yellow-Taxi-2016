library(ggmap)
library(bigrquery)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)

#project id & google map api key, please replace with your id and key 
id = "xxxxx"
api_key = "xxxxx"

#register google map 
register_google(key = api_key)

#passenger count distribution
sql_pcount <-
"SELECT passenger_count, count(*) AS count
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
GROUP BY passenger_count"

p_count <- query_exec(sql_pcount, project = id)
p_count$percent <- percent(p_count$count/sum(p_count$count))
p_count[order(p_count$count),]

#rate_code distribution
sql_rcode <- 
"SELECT rate_code, count(*) AS count
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
GROUP BY rate_code"

r_count <- query_exec(sql_rcode, project = id)
r_count$percent <- percent(r_count$count/sum(r_count$count))
r_count[order(r_count$count),]

#payment type distribution 
sql_payment <- 
"SELECT payment_type, count(*) AS count
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
GROUP BY payment_type"

payment_count <- query_exec(sql_payment, project = id)
payment_count$percent <- percent(payment_count$count/sum(payment_count$count))
payment_count[order(payment_count$count),]

#Get monthly trip counts, avg amount, avg distance 
sql_m <- 
"SELECT month(pickup_datetime) AS m, count(*) AS row_num, avg(trip_distance) AS mean_distance, avg(total_amount) AS mean_amt
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE fare_amount> 0 and trip_distance < 5000 and trip_distance > 0
GROUP BY m"

monthly_count <- query_exec(sql_m, project = id)

ggplot(data=monthly_count, aes(x=factor(m), y=row_num)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(title = "NYC Yellow Taxi Monthly Total Trips Count 2016") +
  labs(x = "Month", y = "Trips Count")

ggplot(data=monthly_count, aes(x=factor(m), y=mean_distance)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(title = "NYC Yellow Taxi Monthly Average Trip Distance") +
  labs(x = "Month", y = "Avg Trip Distance (miles)")

ggplot(data=monthly_count, aes(x=factor(m), y=mean_amt)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(title = "NYC Yellow Taxi Monthly Average Trip Amount") +
  labs(x = "Month", y = "Avg Trip Amount ($)")

#Get trip counts by month and hour 
sql_m_h<- 
"SELECT month(pickup_datetime) AS m, hour(pickup_datetime) AS hour, count(*) AS row_num, avg(trip_distance) AS mean_distance, avg(total_amount) AS mean_amt
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE fare_amount> 0 and trip_distance < 5000 and trip_distance > 0
GROUP BY m, hour"

monthly_hourly_count <- query_exec(sql_m_h, project = id)

ggplot(monthly_hourly_count, aes(x=hour, y=row_num)) +
  labs(title = "NYC Yellow Taxi Monthly Total Trips Count By Hour") +
  labs(x = "Hour", y = "Trips Count") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  facet_wrap( ~ m, ncol = 4)

ggplot(monthly_hourly_count, aes(x=hour, y=mean_distance)) +
  labs(title = "NYC Yellow Taxi Monthly Average Trip Distance By Hour") +
  labs(x = "Hour", y = "Avg Trip Distance (miles)") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  facet_wrap( ~ m, ncol = 4)


ggplot(monthly_hourly_count, aes(x=hour, y=mean_amt)) +
  labs(title = "NYC Yellow Taxi Monthly Average Trip Amount By Hour") +
  labs(x = "Hour", y = "Avg Trip Amount ($)") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  facet_wrap( ~ m, ncol = 4)


#Get trip counts by dayofweek
sql_w <- 
"SELECT CASE 
  WHEN dayofweek(pickup_datetime) = 1 THEN 'Sun'
  WHEN dayofweek(pickup_datetime) = 2 THEN 'Mon'
  WHEN dayofweek(pickup_datetime) = 3 THEN 'Tue'
  WHEN dayofweek(pickup_datetime) = 4 THEN 'Wed'
  WHEN dayofweek(pickup_datetime) = 5 THEN 'Thu'
  WHEN dayofweek(pickup_datetime) = 6 THEN 'Fri'
  WHEN dayofweek(pickup_datetime) = 7 THEN 'Sat'
END AS dow,
count(*) AS row_num, avg(trip_distance) AS mean_distance, avg(total_amount) AS mean_amt
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE fare_amount> 0 and trip_distance < 5000 and trip_distance > 0
GROUP BY dow"

dow_count <- query_exec(sql_w, project = id)

ggplot(data=dow_count, aes(x=factor(dow_count$dow, levels = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')), y=row_num)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(title = "NYC Yellow Taxi Day Of Week Total Trips Count") +
  labs(x = "Day of Week", y = "Trips Count")

ggplot(data=dow_count, aes(x=factor(dow_count$dow, levels = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')), y=mean_distance)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(title = "NYC Yellow Taxi Day Of Week Average Trip Distance") +
  labs(x = "Day of Week", y = "Avg Trip Distance (miles)")

#Get trip counts by dayofweek and hour 
sql_w_h <- 
  "SELECT CASE 
  WHEN dayofweek(pickup_datetime) = 1 THEN 'Sun'
  WHEN dayofweek(pickup_datetime) = 2 THEN 'Mon'
  WHEN dayofweek(pickup_datetime) = 3 THEN 'Tue'
  WHEN dayofweek(pickup_datetime) = 4 THEN 'Wed'
  WHEN dayofweek(pickup_datetime) = 5 THEN 'Thu'
  WHEN dayofweek(pickup_datetime) = 6 THEN 'Fri'
  WHEN dayofweek(pickup_datetime) = 7 THEN 'Sat'
END AS dow,
hour(pickup_datetime) AS hour,
count(*) AS row_num, avg(trip_distance) AS mean_distance, avg(total_amount) AS mean_amt
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE fare_amount> 0 and trip_distance < 5000 and trip_distance > 0
GROUP BY dow, hour"

dow_hourly_count <- query_exec(sql_w_h, project = id)

ggplot(dow_hourly_count, aes(x=hour, y=row_num)) +
  labs(title = "NYC Yellow Taxi Day Of Week Total Trips Count By Hour") +
  labs(x = "Hour", y = "Trips Count") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  facet_wrap( ~ dow)

ggplot(dow_hourly_count, aes(x=hour, y=mean_distance)) +
  labs(title = "NYC Yellow Taxi Day Of Wee Average Trip Distance By Hour") +
  labs(x = "Hour", y = "Avg Trip Distance (miles)") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  facet_wrap( ~ dow)

#################################
#get raw data for analysis, pick 2 days only due to large volumen of data
#Mon and Sat are chosen to represent weekday and weekend

sql_raw <- 
"SELECT *
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE month(pickup_datetime) = 3
AND day(pickup_datetime)>=7
AND day(pickup_datetime) in (7,12)
AND fare_amount> 0 
AND trip_distance < 5000 
AND trip_distance > 0"

raw <- query_exec(sql_raw, project = id, max_pages = Inf)
summary(raw)

#invalid = raw[raw$dropoff_datetime<raw$pickup_datetime,]

pickup <- raw %>% filter(pickup_longitude<0 & pickup_latitude>0 & dropoff_datetime>pickup_datetime)
dropoff <- raw %>% filter(dropoff_longitude<0 & dropoff_latitude>0 & dropoff_datetime>pickup_datetime)

#Get New York City Map 
map <- get_map(location = "New York", zoom = 10, scale = 2)

#Plot pickup points
qmap(location = "New York") + 
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), 
             data=pickup, 
             col="red", alpha=0.005) + 
  labs(title = "Pick-Up Locations") 

#Plot dropoff points
qmap(location = "New York") + 
  geom_point(aes(x=dropoff_longitude, y=dropoff_latitude), 
             data=dropoff, 
             col="red", alpha=0.005) + 
  labs(title = "Drop-off Locations") 

#pick-ups at 5am, pickup and dropoff locations
pickup_5am <- raw %>% filter(pickup_longitude<0 & pickup_latitude>0 
                         & dropoff_datetime>pickup_datetime
                         & hour(pickup_datetime) == 5
                         )

qmap(location = "New York") + 
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), 
             data=pickup_5am, 
             col="red", alpha=0.005) + 
  labs(title = "Pick-Up Locations For 5am Pick-up Trips ") 

qmap(location = "New York") + 
  geom_point(aes(x=dropoff_longitude, y=dropoff_latitude), 
             data=pickup_5am, 
             col="red", alpha=0.005) + 
  labs(title = "Drop-off Locations For 5am Pick-up Trips ") 

#qmap(location = "New York") + stat_density2d(
#  aes(x = pickup_longitude, y = pickup_latitude, fill = ..level.., alpha = 0.25),
#  size = 0.01, bins = 30, data = pickup_5am,
#  geom = "polygon"
#) 

  
#check if airport has longer distance
sql_rate_distance<-
"SELECT rate_code, avg(total_amount) as mean_distance
FROM [bigquery-public-data:new_york.tlc_yellow_trips_2016] 
WHERE fare_amount> 0 
AND trip_distance < 5000
AND rate_code <> 99
AND trip_distance > 0
GROUP BY rate_code"

rate_distance <- query_exec(sql_rate_distance, project = id)
rate_distance[order(rate_distance$mean_distance),]

#pick-ups at 6-7pm, pickup and dropoff locations
pickup_67pm <- raw %>% filter(pickup_longitude<0 & pickup_latitude>0 
                             & dropoff_datetime>pickup_datetime
                             & hour(pickup_datetime) >= 18
                             & hour(pickup_datetime) <= 19
)

qmap(location = "New York") + 
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), 
             data=pickup_67pm, 
             col="red", alpha=0.005) + 
  labs(title = "Pick-Up Locations For 6-7pm Pick-up Trips ") 


qmap(location = "New York") + 
  geom_point(aes(x=dropoff_longitude, y=dropoff_latitude), 
             data=pickup_67pm, 
             col="red", alpha=0.005) + 
  labs(title = "Drop-off Locations For 6-7pm Pick-up Trips ") 

#################pick up locations for weekend and weekdays not much diff###
pickup1 <- raw %>% filter(pickup_longitude<0 & pickup_latitude>0 
                          & dropoff_datetime>pickup_datetime
                          & day(pickup_datetime) == 12)

pickup2 <- raw %>% filter(pickup_longitude<0 & pickup_latitude>0 
                          & dropoff_datetime>pickup_datetime
                          & day(pickup_datetime) == 7)

qmap(location = "New York") + 
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), 
             data=pickup1, 
             col="red", alpha=0.005)

qmap(location = "New York") + 
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), 
             data=pickup2, 
             col="red", alpha=0.005)


########Tip Analysis#########3
tips <- raw %>% filter(tip_amount>0 
                       & pickup_longitude<0 & pickup_latitude>0 
                       & dropoff_longitude<0 & dropoff_latitude>0 
                       & dropoff_datetime>pickup_datetime)
tips$tip_rate <- tips$tip_amount/tips$fare_amount

#remove extreme outliers
summary(tips$tip_rate)
tips <- tips[tips$tip_rate<1,]

#histogram
hist <- ggplot(tips, aes(x=tip_rate)) + geom_histogram(fill = "steelblue", color = "black") + labs(title = "Histogram of Tip Rate ") 
hist + geom_vline(aes(xintercept=mean(tip_rate)),color="orange", linetype="dashed", size=1)

ggplot(tips, aes(x=trip_distance, y=tip_rate, color = passenger_count)) + geom_point()

#tip by pickup location
heatmap <- ggmap(map, extent = "device") + labs(title = "Max Tip Rate Pick-up Location") + stat_summary_2d(data = tips, 
                    aes(x = pickup_longitude, y = pickup_latitude, z =tips$tip_rate),
                    fun = max, alpha = 0.6, bins = 50) + scale_fill_gradient(name = "%Tip Amount", low = "green", high = "red") 

#tip by dropoff location
heatmap1 <- ggmap(map, extent = "device") + labs(title = "Max Tip Rate Drop-off Location") + stat_summary_2d(data = tips, 
                 aes(x = dropoff_longitude, y = dropoff_latitude, z =tip_rate),
                 fun = max, alpha = 0.6, bins = 50) + scale_fill_gradient(name = "Tip Rate", low = "green", high = "red")

heatmap
heatmap1





