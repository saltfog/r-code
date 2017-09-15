
#load need libraries
library(readr)
library(dplyr)
library(lubridate)
library(geosphere)
library(leaflet)
library(ggplot2)
#import the data set
df <- read.csv("~/r-code/jumpmen/CompanyData.csv",stringsAsFactors = FALSE)
head(df)

#lets take a look at the data frame
#lets check the integrity of the data missing values, etc...
#is.na(df) # returns TRUE of data is missing but also print a lot of extra pages

#looks like we have some item_quantity values missing
#the time stamps where imported as strings we need to convert them to a date format
df$when_the_delivery_started <- ymd_hms(substr(df$when_the_delivery_started,1,19))
df$when_the_Jumpman_arrived_at_pickup <- ymd_hms(substr(df$when_the_Jumpman_arrived_at_pickup,1,19))
df$when_the_Jumpman_left_pickup <- ymd_hms(substr(df$when_the_Jumpman_left_pickup,1,19))
df$when_the_Jumpman_arrived_at_dropoff <- ymd_hms(substr(df$when_the_Jumpman_arrived_at_dropoff,1,19))

#it good to code days like monday, tuesday, etc.. into integers so we can use them numerical data. We will create new columns and
#add the below values.
df$wday_delivery_started <- wday(df$when_the_delivery_started)
df$weekend_delivery_started <- ifelse(df$wday_delivery_started %in% c(1,7),1,0)
df$day_delivery_started <- (day(df$when_the_delivery_started))

#creat time differenc columns for the jumpman timestamps
df$delivery_time <- difftime(df$when_the_Jumpman_arrived_at_dropoff,
                             df$when_the_Jumpman_left_pickup,
                             units="hours")
df$loading_time <- difftime(df$when_the_Jumpman_left_pickup,
                            df$when_the_Jumpman_arrived_at_pickup,
                            units="hours")
df$jumpman_arrival_time <- difftime(df$when_the_Jumpman_arrived_at_pickup,
                            df$when_the_delivery_started,
                            units="hours")

#delivery distance based off the lat and long to meters
df$delivery_distance <- 0
for(i in 1:nrow(df))
{ 
  df[i,'delivery_distance'] <- distm(c(df[i,"dropoff_lat"],df[i,"dropoff_lon"]),
                                     c(df[i,"pickup_lat"],df[i,"pickup_lon"]),
                                     fun=distHaversine)/1609.34
}

#compute the average jumpman speed and put in new column
df$jumpman_avg_speed <- df$delivery_distance/as.numeric(df$delivery_time)

#calculate average jumpman speed to delivery
df$jumpman_avg_speed <- df$delivery_distance/as.numeric(df$delivery_time)

#lets retreive the distinct values and discard the rest
df_unique <- df %>% distinct(delivery_id, .keep_all = TRUE)

#weekend vs weekday by dropoffs
leaflet() %>% setView(-73.972887,40.732828,zoom=12) %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==0),
                   lat=~dropoff_lat,lng=~dropoff_lon,weight=1,radius=2,opacity=1,color="Green") %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==1),
                   lat=~dropoff_lat,lng=~dropoff_lon,weight=1,radius=2,opacity=1,color="Red") %>%
  addLegend("bottomleft",colors =c("Red", "Green"),labels= c("Weekend","Weekday"),opacity = 1)

#weekend vs weekday by pickup
leaflet() %>% setView(-73.972887,40.732828,zoom=12) %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==0),
                   lat=~pickup_lat,lng=~pickup_lon,weight=1,radius=3,opacity=1,color="Orange") %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==1),
                   lat=~pickup_lat,lng=~pickup_lon,weight=1,radius=2,opacity=1,color="Blue")%>%
  addLegend("bottomright",colors =c("Blue", "Orange"),labels= c("Weekend","Weekday"),opacity = 1) 

#unique number of customers
paste(length(unique(df_unique$customer_id))," Unique Customers")

ggplot(data.frame(as.vector(table(df_unique$customer_id)))) +
  geom_histogram(bins=30,aes(x=as.vector.table.df_unique.customer_id..))+
  ggtitle("Customer Order Frequency - Histogram")+
  xlab("Orders/Customer")+
  ylab("Number of Customers") + theme_light()

#vehicle usage
ggplot(df_unique,aes(x=vehicle_type, 1,group=1)) +
  stat_summary(fun.y = sum,geom = "bar")+
  ggtitle("Number of Deliveries by Vehicle Type")+
  xlab("Vehicle Type")+ylab("Number of Deliveries") + theme_light()

#vechile usage by days of the week
ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T), 1,group=vehicle_type,color=vehicle_type)) +
  stat_summary(fun.y = sum,geom = "line",size=.5)+
  ggtitle("Vehicle usage by Days of the Week")+
  xlab("Days of the Week")+ylab("Number of Deliveries") + theme_light() 

#range of dates deliver start
paste("Dates range from ",
      min(df_unique$when_the_delivery_started),
      " to ",
      max(df_unique$when_the_delivery_started)
      )

#customers acquired
cust_acq <- df_unique %>%
  group_by(customer_id) %>%
  summarise(first_day=min(day(when_the_delivery_started)))

ggplot(cust_acq,aes(x=first_day,y=1)) +
  stat_summary(fun.y=sum,geom="line", colour= "blue") +
  ggtitle("Number of New Customers Acquired per Day")+
  ylab("Number of Customers")+
  xlab("Days in October 2014") + theme_light() 

#delivery trends
ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T), 1,group=1)) +
  stat_summary(fun.y = sum,geom = "line", colour= "blue")+
  ggtitle("Deliveries by Days of the Week")+
  ylab("Number of Deliveries")+xlab("Days of the Week") + theme_light() 

#delivery time by day of the week
ggplot(df_unique,aes(x=vehicle_type,y=delivery_time))+
  geom_tile()+
  ggtitle("Delivery Time variation across Vehicle Types")+
  xlab("Vehicle Types")+ylab("Delivery Time (Hours)") + theme_light() 

#jumpman arrival by day of the week
ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T),y=delivery_time))+
  geom_tile()+
  ggtitle("Loading Time variation across Days of the Week")+
  xlab("Days of the Week")+ylab("Delivery Time") + theme_light()

ggplot(df_unique,aes(x=vehicle_type,y=loading_time))+
  geom_tile()+
  ggtitle("Loading Time variation across Vehicle Types")+
  xlab("Vehicle Types")+ylab("Loading Time (Hours)") + theme_light()

ggplot(df_unique,aes(x=vehicle_type,y=delivery_distance))+
  geom_boxplot()+
  ggtitle("Delivery Distances by Vehicle Type")+
  xlab("Vehicle Type")+
  ylab("Distance (Miles)") + theme_light()

ggplot(df_unique,aes(x=vehicle_type,y=jumpman_avg_speed))+
  geom_boxplot()+
  ggtitle("Average Delivery Speed by Vehicle Type")+
  xlab("Vehicle Type")+
  ylab("Speed (MPH)") + theme_light()

ggplot(df_unique,aes(x=vehicle_type,y=jumpman_avg_speed))+
  geom_boxplot()+
  ggtitle("Average Delivery Speed by Vehicle Type")+
  xlab("Vehicle Type")+
  ylab("Speed (MPH)")+ylim(0,20) + theme_light()
