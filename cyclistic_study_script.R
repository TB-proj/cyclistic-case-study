Sys.setlocale("LC_TIME", "en_US")

d202111 <- read_csv("C:/Users/Tom/Desktop/datasets/202111-divvy-tripdata.csv")
d202112 <- read_csv("C:/Users/Tom/Desktop/datasets/202112-divvy-tripdata.csv")
d202201 <- read_csv("C:/Users/Tom/Desktop/datasets/202201-divvy-tripdata.csv")
d202202 <- read_csv("C:/Users/Tom/Desktop/datasets/202202-divvy-tripdata.csv")
d202203 <- read_csv("C:/Users/Tom/Desktop/datasets/202203-divvy-tripdata.csv")
d202204 <- read_csv("C:/Users/Tom/Desktop/datasets/202204-divvy-tripdata.csv")
d202205 <- read_csv("C:/Users/Tom/Desktop/datasets/202205-divvy-tripdata.csv")
d202206 <- read_csv("C:/Users/Tom/Desktop/datasets/202206-divvy-tripdata.csv")
d202207 <- read_csv("C:/Users/Tom/Desktop/datasets/202207-divvy-tripdata.csv")
d202208 <- read_csv("C:/Users/Tom/Desktop/datasets/202208-divvy-tripdata.csv")
d202209 <- read_csv("C:/Users/Tom/Desktop/datasets/202209-divvy-publictripdata.csv")
d202210 <- read_csv("C:/Users/Tom/Desktop/datasets/202210-divvy-tripdata.csv")


cyclistic_data <- rbind(d202111,d202112,d202201,d202202,d202203,d202204,d202205,d202206,d202207,d202208,d202209,d202210)

all_trips <- cyclistic_data

all_trips$date <- as.Date(all_trips$started_at) #add a date column
all_trips$month <- format(as.Date(all_trips$started_at), "%b_%y")   #add a month column formatted as short hand_year (e.g. feb_2021)
all_trips$day <- format(as.Date(all_trips$date), "%d")  #add a day column
all_trips$year <- format(as.Date(all_trips$date), "%Y")  #add a year column
all_trips$weekday <- format(as.Date(all_trips$date), "%A")  #add a day of week column
all_trips$time <- format(all_trips$started_at, format = "%H:%M")  #add a time started column
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")  #change format for the time column
all_trips$ride_length <- (as.double(difftime(all_trips$ended_at, all_trips$started_at))) /60  #calculate ride length in minutes

str(all_trips)
colnames(all_trips)
summary(all_trips)
head(all_trips)

all_trips <- distinct(all_trips) #remove any duplicates
all_trips <- all_trips[!all_trips$ride_length<1,] #get rid of negative rides
all_trips <- all_trips[!all_trips$ride_length>1440,] #get rid of too long rides - rides should be limited to 1 day or 1440 minutes

#change a few column names for clarification
all_trips <- rename(all_trips, customer_type = member_casual) 
all_trips <- rename(all_trips, bike_type = rideable_type)

all_trips <- all_trips %>% select(bike_type, customer_type, started_at, date, month, day, year, weekday, time, ride_length)
drop_na(all_trips)

#cleaning
remove_empty(all_trips)
remove_missing(all_trips) 

#this will help keep the results of the analysis in order based on day of week and by month to avoid confusion
all_trips$weekday <- ordered(all_trips$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday", "Saturday", "Sunday"))
all_trips$month <- ordered(all_trips$month, levels=c("Nov_21", "Dec_21", "Jan_22", "Feb_22", "Mar_22", "Apr_22",  "May_22", "Jun_22","Jul_22", "Aug_22", "Sep_22", "Oct_22"))

summary(all_trips$ride_length)  
table(all_trips$customer_type)

##looks at total rides for each customer type in minutes
setNames(aggregate(ride_length ~ customer_type, all_trips, sum), c("customer_type", "total_ride_length(mins)"))

##look at rides based on customer type
all_trips %>% 
  group_by(customer_type) %>% 
  summarise(min_length = min(ride_length), max_length = max(ride_length), 
            median_length = median(ride_length), mean_length = mean(ride_length))

#look at ride lengths broken down by day of week and customer type
aggregate(all_trips$ride_length ~ all_trips$customer_type + all_trips$weekday, FUN = median)

##look at total number of rides and averages based on day of week and customer type
all_trips %>% 
  group_by(customer_type, weekday) %>% 
  summarise(total_rides = n(), avg_ride = mean(ride_length)) %>% 
  arrange(weekday)

all_trips %>%    #total rides broken down by weekday
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n() ) %>% 
  arrange(customer_type, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = customer_type)) + geom_col(position = "dodge") + 
  labs(x= 'Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(250000, 400000, 550000), labels = c("250K", "400K", "550K"))

all_trips %>%   #total rides broken down by month
  group_by(customer_type, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(customer_type) %>% 
  ggplot(aes(x=month, y=total_rides, fill = customer_type)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) + theme(axis.text.x = element_text(angle = 45))

all_trips %>%   #Average length by Customer Type and Day of Week
  group_by(customer_type, weekday) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=weekday, y = average_ride_length, fill = customer_type))+
  geom_col(position = "dodge") + labs (x="Day of Week", y="Average Ride Length(min)", 
                                       title = "Average Ride Length by Customer Type and Day of Week", 
                                       fill = "Type of Membership") 

all_trips %>%  #Average ride length by customer type and month
  group_by(customer_type, month) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=month, y = average_ride_length, fill = customer_type))+ 
  geom_col(position = "dodge") +
  labs (x="Month", y = "Average Ride Length(min)", title = "Average Ride Length by Customer Type and Month", 
        fill = "Type of Membership") + theme(axis.text.x = element_text(angle = 45))

all_trips %>%    #looking at breakdown of bike types rented
  ggplot(aes(x = bike_type, fill = customer_type)) + geom_bar(position = "dodge") + 
  labs(x= 'Bike Type', y='Number of Rentals', title='Bike Type Breakdown', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mil", "1.5Mil"))

all_trips %>%     #Looking at demand over a 24 hour day
  group_by(customer_type, time) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x=time, y=total_rides, color = customer_type, group = customer_type)) +
  geom_line() + scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title ="Demand Throughout the Day", x = "Time", y = "Total Rides")