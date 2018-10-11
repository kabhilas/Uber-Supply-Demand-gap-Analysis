# Load required libraries.
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
require(gridExtra)

# Read the dataset using read.csv function
uber <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)

#Check for the structure of the dataset
str(uber)
head(uber)

# Let's analyse the columns individually
# Check for NAs
length(which(is.na(uber$Request.id)))               # 0 NAs
length(which(is.na(uber$Pickup.point)))             # 0 NAs
length(which(is.na(uber$Driver.id)))                # 2650 NAs
length(which(is.na(uber$Status)))                   # 0 NAs
length(which(is.na(uber$Request.timestamp)))        # 0 NAs
length(which(is.na(uber$Drop.timestamp)))           # 3914 NAs
length(which(is.na(uber)))                          # Total NAs : 2650 + 3914 =  6564
sapply(colnames(uber), function(x) length(which(is.na(uber[,x]))))

# Check the summary for some categorical columns
summary(as.factor(uber$Pickup.point))
#  Airport    City 
#     3238    3507

summary(as.factor(uber$Status))
#   Cancelled      No Cars Available    Trip Completed 
#     1264              2650                2831 

# Check for blanks
length(which(uber$Request.id == ""))                # 0 blanks
length(which(uber$Pickup.point == ""))              # 0 blanks
length(which(uber$Driver.id == ""))                 # 0 blanks
length(which(uber$Status == ""))                    # 0 blanks
length(which(uber$Request.timestamp == ""))         # 0 blanks
length(which(uber$Drop.timestamp == ""))            # 0 blanks
sapply(colnames(uber), function(x) length(which(uber[,x] == "")))

# Check if Request.id is unique or not
sum(duplicated(uber$Request.id))  # The Request.ids are unique

# The droptime and drier ID has NA values. That can be due to incomplete/cancelled request.
# Let's analyze that.
summarise(group_by(filter(uber, is.na(uber$Drop.timestamp)), Status), count = length(Request.id)) 
#     Status               count
#     <chr>                <int>
#  1 Cancelled              1264
#  2 No Cars Available      2650
# It can be infered that cancelled and No Cars Available make the total(1264+2650 = 3914) numbers of NA
# values for Drop.timestamp. 
# 1> As the status are cancelled and No Cars Available, it is obvious that the Drop.timestamp will be NA for those.

summarise(group_by(filter(uber, is.na(uber$Driver.id)), Status), count = length(Request.id))
#    Status             count
#    <chr>              <int>
#  1 No Cars Available   2650
#The 2650 Driver.id are blank because there were no cars available.

# The time in Request.timestamp and Drop.timestamp atre in two different formats:
# 1. %d/%m/%Y %H:%M
# 2. %d-%m-%Y %H:%M:%S
# Converting both date and time columns in POSIXct format
uber$Request.timestamp <- parse_date_time(x = uber$Request.timestamp,
                                          orders = c("d/m/Y H:M", "d-m-Y H:M:S"),
                                          locale = "eng")
uber$Drop.timestamp    <- parse_date_time(x = uber$Drop.timestamp,
                                          orders = c("d/m/Y H:M", "d-m-Y H:M:S"),
                                          locale = "eng")

# Checking for nulls again in date fields to make sure NAs are treated properly
length(which(is.na(uber$Request.timestamp)))        # 0 NAs
length(which(is.na(uber$Drop.timestamp)))           # 3914 NAs

# Let's extract the Request/Drop Date and time
uber$Request.date <- date(uber$Request.timestamp)
uber$Request.hour <- hour(uber$Request.timestamp)
uber$Drop.date <- date(uber$Drop.timestamp)
uber$Drop.hour <- hour(uber$Drop.timestamp)

# Let's analyze the date and hour columns for Request and drop:
# After seeing the date fields it can be infered that the data is for a specific month and year
sort(unique(uber$Request.date))
# [1] "2016-07-11" "2016-07-12" "2016-07-13" "2016-07-14" "2016-07-15"
weekdays(unique(uber$Request.date))
# [1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"  
sort(unique(uber$Drop.date))
# [1] "2016-07-11" "2016-07-12" "2016-07-13" "2016-07-14" "2016-07-15" "2016-07-16"

# Create a Trip.minutes column to consider the time taken in minutes for the trip
uber$Trip.minutes <- round(uber$Drop.timestamp - uber$Request.timestamp, digits = 2) 
length(which(is.na(uber$Trip.minutes)))   # 3914
air_trip_avg <- filter(uber, !is.na(uber$Trip.minutes), Pickup.point=="Airport")
round(mean(air_trip_avg$Trip.minutes), 2)
#Average trip time from city to airport = 52.24 mins

city_trip_avg <- filter(uber, !is.na(uber$Trip.minutes), Pickup.point=="City")
round(mean(city_trip_avg$Trip.minutes), 2)
#Average trip time from city to airport = 53.57 mins
###############################################################################################################
# Distribution of requests over different time slots
# Let's divide the request time in time slots for analysing behaviour of cab requests 
# 0000 to 0359 <-  Late_Night
# 0400 to 0559 <-  Early_Morning
# 0600 to 1059 <-  Morning_Rush
# 1100 to 1659 <-  Day_Time
# 1700 to 2359 <-  Evening_Rush
# ------------------------------------------------------

uber$Time.slot[uber$Request.hour >= 0 & uber$Request.hour <= 3] <- "Late_Night"
uber$Time.slot[uber$Request.hour >= 4 & uber$Request.hour <= 5] <- "Early_Morning"
uber$Time.slot[uber$Request.hour >= 6 & uber$Request.hour <= 10] <- "Morning_Rush"
uber$Time.slot[uber$Request.hour >= 11 & uber$Request.hour <= 16] <- "Day_Time"
uber$Time.slot[uber$Request.hour >= 17 & uber$Request.hour <= 23] <- "Evening_Rush"

summarise(group_by(uber, Time.slot), count = length(Request.id))
#   Time.slot       count
#   <chr>           <int>
# 1 Late_Night        375
# 2 Early_Morning     648
# 3 Morning_Rush      1901
# 4 Day_Time          981
# 5 Evening_Rush      2840
# There is more demand during Morning_Rush and Evening_Rush time slots. 

# Create subsets of airport and city
airport <- subset(uber, uber$Pickup.point == "Airport")
city <- subset(uber, uber$Pickup.point == "City")
summarise(group_by(uber, Pickup.point), count = length(Request.id))
# Pickup.point  count
#  <chr>        <int>
#1 Airport       3238
#2 City          3507

# There is more demand during Evening rush at Airport
summarise(group_by(airport, Time.slot), count = length(Request.id))
#  Time.slot      count
#   <chr>          <int>
#1 Late_Night      181
#2 Early_Morning   164
#3 Morning_Rush    409
#4 Day_Time        403
#5 Evening_Rush    2081

# There is more demand during Morning rush at City
summarise(group_by(city, Time.slot), count = length(Request.id))
# Time.slot       count
# <chr>            <int>
#1 Late_Night      194
#2 Early_Morning   484
#3 Morning_Rush    1492
#4 Day_Time        578
#5 Evening_Rush    759
sum(is.na(uber$Time.slot))  # No NA value for Time.slot

###############################################################################################################
# We can plot graphs for pickup point as "Airport" and "City" against the Request.hour see the distribution 
# of demand and supply over the hours in a day.
#1> Demand at Airport throughout the day for all  days
air_dem_5days  <- ggplot(airport, aes(x = as.factor(Request.hour))) +
                  geom_bar(fill = "black") + labs(title ="Demand at Airport throughout the 5 day", x="Hours", y="No. of Requests") + ylim(0,500) +
                  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 5, vjust = -0.25, colour = "black")
air_dem_5days

#2> Demand at City throughout the day for all  days
city_dem_5days <- ggplot(city, aes(x = as.factor(Request.hour))) +
                  geom_bar(fill = "red") + labs(title ="Demand at City throughout the 5 day", x="Hours", y="No. of Requests") + ylim(0,500) +
                  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 5, vjust = -0.25, colour = "black")
city_dem_5days

grid.arrange(air_dem_5days, city_dem_5days, ncol=2)


# Conclusion:
# > City has more demand during Early_Morning and Morning_Rush period
# > Airport has more demand during Evening_Rush
# 
###############################################################################################################
# Frequency of requests that get cancelled or show 'no cars available' 
# Let's distribute the demand at Airport, City and both based on Status
air_status  <- ggplot(airport, aes(x = as.factor(Request.hour),fill = Status)) + geom_bar(position = "dodge") +
               labs(title ="Status of requests made from Air to City", x="Hours", y="No. of Requests") +
               geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 4, vjust = -0.25, colour = "black")
air_status

city_status <- ggplot(city, aes(x = as.factor(Request.hour),fill = Status)) + geom_bar(position = "dodge") +
               labs(title ="Status of requests made from City to Air", x="Hours", y="No. of Requests") +
               geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 4, vjust = -0.25, colour = "black")
city_status

both_status <- ggplot(uber, aes(x = as.factor(Request.hour),fill = Status)) + geom_bar(position = "dodge") +
               labs(title ="Status of requests from both air and city") +
               geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 4, vjust = -0.25, colour = "black")
both_status

grid.arrange(air_status, city_status, ncol=2)

# Conclusion:
# > "No cars available" are more during Evening_Rush at Airport.
# > More cars are cancelled during Early_Morning and Morning_Rush at City.
# > There is a gap in demand and supply at airport during Evening_Rush. 
# 
air_status + facet_wrap( ~ airport$Request.date, nrow =5, ncol = 1) + labs(x = "Request.hour", y = "Request count", fill = "Status" )
city_status + facet_wrap( ~ city$Request.date, nrow =5, ncol = 1) + labs(x = "Request.hour", y = "Request count", fill = "Status" )
both_status + facet_wrap( ~ uber$Request.date, nrow =5, ncol = 1) + labs(x = "Request.hour", y = "Request count", fill = "Status" )
# > Data is evenly distributed among the five days
#
###############################################################################################################
# The most problematic types of requests (city to airport / airport to city etc.) 
airport_request <- ggplot(airport, aes(x = as.factor(Request.hour),fill = Status)) + geom_bar() + 
                   labs(title ="Status of requests made at Airport") + ylim(0,500)
airport_request
city_request    <- ggplot(city, aes(x = as.factor(Request.hour),fill = Status)) + geom_bar() + 
                   labs(title ="Status of requests made at City") + ylim(0,500)
city_request

grid.arrange(airport_request, city_request, ncol=2)

# Conclusion:
# > "Trip completed" is evenly distributed among the hours for both Pickup points.
# > There is less requests during Late_Night, average requests during Day_Time and high requests during
#   Early_Morning, Morning_Rush and Evening_Rush.
# > Most problematic types of requests are :
#   1. Requests made at city during Early_Morning and Morning_Rush.
#   2. Requests made at Airport during Evening_Rush.
#
###############################################################################################################
# Plotting requests made at Airport, city and both with respect to time slots
air_timeslot_status  <- ggplot(airport, aes(x = as.factor(Time.slot),fill = Status)) + geom_bar(position = "dodge") +  
                        labs(title ="Status of requests made at Airport based on time slots") +
                        geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 5, vjust = -0.25, colour = "black")
air_timeslot_status

city_timeslot_status <- ggplot(city, aes(x = as.factor(Time.slot),fill = Status)) + geom_bar(position = "dodge") +  
                        labs(title ="Status of requests made at City based on time slots") +
                        geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 5, vjust = -0.25, colour = "black")
city_timeslot_status

both_timeslot_status <- ggplot(uber, aes(x = as.factor(Time.slot),fill = Status)) + geom_bar(position = "dodge") +  
                        labs(title ="Status of requests made at both Airport and City based on time slots") +
                        geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9), size = 5, vjust = -0.25, colour = "black")
both_timeslot_status
grid.arrange(air_timeslot_status, city_timeslot_status, ncol=2)

# Conclusion:
# > Thers is huge gap in demand and supply during Evening_Rush
# > Demand = Trip Completed + Cancelled  + No Cabs Available      
# > Supply = Trip Completed (Cancelled can not be considered because the request was cancelled either 
#   by requestor or driver. It can not be considered that the demand was met)
#
#################################################################################################################
# Let's analyse the Demand and Supply gap during most problematic scenarios.
# 1> Requests made at Airport during Evening_Rush.
summarise(group_by(filter(uber, Pickup.point=="Airport", Time.slot=="Evening_Rush"), 
                   Status), cnt = length(Request.id))
#   Status              cnt
#   <chr>             <int>
# 1 Cancelled           109
# 2 No Cars Available  1457
# 3 Trip Completed      515
# Demand at airport during Evening_Rush = 515 + 109 + 1457 =  2081
# Supply at airport during Evening_Rush = 515
# Gap = 2081 - 515 = 1566

air_evening_rush <- summarise(group_by(filter(uber, Pickup.point=="Airport", Time.slot=="Evening_Rush"), 
                                       Request.date, Status), cnt = length(Request.id))
air_evening_rush_spread <- data.frame(spread(air_evening_rush,key=Status,value=cnt))
air_evening_rush_mutate <- mutate(air_evening_rush_spread, 
                                  Demand = Cancelled + No.Cars.Available + Trip.Completed, 
                                  supply = Trip.Completed)
air_evening_rush_select <- select(air_evening_rush_mutate, Request.date, Demand, supply)
air_evening_rush_demand_supply <- gather(air_evening_rush_select, key="Demand_Supply", values=c(2,3))
air_evening_rush_demand_supply

ggplot(data = air_evening_rush_demand_supply) +
              geom_col(aes(x=as.character(Request.date),y=value,fill=Demand_Supply)) + 
              geom_text(aes(x = as.character(Request.date), y = value, label = value, group = Demand_Supply),
              position = position_stack(vjust = .25)) +
              labs(title="Airport evening Demand_Supply Gap graph(1700 to 2359) ", 
              x="Date", y="No. of Requests", fill="Demand/Supply") 

# 2> Requests made at City during Early_Morning and Morning_Rush
summarise(group_by(filter(uber, Pickup.point=="City", Time.slot %in% c("Early_Morning","Morning_Rush")), 
                   Status), cnt = length(Request.id))
#   Status              cnt
#   <chr>              <int>
# 1 Cancelled           922
# 2 No Cars Available   477
# 3 Trip Completed      577
# Demand at city during Early_Morning and Morning_Rush = 922 + 477 + 577 =  1976
# Supply at city during Early_Morning and Morning_Rush = 577
# Gap = 1976 - 577 = 1399

city_morning_rush <- summarise(group_by(filter(uber, Pickup.point=="City",(Time.slot %in% c("Early_Morning","Morning_Rush"))), 
                                     Request.date,Status), cnt = length(Request.id))
city_morning_rush_spread <- data.frame(spread(city_morning_rush,key=Status,value=cnt))
city_morning_rush_mutate <- mutate(city_morning_rush_spread, 
                                   Demand = Cancelled + No.Cars.Available + Trip.Completed, 
                                   supply = Trip.Completed)
city_morning_rush_select <- select(city_morning_rush_mutate, Request.date,Demand,supply)
city_morning_rush_demand_supply <- gather(city_morning_rush_select, key="Demand_Supply",values=c(2,3))
city_morning_rush_demand_supply

ggplot(data = city_morning_rush_demand_supply) +
              geom_col(aes(x=as.character(Request.date),y=value,fill=Demand_Supply)) + 
              geom_text(aes(x = as.character(Request.date), y = value, label = value, group = Demand_Supply),
              position = position_stack(vjust = .25)) +
              labs(title="City Morning Demand_Supply Gap graph(0400 to 1059)", 
              x="Date", y="No. of Requests", fill="Demand/Supply") 

################################################# Conclusion ####################################################
#---------------------------------------------------------------------------------------------------------------#
# 1> The Uber Request data has data evenly distributed among five working days and contais data for request 
#    made from Airport and City.
# 2> The NA values for Driver.id and Drop.timestamp are justified as either the trip was cancelled or there 
#    were no cars available.
# 3> The average trip time from Airport to City or vice versa is approx 52 minutes.
# 4> There is high number of "No cars available" cars at Airport during Evening rush(5 PM till 12 AM). 
# 5> There is high number of "Cancelled" cars at Airport during early morning and morning rush(4 AM till 11 AM). 
# 6> The major issue when pickup point is Airport is Demand/Supply gap during evening (Gap = 1566)
# 7> The major issue when pickup point is City is Demand/Supply gap during morning (Gap = 1399)
# 8> The reason for gap at Airport could be due to waiting time. Drivers need to wait for long to get next request at 
#    Airport during evening time. For gap in city, high demand during working hours could be the reason for more 
#    cancelled requests. 
#---------------------------------------------------------------------------------------------------------------#
