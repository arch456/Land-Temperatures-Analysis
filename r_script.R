library(tidyverse)
library(lubridate)

#load the data
temp_global <- read_csv('C:/Users/archa/Documents/Archana/DSP_539/Project/data/GlobalTemperatures.csv')

# Remove the NAs
temp_global <- filter(temp_global,!is.na(LandAverageTemperature) & !is.na(LandAverageTemperatureUncertainty))

head(temp_global)

temp_global$dt<- as.Date(temp_global$dt, "%Y-%m-%d")
temp_global$dt <- strftime(temp_global$dt, "%Y")

#Extract data from 1900 onwards
temp_global <- filter(temp_global,dt >= "1900-01-01")

temp_global <- temp_global %>%
  group_by(dt) %>%
  summarise(LandAverageTemperature = mean(LandAverageTemperature))

#plot the data
ggplot(temp_global,aes(x = dt,y = LandAverageTemperature)) +
  geom_point(color = "blue",size = 3) + scale_x_discrete(breaks = seq(min(temp_global$dt),max(temp_global$dt),10)) +
  labs(title = "Land Temperature over the years",y = "Average Temperature", x  = "Year")

# Calculate the percentage increase overall
min <- (filter(temp_global,dt == min(temp_global$dt)))$LandAverageTemperature
max <- (filter(temp_global,dt == max(temp_global$dt)))$LandAverageTemperature
percent_increase <- (max-min)*100/min
print(percent_increase)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# load the data
temp_country <- read_csv('C:/Users/archa/Documents/Archana/DSP_539/Project/data/GlobalLandTemperaturesByCountry.csv')

#Remove the rows with NAs in AverageTemperature and AverageTemperatureUncertainty columns
temp_country <- filter(temp_country,!is.na(AverageTemperature) & !is.na(AverageTemperatureUncertainty))

head(temp_country)

temp_country$dt<- as.Date(temp_country$dt, "%Y-%m-%d")
temp_country$dt <- strftime(temp_country$dt, "%Y")

#Extract data from 1900 onwards, country United States

temp_country_US <- filter(temp_country,dt>="1900", Country =="United States")
grouped_temp_US <- group_by(temp_country_US,dt)
summ_temp_US <- summarise(grouped_temp_US,temp = mean(AverageTemperature))

ggplot(summ_temp_US, aes(x = dt, y = temp)) +
  geom_point(color = "red",size = 3) + scale_x_discrete(breaks = seq(min(summ_temp_US$dt),max(summ_temp_US$dt),10)) +
labs(title = "Land Temperature over the years in US",y = "Average Temperature", x  = "Year")

# Calculate the percentage increase
min <- (filter(summ_temp_US,dt == min(summ_temp_US$dt)))$temp
max <- (filter(summ_temp_US,dt == max(summ_temp_US$dt)))$temp
percent_increase <- (max-min)*100/min
print(percent_increase)

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#GlobalLandTemperaturesByState

#load the data
temp_state <- read_csv('C:/Users/archa/Documents/Archana/DSP_539/Project/data/GlobalLandTemperaturesByState.csv')


#Remove the rows with NAs in AverageTemperature and AverageTemperatureUncertainty columns
temp_state <- filter(temp_state,!is.na(AverageTemperature))
temp_state <- filter(temp_state,!is.na(AverageTemperatureUncertainty))

head(temp_state)

temp_state$dt <- as.Date(as.character(temp_state$dt))


head(temp_state)

#Extract data from 1900 onwards, country United States
temp_state_US <- filter(temp_state,dt>="1900-01-01", Country =="United States")

temp_state_US$dt <- strftime(temp_state_US$dt, "%Y")

summ_temp_US <- group_by(temp_state_US,State,dt) %>%
  summarise(AverageTemperature = mean(AverageTemperature))

# plot the data for each state
ggplot(summ_temp_US, aes(x = dt, y = AverageTemperature, color = State)) +
  geom_point() + facet_wrap(~State) + scale_x_discrete(breaks = seq(min(summ_temp_US$dt),max(summ_temp_US$dt),75)) +
  labs(title = "Average Land Temperature in all the States",y = "Average Temperature", x  = "Year")

# Calculate the percentage increase by state
state_names <- unique(summ_temp_US$State)
print(state_names)
percent_increase <- c()
for (state in state_names){
  min <- filter(summ_temp_US, State == state, dt == min(summ_temp_US$dt))$AverageTemperature
  max <- filter(summ_temp_US, State == state, dt == max(summ_temp_US$dt))$AverageTemperature
  percent_increase <- c(percent_increase,(max-min)*100/min)
}
#print(percent_increase)
state_increase <- data.frame("State" = c(state_names), "Increase" = c(percent_increase))

# plot the data for percent increase in each state over the last century
ggplot(state_increase, aes(y = State, x = Increase)) +
  geom_point(size = 3) + labs(title = "Average Increase in all the States over the last century",x = "Increase in Temperature", y  = "State")


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#GlobalLandTemperaturesByMajorCity

#load the data
temp_city <- read_csv('C:/Users/archa/Documents/Archana/DSP_539/Project/data/GlobalLandTemperaturesByMajorCity_updated.csv')

#Remove the rows with NAs in AverageTemperature and AverageTemperatureUncertainty columns
temp_city <- filter(temp_city,!is.na(AverageTemperature))
temp_city <- filter(temp_city,!is.na(AverageTemperatureUncertainty))

head(temp_city)

#date<- as.Date(temp_state$dt, "%Y-%m-%d")

#temp_country$dt <- strftime(temp_country$dt, "%Y")
temp_city$dt <- as.Date(as.character(temp_city$dt))
temp_city$dt<- as.Date(temp_city$dt, "%Y-%m-%d")
#temp_state <- cbind(date,temp_state[2:5])

tail(temp_city)

#Extract data from 2000 onwards, country United States
temp_city_US <- filter(temp_city,dt>="2000-01-01", Country =="United States")

# Order the legend
ordered_legend_tc <- temp_city_US %>% filter(dt == max(dt)) %>% # get the data from recent year
  arrange(desc(AverageTemperature)) %>%   pull(City)


ggplot(temp_city_US, aes(x = dt, y = AverageTemperature,color = City)) +
  geom_line(size=1) + scale_color_discrete(breaks = ordered_legend_tc, name = "City")+
  xlab("Year")

#Extract data from 1900 onwards, for cities in United States
temp_city_US <- filter(temp_city,dt>="1900-01-01", Country =="United States")

temp_city_US$dt <- strftime(temp_city_US$dt, "%Y")

summ_temp_city_US <- group_by(temp_city_US,City,dt) %>%
  summarise(AverageTemperature = mean(AverageTemperature))

# plot the data for each city
ggplot(summ_temp_city_US, aes(x = dt, y = AverageTemperature, color = City)) +
  geom_point(size = 2) + facet_wrap(~City) + scale_x_discrete(breaks = seq(min(summ_temp_US$dt),max(summ_temp_US$dt),50)) +
  labs(title = "Average Land Temperature in Cities",y = "Average Temperature", x  = "Year")

# Calculate the percentage increase by city
city_names <- unique(summ_temp_city_US$City)
percent_increase_city <- c()
for (city in city_names){
  min <- filter(summ_temp_city_US, City == city, dt == min(summ_temp_city_US$dt))$AverageTemperature
  max <- filter(summ_temp_city_US, City == city, dt == max(summ_temp_city_US$dt))$AverageTemperature
  percent_increase_city <- c(percent_increase_city,(max-min)*100/min)
}
#print(percent_increase)
city_increase <- data.frame("City" = c(city_names), "Increase" = c(percent_increase_city))

# plot the data for percent increase in each major city over the last century
ggplot(city_increase, aes(y = City, x = Increase, color = City)) +
  geom_point(size = 5) + labs(title = "Average Increase in major Cities over the last century",x = "Increase in Temperature", y  = "City")


