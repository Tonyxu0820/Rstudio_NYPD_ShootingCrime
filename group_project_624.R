###NYPD Shooting Incident data Historic data set
#install.packages('tidyverse')
library(tidyverse)
library(ggmap)
library(stringr)
library(viridis)
library(ggplot2)
library(xts)
library(fpp2)
library(forecast)
library(quantmod)
library(tseries)

#read data
Shooting = read.csv('NYPD_Shooting_Incident_Data__Historic_.csv')

#Drop unneeded columns
Shooting$INCIDENT_KEY=NULL
Shooting$PRECINCT=NULL
Shooting$JURISDICTION_CODE=NULL

#Drop the column with too many missing values
#Percentage of Missing value 13581/23585=57.58%
table(Shooting$LOCATION_DESC)
Shooting$LOCATION_DESC=NULL

#New data set 
Cleanned_shooting=data.frame(Shooting[1:14])
Cleanned_shooting$X_COORD_CD = NULL
Cleanned_shooting$Y_COORD_CD = NULL

#Dealing with missing valuses 
#replace the blank cell by character "UNKNOWN"
Cleanned_shooting$PERP_AGE_GROUP[Cleanned_shooting$PERP_AGE_GROUP==" "]<- as.character(" 
UNKNOWN")
Cleanned_shooting$PERP_SEX[Cleanned_shooting$PERP_SEX==" "]<- as.character(" 
UNKNOWN")
Cleanned_shooting$PERP_RACE[Cleanned_shooting$PERP_RACE==" "]<- as.character(" 
UNKNOWN")

#Create new column OCCUR_TIME_period based on OCCUR_TIME
parse_number(Cleanned_shooting$OCCUR_TIME)
table(parse_number(Cleanned_shooting$OCCUR_TIME))
Cleanned_shooting$OCCUR_TIME_period<- parse_number(Cleanned_shooting$OCCUR_TIME)
Cleanned_shooting$OCCUR_TIME_period <- replace(Cleanned_shooting$OCCUR_TIME_period, Cleanned_shooting$OCCUR_TIME_period >=0&Cleanned_shooting$OCCUR_TIME_period <=7, 1)
Cleanned_shooting$OCCUR_TIME_period <- replace(Cleanned_shooting$OCCUR_TIME_period, Cleanned_shooting$OCCUR_TIME_period > 7&Cleanned_shooting$OCCUR_TIME_period <=11, 2)
Cleanned_shooting$OCCUR_TIME_period <- replace(Cleanned_shooting$OCCUR_TIME_period, Cleanned_shooting$OCCUR_TIME_period > 11&Cleanned_shooting$OCCUR_TIME_period <=17, 3)
Cleanned_shooting$OCCUR_TIME_period <- replace(Cleanned_shooting$OCCUR_TIME_period, Cleanned_shooting$OCCUR_TIME_period > 17&Cleanned_shooting$OCCUR_TIME_period <=23, 4)
table(Cleanned_shooting$OCCUR_TIME_period)

##Transfer date to correct format
Cleanned_shooting$OCCUR_DATE <- as.Date(Cleanned_shooting$OCCUR_DATE, format = "%m/%d/%y")

##Separate data set before COVID and after COVID ~2020.3/2020.3~
#Extract year and month from date
Cleanned_shooting$year <- format(Cleanned_shooting$OCCUR_DATE, format="%Y")
Cleanned_shooting$month <- format(Cleanned_shooting$OCCUR_DATE, format="%m")
BeforeCovid=Cleanned_shooting%>%
  filter(year<2020)

AfterCovid=Cleanned_shooting%>%
  filter(year>=2020)

###########################################################################################################
#Using Spatial Analysis to find which county has the highest crime density
#Create api key
API_Key = 'AIzaSyAOvByHadRfzz1yuDuJyaMBPE8_c7DVxP4'

#Apply api
register_google(key = API_Key)

#Create New York map
newyork_map = get_map(location = c(-73.93835,40.66392),zoom = 10,scale = 4)

#Visualizing the density of crime before COVID 
ggmap(newyork_map) +
  stat_density2d(data = BeforeCovid, aes(x = Longitude, y = Latitude, fill = ..density..), geom = 'tile', contour = F, alpha = .7) +  
  scale_fill_viridis(option = 'inferno') + 
  labs(title = str_c('Heatmap Before Covid\n')) +
  theme(text = element_text(color = "#444444")
        ,plot.title = element_text(size = 15, face = 'bold')
        ,plot.subtitle = element_text(size = 12)
        ,axis.text = element_blank()
        ,axis.title = element_blank())

#Visualizing the density of crime after COVID 
ggmap(newyork_map) +
  stat_density2d(data = AfterCovid, aes(x = Longitude, y = Latitude, fill = ..density..), geom = 'tile', contour = F, alpha = .7) +  
  scale_fill_viridis(option = 'inferno') + 
  labs(title = str_c('Heatmap after Covid\n')) +
  theme(text = element_text(color = "#444444")
        ,plot.title = element_text(size = 15, face = 'bold')
        ,plot.subtitle = element_text(size = 12)
        ,axis.text = element_blank()
        ,axis.title = element_blank())


#Visualizing the density of crime by heat map 
ggmap(newyork_map) +
  stat_density2d(data = Cleanned_shooting, aes(x = Longitude, y = Latitude, fill = ..density..), geom = 'tile', contour = F, alpha = .7) +  
  scale_fill_viridis(option = 'inferno') + 
  labs(title = str_c('Brooklyn and Bronx are the most two crimes counties\n')) +
  theme(text = element_text(color = "#444444")
        ,plot.title = element_text(size = 15, face = 'bold')
        ,plot.subtitle = element_text(size = 12)
        ,axis.text = element_blank()
        ,axis.title = element_blank())
#Brooklyn and Bronx are the most two crimes counties

###########################################################################################################
#Then we use the 'tidycensus' package from r to find why Brooklyn and Bronx are the most two crimes counties
#install.packages('tidycensus')
library(tidycensus)
#Need API key to apply the data
census_api_key("f23f051aeddc88b3b1277add1ee6e05e093cfbf5", install = TRUE)

##New York City's counties' income (2006-2020)
NY_income_06_10 <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "NY",
  county = c("Kings", "New York","Bronx","Queens", "Richmond"),
  year = 2010,
)
NY_income_11_15 <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "NY",
  county = c("Kings", "New York","Bronx","Queens", "Richmond"),
  year = 2015,
)
NY_income_16_20 <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "NY",
  county = c("Kings", "New York","Bronx","Queens", "Richmond"),
  year = 2020,
)

## Comparing with the income facotrs for five areas
#Extract year and month from date
Cleanned_shooting$year <- format(Cleanned_shooting$OCCUR_DATE, format="%Y")
Cleanned_shooting$month <- format(Cleanned_shooting$OCCUR_DATE, format="%m")
year <- c("2006", "2007", "2008", "2009", "2010","2011", "2012", "2013", "2014", "2015","2016", "2017", "2018", "2019", "2020")
values <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
Cleanned_shooting$group <- values[match(Cleanned_shooting$year, year)]


library(dplyr)
NY_income_06_10$YEAR=1
NY_income_11_15$YEAR=2
NY_income_16_20$YEAR=3
NY_INCOME=rbind(NY_income_06_10,NY_income_11_15,NY_income_16_20)

library(ggplot2)
library(devtools)

ggplot(data = NY_INCOME, mapping = aes(x=YEAR, y=estimate, fill=NAME)) +
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer()+ 
  labs(fill = "conty")

shooting_number=Cleanned_shooting%>%
  group_by(Cleanned_shooting$BORO,Cleanned_shooting$group)%>%
  count(Cleanned_shooting$BORO)
ggplot(data = shooting_number, aes(x=shooting_number$`Cleanned_shooting$group`, y=n,fill=shooting_number$`Cleanned_shooting$BORO`)) +
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer()+ 
  labs(fill = "conty")

##Race component for each county
#Queens County
Queens_race <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE"),
  breakdown_labels = TRUE,
  state = "NY",
  county = c("Queens"),
  year = 2019
)
Queens_race <- as.data.frame(Queens_race)
Queens_race <- Queens_race[2:7,]
Queens_race <- Queens_race %>% 
  mutate(prop = round(value / sum(Queens_race$value) *100,2))
Queens_race

#Brooklyn County 
Brooklyn_race <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE"),
  breakdown_labels = TRUE,
  state = "NY",
  county = c("Kings"),
  year = 2019
)
Brooklyn_race <- as.data.frame(Brooklyn_race)
Brooklyn_race <- Brooklyn_race[2:7,]
Brooklyn_race <- Brooklyn_race %>% 
  mutate(prop = round(value / sum(Brooklyn_race$value) *100,2))
Brooklyn_race

#Manhattan County
Manhattan_race <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE"),
  breakdown_labels = TRUE,
  state = "NY",
  county = c("New York"),
  year = 2019
)
Manhattan_race <- as.data.frame(Manhattan_race)
Manhattan_race <- Manhattan_race[2:7,]
Manhattan_race <- Manhattan_race %>% 
  mutate(prop = round(value / sum(Manhattan_race$value) *100,2))
Manhattan_race

#Bronx County
Bronx_race <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE"),
  breakdown_labels = TRUE,
  state = "NY",
  county = c("Bronx"),
  year = 2019
)
Bronx_race <- as.data.frame(Bronx_race)
Bronx_race <- Bronx_race[2:7,]
Bronx_race <- Bronx_race %>% 
  mutate(prop = round(value / sum(Bronx_race$value) *100,2))
Bronx_race

#Staten Island County
Staten_Island_race <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE"),
  breakdown_labels = TRUE,
  state = "NY",
  county = c("Richmond"),
  year = 2019
)
Staten_Island_race <- as.data.frame(Staten_Island_race)
Staten_Island_race<- Staten_Island_race[2:7,]
Staten_Island_race <- Staten_Island_race %>% 
  mutate(prop = round(value / sum(Staten_Island_race$value) *100,2))
Staten_Island_race

#Combine them all together
NewYorkCounties_race <- rbind(Queens_race,Manhattan_race,Brooklyn_race,Bronx_race,Staten_Island_race)

#Visualizing the proportion of race of each county by stack plot
ggplot(NewYorkCounties_race, aes(fill=RACE, y=prop, x=NAME)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer()
###########################################################################################################
##Time Series
#Extract year and month from date
Cleanned_shooting$year <- format(Cleanned_shooting$OCCUR_DATE, format="%Y")
Cleanned_shooting$month <- format(Cleanned_shooting$OCCUR_DATE, format="%m")

#Drop Unnecessary Column
newdata <- Cleanned_shooting[c(-1,-2,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-16)]
head(newdata)

#Find unique county
colnames(newdata)[1] <- "Location"
unique(newdata$Location)

#Subset the newdata into different county
#Count the frequency of cases per month
#Build new data frame and drop columns for month and year
#Build the ts file
#BRONX
bronx <- subset(newdata, newdata$Location == 'BRONX')
head(bronx)
table(bronx)
bronx2 = table(bronx)
df_bronx = as.data.frame(bronx2)
head(df_bronx)
df_bronx_to_ts = df_bronx[c(-1,-2,-3)]
head(df_bronx_to_ts)
bronx_ts = ts(df_bronx_to_ts,start=c(2006,01),frequency=12)
bronx_ts

#BROOKLYN
brooklyn <- subset(newdata, newdata$Location == 'BROOKLYN')
head(brooklyn)
table(brooklyn)
brooklyn2 = table(brooklyn)
df_brooklyn = as.data.frame(brooklyn2)
df_brooklyn_to_ts = df_brooklyn[c(-1,-2,-3)]
brooklyn_ts = ts(df_brooklyn_to_ts,start=c(2006,01),frequency=12)
brooklyn_ts


#MANHATTAN 
manhattan <- subset(newdata, newdata$Location == 'MANHATTAN')
head(manhattan)
table(manhattan)
manhattan2 = table(manhattan)
df_manhattan = as.data.frame(manhattan2)
df_manhattan_to_ts = df_manhattan[c(-1,-2,-3)]
manhattan_ts = ts(df_manhattan_to_ts,start=c(2006,01),frequency=12)
manhattan_ts

#QUEENS
queens <- subset(newdata, newdata$Location == 'QUEENS')
head(queens)
table(queens)
queens2 = table(queens)
df_queens = as.data.frame(queens2)
df_queens_to_ts = df_queens[c(-1,-2,-3)]
queens_ts = ts(df_queens_to_ts,start=c(2006,01),frequency=12)
queens_ts

#STATEN ISLAND
staten_island <- subset(newdata, newdata$Location == 'STATEN ISLAND')
head(staten_island)
table(staten_island)
staten_island2 = table(staten_island)
df_staten_island = as.data.frame(staten_island2)
df_staten_island_to_ts = df_staten_island[c(-1,-2,-3)]
staten_island_ts = ts(df_staten_island_to_ts,start=c(2006,01),frequency=12)
staten_island_ts

#Split data into train and test set
#BRONX
train_bronx = window(bronx_ts,start=c(2006,01),end=c(2016,12))
test_bronx = window(bronx_ts, start=c(2017,01),end=c(2020,12))
nrow(train_bronx)
nrow(test_bronx)
bronx_ts

#BROOKLYN
train_brooklyn = window(brooklyn_ts,start=c(2006,01),end=c(2016,12))
test_brooklyn = window(brooklyn_ts, start=c(2017,01),end=c(2020,12))
nrow(train_brooklyn)
nrow(test_brooklyn)
brooklyn_ts

#MANHATTAN 
train_manhattan = window(manhattan_ts,start=c(2006,01),end=c(2016,12))
test_manhattan = window(manhattan_ts, start=c(2017,01),end=c(2020,12))
nrow(train_manhattan)
nrow(test_manhattan)
manhattan_ts

#QUEENS
train_queens = window(queens_ts,start=c(2006,01),end=c(2016,12))
test_queens = window(queens_ts, start=c(2017,01),end=c(2020,12))
nrow(train_queens)
nrow(test_queens)
queens_ts

#STATEN ISLAND
train_staten_island = window(staten_island_ts,start=c(2006,01),end=c(2016,12))
test_staten_island = window(staten_island_ts, start=c(2017,01),end=c(2020,12))
nrow(train_staten_island)
nrow(test_staten_island)
staten_island_ts

#Time series analysis for each county
#Bronx
#Seasonal Naive Method
seasonal_naive_model_bronx = snaive(train_bronx,h=48)
seasonal_naive_model_bronx
accuracy(seasonal_naive_model_bronx, bronx_ts)
#train RMSE 18.11583
#test RMSE 23.50842

#Naive Method
naive_model_bronx = naive(train_bronx,h=48)
naive_model_bronx
accuracy(naive_model_bronx, bronx_ts)
#train RMSE 14.99491
#test RMSE 12.40716

#Drift Method
drift_model_bronx = rwf(train_bronx,h=48,drift = T)
drift_model_bronx$mean
accuracy(drift_model_bronx, bronx_ts)
#train RMSE 14.99475
#test RMSE 12.71213

#Simple Exponential Smoothing
ses_model_bronx= ses(train_bronx,h = 48)
ses_model_bronx$mean
accuracy(ses_model_bronx, bronx_ts)
#train RMSE 13.73497
#test RMSE 12.73147

#Holt’s Method
holt_model_bronx = holt(train_bronx,h=48)
holt_model_bronx$mean
accuracy(holt_model_bronx, bronx_ts)
#train RMSE 13.73633
#test RMSE 12.92554

#Holt’s Method with Damping
holt_damped_model_bronx = holt(train_bronx,h=48,damped = T)
holt_damped_model_bronx$mean
accuracy(holt_damped_model_bronx, bronx_ts)
#train RMSE 13.73703
#test RMSE 12.73706

#Auto-ETS Model
ets_auto_bronx = ets(train_bronx)
summary(ets_auto_bronx)
#AICc 1342.383
ets_auto_bronx_forecast = forecast(ets_auto_bronx, h = 48)
accuracy(ets_auto_bronx_forecast, bronx_ts)
#train RMSE 13.73498
#test RMSE 12.73047
ets_auto_bronx_forecast

#Comparing Models
rbind(seasonal_naive_model = accuracy(f = seasonal_naive_model_bronx, x = bronx_ts)[2,],
      naive_model = accuracy(f = naive_model_bronx, x = bronx_ts)[2,],
      drift_model = accuracy(f = drift_model_bronx, x= bronx_ts)[2,],
      ses_model = accuracy(f = ses_model_bronx, x = bronx_ts)[2,],
      holt_model = accuracy(f = holt_model_bronx, x = bronx_ts)[2,],
      holt_damped_model = accuracy(f = holt_damped_model_bronx, x = bronx_ts)[2,],
      auto_ets_model = accuracy(f = ets_auto_bronx_forecast, x = bronx_ts)[2,]
)
#naive_model is the best #12.40716

#Brooklyn
#Seasonal Naive Method
seasonal_naive_model_brooklyn = snaive(train_brooklyn,h=48)
seasonal_naive_model_brooklyn
accuracy(seasonal_naive_model_brooklyn, brooklyn_ts)
#train RMSE 32.09595
#test RMSE 33.40472

#Naive Method
naive_model_brooklyn = naive(train_brooklyn,h=48)
naive_model_brooklyn
accuracy(naive_model_brooklyn, brooklyn_ts)
#train RMSE 23.57204
#test RMSE 20.93243

#Drift Method
drift_model_brooklyn = rwf(train_brooklyn,h=48,drift = T)
drift_model_brooklyn$mean
accuracy(drift_model_brooklyn, brooklyn_ts)
#train RMSE 23.57183
#test RMSE 22.08573

#Simple Exponential Smoothing
ses_model_brooklyn = ses(train_brooklyn,h = 48)
ses_model_brooklyn$mean
accuracy(ses_model_brooklyn, brooklyn_ts)
#train RMSE 22.04553
#test RMSE 17.30629

#Holt’s Method
holt_model_brooklyn = holt(train_brooklyn,h=48)
holt_model_brooklyn$mean
accuracy(holt_model_brooklyn, brooklyn_ts)
#train RMSE 22.04751
#test RMSE 17.29176

#Holt’s Method with Damping
holt_damped_model_brooklyn = holt(train_brooklyn,h=48,damped = T)
holt_damped_model_brooklyn
accuracy(holt_damped_model_brooklyn, brooklyn_ts)
#train RMSE 22.03798
#test RMSE 17.30471

#Auto-ETS Model
ets_auto_brooklyn = ets(train_brooklyn)
summary(ets_auto_brooklyn)
#AICc 1457.243
ets_auto_brooklyn_forecast = forecast(ets_auto_brooklyn, h = 48)
accuracy(ets_auto_brooklyn_forecast, brooklyn_ts)
#train RMSE 23.77233
#test RMSE 63.23772
ets_auto_brooklyn_forecast

#Comparing Models
rbind(seasonal_naive_model = accuracy(f = seasonal_naive_model_brooklyn, x = brooklyn_ts)[2,],
      naive_model = accuracy(f = naive_model_brooklyn, x = brooklyn_ts)[2,],
      drift_model = accuracy(f = drift_model_brooklyn, x= brooklyn_ts)[2,],
      ses_model = accuracy(f = ses_model_brooklyn, x = brooklyn_ts)[2,],
      holt_model = accuracy(f = holt_model_brooklyn, x = brooklyn_ts)[2,],
      holt_damped_model = accuracy(f = holt_damped_model_brooklyn, x = brooklyn_ts)[2,],
      auto_ets_model = accuracy(f = ets_auto_brooklyn_forecast, x = brooklyn_ts)[2,]
)
#holt_model is the best #17.29176

#Manhattan
#Seasonal Naive Method
seasonal_naive_model_manhattan = snaive(train_manhattan,h=48)
seasonal_naive_model_manhattan
accuracy(seasonal_naive_model_manhattan, manhattan_ts)
#train RMSE 10.26686
#test RMSE 10.97345

#Naive Method
naive_model_manhattan = naive(train_manhattan,h=48)
naive_model_manhattan
accuracy(naive_model_manhattan, manhattan_ts)
#train RMSE 8.936160
#test RMSE 9.405229

#Drift Method
drift_model_manhattan = rwf(train_manhattan,h=48,drift = T)
drift_model_manhattan$mean
accuracy(drift_model_manhattan, manhattan_ts)
#train RMSE 8.936001
#test RMSE 10.345915

#Simple Exponential Smoothing
ses_model_manhattan = ses(train_manhattan,h = 48)
ses_model_manhattan$mean
accuracy(ses_model_manhattan, manhattan_ts)
#train RMSE 7.558886
#test RMSE 6.373660

#Holt’s Method
holt_model_manhattan = holt(train_manhattan,h=48)
holt_model_manhattan$mean
accuracy(holt_model_manhattan, manhattan_ts)
#train RMSE 7.567393
#test RMSE 6.443862

#Holt’s Method with Damping
holt_damped_model_manhattan = holt(train_manhattan,h=48,damped = T)
holt_damped_model_manhattan$mean
accuracy(holt_damped_model_manhattan, manhattan_ts)
#train RMSE 7.548805
#test RMSE 6.405007

#Auto-ETS Model
ets_auto_manhattan = ets(train_manhattan)
summary(ets_auto_manhattan)
#AICc 1182.767
ets_auto_manhattan_forecast = forecast(ets_auto_manhattan, h = 48)
accuracy(ets_auto_manhattan_forecast, manhattan_ts)
#train RMSE 7.591666
#test RMSE 6.649744
ets_auto_manhattan_forecast

#Comparing Models
rbind(seasonal_naive_model = accuracy(f = seasonal_naive_model_manhattan, x = manhattan_ts)[2,],
      naive_model = accuracy(f = naive_model_manhattan, x = manhattan_ts)[2,],
      drift_model = accuracy(f = drift_model_manhattan, x= manhattan_ts)[2,],
      ses_model = accuracy(f = ses_model_manhattan, x = manhattan_ts)[2,],
      holt_model = accuracy(f = holt_model_manhattan, x = manhattan_ts)[2,],
      holt_damped_model = accuracy(f = holt_damped_model_manhattan, x = manhattan_ts)[2,],
      auto_ets_model = accuracy(f = ets_auto_manhattan_forecast, x = manhattan_ts)[2,]
)
#ses_model is the best #6.373660

#Queens
#Seasonal Naive Method
seasonal_naive_model_queens = snaive(train_queens,h=48)
seasonal_naive_model_queens
accuracy(seasonal_naive_model_queens, queens_ts)
#train RMSE 12.20963
#test RMSE 11.10649

#Naive Method
naive_model_queens = naive(train_queens,h=48)
naive_model_queens
accuracy(naive_model_queens, queens_ts)
#train RMSE 10.64916
#test RMSE 10.74031

#Drift Method
drift_model_queens = rwf(train_queens,h=48,drift = T)
drift_model_queens$mean
accuracy(drift_model_queens, queens_ts)
#train RMSE 10.64906
#test RMSE 11.35802


#Simple Exponential Smoothing
ses_model_queens = ses(train_queens,h = 48)
ses_model_queens$mean
accuracy(ses_model_queens, queens_ts)
#train RMSE 8.662877
#test RMSE 9.000621

#Holt’s Method
holt_model_queens = holt(train_queens,h=48)
holt_model_queens$mean
accuracy(holt_model_queens, queens_ts)
#train RMSE 8.517354
#test RMSE 11.327417

#Holt’s Method with Damping
holt_damped_model_queens = holt(train_queens,h=48,damped = T)
holt_damped_model_queens$mean
accuracy(holt_damped_model_queens, queens_ts)
#train RMSE 8.633612
#test RMSE 9.317283

#Auto-ETS Model
ets_auto_queens = ets(train_queens)
summary(ets_auto_queens)
#AICc 1217.717
ets_auto_queens_forecast = forecast(ets_auto_queens, h = 48)
accuracy(ets_auto_queens_forecast, queens_ts)
#train RMSE 8.486445
#test RMSE 12.316493
ets_auto_queens_forecast

#Comparing Models
rbind(seasonal_naive_model = accuracy(f = seasonal_naive_model_queens, x = queens_ts)[2,],
      naive_model = accuracy(f = naive_model_queens, x = queens_ts)[2,],
      drift_model = accuracy(f = drift_model_queens, x= queens_ts)[2,],
      ses_model = accuracy(f = ses_model_queens, x = queens_ts)[2,],
      holt_model = accuracy(f = holt_model_queens, x = queens_ts)[2,],
      holt_damped_model = accuracy(f = holt_damped_model_queens, x = queens_ts)[2,],
      auto_ets_model = accuracy(f = ets_auto_queens_forecast, x = queens_ts)[2,]
)
#ses_model is the best #9.000621


#Staten Island
#Seasonal Naive Method
seasonal_naive_model_staten_island = snaive(train_staten_island,h=48)
seasonal_naive_model_staten_island
accuracy(seasonal_naive_model_staten_island, staten_island_ts)
#train RMSE 3.876209
#test RMSE 3.702477

#Naive Method
naive_model_staten_island = naive(train_staten_island,h=48)
naive_model_staten_island
accuracy(naive_model_staten_island, staten_island_ts)
#train RMSE 3.648695
#test RMSE 3.329164

#Drift Method
drift_model_staten_island = rwf(train_staten_island,h=48,drift = T)
drift_model_staten_island$mean
accuracy(drift_model_staten_island, staten_island_ts)
#train RMSE 3.648567
#test RMSE 3.877091

#Simple Exponential Smoothing
ses_model_staten_island = ses(train_staten_island,h = 48)
ses_model_staten_island$mean
accuracy(ses_model_staten_island, staten_island_ts)
#train RMSE 2.692277
#test RMSE 2.598390

#Holt’s Method
holt_model_staten_island = holt(train_staten_island,h=48)
holt_model_staten_island$mean
accuracy(holt_model_staten_island, staten_island_ts)
#train RMSE 2.714090
#test RMSE 2.602276

#Holt’s Method with Damping
holt_damped_model_staten_island = holt(train_staten_island,h=48,damped = T)
holt_damped_model_staten_island$mean
accuracy(holt_damped_model_staten_island, staten_island_ts)
#train RMSE 2.689171
#test RMSE 2.597269

#Auto-ETS Model
ets_auto_staten_island = ets(train_staten_island)
summary(ets_auto_staten_island)
#AICc 912.1796
ets_auto_staten_island_forecast = forecast(ets_auto_staten_island, h = 48)
accuracy(ets_auto_staten_island_forecast, staten_island_ts)
#train RMSE 2.692277
#test RMSE 2.598383
ets_auto_queens_forecast

#Comparing Models
rbind(seasonal_naive_model = accuracy(f = seasonal_naive_model_staten_island, x = staten_island_ts)[2,],
      naive_model = accuracy(f = naive_model_staten_island, x = staten_island_ts)[2,],
      drift_model = accuracy(f = drift_model_staten_island, x= staten_island_ts)[2,],
      ses_model = accuracy(f = ses_model_staten_island, x = staten_island_ts)[2,],
      holt_model = accuracy(f = holt_model_staten_island, x = staten_island_ts)[2,],
      holt_damped_model = accuracy(f = holt_damped_model_staten_island, x = staten_island_ts)[2,],
      auto_ets_model = accuracy(f = ets_auto_staten_island_forecast, x = staten_island_ts)[2,]
)
#holt_damped_model is the best #2.597269