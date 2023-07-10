
library(EIAapi)
library(tidyverse)

api_key <- Sys.getenv("EIA_KEY")

#

df1 <- eia_get(
api_key = api_key,
api_path = "electricity/rto/fuel-type-data/data/",
data = "value"
)


facets_list <- list(respondent = "NE")
df1 <- eia_get(
  api_key = api_key,
  api_path = "electricity/rto/fuel-type-data/data/",
  data = "value",
  facets = facets_list
)



# daily electricty generation by source
#https://api.eia.gov/v2/electricity/rto/daily-fuel-type-data/data/?frequency=daily&data[0]=value&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000
facets <- list(respondent = "NE")
df1 <- eia_get(
  api_key = api_key,
  api_path = "electricity/state-electricity-profiles/source-disposition/data/",
  data = "total-net-generation"
)



glimpse(df1)
str(df1)


df1$time <- as.POSIXct(paste(substr(df1$period, start = 1, stop = 10)," ",
                              substr(df1$period, start = 12, stop = 13), ":00:00", sep = ""))
df1$time <- as.POSIXct(paste(substr(df1$period, start = 1, stop = 10)," ",                              substr(df1$period, start = 12, stop = 13), ":00:00",sep = ""))
glimpse(df1)
df1 %>% ggplot(aes(time,value))+geom_line()
df1 %>% filter(respondent=="SW") %>% ggplot(aes(time,value))+geom_line()
df1 %>% filter(respondent=="SW") %>% ggplot(aes(time,value))+geom_point()
df1 %>% filter(respondent=="SW", fueltype=='NG') %>% ggplot(aes(time,value))+geom_line()
df1 %>% filter(respondent=="SW") %>% ggplot(aes(time,value))+geom_line(group='fueltype')
df1 %>% filter(respondent=="SW") %>% ggplot(aes(time,value,group=fueltype))+geom_line(group='fueltype')
df1 %>% filter(respondent=="SW")


# Look at just lower 48
df48 <- df1 %>% filter(respondent=='US48')
df48$fueltype<-as.factor(df48$fueltype)
df48 %>% ggplot(aes(time,value,group=fueltype))+geom_line(aes(colour=fueltype))

# sum by fuel type
df48 %>% group_by(fueltype) %>% summarise(energy=sum(value)) %>% View()

df48 %>% group_by(`type-name`) %>% 
  summarise(energy=sum(value)) %>% 
  ggplot(aes(`type-name`,energy))+
  geom_col()+
  coord_flip()
