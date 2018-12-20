#----------
#
#Figure out how agrigate time series (not eqaully spaced or sampled)
#
#
#---------

library(dplyr)
library(tidyverse)
library(lubridate)
library(tibbletime)
library(data.table)


source("Data cleaning.R") #get data from data cleaning
summary(FDOM_c2$DateTime)


 FDOM_c2_f1 <-FDOM_c2 %>%
  mutate(DateTime = parse_date_time(paste(Date, Time, sep = " "), order="mdyHMS")) # get date time in proper variable type
 
 FDOM_c2_f1

FDOM_c2_f2 <- as_tbl_time(FDOM_c2_f1, index = DateTime)

FDOM_c2_f3 <- FDOM_c2_f2 %>%
  mutate(time_since_last = (Time - lag(Time)))%>%
  select(DateTime, Count, time_since_last) %>%
  as.data.frame()#getting lag time


tail(FDOM_c2_f3, 300)


#group if time_since last is less than 5 s if greater start new group
FDOM_c2_f3_g1 <- FDOM_c2_f3 %>% 
  filter(time_since_last > "60 secs") %>%
  as.data.frame()

FDOM_c2_f3_g1#only starting times 

#Use loops to get mean of each time group

A <- FDOM_c2_f3_g1$DateTime
length(A)

i = 0

result <- sapply(1:(length(A) - 1) %>%
  map(function(i) {
    p <- FDOM_c2_f3$Count[which((FDOM_c2_f3$DateTime >= A[i]) & (FDOM_c2_f3$DateTime < A[i + 1]))]
    list(A[i], mean(p))
  }), c) %>% t %>% as.matrix()

names(result) <- c("Date", "Mean")
result

as.POSIXct(result$Date)

as_datetime(result$Date)
summary(result)
#FDOM_c2_f3_edit <- setDT(FDOM_c2_f3)[, `:=`(start = DateTime, end = c(DateTime[2:(.N - 1)] - 1, DateTime[.N], NA))][-.N]
#FDOM_c2_f3_edit

#FDOM_c2_f3_g1_edit <- setDT(FDOM_c2_f3_g1)[, `:=`(start = DateTime, end = DateTime)]

#setkey(FDOM_c2_f3_g1_edit, start, end)
#overlaps <- foverlaps(FDOM_c2_f3_edit, FDOM_c2_f3_g1_edit, type = "any", which = TRUE)


#FDOM_c2_f3_edit[overlaps$yid, Period := overlaps$xid]
#na.omit(FDOM_c2_f3_edit[, list(Mean_Flow = mean(Flow)), by = Period])

I chose to handle this by adding a column that gives delta time between samples. When this delta is big (~600 sec) that means a new time bucket must be created. I first created a new dataframe from the original that only has the starting time of each bucket and then used this this new dataframe to parse the original dataframe and get an average count for each time. 

```{R}
FDOM181118_clean_lag <-FDOM181118_clean %>%
  mutate(DateTime = parse_date_time(paste(Date, Time, sep = " "), order="mdyHMS")) %>%# get date time in proper variable type 
  as_tbl_time(FDOM_c2_f1, index = DateTime) %>%
  mutate(time_since_last = (Time - lag(Time)))%>%
  select(DateTime, Count, time_since_last) %>%
  as.data.frame() #getting lag time
#filter(is.na(time_since_last)) #see if an NA's only one at the start 

FDOM181118_clean_lag[1,3] = "600" #fix first time bucket

tail(FDOM181118_clean_lag)





#group if time_since last is less than 60 s if greater start new group
FDOM181118_clean_start <- FDOM181118_clean_lag %>% 
  filter(time_since_last > "60 secs") %>%
  drop_na() %>%
  as.data.frame()

head(FDOM181118_clean_start)#only starting times 

#Use loop to get mean of each time group

A <- FDOM181118_clean_start$DateTime
length(A)
i = 0

result <- sapply(0:(length(A) - 1) %>%
                   map(function(i) {
                     p <- FDOM181118_clean_lag$Count[which((FDOM181118_clean_lag$DateTime >= A[i]) & (FDOM181118_clean_lag$DateTime < A[i + 1]))] # break up data into time buckets
                     list(A[i], mean(p)) #get mean
                   }), c) %>% t %>% cbind.data.frame()

names(result) <- c("Date", "Mean")

as.POSIXct(result$Date)