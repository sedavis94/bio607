#----------
#
#Figure out how to clean data
#
#
#---------

#needs to have all columns filled and
# no extra columns
#get rid of 0 and too large of numbers 

#make sure it has all columns 

FDOM <- read_csv("../data/181118.csv")

head(FDOM)

library(tidyverse)
library(dplyr)

FDOM_c1 <- FDOM %>%
  drop_na(Date, Time, DateTime,'N/U', Count, y) %>%#drops rows with missing values
  filter(is.na(bad1)) %>%#drops rows with extra values 
  dplyr::select(Date, Time, Count) # only uses date and count

FDOM_c1

#get rid of low numbers


FDOM_c2 <- FDOM_c1 %>%
  dplyr::filter(Count > 100) %>%
  dplyr::filter(Count < 1000)

FDOM_c2
