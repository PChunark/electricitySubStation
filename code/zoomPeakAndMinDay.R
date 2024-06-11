library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)


# find peak MW to get a date
print(R4SubProfile2019 %>% 
  filter(egatsub == "CM2/115")%>% 
  slice_max(mw, n=30), n=Inf)  

print(R4SubProfile2030 %>% 
  filter(egatsub == "CM2/115") %>% 
    slice_max(mw, n=30), n = Inf)


# find min MW to get a date
print(R4SubProfile2019 %>% 
        filter(egatsub == "CM2/115")%>% 
        filter(mw >3.28) %>% 
        slice_min(mw, n=50), n=Inf)

print(R4SubProfile2030 %>% 
        filter(egatsub == "CM2/115")%>% 
        slice_min(mw, n=30), n=Inf)
 

a <- R4SubProfile2030 %>% 
  distinct(egatsub) %>% 
  pull(egatsub)

a %>% map2_chr(paste("adf", a))
accumulate(a,)
   
  a <-
    R4SubProfile2019 %>% 
    filter(egatsub == "CTG/115") %>% 
    mutate(date = day(TIME_LOCAL)) %>%
    filter(egatsub == "CTG/115",
           date == 21,
           month == "Dec")
  
  b <-
    R4SubProfile2030 %>% 
    mutate(date = day(time2)) %>% 
    filter(egatsub == "CTG/115",
           date == 11,
           month == "Apr") %>% 
    select(TIME_LOCAL = time2, egatsub,mw, month, season, dummy,date)

c <-full_join(a,b)  
