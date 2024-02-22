library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)

file_paths <- 
    list.files(path = "data/", pattern = "Load profile", full.names = T)
filenms <-
  list.files(path = "data/", pattern = "Load profile", full.names = F)

names(file_paths) <- c("MEA", "Central R1", "NorthE R2", "South R3", "North R4") 

# substation_profile2019<- 
  file_paths %>% 
  map(read_excel) %>% 
    pivot_longer(col = -TIME_LOCAL, names_to = "egatsub", values_to = "kw")
  set_names(., nm=filenms) %>% 
    



names(substation_profile2019) <- filenms


a<- file_paths %>% 
  map_df(function(path){
      read_excel(path) %>% 
      mutate(total = rowSums(across(where(is.numeric)))) %>% 
      pivot_longer(col = -TIME_LOCAL, names_to = "egatsub", values_to = "kw") %>%  
      arrange(egatsub) %>% 
      separate(col = egatsub, into = c("sub","voltage"), sep = "/", remove = FALSE) 
                    },
      .id = "region"
      ) 

b <- a %>% unnest()
     map_df(.x = file_paths, .id = "region")







file_paths %>% 
  map(read_excel)

write.xlsx(a,"data/MEA.xlsx")
