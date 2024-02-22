library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)

file_paths <- 
    list.files(path = "data/", pattern = "Load profile", full.names = T)
filenms <-
  list.files(path = "data/", pattern = "Load profile", full.names = F)
  

# substation_profile2019<- 
  file_paths %>% 
  map(read_excel) %>% 
    pivot_longer(col = -TIME_LOCAL, names_to = "egatsub", values_to = "kw")
  set_names(., nm=filenms) %>% 
    



names(substation_profile2019) <- filenms


a<- file_paths[1] %>% 
  map(function(path){
      read_excel(path) %>% 
      pivot_longer(col = -TIME_LOCAL, names_to = "egatsub", values_to = "kw") %>%  
      arrange(egatsub) %>% 
      separate(col = egatsub, into = c("sub","voltage"), sep = "/", remove = FALSE)
                    }
      ) 
# %>% 
  # set_names(., nm=filenms) 
write.xlsx(a[[1]],"data/MEA.xlsx")
