library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)
library(qpcR)


file_path <- 
  list.files("data/subSummaryInfo",
            pattern = "*_Base_R1",
            full.names = T)


data1 <-
 
 file_path %>%
           map(function(path){
             read_csv(path,
                      id = "file",
                      col_select = c("path" = "file",
                                     "timestamp" = "...1", 
                                     everything()
                                     )
                      ) %>% 
               separate(col = "path", into = c("root","filename"), sep = "data/subSummaryInfo/") %>% 
               separate(col = "filename", into = c("year","case"), sep = "_") %>%
               separate(col = "timestamp", into = c("date","time"),sep = " ") %>%
               dplyr::select(!c("root", "case")) %>%
               unite(col = "ymd", year:date,sep = "-") %>%
               unite(col = "timestamp", ymd:time, sep = " ") %>%
               mutate(timestamp = as.POSIXct(timestamp,
                                              format="%Y-%m-%d %H:%M:%S", tz= "UTC")) %>%
               janitor::clean_names("screaming_snake") %>%   #change header name from "/" to "_"
               pivot_longer(-"TIMESTAMP", names_to = "Substation", values_to = "MW") #%>%
               # mutate(YEAR = year(TIMESTAMP)) %>%
               # pivot_wider(names_from = "YEAR", values_from = "MW")
           }
           )  
  
data2 <-
  data1 %>% 
  reduce(full_join) 


data4 <- list()


for (sub in unique(data2$Substation)){
  
  data4[[sub]]<-
    data2 %>% 
    filter(Substation == sub) %>% 
    mutate(YEAR = year(TIMESTAMP)) 
 }
 

data6 <- data4[1:3] %>% reduce(full_join)

data7 <- list()

for (y in unique(data6$YEAR)){

  data7[[y]] <-
  data6 %>% 
    filter(YEAR == y)
  for(sub in unique(data6$Substation)){
    filename <- paste0("processData/R1/",sub,"_",y, ".csv")
    write.csv(data7[[y]],
              file = filename,
              row.names = FALSE)
  }
  
}

# for (i in unique(data6$YEAR)){
#   assign(paste0("forecast_",i),as.data.frame(data7[[i]]))
# }
# 
# for (i in unique(data2$Substation)){
#   assign(paste0("forecast_",i),as.data.frame(data4[[i]]))
# }

# data8 <- data7[1:4]%>% reduce(qpcR:::cbind.na)
# write.csv(data8,
#           file = "processData/R1sub.csv",
#           row.names = FALSE)
# 





filename <- paste0("processData", sub, ".csv")
  write.csv(data4[[sub]],
            file = filename,
            row.names = FALSE) #remove row index
  print(paste(sub, "is sucessfully writen."))

