library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)
library(plotly)
library(shiny)
library(shinythemes)



# Read CSV file and plot #####
csv_file_path <- function(path, pattern, region,substation){
  file_path <-
    list.files(path = path,
               pattern = pattern,
               full.names = T)
  
  names(file_path) <- c("MEA", "R1", "R2", "R3", "R4")
  
  data1 <- 
    file_path %>% 
    map(function(path){
      read_csv(path, show_col_types = FALSE) %>% 
        pivot_longer(col = -"TIME_LOCAL", names_to = "egatsub", values_to = "mw") %>% 
        mutate(month = month(TIME_LOCAL),
               month = month.abb[month],
               month = factor(month, levels = month.abb)) %>%
        mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                                if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                        if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>% 
        mutate(dummy = "dummy")
      
    }
    )
  
  data2 <- as_tibble(data1[[region]])
  
  # plot<-
    data2 %>% 
    filter(egatsub == substation) %>% 
    ggplot()+
    geom_line(aes(x = TIME_LOCAL, 
                  y = mw, 
                  group = dummy,
                  color =season),
              show.legend = F,
              linewidth = 0.2)+
    geom_hline(yintercept=0)+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust =1))+
    labs(x = NULL,
         y = "Substation supply (MW)",
         subtitle = "2562")+
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
}

# Read Excel file and plot #####
excel_file_path <- function(path, pattern, region, substation){
  file_path <-
    list.files(path = path,
               pattern = pattern,
               full.names = T)
  
  names(file_path) <- c("MEA", "R1", "R2", "R3", "R4")
  
  data1 <- 
    file_path %>% 
    map(function(path){
      read_excel(path) %>% 
        pivot_longer(col = -"TIME_LOCAL", names_to = "egatsub", values_to = "mw") %>% 
        mutate(month = month(TIME_LOCAL),
               month = month.abb[month],
               month = factor(month, levels = month.abb)) %>%
        mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                                if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                        if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>% 
        mutate(dummy = "dummy")
      
    }
    )
  data2 <- as_tibble(data1[[region]])
  
  # plot<-
  data2 %>% 
    filter(egatsub == substation) %>% 
    ggplot()+
    geom_line(aes(x = TIME_LOCAL, 
                  y = mw, 
                  group = dummy,
                  color =season),
              show.legend = F,
              linewidth = 0.2)+
    geom_hline(yintercept=0)+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust =1))+
    labs(x = NULL,
         y = "Substation supply (MW)",
         subtitle = "2562")+
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
}


# unfiltered profile #####
csv_file_path("data/subSummaryInfo","2019_", "R4", "CM3/115")
# Filtered profile #####
excel_file_path("data/subSummaryInfo","df_", "R4", "CM3/115")

