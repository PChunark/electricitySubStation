library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)

file_path_2022subProfile <-
  list.files(path = "data/subSummaryInfo",
             pattern = "2022_",
             full.names = T)

file_path_2030subProfile <-
  list.files(path = "data/subSummaryInfo",
             pattern = "2030_",
             full.names = T)

names(file_path_2022subProfile) <- c("MEA", "R1", "R2", "R3", "R4")
names(file_path_2030subProfile) <- c("MEA", "R1", "R2", "R3", "R4")

data2022 <- file_path_2022subProfile %>% 
  map(function(path){
    read_csv(path, show_col_types = FALSE) %>% 
      pivot_longer(col = -"...1", names_to = "egatsub", values_to = "mw") %>% 
      select(time= "...1", egatsub, mw) %>% 
      mutate(time2 = as.POSIXct(time, format = "%m-%d %H:%M:%S")) %>% 
      mutate(month = month(time2),
             month = month.abb[month],
             month = factor(month, levels = month.abb)) %>%
      mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                              if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                      if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>% 
      mutate(dummy = "dummy")
  }
  
  )

data2030 <- file_path_2030subProfile %>% 
  map(function(path){
    read_csv(path, show_col_types = FALSE) %>% 
      pivot_longer(col = -"...1", names_to = "egatsub", values_to = "mw") %>% 
      select(time= "...1", egatsub, mw) %>% 
      mutate(time2 = as.POSIXct(time, format = "%m-%d %H:%M:%S")) %>% 
      mutate(month = month(time2),
             month = month.abb[month],
             month = factor(month, levels = month.abb)) %>%
      mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                              if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                      if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>% 
      mutate(dummy = "dummy")
  }
    
  )

R4SubProfile2022 <- as_tibble(data2022[["R4"]])
R4SubProfile2030 <- as_tibble(data2030[["R4"]])

plot<-
R4SubProfile2022 %>% 
  filter(egatsub == "SLB/115") %>% 
  ggplot()+
  geom_line(aes(x = time2, y = mw, group = dummy,color =season))+
  geom_hline(yintercept=0)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust =1))+
  labs(x = NULL,
       y = "Substation supply (MW)",
       subtitle = "2562")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(breaks = seq(-30,30,10),
                     limits = c(-30,30))


profile<-
R4SubProfile2030 %>% 
  filter(egatsub == "SLB/115") %>% 
  ggplot()+
  geom_line(aes(x = time2, y = mw, group = dummy,color =season),
            linewidth = 0.2)+
  geom_hline(yintercept=0)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust =1))+
  labs(x = NULL,
       y = "Substation supply (MW)",
       subtitle = "2573")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(breaks = seq(-30,30,10),
                     limits = c(-30,30))

ggsave("figures/R4_SLB115_2030.png", width = 7, height = 3.5, units = "in", dpi = 300)
profileFigure <- c(profileFigure, list("SLB/115_R4_2030" = profile))
