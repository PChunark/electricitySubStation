library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)

file_path_2019subProfile3U <-
  list.files(path = "data/subSummaryInfo",
             pattern = "2019_",
             full.names = T)

names(file_path_2019subProfile3U) <- c("MEA", "R1", "R2", "R3", "R4")


data2019 <- 
  file_path_2019subProfile3U %>% 
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

R4SubProfile2019 <- as_tibble(data2019[["R4"]])

plot<-
  R4SubProfile2019 %>% 
  filter(egatsub == "SLB/115") %>% 
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
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(breaks = seq(-30,30,10),
                     limits = c(-30,30))

ggsave("figures/R4_SLB115_2019.png", width = 7, height = 3, units = "in", dpi = 300)
profileFigure <- c(profileFigure, list("SLB/115_R4_2019" = profile))
