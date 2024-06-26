library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)

file_paths <- 
    list.files(path = "data/", pattern = "Load profile", full.names = T)
filenms <-
  list.files(path = "data/", pattern = "Load profile", full.names = F)

names(file_paths) <- c("MEA", "R1", "R2", "R3", "R4") 

# substation_profile2019<- 
  file_paths %>% 
  map(read_excel) %>% 
    pivot_longer(col = -TIME_LOCAL, names_to = "egatsub", values_to = "kw")
  set_names(., nm=filenms) 
    
profileFigure <- list()


# names(substation_profile2019) <- filenms


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
# Get abbreviation sub station list 
egatsub_abv <- 
     unique(a[c("egatsub","region")]) %>% 
     filter(egatsub != "total") %>%
     separate(col = egatsub, into = c("sub","voltage"), sep = "/", remove = FALSE)

# Export to excel file
write.csv(b,file = "egatsub.csv") 

 
# Facet plots #####

profile <-
a %>% 
  mutate(month = month(TIME_LOCAL),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         year = year(TIME_LOCAL)) %>%
  mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                          if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                  if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>%
  mutate(dummy = "dummy") %>% 
  filter(region == "MEA",
         egatsub != "total",
         month %in% c(month.abb[1:12]),
         year == 2019) %>% 
  ggplot()+
  geom_line(aes(x = TIME_LOCAL, 
                y = kw/1000,
                group = dummy, 
                color = season), 
            show.legend = T)+
  facet_wrap(~egatsub,
             ncol = 3)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust =1))+
  labs(x = NULL,
       y = "Substation supply (MW)")

ggsave("figures/MEASub2019.png", width = 10, height = 9)
profileFigure <- c(profileFigure, list("MEAsub2019" = profile))


# Regional plots for 2019 raw data #####



function(region , sub ){
  
}


profile <-
  a %>% 
  mutate(month = month(TIME_LOCAL),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         year = year(TIME_LOCAL)) %>%
  mutate(season = if_else(month %in% c("Mar","Apr","May"), "Summer",
                          if_else(month %in% c("Jun","Jul","Aug","Sep","Oct"), "Rainy",
                                  if_else (month %in% c("Nov","Dec","Jan","Feb"),"Winter", "Others")))) %>%
  mutate(dummy = "dummy") %>% 
  filter(region == "R4",
         egatsub == "SLB/115",
         month %in% c(month.abb[1:12]),
         year == 2019) %>% 
  ggplot()+
  geom_line(aes(x = TIME_LOCAL, 
                y = kw/1000,
                group = dummy, 
                color = season),
            linewidth = 0.2,
            show.legend = F)+
  geom_hline(yintercept=0)+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(breaks = seq(-30,30,10),
                     limits = c(-30,30))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust =1))+
  labs(x = NULL,
       y = "Substation supply (MW)",
       subtitle = "2562")

ggsave("figures/R4_SLB115_2019.png", width = 7, height = 3, units = "in", dpi = 300)
profileFigure <- c(profileFigure, list("SLB/115_R4_2019" = profile))

write.xlsx(a,"data/profileSubs.xlsx")
