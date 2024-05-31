library(tidyverse)
library(fs)
library(readxl)
library(openxlsx)
library(gridExtra)
library(grid)
library(extrafont)
library(extrafontdb)

file_path_subinfo <-
  list.files(path = "data/subSummaryInfo",
             pattern = "Total_",
             full.names = T)


plot1<-
  file_path_subinfo %>% 
  map(function(path){
                        read_excel(path) %>% 
                        select(!"...1") %>%
                        mutate(year2 = Year+543) %>% 
                        filter(Substation != "Dummy", 
                               Year %in% c("2019")) %>% 
                        separate(col = Substation, 
                                 into = c("sub","voltage"), 
                                 sep = "/", 
                                 remove = F) %>% 
  group_by(Region) %>%
  top_n(n=-3, wt=Min) %>% 
      ggplot()+
      geom_point(aes(x = Substation,
                     y = Min,
                     color = as.factor(Year)
      ),
      size = 1.5,
      ) +
      theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))+
      facet_wrap(year2~Region)
                       }
  )


plot2<-
  file_path_subinfo %>% 
  map(function(path){
    read_excel(path) %>% 
      select(!"...1") %>%
      mutate(year2 = Year+543) %>%
      filter(Substation != "Dummy", 
             Year %in% c("2030")) %>% 
      separate(col = Substation, 
               into = c("sub","voltage"), 
               sep = "/", 
               remove = F) %>% 
      group_by(Region) %>%
      top_n(n=-3, wt=Min) %>% 
      ggplot()+
      geom_point(aes(x = Substation,
                     y = Min,
                     color = as.factor(Year)
      ),
      size = 1.5,
      ) +
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 90))+
      facet_wrap(year2~Region)
  }
  )


x.grob <- grid::textGrob("สถานีไฟฟ้าแรงสูง",
                         gp=gpar(fontfamily = "TH SarabunPSK", fontsize=16))
y.grob <- grid::textGrob("ปริมาณไฟฟ้าไหลย้อนกลับ (MW)",
                         gp=gpar(fontfamily = "TH SarabunPSK", fontsize=16), rot=90)


plot <- ggpubr::ggarrange(plotlist = c(plot1,plot2), nrow = 2, ncol = 5)
plot <- gridExtra::grid.arrange(arrangeGrob(plot, bottom = x.grob,left = y.grob))

ggsave("figures/top3MinEly.png",plot, width = 8, height = 6, units = "in")


