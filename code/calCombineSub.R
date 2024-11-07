source("code/get3USubProfile2019.R")

dataplot<-
  R1SubProfile2019_filtered %>% 
  filter(egatsub %in% c("CBD/22", "CBD/115")) %>% 
  # select(TIME_LOCAL:mw) %>% 
  pivot_wider(names_from = egatsub, values_from = mw) %>% 
  mutate(add = rowSums(across(where(is.numeric)), na.rm=TRUE))

plot <-
dataplot %>% 
  ggplot()+
  geom_line(aes(x = TIME_LOCAL,
                y = add,
                group = dummy,
                color = season),
            show.legend = F,
            linewidth = 0.2)+
  geom_hline(yintercept=0)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust =1))+
  labs(x = NULL,
       y = "Substation supply (MW)",
       subtitle = "2562")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

ggsave("figures/combineSubVoltage/R1_3U_CBD22+115_2019_filtered.png", width = 7, height = 3, units = "in", dpi = 300)
