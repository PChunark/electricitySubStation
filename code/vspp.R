library(tidyverse)


# For Thai font, please save file in CSV UTF-8

vsppNE<- read_csv("data/vspptmo_adjustNE.csv")%>%
       select(region = COUNTRY_ZONE,
              province = PROVINCE,
              egatSubStation = EGAT_SUB,
              fuel = FUEL2,
              contractCapMW = CONTRACTED_CAP_MW) %>%
       filter(region == "ภาคตะวันออกเฉียงเหนือ")


# Plot VSPP capacity in NE
vsppNE %>%
  ggplot(aes(x = province, y = contractCapMW, group = fuel, color = fuel))+
  geom_point()+
  labs(
    x = "จังหวัด",
    y = "Contracted Capacity (MW)",
    title = "กำลังการผลิตตามสัญญาของ VSPP ในภาคอีสาน"
  )+
  theme_light()
