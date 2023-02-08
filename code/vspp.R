library(tidyverse)


# For Thai font, please save file in CSV UTF-8

vsppNE<- read_csv("data/vspptmo_adjustNE.csv")%>%
       select(region = COUNTRY_ZONE,
              province = PROVINCE,
              egatSubStation = EGAT_SUB,
              fuel = FUEL2,
              contractCapMW = CONTRACTED_CAP_MW) %>%
       filter(region == "ภาคตะวันออกเฉียงเหนือ")

# Create yearly typical profile
read_csv("data/typicalREprofile1day.csv")%>% slice(rep(1:n(), 365))
b <- data.frame(seq(as.POSIXct("2019/01/01 00:30:00"),
    as.POSIXct("2019/12/31 00:00:00"),
    by = (30*60)))


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


