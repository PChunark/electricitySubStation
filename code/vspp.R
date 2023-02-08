library(tidyverse)


# For Thai font, please save file in CSV UTF-8

vsppNE<- read_csv("data/vspptmo_adjustNE.csv")%>%
       select(region = COUNTRY_ZONE,
              province = PROVINCE,
              egatSubStation = EGAT_SUB,
              fuel = FUEL2,
              contractCapMW = CONTRACTED_CAP_MW) %>%
       filter(region == "ภาคตะวันออกเฉียงเหนือ")

# Energy and MW data of VSPP in MEA and PEA

a <- read_csv("data/rawVsppPEA.csv")%>%
  select(year, regionNumber, province, fuel, contractCapacityMW, month.abb) %>%
  replace_na(list(contractCapacityMW = 0))%>%
  pivot_longer(col = month.abb,
               names_to = "month",
               values_to = "energy")%>%
  mutate(energy = energy/1000)%>%
  filter(year == "2564", regionNumber == "2") %>% 
  group_by(province, fuel) %>%
  summarise(sum = Jan)
  
  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

# Create yearly typical profile
typicalProfile1Year <- 
  read_csv("data/typicalREprofile1day.csv")%>% 
  slice(rep(1:n(), 365))

df <-
  data.frame(TIMESTAMP = seq(as.POSIXct("2019/01/01 00:30:00"),
    as.POSIXct("2020/01/01 00:00:00"),
    by = (30*60))) %>%
  cbind(typicalProfile1Year)





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


