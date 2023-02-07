library(tidyverse)


Sys.setlocale("LC_CTYPE", "Thai")
options(encoding = "UTF-8")
locale=locale(encoding="latin1")

read_csv("data/All_Vspp_20221124_PWirat.csv") %>%
      select("COUNTRY_ZONE", "PROVINCE","EGAT_SUB","PEA_SUB") 
   
  str(read_csv("data/All_Vspp_20221124_PWirat.csv"))
read_csv("data/VSPPTMO_NE.csv")
