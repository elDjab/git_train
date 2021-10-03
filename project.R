setwd("D:/ProjetCRES")
setwd("D:/ProjetCRES/SA_MNT_ACCESS/R24_CI/FINAL/")

library(rio)
library(tidyverse)


quant_ind <- rio::import("SEC_3_QUANTIFICATION_R24.dta")
ident_ind <- rio::import("SEC_00_INDENTIFICATION_R24.dta")
metadata_ind <- rio::import("META_DATA_R24.dta")

base_f <- merge(quant_ind, ident_ind, all.x = F, all.y = F, by="hhiid")
base_f <- merge(base_f, metadata_ind, all.x = F, all.y = F, by="hhiid")

base_f <- base_f %>% 
  select(s00_28_ri,s03_mode_conso,s03_id_repas2_ri,s03_libelle2_ri,s03_01_1_ri,s03_01_2_ri,
         s03_01_3_ri,s03_02_ri,s03_03_ri,s03_04_ri,s03_05_ri,s03_06_ri,s03_07_ri,s03_08_ri,s03_09_ri) 

base_f <- base_f%>% 
  rename(type_ind=s00_28_ri,mode_conso=s03_mode_conso,method_quant=s03_03_ri,quant_conso=s03_04_ri)


base_f %>% 
  group_by(method_quant=ifelse(s03_mode_conso==1,"Individuellement","En groupe")) %>% 
  count() %>% 
  ungroup()

base_f %>% 
  distinct(method_quant=ifelse(s03_mode_conso==1,"Individuellement","En groupe"))

base_f %>% 
  distinct(method_quant=ifelse(s03_mode_conso==1,"Individuellement","En groupe"))

base_f %>% 
  filter(s03_03_ri==4) %>% 
  count(method_quant=ifelse(s03_mode_conso==1,"Individuellement","En groupe"))

base_f_mil <- base_f %>% 
  filter(s03_03_ri==4, s03_mode_conso==1)

p <- ggplot(base_f_mil,aes(x=typ , y = s03_04_ri, fill=s00_28_ri)) + geom_boxplot()
p
