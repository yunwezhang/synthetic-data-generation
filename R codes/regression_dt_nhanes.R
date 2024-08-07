install.packages("NHANES")
library("NHANES")
library(tidyverse)
library(devtools)
install_github("silentspringinstitute/RNHANES")
library(tidyverse)
library(RNHANES)
d07 = nhanes_load_data("DEMO_E", "2007-2008") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR,RIDRETH1,DMDEDUC2,DMDMARTL,DMDHHSIZ,INDHHIN2) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR,Race=RIDRETH1,
            Education=DMDEDUC2,Marriage=DMDMARTL,Total_household=DMDHHSIZ,Total_income=INDHHIN2) %>% 
  full_join(nhanes_load_data("VID_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS,Race,Education,Marriage,Total_household,Total_income) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS,Race,Education,Marriage,Total_household,Total_income) %>% 
  full_join(nhanes_load_data("BIOPRO_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, LBXSCA,LBXSCH,LBXSCR,LBXSGL,LBXSIR,LBXSNASI,LBXSTP,Race,Education,Marriage,Total_household,Total_income) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA,Cholesterol=LBXSCA,Creatinine=LBXSCR,
            Glucose_serum=LBXSGL,Iron=LBXSIR,Sodium=LBXSNASI,protein=LBXSTP,Race,Education,Marriage,Total_household,Total_income)


d09 = nhanes_load_data("DEMO_F", "2009-2010") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR,RIDRETH1,DMDEDUC2,DMDMARTL,DMDHHSIZ,INDHHIN2) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR,Race=RIDRETH1,
            Education=DMDEDUC2,Marriage=DMDMARTL,Total_household=DMDHHSIZ,Total_income=INDHHIN2) %>% 
  full_join(nhanes_load_data("VID_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS,Race,Education,Marriage,Total_household,Total_income) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS,Race,Education,Marriage,Total_household,Total_income) %>% 
  full_join(nhanes_load_data("BIOPRO_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, LBXSCA,LBXSCH,LBXSCR,LBXSGL,LBXSIR,LBXSNASI,LBXSTP,Race,Education,Marriage,Total_household,Total_income) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA,Cholesterol=LBXSCA,Creatinine=LBXSCR,
            Glucose_serum=LBXSGL,Iron=LBXSIR,Sodium=LBXSNASI,protein=LBXSTP,Race,Education,Marriage,Total_household,Total_income)

dat = rbind(d07, d09)


dat1 = dat %>% 
  # exclude missings
  filter(!is.na(vitD), !is.na(Calcium))
dat1=dat1[,colnames(dat)%in%c("vitD","Calcium")]
write_csv(dat1,file="vitd_1.csv")

dat2 = dat %>% 
  # exclude missings
  filter(!is.na(vitD), !is.na(Calcium))
dat2=dat2[,colnames(dat)%in%c("vitD","Calcium","Cholesterol", "Creatinine","Glucose_serum",
                             "Iron", "Sodium","protein")]
write_csv(dat2,file="vitd_2.csv")

dat3 =dat[,!colnames(dat)%in%c("SEQN","wave")]
dat3 = dat %>% 
  # exclude missings
  filter(!is.na(vitD), !is.na(Calcium)) %>% 
  mutate(Gender = recode_factor(RIAGENDR, 
                                '1' = "Males", 
                                '2' = "Females"),
         Race = recode_factor(Race, 
                                '1' = "Hispanic", 
                                '2' = "Hispanic",
                                '3'="Black",
                              '4'="White",
                              '5'="Other"),
         Education = recode_factor(Education, 
                                 '1'= "no_high", 
                                '2' = "no_high",
                                '3'="high_grad",
                                '4'="college_entry",
                                '5'="college_grad",
                                '7'="Other",
                                '9'="Other"))
dat3$Marriage=as.factor(dat3$Marriage)
dat3$Total_income=as.factor(dat3$Total_income)
summary(dat3)
write_csv(dat3,file="vitd_3.csv")



