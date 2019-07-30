# read in CiOB-ECHO survey data 

df <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/questionnaire.csv")

# read in medical record abstraction data 

df_mr <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/medicalrecordabstraction.csv")

# recode variables you need 
df_mr$mat_age <- ifelse(df_mr$age_dlvry_mr==9999, NA, df_mr$age_dlvry_mr)

# note: hx means history, fhx means family history 
df_mr$preeclampsia <- ifelse(df_mr$preclmps_hx_ns_mr==9,NA,df_mr$preclmps_hx_ns_mr)
df_mr$hypertension <- ifelse(df_mr$htn_hx_mr==9, NA, df_mr$htn_hx_mr)
# only keep age for now
df_mr <- df_mr %>% select(ppt_id, mat_age, preeclampsia, hypertension)

# fetal growth created variables 

df_f <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/fetal growth.csv")

df_f <- df_f %>% select(ID, ga_weeks_comb, SGA_10, baby_wt_grams, lbw_cat, ga_cat.4)
df_f$ppt_id <- df_f$ID

# recode key variables  
                   
df$mat_edu <- ifelse(df$edu_m>=97, NA, df$edu_m)                   
df$mat_edu <- factor(df$mat_edu, levels=c(0,1,2,3,4,5), 
                   labels=c("Less than high school","High school grad","Some college","College grad","Master's degree","Doctoral degree"))


df$mat_race <- ifelse(df$race_m>=97,NA, df$race_m)
#df$mat_race <- factor(df$mat_race, levels=c(1,2,3,4,5,6), 
#                      labels=c("Asian","Pacific Islander","Black","White","Native American","Other"))

df$mat_eth <- ifelse(df$latina_m>=97,NA, df$latina_m)

df$mat_race_eth <- ifelse(df$mat_eth==1, 1, 
                          ifelse(df$mat_eth==0 & (df$mat_race==1 | df$mat_race==2), 2, 
                                 ifelse(df$mat_eth==0 & df$mat_race==3, 3, 
                                        ifelse(df$mat_eth==0 & df$mat_race==4, 4, 
                                               ifelse(df$mat_eth==0 & (df$mat_race==5 | df$mat_race==6), 5, NA)))))

df$mat_race_eth <- factor(df$mat_race_eth, levels=c(1,2,3,4,5), 
                          labels=c("Latina","Asian/PI", "Black","White","Other or multiple"))


df$marital <- ifelse(df$marital_stat>=97,NA, df$marital_stat)

df$marital <- ifelse(df$marital==4, 3, df$marital)
df$marital <- factor(df$marital, levels=c(1,3,5), labels=c("Married","Widowed, separated, or divorced","Never married"))

df$medi_cal <- ifelse(df$medi_cal_m>=97,NA, df$medi_cal_m)


# create income categories 

df$income_hh <- ifelse(df$income_hh %in% c(97,98,99), NA, df$income_hh)
df$income_20k <- ifelse(df$income_20k %in% c(97,98,99), NA, df$income_20k)
df$income_40k <- ifelse(df$income_40k %in% c(97,98,99), NA, df$income_40k)

df$hh_income_cat1 <- ifelse(df$income_hh==1,1, 
                            ifelse(df$income_hh==2,1,
                                   ifelse(df$income_hh==3, 1, 
                                          ifelse(df$income_hh==4,1,
                                                 ifelse(df$income_hh==5,1, 
                                                        ifelse(df$income_hh==6,1, 
                                                               ifelse(df$income_hh==7,1,
                                                                      ifelse(df$income_hh==8,1, 0))))))))

df$hh_income_cat1 <-  ifelse(((df$income_20k==0 & !is.na(df$income_20k)) | (df$income_40k==0 & !is.na(df$income_40k))), 1, df$hh_income_cat1)



df$hh_income_cat2 <- ifelse(df$income_hh==9,2, 
                            ifelse(df$income_hh==10,2, 
                                   ifelse(df$income_hh==11,2, 
                                          ifelse(df$income_hh==12,2, 
                                                 ifelse(df$income_hh==13,2,
                                                        ifelse(df$income_hh==14,2,
                                                               ifelse(df$income_hh==15,2, 0)))))))

df$hh_income_cat2 <- ifelse((df$income_80k==0 & !is.na(df$income_80k)), 2, df$hh_income_cat2)


df$hh_income_cat3 <-  ifelse(df$income_hh==16,3, 
                             ifelse(df$income_hh==17,3, 
                                    ifelse(df$income_hh==18,3,
                                           ifelse(df$income_hh==19,3,
                                                  ifelse(df$income_hh==20,3,0)))))

df$hh_income_cat <- ifelse(df$hh_income_cat1==1,1, 
                           ifelse(df$hh_income_cat2==2, 2, 
                                  ifelse(df$hh_income_cat3==3, 3, NA)))



df$hh_income_cat <- factor(df$hh_income_cat, levels=c(1,2,3), labels=c("<40,000","$40,000-$79,999","$80,000+"))
# merge on biological measures of stress 
library(readxl)
library(dplyr)
library(ggplot2)

df_crh <- read_xlsx("/Users/danagoin/Documents/CiOB-ECHO/Stress Biomarkers/CRH results/Fisher Lab/CORRECTEDCiOB2CRHCompiledData8.2.18.xlsx", sheet="ALL VALUES", range="B4:C435")

# the consonant at the beginning of some of the ppt id numbers corresponds to the language the interview was conducted in - E for English and S for Spanish

df_crh$ppt_id <- as.numeric(gsub("[^0-9]", "", df_crh$`PT ID`))

df_m <- full_join(df_crh, df)
dim(df_m)
# 2 in df_crh that are not in main survey data -- persion 415 and person 633
sum(!unique(df_crh$ppt_id) %in% unique(df$ppt_id))
df_crh[!unique(df_crh$ppt_id) %in% unique(df$ppt_id),]

# 81 in survey data that don't have CRH measurements 
sum(!unique(df$ppt_id) %in% unique(df_crh$ppt_id))
df[!unique(df$ppt_id) %in% unique(df_crh$ppt_id),1:5]
# look into why these people don't have CRH measurements 

# log-CRH is approximately normally distributed
qqnorm(log(df_crh$`pg/ml`))
qqline(log(df_crh$`pg/ml`))

# merge on medical record data 
df_m <- left_join(df_m, df_mr)
df_m <- left_join(df_m, df_f)
df_m$CRH_pg_ml <- df_m$`pg/ml`

# look at distribtuion of CRH by potential confounders 

# race
ggplot(df_m, aes(x=mat_race_eth, y=CRH_pg_ml)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Latina","Black/African American","White","Asian/Pacific Islander","Other or multiple","Missing"))

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$CRH_pg_ml~df_m$mat_race_eth)


# educ
ggplot(df_m, aes(x=mat_edu, y=CRH_pg_ml)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Less than high school","High school grad", "Some college", "College grad", "Master's degree","Doctoral degree","Missing"))

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$CRH_pg_ml~df_m$mat_edu)


# marital status
ggplot(df_m, aes(x=marital, y=CRH_pg_ml)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Married","Widowed, separated, or divorced","Never married","Missing"))

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$CRH_pg_ml~df_m$marital)

# household income
ggplot(df_m, aes(x=hh_income_cat, y=CRH_pg_ml)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("<40,000","$40,000-$79,999","$80,000+","Missing"))

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$CRH_pg_ml~df_m$hh_income_cat)



# look at SGA and PTB in relation to CRH levels 
df_m$ptb <- ifelse(df_m$ga_cat.4=="Preterm",1,0)
df_m$sga <- ifelse(df_m$SGA_10=="1: Small for GA",1,0)

summary(glm(ptb ~ CRH_pg_ml, data=df_m))
summary(glm(sga ~ CRH_pg_ml + hh_income_cat + mat_edu + marital + mat_age + mat_race_eth, data=df_m))
summary(glm(preeclampsia ~ CRH_pg_ml, data=df_m))
summary(glm(hypertension ~ CRH_pg_ml + hh_income_cat + mat_edu + marital + mat_age + mat_race_eth, data=df_m))


df_m$mat_age_sq <- df_m$mat_age^2
summary(glm(ptb ~ mat_age + mat_age_sq, data=df_m))
summary(glm(sga ~ mat_age, data=df_m))



