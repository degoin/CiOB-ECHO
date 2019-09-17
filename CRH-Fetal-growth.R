rm(list=ls())

library(dplyr)
library(readxl)
library(ggplot2)


# read in CiOB survey data

df <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/CiOB2 data/questionnaire.csv")

# read in biospecimen collection data 

df_b <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/CiOB2 data/biospecimen.csv")
df_b <- df_b %>% select(ppt_id, secondtri_date)


# read in medical record abstraction data 

df_mr <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/CiOB2 data/medicalrecordabstraction.csv")


# note: hx means history, fhx means family history 
df_mr$preeclampsia <- ifelse(df_mr$preclmps_hx_ns_mr==9,NA,df_mr$preclmps_hx_ns_mr)
df_mr$hypertension <- ifelse(df_mr$htn_hx_mr==9, NA, df_mr$htn_hx_mr)

# ever miscarried 
df_mr$ever_miscarriage <- ifelse(df_mr$miscarriage_n_mr>0 & df_mr$miscarriage_n_mr<9999 & !is.na(df_mr$miscarriage_n_mr),1, 
                                 ifelse(df_mr$miscarriage_n_mr==0 | is.na(df_mr$miscarriage_n_mr), 0, NA ))

# recurrent miscarried 
df_mr$recur_miscarriage <- ifelse(df_mr$miscarriage_n_mr>2 & df_mr$miscarriage_n_mr<9999 & !is.na(df_mr$miscarriage_n_mr),1, 
                                 ifelse(df_mr$miscarriage_n_mr<=2 | is.na(df_mr$miscarriage_n_mr), 0, NA ))

# preeclampsia or gestational hypertension noted in notes of pregnancy complications 

df_mr$preeclampsia <- ifelse(grepl("preeclampsia",df_mr$pn_cmp_sp_mr, ignore.case = T) & !grepl("hx of preeclampsia", df_mr$pn_cmp_sp_mr, ignore.case = T),1, 
                             ifelse(grepl("gestational hypertension", df_mr$pn_cmp_sp_mr, ignore.case = T)  & !grepl("hx of gestational hypertension", df_mr$pn_cmp_sp_mr, ignore.case = T), 1,
                                    ifelse(grepl("gestational htn", df_mr$pn_cmp_sp_mr, ignore.case = T)  & !grepl("hx of gestational htn", df_mr$pn_cmp_sp_mr, ignore.case = T), 1,
                                          ifelse(grepl("gest htn", df_mr$pn_cmp_sp_mr, ignore.case = T)  & !grepl("hx gest htn", df_mr$pn_cmp_sp_mr, ignore.case = T), 1,
                                             ifelse(grepl("Preclampsia", df_mr$pn_cmp_sp_mr, ignore.case = T) & !grepl("hx of preclampsia", df_mr$pn_cmp_sp_mr, ignore.case = T), 1, 0)))))
                                                 
df_mr$hypertension <-   ifelse(grepl("hypertension", df_mr$pn_cmp_sp_mr, ignore.case=T) & !grepl("gestational hypertension", df_mr$pn_cmp_sp_mr, ignore.case = T) & !grepl("gest hypertension", df_mr$pn_cmp_sp_mr, ignore.case=T) & !grepl("hx of hypertension", df_mr$pn_cmp_sp_mr, ignore.case=T), 1,
                               ifelse(grepl("elevated bp", df_mr$pn_cmp_sp_mr, ignore.case=T) & !grepl("hx of elevated bp", df_mr$pn_cmp_sp_mr, ignore.case = T), 1,  
                               ifelse(grepl("high blood pressure", df_mr$pn_cmp_sp_mr, ignore.case = T) & !grepl("hx of high blood pressure", df_mr$pn_cmp_sp_mr, ignore.case = T), 1, 
                                      ifelse(grepl("elevated blood pressure", df_mr$pn_cmp_sp_mr, ignore.case=T) & !grepl("hx of elevated blood pressure", df_mr$pn_cmp_sp_mr, ignore.case = T), 1, 0))))


df_mr$infection <- ifelse(grepl("infection", df_mr$pn_cmp_sp_mr, ignore.case = T),1, 
                          ifelse(grepl("GBS", df_mr$pn_cmp_sp_mr, ignore.case = T),1, 
                                 ifelse(grepl("chlamydia", df_mr$pn_cmp_sp_mr, ignore.case = T),1,  
                                        ifelse(grepl("UTI", df_mr$pn_cmp_sp_mr, ignore.case = T),1, 
                                               ifelse(grepl("bacterial vaginosis", df_mr$pn_cmp_sp_mr, ignore.case = T),1, 
                                                      ifelse(grepl("genital warts", df_mr$pn_cmp_sp_mr, ignore.case = T),1,
                                                             ifelse(grepl("chorioamnionitis", df_mr$pn_cmp_sp_mr, ignore.case = T),1,
                                                                    ifelse(grepl("strep throat", df_mr$pn_cmp_sp_mr, ignore.case = T),1,
                                                                           ifelse(grepl("amoxicillin", df_mr$pn_cmp_sp_mr, ignore.case = T),1,
                                                                                  ifelse(grepl("hand, foot, mouth disease", df_mr$pn_cmp_sp_mr, ignore.case = T),1,
                                                                                         ifelse(grepl("Trichomonas", df_mr$pn_cmp_sp_mr, ignore.case = T) | grepl("trichomoniasis", df_mr$pn_cmp_sp_mr, ignore.case = T)   ,1,0)))))))))))


df_mr$anemia_preg <- ifelse(grepl("anemia", df_mr$pn_cmp_sp_mr, ignore.case = T), 1, 0)

df_mr$alcohol_use_preg <- ifelse(grepl("ETOH", df_mr$sub_use_sp_mr, ignore.case = T), 1, 
                                 ifelse(grepl("margarita", df_mr$sub_use_sp_mr, ignore.case = T), 1, 
                                        ifelse(grepl("drink", df_mr$sub_use_sp_mr, ignore.case = T), 1, 0)))

df_mr$marijuana_use_preg <- ifelse(grepl("marijuana", df_mr$sub_use_sp_mr, ignore.case=T), 1, 
                                   ifelse(grepl("marihuana", df_mr$sub_use_sp_mr, ignore.case = T), 1, 0))

# fetal growth created variables 
# this data set came from Stephanie, to be consistent with how fetal growth measures have previously been defined 
df_f <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/CiOB2 data/fetal growth.csv")

df_f <- df_f %>% select(ID, ga_weeks_comb, SGA_10, baby_wt_grams, lbw_cat, ga_cat.4)
df_f$ppt_id <- df_f$ID

df <- left_join(df, df_f)
df <- left_join(df, df_b)
df <- left_join(df, df_mr)

# create gestational age at time of second trimester visit 

df$gestwks_secondtri <- round(difftime(strptime(df$secondtri_date, format="%Y-%m-%d"), strptime(df$lmp_dt_mr, format="%Y-%m-%d"), units = "weeks"),0)
# ask if it's possible that people were seen before 12 or after 28 weeks for their second trimester visit 
df$gestwks_secondtri <- ifelse(df$gestwks_secondtri<12 | df$gestwks_secondtri>28, NA, df$gestwks_secondtri)
df$gestwks_secondtri <- ifelse(is.na(df$gestwks_secondtri), round(df$ga_weeks_comb - floor(difftime(strptime(df$dob_c_mr, format="%Y-%m-%d"), strptime(df$secondtri_date, format="%Y-%m-%d"), units = "weeks")), 0), df$gestwks_secondtri)

summary(df$gestwks_secondtri)
#View(df %>% select(ppt_id, lmp_dt_mr, secondtri_date, dob_c_mr, gestwks_secondtri))

# recode key variables  
                   
df$mat_edu <- ifelse(df$edu_m>=97, NA, df$edu_m)                   
df$mat_edu <- factor(df$mat_edu, levels=c(0,1,2,3,4,5), 
                   labels=c("Less than high school","High school grad","Some college","College grad","Master's degree","Doctoral degree"))


df$mat_race <- ifelse(df$race_m>=97,NA, df$race_m)

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

df$parity <- ifelse(df$parity_mr==9, NA, ifelse(df$parity_mr>3, 3, df$parity_mr))
df$parity <- factor(df$parity, levels=c(0,1,2,3), labels=c("0","1","2","3+"))

df$num_child_at_home <- ifelse(df$child_n==0 & !is.na(df$child_n), 0, 
                               ifelse(df$child_live_n>3 & !is.na(df$child_live_n), 3, df$child_live_n))

# if missing child_live_n but not missing child_n, replace with value from child_n 
df$num_child_at_home <- ifelse(is.na(df$num_child_at_home) & !is.na(df$child_n) & df$child_n>3 & df$child_n<95, 3, 
                               ifelse(is.na(df$num_child_at_home) & df$child_n==99, NA, 
                               ifelse(is.na(df$num_child_at_home) & !is.na(df$child_n), df$child_n, df$num_child_at_home)))

df$num_child_at_home <- factor(df$num_child_at_home, levels=c(0,1,2,3), labels=c("0","1","2","3+"))

# infant sex: 0 = female, 1 = male 
df$infant_sex <- ifelse(df$sex_c_mr==1, 1, 
                        ifelse(df$sex_c_mr==0, 0, NA))
df$infant_sex <- factor(df$infant_sex, levels=c(0,1), labels = c("Female","Male"))

# recode variables you need 
# age at delivery from medical record
df$mat_age <- ifelse(df$age_dlvry_mr==9999, NA, df$age_dlvry_mr)
# if missing, use mother's date of birth and date of child's birth from the medical record and round to nearest year 
df$mat_age <- ifelse(is.na(df$mat_age), 
                        round(difftime(strptime(df$dob_c_mr, format="%Y-%m-%d"), strptime(df$dob_m_mr, format="%Y-%m-%d"), units = "days")/365.25,0), df$mat_age)
# if still missing, use mother's date of birth and date of second trimester visit and round to nearest year 
df$mat_age <- ifelse(is.na(df$mat_age), 
                        round(difftime(strptime(df$secondtri_date, format="%Y-%m-%d"), strptime(df$dob_m_mr, format="%Y-%m-%d"), units = "days")/365.25,0), df$mat_age)
# should have no missing after this 
sum(is.na(df$mat_age))

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

df_crh1 <- read_xlsx("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/Stress biomarkers data/CRH results/Fisher Lab/CORRECTEDCiOB2CRHCompiledData8.2.18.xlsx", sheet="ALL VALUES", range="B4:C435")
names(df_crh1) <- c("ppt_id","CRH_pg_ml")
# add second batch 
df_crh2 <- read_xlsx("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Data/Stress biomarkers data/CRH results/Fisher Lab/LastBatchCRH ELISAWFBC10.30.18.xlsx", range="O3:P69")
names(df_crh2) <- c("ppt_id","CRH_pg_ml")

# combine

df_crh <- data.frame(rbind(df_crh1, df_crh2))

# the consonant at the beginning of some of the ppt id numbers corresponds to the language the interview was conducted in - E for English and S for Spanish

df_crh$ppt_id <- as.numeric(gsub("[^0-9]", "", df_crh$ppt_id))

#df_m <- full_join(df_crh, df)
#dim(df_m)

# 2 in df_crh that are not in main survey data -- persion 415 (disenrolled) and person 633 (disenrolled)
# disenrolled means we can't use the sample in the analysis 

#sum(!unique(df_crh$ppt_id) %in% unique(df$ppt_id))
#df_crh[!unique(df_crh$ppt_id) %in% unique(df$ppt_id),]


# 15 in survey data that don't have CRH measurements 
#sum(!unique(df$ppt_id) %in% unique(df_crh$ppt_id))
#df$ppt_id[!unique(df$ppt_id) %in% unique(df_crh$ppt_id)]
# Erin is looking into why these 15 people don't have CRH measurements 

# in the meantime, just use those that have CRH values 

df_m <- left_join(df_crh, df)
dim(df_m)

df_m$log_CRH <- log(df_m$CRH_pg_ml)

df_m$ptb <- ifelse(df_m$ga_cat.4=="Preterm",1,ifelse(df_m$ga_cat.4=="Full Term",0,NA))
df_m$sga <- ifelse(df_m$SGA_10=="1: Small for GA",1, ifelse(df_m$SGA_10=="0: Normal for GA",0,NA))

# follow up with creater of geocoding software re: updates 

# bmi 
# weight
df_m$pp_weight_lbs <- ifelse(df_m$wt_preprg_mr == 9999, NA, 
                             ifelse(df_m$wt_preprg_mr<80, NA, df_m$wt_preprg_mr))
# what are weights that are infeasible? less than 80 lbs? 


# height 
# recode 9999 to 0 for feet, because this corresponds usually to height being recorded in inches only
df_m$height_ft_mr <- ifelse(df_m$height_ft_mr==9999, 0, df_m$height_ft_mr)

# recode 9999 to NA for inches, because this corresponds to missing both feet and inches
df_m$height_in_mr <- ifelse(df_m$height_in_mr==9999, NA, df_m$height_in_mr)

# change height_mr to character to enable regular expressions / string operations 
df_m$height_mr <- as.character(df_m$height_mr)

# if the values for height in inches and feet are missing, replace with values from height_mr 
df_m$height_ft_mr <- ifelse(is.na(df_m$height_ft_mr) & str_detect(df_m$height_mr,"([0-9]')"), str_extract(str_extract(df_m$height_mr, "([0-9]')"), "([0-9])"), 
                            ifelse(is.na(df_m$height_ft_mr) & str_detect(df_m$height_mr, "([0-9] ft)"), str_extract(str_extract(df_m$height_mr,"([0-9] ft.)"), "([0-9])"), df_m$height_ft_mr))

df_m$height_in_mr <- ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr,"([0-9]\")"), str_extract(str_extract(df_m$height_mr, "([0-9]\")"), "([0-9])"), 
                            ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr,"([0-9][0-9]\")"), str_extract(str_extract(df_m$height_mr, "([0-9][0-9]\")"), "([0-9][0-9])"), 
                                   ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr,"([0-9]'')"), str_extract(str_extract(df_m$height_mr, "([0-9]'')"), "([0-9])"), 
                                          ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr,"([0-9][0-9]'')"), str_extract(str_extract(df_m$height_mr, "([0-9][0-9]'')"), "([0-9][0-9])"), 
                                                 ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr, "([0-9] in)"), str_extract(str_extract(df_m$height_mr,"([0-9] in)"), "([0-9])"), 
                                                        ifelse(is.na(df_m$height_in_mr) & str_detect(df_m$height_mr, "([0-9][0-9] in)"), str_extract(str_extract(df_m$height_mr,"([0-9][0-9] in)"), "([0-9][0-9])"), df_m$height_in_mr))))))
                                   
                                                
# check in with whether the maps have been updated for the geocoding 

# calculate height in inches 
df_m$height_ft_mr <- as.numeric(df_m$height_ft_mr)
df_m$height_in_mr <- as.numeric(df_m$height_in_mr)

df_m <- df_m %>% mutate(pp_height_in = height_ft_mr*12 + height_in_mr)

df_m$height_num <- ifelse(df_m$height_mr=="9999",NA,as.numeric(df_m$height_mr))

# for those recorded height in cm, capture and convert to inches
df_m$height_mr_cm <- ifelse(grepl("cm",df_m$height_mr, ignore.case = T), str_replace(df_m$height_mr,"cm",""), NA)
# anything greater than 120 centimeters, which is just less than 4 ft, is likely recorded in cm 
df_m$height_mr_cm <- ifelse(is.na(df_m$height_mr_cm) & df_m$height_num>120, df_m$height_num, df_m$height_mr_cm)
df_m$height_mr_cm <- as.numeric(df_m$height_mr_cm)
# height less than 120 cm is implausible, recode to missing
df_m$height_mr_cm <- ifelse(df_m$height_mr_cm<120, NA, df_m$height_mr_cm)  
  

df_m$pp_height_in <- ifelse(is.na(df_m$pp_height_in), df_m$height_mr_cm/2.54, df_m$pp_height_in)

# recode prepregnancy BMI if missing from height and weight calculated variables 
df_m$pp_bmi <- ifelse(df_m$bmi_preprg_mr==9999,NA,df_m$bmi_preprg_mr)
df_m$pp_bmi <- ifelse(is.na(df_m$pp_bmi), 703*df_m$pp_weight_lbs/(df_m$pp_height_in^2), df_m$pp_bmi)



#df_m %>% filter(is.na(pp_height_in)) %>% select(pp_height_in, height_ft_mr, height_in_mr, height_mr, height_num, height_mr_cm) 
#View(df_m %>% select(pp_height_in, height_ft_mr, height_in_mr, height_mr, height_mt_mr, height_num, height_mr_cm))

save(df_m, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Projects/CRH and fetal growth/data/CRH_fetal_growth_data")


# log-CRH is approximately normally distributed
pdf(file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/qqplot_log_CRH.pdf")
qqnorm(df_m$log_CRH)
qqline(df_m$log_CRH)
dev.off()


# distribution of CRH
p0 <- ggplot(df_m) + theme_bw()  + geom_histogram(aes(log_CRH), fill="#2b8cbe", color="black") + labs(x="log CRH") 

ggsave(p0, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/log_CRH_hist.pdf")


# race
p1<- ggplot(df_m, aes(x=mat_race_eth, y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("Latina","Black/African American","White","Asian/Pacific Islander","Other or multiple","Missing"))

ggsave(p1, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_race.pdf", width=10)

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$log_CRH~df_m$mat_race_eth)


# educ
p2 <- ggplot(df_m, aes(x=mat_edu, y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("Less than \nhigh school","High school grad", "Some college", "College grad", "Master's degree","Doctoral degree","Missing"))

ggsave(p2, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_educ.pdf", width=10)



# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$log_CRH~df_m$mat_edu)


# marital status
p3 <- ggplot(df_m, aes(x=marital, y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("Married","Widowed, separated, or divorced","Never married","Missing"))

ggsave(p3, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_marital.pdf")

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$log_CRH~df_m$marital)

# household income
p4 <- ggplot(df_m, aes(x=hh_income_cat, y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("<40,000","$40,000-$79,999","$80,000+","Missing"))

ggsave(p4, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_income.pdf")

# use kruskal-wallis test, a non-parametric method for testing whether samples originate from the same distribution 
kruskal.test(df_m$log_CRH~df_m$hh_income_cat)


# age
p5 <- ggplot(data=df_m, aes(x=mat_age, y=log_CRH)) + geom_point() + 
  geom_smooth(method="lm", se=F, linetype=2, color="black") + theme_bw() + 
  labs(x="Maternal age", y="log CRH") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))

ggsave(p5, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_age.pdf")

summary(glm(log_CRH ~ mat_age, data=df_m))


# birth weight
p6 <- ggplot(data=df_m, aes(x=log_CRH, y=baby_wt_grams)) + geom_point() + 
  geom_smooth(method="lm", se=F, linetype=2, color="black") + theme_bw() + 
  labs(x="log CRH", y="Birth weight") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))

ggsave(p6, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_bw.pdf")

summary(glm(baby_wt_grams ~ log_CRH, data=df_m))

# term birth weight 
p7 <- ggplot(data=df_m[df_m$ga_cat.4=="Full Term",], aes(x=log_CRH, y=baby_wt_grams)) + geom_point() + 
  geom_smooth(method="lm", se=F, linetype=2, color="black") + theme_bw() + 
  labs(x="log CRH", y="Term birth weight") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))

ggsave(p7, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_bw_term.pdf")


# pre-pregnancy BMI
p8 <- ggplot(data=df_m, aes(x=pp_bmi, y=log_CRH)) + geom_point() + 
  geom_smooth(method="lm", se=F, linetype=2, color="black") + theme_bw() + 
  labs(x="pre-pregnancy BMI", y="log CRH") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))

ggsave(p8, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_bmi.pdf")




# look at SGA and PTB in relation to CRH levels 
df_m$ptb <- ifelse(df_m$ga_cat.4=="Preterm",1,0)
df_m$sga <- ifelse(df_m$SGA_10=="1: Small for GA",1,0)

# t-tests 
t.test(log(df_m$CRH_pg_ml) ~ df_m$ptb)
t.test(log(df_m$CRH_pg_ml) ~ df_m$sga)
t.test(log(df_m$CRH_pg_ml) ~ df_m$preeclampsia)
t.test(log(df_m$CRH_pg_ml) ~ df_m$hypertension)


summary(glm(sga ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))
summary(glm(ptb ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))
summary(glm(baby_wt_grams ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))
summary(glm(baby_wt_grams ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m %>% filter(ga_cat.4=="Full Term")))
summary(glm(ga_weeks_comb ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))
summary(glm(preeclampsia ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))
summary(glm(hypertension ~ log_CRH + factor(mat_race_eth) + mat_age + factor(mat_edu) + factor(hh_income_cat) + factor(marital) + pp_bmi + medi_cal, data=df_m))

# 19 missing birth weight 
# 11 missing race 
# 17 missing maternal age 
# 10 missing maternal education
# 44 missing household income 
# 14 missing marital status 


#to know how many were used in model -- nrow(model.frame(mh))


# preeclampsia
p10 <- ggplot(df_m, aes(x=factor(preeclampsia), y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("No preeclampsia","Preeclampsia","Missing"))

ggsave(p10, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_preeclampsia.pdf")

# hypertension
p11 <- ggplot(df_m, aes(x=factor(hypertension), y=log_CRH)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="log CRH") + 
  scale_x_discrete(labels=c("No hypertension","Hypertension","Missing"))

ggsave(p11, file="/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/CRH_by_hypertension.pdf")


# gestational age in weeks 
ggplot(data=df_m, aes(x=log_CRH, y=ga_weeks_comb)) + geom_point() + 
  geom_smooth(method="loess", linetype=2, color="black") + theme_bw() + 
  labs(x="log CRH", y="Gestational age (weeks)") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))

# gestational age at measurement in weeks 
ggplot(data=df_m, aes(x=gestwks_secondtri, y=CRH_pg_ml)) + geom_point() + 
  geom_smooth(method="loess", linetype=2, color="black") + theme_bw() + 
  labs(y="CRH", x="Gestational age at measurement (weeks)") + theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"))


library(xtable)
table <- read_xlsx("/Users/danagoin/Documents/Research projects/CiOB-ECHO/Fetal growth and pregnancy complications/results/preliminary results 8-6-2019.xlsx", range="A3:D10")
xtable(table)
