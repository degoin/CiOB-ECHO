# read in CiOB-ECHO survey data 

df <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/questionnaire.csv")

# read in medical record abstraction data 

df_mr <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/PEECProject3Chemical-LizDescriptiveStatsC_DATA_2019-05-24_1412.csv")

# fetal growth created variables 

df_f <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/fetal growth.csv")

df_f <- df_f %>% select(ID, ga_weeks_comb, SGA_10, baby_wt_grams, lbw_cat, ga_cat.4)

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


# merge on biological measures of stress 
library(readxl)
library(dplyr)
df_crh <- read_xlsx("/Users/danagoin/Documents/CiOB-ECHO/Stress Biomarkers/CRH results/Fisher Lab/CORRECTEDCiOB2CRHCompiledData8.2.18.xlsx", sheet="ALL VALUES", range="B4:C435")

# the consonant at the beginning of some of the ppt id numbers corresponds to the language the interview was conducted in - E for English and S for Spanish

df_crh$ppt_id <- as.numeric(gsub("[^0-9]", "", df_crh$`PT ID`))

df_m <- left_join(df_crh, df %>% select(ppt_id, site, dob_m))
# appears to be a good match, just ppt_id = 415 isn't in main survey data  

# log-CRH is approximately normally distributed
qqnorm(log(df_crh$`pg/ml`))
qqline(log(df_crh$`pg/ml`))

# create SGA variable 
# create SGA based on Talge et al 2014
file$birthwt <- as.numeric(file$nbthwt1)
file$sga <- ifelse(file$nsex=="1" & file$gestwks==22 & file$birthwt<=375,1,
                   ifelse(file$nsex=="1" & file$gestwks==23 & file$birthwt<=436,1,
                          ifelse(file$nsex=="1" & file$gestwks==24 & file$birthwt<=497,1,
                                 ifelse(file$nsex=="1" & file$gestwks==25 & file$birthwt<=561,1,
                                        ifelse(file$nsex=="1" & file$gestwks==26 & file$birthwt<=629,1,
                                               ifelse(file$nsex=="1" & file$gestwks==27 & file$birthwt<=706,1,
                                                      ifelse(file$nsex=="1" & file$gestwks==28 & file$birthwt<=802,1,
                                                             ifelse(file$nsex=="1" & file$gestwks==29 & file$birthwt<=924,1,
                                                                    ifelse(file$nsex=="1" & file$gestwks==30 & file$birthwt<=1068,1,
                                                                           ifelse(file$nsex=="1" & file$gestwks==31 & file$birthwt<=1231,1,
                                                                                  ifelse(file$nsex=="1" & file$gestwks==32 & file$birthwt<=1415,1,
                                                                                         ifelse(file$nsex=="1" & file$gestwks==33 & file$birthwt<=1627,1,
                                                                                                ifelse(file$nsex=="1" & file$gestwks==34 & file$birthwt<=1859,1,
                                                                                                       ifelse(file$nsex=="1" & file$gestwks==35 & file$birthwt<=2105,1,
                                                                                                              ifelse(file$nsex=="1" & file$gestwks==36 & file$birthwt<=2355,1,
                                                                                                                     ifelse(file$nsex=="1" & file$gestwks==37 & file$birthwt<=2588,1,
                                                                                                                            ifelse(file$nsex=="1" & file$gestwks==38 & file$birthwt<=2782,1,
                                                                                                                                   ifelse(file$nsex=="1" & file$gestwks==39 & file$birthwt<=2926,1,
                                                                                                                                          ifelse(file$nsex=="1" & file$gestwks==40 & file$birthwt<=3017,1,
                                                                                                                                                 ifelse(file$nsex=="1" & file$gestwks==41 & file$birthwt<=3065,1,
                                                                                                                                                        ifelse(file$nsex=="1" & file$gestwks==42 & file$birthwt<=3082,1,
                                                                                                                                                               ifelse(file$nsex=="1" & file$gestwks==43 & file$birthwt<=3067,1,
                                                                                                                                                                      ifelse(file$nsex=="1" & file$gestwks==44 & file$birthwt<=3027,1,
                                                                                                                                                                             ifelse(file$nsex=="2" & file$gestwks==22 & file$birthwt<=354 ,1,
                                                                                                                                                                                    ifelse(file$nsex=="2" & file$gestwks==23 & file$birthwt<=416 ,1,
                                                                                                                                                                                           ifelse(file$nsex=="2" & file$gestwks==24 & file$birthwt<=473 ,1,
                                                                                                                                                                                                  ifelse(file$nsex=="2" & file$gestwks==25 & file$birthwt<=529 ,1,
                                                                                                                                                                                                         ifelse(file$nsex=="2" & file$gestwks==26 & file$birthwt<=597 ,1,
                                                                                                                                                                                                                ifelse(file$nsex=="2" & file$gestwks==27 & file$birthwt<=677 ,1,
                                                                                                                                                                                                                       ifelse(file$nsex=="2" & file$gestwks==28 & file$birthwt<=770 ,1,
                                                                                                                                                                                                                              ifelse(file$nsex=="2" & file$gestwks==29 & file$birthwt<=882 ,1,
                                                                                                                                                                                                                                     ifelse(file$nsex=="2" & file$gestwks==30 & file$birthwt<=1018 ,1,
                                                                                                                                                                                                                                            ifelse(file$nsex=="2" & file$gestwks==31 & file$birthwt<=1166 ,1,
                                                                                                                                                                                                                                                   ifelse(file$nsex=="2" & file$gestwks==32 & file$birthwt<=1335 ,1,
                                                                                                                                                                                                                                                          ifelse(file$nsex=="2" & file$gestwks==33 & file$birthwt<=1538 ,1,
                                                                                                                                                                                                                                                                 ifelse(file$nsex=="2" & file$gestwks==34 & file$birthwt<=1772 ,1,
                                                                                                                                                                                                                                                                        ifelse(file$nsex=="2" & file$gestwks==35 & file$birthwt<=2021 ,1,
                                                                                                                                                                                                                                                                               ifelse(file$nsex=="2" & file$gestwks==36 & file$birthwt<=2261 ,1,
                                                                                                                                                                                                                                                                                      ifelse(file$nsex=="2" & file$gestwks==37 & file$birthwt<=2477 ,1,
                                                                                                                                                                                                                                                                                             ifelse(file$nsex=="2" & file$gestwks==38 & file$birthwt<=2665 ,1,
                                                                                                                                                                                                                                                                                                    ifelse(file$nsex=="2" & file$gestwks==39 & file$birthwt<=2810 ,1,
                                                                                                                                                                                                                                                                                                           ifelse(file$nsex=="2" & file$gestwks==40 & file$birthwt<=2904 ,1,
                                                                                                                                                                                                                                                                                                                  ifelse(file$nsex=="2" & file$gestwks==41 & file$birthwt<=2958 ,1,
                                                                                                                                                                                                                                                                                                                         ifelse(file$nsex=="2" & file$gestwks==42 & file$birthwt<=2985 ,1,
                                                                                                                                                                                                                                                                                                                                ifelse(file$nsex=="2" & file$gestwks==43 & file$birthwt<=2981 ,1,
                                                                                                                                                                                                                                                                                                                                       ifelse(file$nsex=="2" & file$gestwks==44 & file$birthwt<=2952 ,1,0))))))))))))))))))))))))))))))))))))))))))))))


# potential covariates




