############################################################################
# The Impacts of Remote Learning in Secondary Education: 
# Evidence from Brazil during the Pandemic 
#
# Guilherme Lichand, Carlos Alberto Dória, Onicio Leal Neto and João Cossi
#
#
# 4.Main results 
# Last update: 27/04/21
############################################################################

#In this script, we generate the main results of the paper.


#SET ENVIRONMENT 
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
library(lmtest)
library(multiwayvcov)



# Open data 
main_data<-readRDS("~/final_data.rds")


# Generate data for results. We transform the main data into a single dataframe
# and generate variables for the difference-in-difference model. We also define 
# the proxy for dropouts at the quarter level for the difference-in-difference 
# model and weights (inverse of the propensity scores). 
results_data<-main_data[[1]] %>%
  rbind(main_data[[2]]) %>% 
  rbind(main_data[[3]]) %>% 
  filter(quarter==1 | quarter==4) %>% 
  mutate(treated=ifelse(year==2020, 1, 0), q4=ifelse(quarter==4, 1,0)) %>% 
  mutate(proxy_quarter=ifelse(is.na(grades), 1, 0)) %>% 
  mutate(proxy_quarter=proxy_quarter*proxy_grade) %>% 
  mutate(proxy_quarter=ifelse(q4==1, proxy_grade,proxy_quarter)) %>% 
  mutate(highschool=ifelse(series>9,1,0)) %>% 
  mutate(pscore=1-pscore) %>% 
  mutate(ipw=1/pscore)

saveRDS(results_data, "~/results_data.RDS")
results_data <-readRDS("~/results_data.RDS")


##Define important parameters

#Outcomes of interest
outcomes<-c("proxy_quarter", "score")

#Define baseline tests standard deviation
sd<-results_data %>% 
  filter(year==2019 & q4==1) %>% 
  filter(!is.na(score)) %>% 
  ungroup() %>% 
  summarise(sd=sd(score)) %>% 
  pull()


#Define samples of interest
#Sample one (s1) includes the fourth quarter of 2019 and 2020.
s1<-results_data %>% 
  filter(q4==1 & year>2018) %>% 
  mutate(int=treated)

#Sample two (s2) includes the fourth quarter of all years (2018 to 2020)
s2<-results_data %>% 
  filter(q4==1) %>%
  mutate(rep=0) %>%
  as.data.frame(.) 

# To ease the estimation process, we duplicate all observations for 2019.
# We cluster standard-errors, so this increase in observation will not affect
# the estiamtes below.
rep<-as.data.frame(rep(s2[s2$year==2019,]))
rep<-mutate(rep, rep=1)
s2<-s2 %>% 
  rbind(rep) %>% 
  filter(!is.na(pscore)) %>% 
  mutate(treated=ifelse(year==2019 & rep==1, 1, treated), time=ifelse(year==2020 | (year==2019 & rep==0), 1, 0)) %>% 
  mutate(int=treated*time)

# Sample three (s3) includes the first and fourth quarters of 2020.
s3<-results_data %>% 
  filter(year>2018) %>% 
  filter(!is.na(pscore)) %>% 
  mutate(int=treated*q4)

##DEFINE IMPORTANT FUNCTIONS

#Define function that estimates most results in Table 1. We estimate a linear model and
#adjust the variance-covariance matrix to the clustering problem.
full_results<-function(x){
  c1<-lm(s1[[x]]~int+treated+q4+ever_treated*q4+factor(series), data=s1)
  c1<-coeftest(c1,  vcov=vcovHC(c1, cluster="CD_ESCOLA"))
  c2<-lm(s2[[x]]~int+treated+time+ever_treated*q4+factor(series), data=s2)
  c2<-coeftest(c2,  vcov=vcovHC(c2, cluster="CD_ESCOLA"))
  c3<-lm(s3[[x]]~int+treated+q4+ever_treated*q4+factor(series), data=s3)
  c3<-coeftest(c3,  vcov=vcovHC(c3, cluster="CD_ESCOLA"))
  return(cbind(c1[2,1:2], c2[2,1:2], c3[2,1:2]))
}

#Define main function, which is used for most of the results in the paper
main_specification<-function(x){
  d<-lm(proxy_quarter~int+treated+q4+ever_treated*q4+factor(series), data=x)
  d<-coeftest(d,  vcov=vcovHC(d, cluster="CD_ESCOLA"))
  s<-lm(score~int+treated+q4+poly(pscore,3)+ever_treated*q4+factor(series), data=x, weights = ipw)
  s<-coeftest(s,  vcov=vcovHC(s, cluster="CD_ESCOLA"))
  return(t(cbind(t(d[2,1:2]), t(s[2,1:2]))))
}

#The function above cannot implemented in the grade-level due to the lack of variation in fixed-effects.
#We define an alternative function where we do not control for grades' fixed-effects.
main_specification_wfe<-function(x){
  d<-lm(proxy_quarter~int+treated+q4+ever_treated*q4, data=x)
  d<-coeftest(d,  vcov=vcovHC(d, cluster="CD_ESCOLA"))
  s<-lm(score~int+treated+q4+poly(pscore,3)+ever_treated*q4+factor(series), data=x, weights = ipw)
  s<-coeftest(s,  vcov=vcovHC(s, cluster="CD_ESCOLA"))
  return(t(cbind(t(d[2,1:2]), t(s[2,1:2]))))
}



##ESTIMATE MAIN RESULTS (TABLE 1)

#Estimate results of columns (1)-(3) of Table 1
Table1<-map(outcomes, full_results)
Table1<-rbind(Table1[[1]], Table1[[2]])

#Columns (4) is equal to (3) for dropouts, so we replicate the
#results for the following columns.
Table1<-cbind(Table1, Table1[,3])

#Estimate results for scores (column (4))
c4<-lm(score~int+treated+q4+poly(pscore,3)+ever_treated*q4+factor(series), data=s3)
Table1[3:4,4]<-coeftest(c4,  vcov=vcovHC(c4, cluster="CD_ESCOLA"))[2,1:2]

#Estimate main specification (column (5)) 
Table1<-cbind(Table1, main_specification(s3))

#Round estimates and standarize scores'coefficients
Table1[3:4,]<-Table1[3:4,]/sd
Table1<-round(Table1, 4)



##ESTIMATE TREATMENT EFFECTS AT THE SERIES LEVEL (FIGURE 1)

#Split data according to series
series_data<-split(s3, s3$series)

#Map estimates to all series
Figure1<-map_dfc(series_data, main_specification_wfe) 



#ESTIMATE TREATMENT EFFECTS OF SCHOOL REOPENING (TABLE 2)

#Arrange data for municipalities that reopened or not the schools
munic_data <- main_data[[2]] %>%
  select(!c(always_treated, ever_treated)) %>% 
  left_join(treated, by="munic_code") %>% 
  mutate(year=2019) %>% 
  rbind(main_data[[3]]) %>% 
  filter(quarter==4) %>% 
  mutate(q4=ifelse(quarter==4,1,0)) %>% 
  ungroup() %>% 
  mutate(across(c(always_treated, ever_treated), ~as.numeric(.x))) %>% 
  mutate(y20=ifelse(year==2020,1,0)) %>% 
  mutate(proxy_quarter=ifelse(is.na(grades), 1, 0)) %>%
  mutate(highschool=ifelse(series>9, 1,0)) %>% 
  mutate(int=always_treated*y20)

c1<-lm(proxy_quarter~int+always_treated+y20, data=munic_data[munic_data$highschool==0, ])
c1<-format(coeftest(c1,  vcov=vcovHC(c1, cluster="munic_code")), scientific = FALSE) 
c2<-lm(proxy_quarter~int+always_treated+y20, data=munic_data[munic_data$highschool==1, ])
c2<-coeftest(c2,  vcov=vcovHC(c2, cluster="munic_code"))
