############################################################################
# The Impacts of Remote Learning in Secondary Education: 
# Evidence from Brazil during the Pandemic 
#
# Guilherme Lichand, Carlos Alberto Dória, Onicio Leal Neto and João Cossi
#
#
# 2.Edit data
# Last update: 27/04/21
############################################################################


#In this script, we transform and merge data sources. 

#SET ENVIRONMENT 

#Set libraries 
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)


#OPEN DATA 
outcomes<-readRDS("~/outcomes.rds")
scores<-readRDS("~/scores.rds")
series<-readRDS("~/series.rds")
socioecon<-readRDS("~/socioecon.rds")
school_characteristics<-readRDS("~/school_characteristics.rds")
munic_characteristics<-readRDS("~/munic_characteristics.rds")


##EDIT DATA 

#1) Outcomes data 
#Clean students' outcomes data 
outcomes<-map(outcomes, ~.x %>% 
                filter(NR_BIMESTRE!=5) %>% 
                mutate(subject=floor(CD_DISCIPLINA/1000)))

#Some  students have duplicate information with missing variables in one of them. 
#Thus, we need to find duplicates in the outcomes data. 
duplicates<-map(outcomes, ~.x %>% 
                  get_dupes(subject, CD_ALUNO, NR_BIMESTRE))

#Next, for the duplicates data we and adjust information for frequency and grades 
duplicates <- map(duplicates, ~.x %>% 
                    group_by(subject, CD_ALUNO, NR_BIMESTRE) %>% 
                    mutate(frequency=min(FREQ, na.rm = TRUE), grades=max(NR_NOTA, na.rm = TRUE)) %>% 
                    mutate(grades=ifelse(is.infinite(grades), NA, grades), frequency=ifelse(is.infinite(frequency), NA, frequency)) %>% 
                    distinct(subject, CD_ALUNO, NR_BIMESTRE, .keep_all =TRUE) %>% 
                    select(subject, CD_ALUNO, NR_BIMESTRE, grades, frequency))

#Now, we join correct information for duplicates in the main dataset
outcomes<-map2(outcomes, duplicates, ~.x %>%
                 distinct(subject, CD_ALUNO, NR_BIMESTRE, .keep_all =TRUE) %>% 
                 left_join(.y, by=c("subject", "CD_ALUNO", "NR_BIMESTRE")) %>% 
                 mutate(frequency=ifelse(is.na(frequency),FREQ, frequency), grades=ifelse(is.na(grades),NR_NOTA, grades)) %>% 
                 select(subject, CD_ALUNO, NR_BIMESTRE, grades, frequency))

#The original dataset is not a complete panel (individuals are dropped from the data
# along the year). Now, we complete the panel and generate proxies for dropout.               
outcomes<-map(outcomes, ~.x %>%     
                complete(CD_ALUNO, NR_BIMESTRE, subject) %>% 
                mutate(proxy_freq=ifelse(is.na(frequency) & NR_BIMESTRE==4, 1,0), proxy_grade=ifelse(is.na(grades) & NR_BIMESTRE==4, 1,0)) %>% 
                group_by(CD_ALUNO) %>%
                mutate(proxy_freq=max(proxy_freq), proxy_grade=max(proxy_grade)) %>%
                rename(quarter=NR_BIMESTRE))  


#Finally, we collapse the main variables (grades, frequency) at the 
#student x quarter level. This is a very cumbersome process, because 
#it requires the evaluation of NA's in two variables for every combination
#of student x quarter. We need to split it into a few different steps. 

#First, we generate frequency data separately. We collapse only observations
#with non-missing frequency data, which speeds the process considerably. 
frequency<-map(outcomes, ~.x %>% 
                 select(c(CD_ALUNO, quarter, frequency)) %>% 
                 filter(!is.na(frequency)) %>% 
                 group_by(CD_ALUNO, quarter) %>%  
                 mutate(frequency=mean(frequency)) %>% 
                 distinct(CD_ALUNO, quarter, .keep_all =TRUE))


#Second, we do the same for scorecard grades.
grades<-map(outcomes, ~.x %>% 
              select(c(CD_ALUNO, quarter, grades)) %>% 
              filter(!is.na(grades)) %>% 
              group_by(CD_ALUNO, quarter) %>%  
              mutate(grades=mean(grades)) %>% 
              distinct(CD_ALUNO, quarter, .keep_all =TRUE))


#Third, we reduce the outcomes data to the student x quarter level without duplicates
outcomes<-map(outcomes, ~.x %>% 
                select(c(CD_ALUNO, quarter, proxy_freq, proxy_grade)) %>% 
                distinct(CD_ALUNO, quarter, .keep_all =TRUE))

#Finally, we merge the list of students with information for grades and frequency.
outcomes<-map2(outcomes, frequency, ~left_join(.x, .y, by=c("CD_ALUNO", "quarter")))
outcomes<-map2(outcomes, grades, ~left_join(.x, .y, by=c("CD_ALUNO", "quarter")))

outcomes<-readRDS("~/outcomes_adj.RDS")

# 2) Scores

#We create variables for scores (correct questions/total questions), change variable names and adjust a 
#a problem with the grades' distribution in 2018.
scores<-map(scores, ~.x %>% 
              mutate(QTD_ACERTOS=ifelse(BIMESTRE==1 & QTD_ACERTOS>6 & DT_ANO_LETIVO==2018, QTD_ACERTOS-1,QTD_ACERTOS)) %>% 
              mutate(BIMESTRE=ifelse(BIMESTRE==3, 4, BIMESTRE), score=QTD_ACERTOS/NQUESTOES) %>% 
              rename(quarter=BIMESTRE) %>% 
              select(CD_ALUNO, quarter, score) %>%
              mutate(score=ifelse(score>1, NA, score)) %>% 
              group_by(CD_ALUNO, quarter) %>%  
              mutate(score=mean(score)) %>% 
              distinct(CD_ALUNO, quarter, .keep_all = TRUE))


# 3) Series 

# We transform the series' variable into a numerical one. In the original data it is a 
# string variable with several different definitions for each grade.

#We write the following function to standarize grades. It is a nested ifelse function
# that transforms the grades variable into a 1-12 scale.
ad_series<-function(x){ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 1° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 1O ANO",1,
                              ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 2° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 2O ANO",2, 
                                     ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 3° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 3O ANO",3, 
                                            ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 4° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 4O ANO",4,
                                                   ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 5° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 5O ANO",5,
                                                          ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 6° ANO" | x=="ENSINO FUNDAMENTAL 9 ANOS - RCI - 6º ANO, 6° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 6O ANO",6,
                                                                 ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 7° ANO" | x=="ENSINO FUNDAMENTAL 9 ANOS - RCI - 7º ANO, 7° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 7O ANO",7, 
                                                                        ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 8° ANO" | x=="ENSINO FUNDAMENTAL 9 ANOS - RCI - 8º ANO, 8° ANO" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 8O ANO",8, 
                                                                               ifelse(x=="ENSINO FUNDAMENTAL DE 9 ANOS, 9° ANO" | x=="EJA FUNDAMENTAL - ANOS FINAIS - EFF, 0° ANO" | x=="EJA FUNDAMENTAL - ANOS FINAIS - MULTISSERIADA, 0° ANO" | x=="EJA FUNDAMENTAL - ANOS FINAIS - MULTISSERIADA, 0A SERIE" | x=="ENSINO FUNDAMENTAL DE 9 ANOS, 9O ANO",9,
                                                                                      ifelse(x=="ENSINO MEDIO, 1A SERIE" | x=="EJA ENSINO MEDIO - EEM, 0A SERIE" | x=="EJA ENSINO MEDIO - MULTISSERIADA, 0A SERIE" | x=="EJA ENSINO MEDIO, 1A SERIE, 0° ANO" | x=="ENSINO FUNDAMENTAL 9 ANOS - RCI - 9º ANO, 9° ANO" | x=="ENSINO MEDIO - VENCE, 1A SERIE" | x=="EJA ENSINO MEDIO, 1A SERIE",10,
                                                                                             ifelse(x=="ENSINO MEDIO, 2A SERIE" | x=="EJA ENSINO MEDIO, 2A SERIE" | x=="ENSINO MEDIO - VENCE, 2A SERIE",11,                                                    
                                                                                                    ifelse(x=="ENSINO MEDIO, 3A SERIE" | x=="EJA ENSINO MEDIO, 3A SERIE" | x=="ENSINO MEDIO - VENCE, 3A SERIE",12, NA))))))))))))}

#Define series variable for each year
series<-map(series, ~.x %>%
              distinct(CD_ALUNO, .keep_all =TRUE) %>% 
              mutate(year=as.numeric(DT_ANO_LETIVO)) %>%
              mutate(year=ifelse(year>2020, 2020, year)) %>%   
              select(CD_ALUNO, CD_ESCOLA, SERIE, year) %>%
              mutate(series=ad_series(SERIE)) %>% 
              filter(!is.na(series)) %>% 
              select(!SERIE)) 

#There is a problem with the data above (not all students make the test). We would lose 5% of 2020 students
#that do not take any tests that year.
#Thus, we build a proxy for the correct series using data for other years. We assume  
#that the grade of a certain individual is the one in the previous year plus one. Using this,
#we are able to cut attrition by half.

#2020
additional_series_2020 <- series[[1]] %>% 
  mutate(year=year+1, series=series+1) %>% 
  rbind(series[[2]]) %>% 
  mutate(year=year+1, series=series+1) %>% 
  filter(series>0 & series<13) %>% 
  distinct(CD_ALUNO, .keep_all = TRUE)

#2019
additional_series_2019 <- series[[1]] %>% 
  mutate(year=year+1, series=series+1) 

additional_series_2019 <- series[[3]] %>% 
  mutate(year=year-1, series=series-1) %>%
  rbind(additional_series_2019) %>% 
  distinct(CD_ALUNO, .keep_all = TRUE)

#2018
additional_series_2018 <- series[[3]] %>% 
  mutate(year=year-1, series=series-1) %>% 
  rbind(series[[2]]) %>% 
  mutate(year=year-1, series=series-1) %>% 
  filter(series>0 & series<13) %>% 
  distinct(CD_ALUNO, .keep_all = TRUE)

additional_series<-list(additional_series_2018, additional_series_2019, additional_series_2020)

series<-map2(series, additional_series, ~.x %>% 
               rbind(.y) %>% 
               distinct(CD_ALUNO, .keep_all = TRUE))



# 4) Socioeconomic characteristics

#Define dummy variables for males and white students
socioecon<-map(socioecon, ~.x %>% 
                 mutate(male=ifelse(TP_SEXO==1,1,0), white=ifelse(CD_COR_RACA==1 | CD_COR_RACA==4, 1,0)) %>% 
                 mutate(CD_ALUNO=as.integer(CD_ALUNO)) %>% 
                 select(c(CD_ALUNO, white, male)) %>% 
                 distinct(CD_ALUNO, .keep_all = TRUE))

#Data for school and municipal characteristics does not require further edits


#MERGE DATA 

#We merge data at the student x quarter level.
main_data<-pmap(list(outcomes, socioecon, series, scores), function(x,y,w,z) x %>% 
                  left_join(y, by="CD_ALUNO") %>% 
                  left_join(w, by="CD_ALUNO") %>% 
                  left_join(z, by=c("CD_ALUNO", "quarter")) %>% 
                  left_join(school_characteristics, by="CD_ESCOLA") %>% 
                  left_join(munic_characteristics, by=c("munic_code", "year", "quarter")))

#Drop missing data and students below grades 5
main_data<-map(main_data, ~.x %>% 
                 filter((!is.na(male) | !is.na(series))) %>%  
                 filter(series>5 & series<13))         


saveRDS(main_data, "~/main_data.rds")
