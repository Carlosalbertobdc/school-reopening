############################################################################
# The Impacts of Remote Learning in Secondary Education: 
# Evidence from Brazil during the Pandemic 
#
# Guilherme Lichand, Carlos Alberto Dória, Onicio Leal Neto and João Cossi
#
#
# 1.Import raw data 
# Last update: 27/04/21
############################################################################


# In this script, we import raw data for the SEDUC server. We organize
# data into lists according to the information years.



#SET ENVIRONMENT 

#Set libraries 
library(odbc)
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)

#Define connection to the server.
#Credentials shoul be included below.
con <- dbConnect(
  odbc(),
  Driver = "", 
  Server = "", Database = "",
  UID = "",
  PWD = "", 
  Port = 
)


#IMPORT RAW DATA 

##Define functions that imports data
import<-function(x){query<-paste("SELECT * FROM ", x, sep="")
dbGetQuery(con, query)}

#######################################################################
# We import 5 different data sources:
# 1) Outcomes: scorecard grades and frequency for portuguese and math.
# 2) Scores: standarized scores
# 3) Students' series 
# 4) Socioeconomic characteristics: students' gender and race
# 5) School characteristics: income in school's neighborhood and 
# indicator variable for prior online teaching 
# 6) Municipality characteristics: Pandemic-related variables and 
# school reopening status
########################################################################

## 1) Outcomes
outcomes_1819<-import("TB_FREQ_NOTA_ALUNOS_2018_2019")
outcomes_2020<-import("TB_FECHAMENTO_2020_LP_MAT")
outcomes<-list(outcomes_1819[outcomes_1819$DT_ANO_LETIVO==2018,],outcomes_1819[outcomes_1819$DT_ANO_LETIVO==2019,], outcomes_2020)

## 2) Scores 
#SEDUC has four yearly tests. Three of them are called AAP and the last one is called ADR.
#For 2018 and 2019, all tests are in the same data and they are ordered from zero to three ("BIMESTRE variable").
names<-c("TB_AAP_ALUNOS_2018", "TB_AAP_ALUNOS_2019")
scores<-map(names, import)
scores<-map(scores, ~.x %>%  mutate(BIMESTRE=ifelse(BIMESTRE==3, 4, BIMESTRE)))

#For the year of 2020, the ADR is in a separate file. In this year, the ADR test is 
#indicated by BIMESTRE (quarter)=0. 
scores_2020_1<-import("TB_AAP_ALUNOS_2020")
scores_2020_2<-import("TB_ADR_ALUNOS_2020")

#Append test scores for different quarters of 2020
scores_2020<-scores_2020_1 %>% 
  mutate(DT_ANO_LETIVO=as.integer(DT_ANO_LETIVO)) %>% 
  rbind(scores_2020_2) %>% 
  mutate(DT_ANO_LETIVO=2020, BIMESTRE=ifelse(BIMESTRE==0, 4, BIMESTRE)) 

#Append scores for all years
scores<-list(scores[[1]], scores[[2]], scores_2020)

## 3) Students' series

# We only have data for series in the data for standarized scores. 
# We build the series data as a subset of the scores data.
series <- map(scores, ~.x %>% 
  distinct(CD_ALUNO, .keep_all = TRUE) %>% 
  select(c(CD_ALUNO, CD_ESCOLA, SERIE, DT_ANO_LETIVO)))
  

## 4) Socioeconomic characteristics
names<-c("Dados_adicionais_2018", "Dados_adicionais_2019", "TB_DADOS_ADICIONAIS_ALUNO")
socioecon<-map(names, import)


## 5) School characteristics

#Income at school neighborhood 
income<-import("income_updated") %>% 
  set_names(., c("","munic_code","" ,"income", "CD_ESCOLA")) %>%  
  select(c(income, CD_ESCOLA)) %>% 
  mutate(across(c(income, CD_ESCOLA), ~as.numeric(gsub('"', '', .x)))) 


#School online prior learning 
online<-import("online") %>% 
  set_names(., c("", "munic_code", "online", "CD_ESCOLA")) %>% 
  select(c(munic_code, online, CD_ESCOLA)) %>% 
  mutate(across(everything(), ~as.numeric(.x)))

#Merge characteristics at the school-level
school_characteristics <- full_join(income, online, by="CD_ESCOLA")


## 6) Municipality characteristics

#Covid severity at the municipality x quarter level (this database is stored in a csv format, so we need to parse the R output) 
health<-import("saude_munic")
colnames(health)<-"a"
health <- data.frame(do.call('rbind', strsplit(as.character(health$a),',',fixed=TRUE)))

health<-health %>% 
  set_names(., c("munic_code", "quarter","mean_cases","mean_deaths","max_cases","max_deaths", "population")) %>% 
  mutate(across(everything(), ~as.numeric(gsub('"', '', .x))))  %>%
  mutate(mean_cases=(mean_cases/population)*1000, mean_deaths=(mean_deaths/population)*1000) %>% 
  select(c(munic_code, quarter, mean_cases, mean_deaths))


#Municipalities that reopened schools in the second semester of 2020
treated<-import("treated") %>% 
  set_names(., c("","munic_code", "always_treated", "ever_treated")) %>% 
  select(c(munic_code, always_treated, ever_treated)) %>% 
  mutate(munic_code=as.numeric(munic_code)) %>% 
  mutate(year=2020) 

#Merge characteristics at the municipality-quarter level
munic_characteristics<-inner_join(health, treated, by="munic_code") 

#Pandemic data is only relevant for the year of 2020. To ease the merge process,
# we replicate data with zeros for the years of 2018 and 2019. 
munic_characteristics<-munic_characteristics %>% 
  mutate(year=year-1) %>% 
  rbind(munic_characteristics) %>% 
  mutate(year=year-1) %>% 
  mutate(across(c(mean_cases,mean_deaths, always_treated, ever_treated), ~replace(., !is.na(.),0))) %>% 
  rbind(munic_characteristics)



###Save datasets 
saveRDS(outcomes, "~/outcomes.rds")
saveRDS(scores, "~/scores.rds")
saveRDS(series, "~/series.rds")
saveRDS(socioecon, "~/socioecon.rds")
saveRDS(school_characteristics, "~/school_characteristics.rds")
saveRDS(munic_characteristics, "~/munic_characteristics.rds")
