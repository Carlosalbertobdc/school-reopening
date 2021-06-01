############################################################################
# The Impacts of Remote Learning in Secondary Education: 
# Evidence from Brazil during the Pandemic 
#
# Guilherme Lichand, Carlos Alberto Dória, Onicio Leal Neto and João Cossi
#
#
# 3.Pre analysis 
# Last update: 27/04/21
############################################################################


# In this script, we make some further procedures that are necessary for the main
# analysis. First, we build a new dataset that validates our proxy for dropouts.
#Second, we estimate the probability that individuals miss the standarized
# tests. 

#SET ENVIRONMENT 
library(dplyr)
library(tidyr)
library(purrr)

#Open main data
main_data<-readRDS("~/main_data.rds")

#Define data to validate proxies
proxy <- main_data[[2]] %>% 
  select(c(CD_ALUNO, series, CD_ESCOLA, proxy_grade)) %>% 
  filter(series!=12) %>% 
  distinct(CD_ALUNO, .keep_all = TRUE) %>% 
  left_join(main_data[[3]], by="CD_ALUNO") %>% 
  distinct(CD_ALUNO, .keep_all = TRUE) %>% 
  mutate(dropout=ifelse(is.na(year), 1, 0)) %>% 
  select(c(CD_ALUNO, series.x, CD_ESCOLA.x, proxy_grade.x, dropout)) %>% 
  set_names(., c("CD_ALUNO", "series", "CD_ESCOLA", "proxy_grade", "dropout"))

#Aggregate proxy data at the school x grade level  
proxy_agg<-proxy %>% 
  group_by(CD_ESCOLA, series) %>% 
  summarise(across(everything(), mean))

# Generate data at the individual (main data is at the student x quarter level). 
# We also generate a dummy variable for students that missed all standardized tests
# in a year. 
individuals<- map(main_data, ~.x %>% 
                    mutate(missing_score=ifelse(is.na(score), 1, 0)) %>% 
                    group_by(CD_ALUNO) %>% 
                    mutate(n_missing=sum(missing_score)) %>%
                    mutate(missed_tests=ifelse(n_missing==4, 1, 0)) %>% 
                    select(!c(missing_score, n_missing))  %>% 
                    select(c(CD_ALUNO, frequency, grades, male, white, income, series, score, missed_tests, year)) %>%
                    mutate(highschool=ifelse(series>9,1,0)) %>%
                    group_by(CD_ALUNO) %>%
                    summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
                    filter(!is.na(male) & !is.na(income) & !is.na(frequency) & !is.na(grades)) %>% 
                    select(!c(score)))


#Aggregate individuals' data for 2019 and 2020 in a single dataframe
individuals <- rbind(individuals[[1]], individuals[[2]], individuals[[3]])

# Split into a list according to the individuals' grades. We need to do that because 
# because we estimate propensity scores for each grade.
individuals<-split(individuals, individuals$series)

#Add index to each element of the list
individuals <- map(individuals, ~.x %>%  
                     cbind(as.data.frame(seq_len(nrow(.)))) %>% 
                     set_names(.,c("CD_ALUNO", "frequency", "grades", "male", "white", "income", "series", "missed_tests", "year","highschool" , "n")) %>%  
                     mutate(y20=ifelse(year==2020,1,0)))


#Estimate Probit model
pscore<-map(individuals, ~glm(missed_tests ~ y20+frequency+frequency*y20+grades+grades*y20+income+income*y20+male+male*y20+white+white*y20+highschool+highschool*y20 , family = binomial(link = "probit"), data = .)[["fitted.values"]]) 

pscore<-map(pscore, ~.x %>% 
              as.data.frame(.) %>% 
              cbind(as.data.frame(seq_len(nrow(.)))) %>% 
              set_names(., c("pscore", "n")))

#Add propensity scores back to individual data 
individuals<-map2(individuals, pscore, ~.x 
                  %>% left_join(.y, by="n") %>% 
                    select(c(CD_ALUNO, year, pscore, missed_tests)))

#Tranform individuals' data in a dataframe
individuals<-rbind(individuals[[1]], individuals[[2]], individuals[[3]], individuals[[4]], individuals[[5]], individuals[[6]], individuals[[7]])


#Add propensity scores back to the main data 
main_data<-map(main_data, ~left_join(.x, individuals, by=c("CD_ALUNO", "year")))

#Keep only students that had at least one test
main_data<-map(main_data, ~.x %>% filter(missed_tests==0))


#Save datasets
saveRDS(main_data, "~/final_data.rds")
saveRDS(proxy, "~/proxy.rds")
saveRDS(proxy_agg, "~/proxy_agg.rds")

