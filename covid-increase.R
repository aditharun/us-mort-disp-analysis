library(tidyverse)

#Mortality rate difference increases in 2020 are, in part, attributable to the COVID19 pandemic3

#we have the icd code for covid (U07.1)
#let us subtract out the aamr for covid and see where we live in 2020
#not a complete analysis - some patients who died of covid may have died of other things just accelerated a few days by covid and things

#U07.1 2020 for Black Males AAMR: 179.9
#U07.1 2020 for Black Females AAMR: 108.6
#U07.1 2020 for White Males AAMR: 100.3
#U07.1 2020 for White Females AAMR: 61.6
# excess aamr black males: 179.9 - 100.3 = 79.6
# excess aamr black females: 108.6 - 61.6 = 47
if (!dir.exists("results")){
  dir.create("results")
}


pandemic_year_data <- "data/2020-cause-aamr-race-gender-year.txt" %>% read_delim(., delim="\t")

covid_aamrs <- pandemic_year_data %>% filter(grepl("COVID", `ICD-10 113 Cause List`)) %>% relocate(13)  %>% slice(5:8) %>% select(1,3,5,7,9)


net_excess_ageadj <- "data/excess_aamr_by_year.csv" %>% read_csv()




overall_ageadj_changes <- net_excess_ageadj %>% group_by(gender, year) %>% group_by(gender) %>% arrange(year) %>% mutate(pct_yoy_change_mag = ((abs(excess_deaths_rate - lag(excess_deaths_rate))) / abs(lag(excess_deaths_rate))) * 100, direction = sign(excess_deaths_rate - lag(excess_deaths_rate)), change = excess_deaths_rate - lag(excess_deaths_rate), pct_yoy_change = pct_yoy_change_mag * direction) %>% ungroup() 



overall_ageadj_changes <- overall_ageadj_changes %>% mutate(plateau_F = ifelse(gender == "Female", ifelse(year > 2015, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau_M = ifelse(gender == "Male", ifelse(year > 2011, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau = ifelse(gender == "Male", plateau_M, plateau_F))

cpal <- c("#374e55", "#df8f44")

#covid contribution in 2020

covid_aamrs <- covid_aamrs %>% group_by(Gender) %>% summarize(m = as.numeric(`Age Adjusted Rate`)[Race == "Black or African American"] - as.numeric(`Age Adjusted Rate`)[Race == "White"])

overall_ageadj_changes %>% select(year, excess_deaths_rate, gender) %>% filter(year >= 2019) %>% group_by(gender) %>% summarize(diff = excess_deaths_rate[year==2020] - excess_deaths_rate[year==2019]) %>% left_join(covid_aamrs, by = c("gender" = "Gender")) %>% mutate(pct = m / diff)

#went up by 101.9 for females and 185.3 for males from 2019 - 2020

#covid accounts for 46.1% for females and 43.2% for males of this jump






