library(tidyverse)




if (!dir.exists("results")){
  dir.create("results")
}


pandemic_year_data <- "data/2020-cause-aamr-race-gender-year.txt" %>% read_delim(., delim="\t")

covid_aamrs <- pandemic_year_data %>% filter(grepl("COVID", `ICD-10 113 Cause List`)) %>% relocate(13)  %>% slice(5:8) %>% select(1,3,5,7,9)
covid_aamrs <- covid_aamrs %>% group_by(Gender) %>% summarize(m = as.numeric(`Age Adjusted Rate`)[Race == "Black or African American"] - as.numeric(`Age Adjusted Rate`)[Race == "White"])  %>% mutate(m = round(m))


net_excess_ageadj <- "data/excess_aamr_by_year.csv" %>% read_csv()


overall_ageadj_changes <- net_excess_ageadj %>% group_by(gender, year) %>% group_by(gender) %>% arrange(year) %>% mutate(pct_yoy_change_mag = ((abs(excess_deaths_rate - lag(excess_deaths_rate))) / abs(lag(excess_deaths_rate))) * 100, direction = sign(excess_deaths_rate - lag(excess_deaths_rate)), change = excess_deaths_rate - lag(excess_deaths_rate), pct_yoy_change = pct_yoy_change_mag * direction) %>% ungroup() 


overall_ageadj_changes <- overall_ageadj_changes %>% mutate(plateau_F = ifelse(gender == "Female", ifelse(year > 2015, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau_M = ifelse(gender == "Male", ifelse(year > 2011, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau = ifelse(gender == "Male", plateau_M, plateau_F))

cpal <- c("#374e55", "#df8f44")



###
#(corresponding to Results section: In 2020, the excess AAMR for males and females increased to levels last observed in 1999 and 2004, respectively (Fig. 1). COVID-19 accounted for 46.1% (excess AAMR of 47 of overall 102) and 43.2% (80 of 185) of this increase in all-cause excess AAMR from 2019 to 2020)
overall_ageadj_changes %>% select(year, excess_deaths_rate, gender) %>% filter(year >= 2019) %>% group_by(gender) %>% summarize(diff = excess_deaths_rate[year==2020] - excess_deaths_rate[year==2019]) %>% left_join(covid_aamrs, by = c("gender" = "Gender")) %>% mutate(frac = m / diff) %>% mutate(pct = round(frac*100, 2))

###


