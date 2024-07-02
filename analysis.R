library(tidyverse)
library(cowplot)



if (!dir.exists("results")){
  dir.create("results")
}


ageadj_file <- "data/cause_of_death_aamr.txt" %>% read_delim(., delim='\t') %>% slice(1:(1675-91))
#overall ageadj
net_excess_ageadj <- "data/excess_aamr_by_year.csv" %>% read_csv()

ageadj <- ageadj_file %>% select(Year, `ICD-10 113 Cause List`, Race, Gender, Deaths, Population, `Age Adjusted Rate`) %>% magrittr::set_colnames(c("year", "icd", "race", "gender", "deaths", "population", "ageadjrate"))

#NA year is 2020
ageadj <- ageadj %>% mutate(year = ifelse(is.na(year), 2020, year))

ageadj <- ageadj %>% mutate(race = ifelse(race == "White", "white", "black"))

ageadj <- ageadj %>% group_by(year, icd, gender) %>% summarize(unadj_excess_num = deaths[race=="black"] - (population[race=="black"] * (deaths[race=="white"]/population[race=="black"])), ageadj_excess = ageadjrate[race=="black"] - ageadjrate[race=="white"]) %>% ungroup()


icd_map <- ageadj %>% select(icd) %>% distinct() %>% mutate(name=c("Accidents", "Alzheimer's", "Assault", "Cerebrovascular Diseases", "Perinatal Period Conditions", "Chronic Liver Disease and Cirrhosis", "Chronic Lower Respiratory Diseases", "Diabetes", "Heart Disease", "Hypertension", "HIV", "Influenza and Pneumonia", "Suicide", "Cancer", "Nephritis", "Parkinson's", "Pneumonitis due to Solids/Liquids", "Septicemia"))

#top causes of death ageadj
ageadj <- ageadj %>% left_join(icd_map, by=c("icd"="icd")) %>% select(-icd)


pandemic_year_data <- "data/2020-cause-aamr-race-gender-year.txt" %>% read_delim(., delim="\t")
covid_aamrs <- pandemic_year_data %>% filter(grepl("COVID", `ICD-10 113 Cause List`)) %>% relocate(13)  %>% slice(5:8) %>% select(1,3,5,7,9)


overall_ageadj_changes <- net_excess_ageadj %>% group_by(gender, year) %>% group_by(gender) %>% arrange(year) %>% mutate(pct_yoy_change_mag = ((abs(excess_deaths_rate - lag(excess_deaths_rate))) / abs(lag(excess_deaths_rate))) * 100, direction = sign(excess_deaths_rate - lag(excess_deaths_rate)), change = excess_deaths_rate - lag(excess_deaths_rate), pct_yoy_change = pct_yoy_change_mag * direction) %>% ungroup() 

###
#(corresponds to Methods section "From 1999-2020, these causes accounted for 83-88% of the overall excess AAMR")

covid_excess_aamrs <- covid_aamrs %>% group_by(Gender) %>% type.convert(as.is = TRUE) %>% summarize(ageadj_excess = `Age Adjusted Rate`[Race == "Black or African American"] - `Age Adjusted Rate`[Race == "White"])

pct_deaths <- rbind(ageadj, (covid_excess_aamrs %>% mutate(unadj_excess_num = NA, year = 2020, name = c("COVID", "COVID")) %>% dplyr::rename(gender = Gender))) %>% group_by(year, gender) %>% summarize(net = sum(ageadj_excess)) %>% ungroup() %>% left_join(net_excess_ageadj, by=c("gender"="gender", "year"="year")) %>% mutate(pct_captured = round((net*100) / excess_deaths_rate)) 

pct_deaths$pct_captured %>% min()
pct_deaths$pct_captured %>% max()
###


###
#(corresponds to Results section "Excess AAMR for Black individuals declined steadily by an annual mean of 5.7% (1999-2015) and 5.2% (1999-2011) for females and males, respectively")

overall_ageadj_changes %>% filter(gender == "Male") %>% filter(year <= 2011)  %>% pull(pct_yoy_change) %>% mean(na.rm=T) %>% round(.,2)
 
overall_ageadj_changes %>% filter(gender == "Female") %>% filter(year <= 2015)  %>% pull(pct_yoy_change) %>% mean(na.rm=T) %>% round(.,2)
###




df <- ageadj %>% group_by(gender, name) %>% arrange(year) %>% mutate(pct_yoy_change_mag = ((abs(ageadj_excess - lag(ageadj_excess))) / abs(lag(ageadj_excess))) * 100, direction = sign(ageadj_excess - lag(ageadj_excess)), change = ageadj_excess - lag(ageadj_excess), pct_yoy_change = pct_yoy_change_mag * direction) %>% ungroup() 

df <- df %>% select(year, gender, change, name) %>% left_join(overall_ageadj_changes %>% select(gender, year, change) %>% magrittr::set_colnames(c("gender", "year", "ovr_change"))) %>% filter(year > 1999) 

#the remaining 15% AAMR that is not accounted for by top causes of death
tail_change <- df %>% group_by(year, gender) %>% summarize(nc = sum(change), t = unique(ovr_change)) %>% ungroup() %>% mutate(Tail = t - nc) %>% select(year, gender, Tail) %>% mutate(name = "Tail") %>% magrittr::set_colnames(c("year", "gender", "change", "name"))

df <- df %>% select(-ovr_change) %>% rbind(tail_change)



### Figure 1

name_map <- data.frame(name = ageadj %>% pull(name) %>% unique(), new_name = c("Accidents", "Alzheimer's", "Assault", "Cerebrovascular\nDiseases", "Perinatal Period\nConditions", "Chronic Liver\nDisease and Cirrhosis", "Chronic Lower\nRespiratory Diseases", "Diabetes", "Heart Disease", "Hypertension", "HIV", "Influenza\nand Pneumonia", "Suicide", "Cancer", "Nephritis", "Parkinson's", "Pneumonitis due to\nSolids and Liquids", "Septicemia")) %>% as_tibble()

ageadj <- ageadj %>% left_join(name_map, by=c("name"="name")) %>% select(-name) %>% magrittr::set_colnames(c("year", "gender", "unadj_excess_num", "ageadj_excess", "name"))


overall_ageadj_changes <- overall_ageadj_changes %>% mutate(plateau_F = ifelse(gender == "Female", ifelse(year > 2015, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau_M = ifelse(gender == "Male", ifelse(year > 2011, "Non-decline", "Decline"), "Decline")) %>% mutate(plateau = ifelse(gender == "Male", plateau_M, plateau_F))

cpal <- c("#374e55", "#df8f44")


fig1a <- overall_ageadj_changes %>% ggplot(aes(x=year, y=excess_deaths_rate, shape=gender)) + geom_line(size = 0.75, color = "grey70") + geom_point(size = 3.25, aes(color = plateau)) + theme_minimal() + theme(panel.border = element_rect(color = "black", fill = "transparent")) + scale_y_continuous(breaks = seq(-100, 450, 50), labels = seq(-100, 450, 50), limits = c(-100, 450)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.65) + scale_x_continuous(breaks=seq(1999, 2020, 1), labels= function(x) ifelse(x %% 2 == 1, x, "")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR per\n100,000 Individuals") + xlab("Year") + theme(legend.title = element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 9)) + ggtitle("Excess Age-Adjusted Mortality Rates") + theme(plot.title = element_text(hjust = 0.5, size = 15))  + theme(panel.grid = element_blank()) + geom_vline(xintercept = 2012, color = "grey80", linetype = "dashed", linewidth = 0.35) + geom_vline(xintercept = 2016, color = "grey80", linetype = "dashed", linewidth = 0.35) + scale_color_manual(values = c("Decline" = cpal[1], "Non-decline" = cpal[2]), labels = c("Decline" = "Decline", "Non-decline" = "Non-Decline"))


fig1b <- overall_ageadj_changes %>% filter(gender == "Male") %>% mutate(color = ifelse(year <= 2011, "Decline", "Non-Decline")) %>% ggplot(aes(x = year, y = change, fill = color)) + geom_bar(stat = "identity") + theme_minimal() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.65) + scale_x_continuous(breaks=seq(2000, 2020, 1), labels= function(x) ifelse(x %% 2 == 0, x, "")) + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR\nChange") + xlab("Year") + theme(legend.title = element_blank(), legend.text = element_text(size = 10)) + ggtitle("Yearly Excess AAMR Change for Males") + theme(plot.title = element_text(hjust = 0.5, size = 14)) + theme(panel.grid = element_blank()) + scale_fill_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2]), labels = c("Decline" = "Overall Excess\nAAMR Decline", "Non-Decline"="No Decline")) + theme(legend.title = element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 9))

#copy of 1b but switch to female gender and year of decline is 2015
fig1c <- overall_ageadj_changes %>% filter(gender == "Female") %>% mutate(color = ifelse(year <= 2015, "Decline", "Non-Decline")) %>% ggplot(aes(x = year, y = change, fill = color)) + geom_bar(stat = "identity") + theme_minimal() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") + scale_x_continuous(breaks=seq(2000, 2020, 1), labels= function(x) ifelse(x %% 2 == 0, x, "")) + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR\nChange") + xlab("Year")  + theme(legend.title = element_blank(), legend.text = element_text(size = 10)) + ggtitle("Yearly Excess AAMR Change for Females") + theme(plot.title = element_text(hjust = 0.5, size = 14)) + theme(plot.title = element_text(hjust = 0.5, size = 14)) + theme(panel.grid = element_blank()) + scale_fill_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2]), labels = c("Decline" = "Overall Excess\nAAMR Decline", "Non-Decline"= "No Decline")) + theme(legend.title = element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 9))



right_half_fig1 <- cowplot::plot_grid(fig1b, fig1c, nrow = 2, ncol = 1, labels = c("B", "C"), label_size = 20)

top_half_fig1 <- plot_grid(fig1a, right_half_fig1, nrow = 1, labels = c("A", ""), label_size = 20)

ggsave(plot = top_half_fig1, filename = "results/decline-fig1.pdf", device = cairo_pdf, height  = 6, width = 15)



#order.of.conditions <- ageadj %>% group_by(name) %>% summarize(x = mean(ageadj_excess)) %>% arrange(desc(x)) %>% pull(name)
#m_ageadj_stack <- ageadj %>% filter(gender == "Male") %>% mutate(name = factor(name, levels = order.of.conditions)) %>% group_by(name) %>% group_split()

#m_pltagestack <- lapply(m_ageadj_stack, function(astack) astack %>% filter(gender == "Male") %>% mutate(plateau = ifelse(year <= 2011, "Decline", "Non-Decline")) %>% ggplot(aes(x=year, y=ageadj_excess, color = plateau)) + geom_point(size = 3) + geom_line(size = 0.85, color = "grey65") + theme_minimal() + theme(legend.position = "none") + geom_vline(xintercept = 2012, linetype = "dashed", color = "grey70") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + ggtitle(unique(astack$name)) + theme(plot.title = element_text(hjust = 0.5, size = 14)) + scale_color_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2])) ) 

#fig1d <- cowplot::plot_grid(plotlist = m_pltagestack, nrow = 6, ncol = 3)


#f_ageadj_stack <- ageadj %>% filter(gender == "Female") %>% mutate(name = factor(name, levels = order.of.conditions)) %>% group_by(name) %>% group_split()

#f_pltagestack <- lapply(f_ageadj_stack, function(astack) astack %>% filter(gender == "Female") %>% mutate(plateau = ifelse(year <= 2015, "Decline", "Non-Decline")) %>% ggplot(aes(x=year, y=ageadj_excess, color = plateau)) + geom_point(size = 3) + geom_line(size = 0.85, color = "grey65") + theme_minimal() + theme(legend.position = "none") + geom_vline(xintercept = 2016, linetype = "dashed", color = "grey70") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + ggtitle(unique(astack$name)) + theme(plot.title = element_text(hjust = 0.5, size = 14)) + scale_color_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2])) ) 

#fig1e <- cowplot::plot_grid(plotlist = f_pltagestack, nrow = 6, ncol = 3)

#fig1d_new <- plot_grid(ggplot() + theme_void() + ggtitle("Excess AAMR by Top Causes of Death for Males") + theme(plot.title = element_text(hjust = 0.5, size = 14)), fig1d, nrow = 2, rel_heights = c(0.02, 1))

#fig1e_new <- plot_grid(ggplot() + theme_void() + ggtitle("Excess AAMR by Top Causes of Death for Females") + theme(plot.title = element_text(hjust = 0.5, size = 14)), fig1e, nrow = 2, rel_heights = c(0.02, 1))


#bottom_half_fig1 <- plot_grid(fig1d_new, ggplot() + theme_void(), fig1e_new, nrow = 1, rel_widths = c(1, 0.1, 1), labels = c("D", "E"), label_size = 20)


#plot_grid(top_half_fig1, bottom_half_fig1, nrow = 2, ncol = 1, rel_heights = c(0.3, 1)) %>% ggsave(filename = "results/decline-fig1.pdf", plot = ., device = cairo_pdf, height = 20, width = 15, units = "in")



avg_change <- rbind(ageadj %>% select(year, gender, ageadj_excess, name), ageadj %>% select(year, gender, ageadj_excess, name) %>% left_join(overall_ageadj_changes %>% select(gender, year, excess_deaths_rate), by=c("gender"="gender", "year"="year")) %>% group_by(year, gender) %>% summarize(nc = sum(ageadj_excess), t = unique(excess_deaths_rate)) %>% ungroup() %>% mutate(Tail = t - nc) %>% select(year, gender, Tail) %>% mutate(name = "Tail") %>% magrittr::set_colnames(c("year", "gender", "ageadj_excess", "name"))) 


#fit a linear model to also get confidence intervals
get_lm_fits <- function(df){

	slope <- coefficients(df %>% lm(ageadj_excess ~ year, .))[[2]]

	ci <- (confint(df %>% lm(ageadj_excess ~ year, .), "year"))[1:2]

	data.frame(slope = slope, lower = ci[1], upper = ci[2], gender = unique(df$gender), name = unique(df$name), plateau = unique(df$plateau))
}


m_avg_stack <- avg_change %>% filter(gender == "Male") %>% filter(year < 2020) %>% mutate(plateau = ifelse(year <= 2011, "Decline", "Non-decline")) %>% group_by(gender, plateau, name) %>% group_split()


m_avg_rate <- do.call(rbind, m_avg_stack %>% lapply(., function(df) get_lm_fits(df))) %>% as_tibble() 


m_order.of.avg.names <- m_avg_rate %>% group_by(name) %>% summarize(diff = slope[plateau == "Decline"] - slope[plateau == "Non-decline"]) %>% arrange(diff) %>% pull(name) %>% rev()

m_avg_rate <- m_avg_rate %>% mutate(name = factor(name, levels = m_order.of.avg.names))



f_avg_stack <- avg_change %>% filter(gender == "Female") %>% filter(year < 2020) %>% mutate(plateau = ifelse(year <= 2015, "Decline", "Non-decline")) %>% group_by(gender, plateau, name) %>% group_split()

f_avg_rate <- do.call(rbind, f_avg_stack %>% lapply(., function(df) get_lm_fits(df))) %>% as_tibble() 

f_order.of.avg.names <- f_avg_rate %>% group_by(name) %>% summarize(diff = slope[plateau == "Decline"] - slope[plateau == "Non-decline"]) %>% arrange(diff) %>% pull(name) %>% rev()

f_avg_rate <- f_avg_rate %>% mutate(name = factor(name, levels = f_order.of.avg.names))



### 
#(corresponding to Results "Notably, among males, mean annual changes in excess AAMR revealed that cancer (-3.97), heart disease (-2.20), accidents (-1.92), and HIV (-1.62) were primary contributors to this decline (Fig. 2A, navy bars). ")

m_avg_rate %>% filter(plateau == "Decline") %>% arrange(desc(abs(slope))) %>% filter(name != "Tail")
###



### 
#(corresponding to Results "For females, significant mean annual reductions were seen in heart disease (-2.78), diabetes (-0.87), cancer (-0.75), accidents (-0.75), and cerebrovascular diseases (-0.67) (Fig. 2B, navy bars)")

f_avg_rate %>% filter(plateau == "Decline") %>% arrange(desc(abs(slope))) %>% filter(name != "Tail")
###



f_avg_rate %>% group_by(name) %>% summarize(diff = slope[plateau != "Decline"] - slope[plateau == "Decline"], pct = round((diff / abs(slope[plateau == "Decline"])) * 100, 2)) %>% arrange(desc(pct))

m_avg_rate %>% group_by(name) %>% summarize(diff = slope[plateau != "Decline"] - slope[plateau == "Decline"], pct = round((diff / abs(slope[plateau == "Decline"])) * 100, 2)) %>% arrange(desc(pct))


m_pct_change <- m_avg_rate %>% group_by(name) %>% summarize(abs_change = (slope[plateau=="Non-decline"] - slope[plateau == "Decline"]), change = (slope[plateau=="Non-decline"] - slope[plateau == "Decline"]) / abs(slope[plateau == "Decline"]) ) %>% mutate(pct_change = round(change*100, 2)) %>% arrange(desc(pct_change)) 

mpct_fig <- m_pct_change %>% mutate(name = factor(name, levels = rev(m_pct_change$name))) %>% ggplot(aes(y=name, x=pct_change)) + geom_bar(stat = "identity", alpha = 0.8, color = "grey50", fill = "grey50") + theme_minimal() + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid = element_blank()) + theme(panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", linewidth = 0.5)) + ggtitle("Males") + theme(plot.title = element_text(size = 14, hjust = 0.5)) + theme(legend.position = "bottom") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12)) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-100, 700)) + xlab("Change in Average Excess AAMR\nFrom Decline to Non-Decline Period (%)") + theme(axis.ticks.x = element_line(color = "black")) + geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") + ylab("")

f_pct_change <- f_avg_rate %>% group_by(name) %>% summarize(abs_change = (slope[plateau=="Non-decline"] - slope[plateau == "Decline"]), change = (slope[plateau=="Non-decline"] - slope[plateau == "Decline"]) / abs(slope[plateau == "Decline"]) ) %>% mutate(pct_change = round(change*100, 2)) %>% arrange(desc(pct_change)) 

fpct_fig <- f_pct_change %>% mutate(name = factor(name, levels = rev(f_pct_change$name))) %>% ggplot(aes(y=name, x=pct_change)) + geom_bar(stat = "identity", alpha = 0.8, color = "grey50", fill = "grey50") + theme_minimal() + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid = element_blank()) + theme(panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", linewidth = 0.5)) + ggtitle("Females") + theme(plot.title = element_text(size = 14, hjust = 0.5)) + theme(legend.position = "bottom") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12)) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-400, 600)) + xlab("Change in Average Excess AAMR\nFrom Decline to Non-Decline Periods (%)") + theme(axis.ticks.x = element_line(color = "black")) + geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") + ylab("")

fig3 <- cowplot::plot_grid(mpct_fig, fpct_fig, nrow = 1, labels = c("A", "B"), label_size = 20) 


m_avg_rate <- m_avg_rate %>% left_join(m_pct_change) %>% mutate(pct_change = round(pct_change, 0), abs_change = round(abs_change, 2))

f_avg_rate <- f_avg_rate %>% left_join(f_pct_change) %>% mutate(pct_change = round(pct_change, 0), abs_change = round(abs_change, 2))

fig2a <- m_avg_rate  %>% ggplot( aes(y = name, x = slope, fill = plateau)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.75) +
    labs(y = "", x = "Average Change in Annual Excess AAMR", fill = "") +   geom_errorbar(aes(xmin = lower, xmax = upper), position = position_dodge(width = 0.8), width = 0.2, color = "grey60", size = 0.55) + geom_text(aes(y=name, x=4, label = paste0("(", abs_change, ", ", pct_change, "%)")), size = 3.5) + 
    theme_minimal() + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid = element_blank()) + geom_vline(xintercept = 0, color = "grey30") + theme(panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", linewidth = 0.5)) + scale_fill_manual(values = c("Decline" = cpal[1], "Non-decline" = cpal[2]), labels = c("Decline" = "Overall Excess AAMR Decline (1999-2011)", "Non-decline" = "No Decline (2012-2019)"), guide = guide_legend(nrow = 2)) + ggtitle("Males") + theme(plot.title = element_text(size = 14, hjust = 0.5)) + theme(legend.position = "bottom") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12)) + xlim(-5, 5)

fig2b <- f_avg_rate %>% ggplot( aes(y = name, x = slope, fill = plateau)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.75) +
    labs(y = "", x = "Average Change in Annual Excess AAMR", fill = "") +   geom_errorbar(aes(xmin = lower, xmax = upper), position = position_dodge(width = 0.8), width = 0.2, color = "grey60", size = 0.55) + geom_text(aes(y=name, x=4, label = paste0("(", abs_change, ", ", pct_change, "%)")), size = 3.5) + 
    theme_minimal() + theme(panel.border = element_rect(color = "black", fill = "transparent"), panel.grid = element_blank()) + geom_vline(xintercept = 0, color = "grey30") + theme(panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", linewidth = 0.5)) + scale_fill_manual(values = c("Decline" = cpal[1], "Non-decline" = cpal[2]), labels = c("Decline" = "Overall Excess AAMR Decline (1999-2015)", "Non-decline" = "No Decline (2016-2019)"), guide = guide_legend(nrow = 2)) + ggtitle("Females") + theme(plot.title = element_text(size = 14, hjust = 0.5)) + theme(legend.position = "bottom") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12)) + xlim(-5, 5)

fig2 <- plot_grid(fig2a, fig2b, nrow = 1, labels = c("A", "B"), label_size = 20)

cowplot::plot_grid(fig2a, fig2b, nrow = 1, labels = c("A", "B"), label_size = 18) %>% ggsave(plot = ., filename = "results/decline-fig2.pdf", device = cairo_pdf, height = 9, width = 15)

#cowplot::plot_grid(fig2a, cowplot::plot_grid(mpct_fig, NULL, labels = c("B", ""), label_size = 18, rel_heights = c(1, 0.07), nrow = 2), fig2b, cowplot::plot_grid(fpct_fig, NULL, labels = c("D", ""), label_size = 18, rel_heights = c(1, 0.1), nrow = 2), nrow = 2, ncol = 2, labels = c("A", "", "C", ""), label_size = 18) %>% ggsave(plot = ., filename = "results/decline-fig2.pdf", device = cairo_pdf, height = 18, width = 15, units = "in")


#fig2 %>% ggsave(filename = "results/decline-fig2.pdf", plot = ., device = cairo_pdf, height = 10, width = 15, units = "in")
