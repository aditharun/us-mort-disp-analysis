library(tidyverse)
library(cowplot)

ageadj <- readRDS("results/yearly-aamrs.rds")

cpal <- c("#374e55", "#df8f44")


order.of.conditions <- ageadj %>% group_by(name) %>% summarize(x = mean(ageadj_excess)) %>% arrange(desc(x)) %>% pull(name)
m_ageadj_stack <- ageadj %>% filter(gender == "Male") %>% mutate(name = factor(name, levels = order.of.conditions)) %>% group_by(name) %>% group_split()

m_pltagestack <- lapply(m_ageadj_stack, function(astack) astack %>% filter(gender == "Male") %>% mutate(plateau = ifelse(year <= 2011, "Decline", "Non-Decline")) %>% ggplot(aes(x=year, y=ageadj_excess, color = plateau)) + geom_point(size = 3) + geom_line(size = 0.85, color = "grey65") + theme_minimal() + theme(legend.position = "none") + geom_vline(xintercept = 2012, linetype = "dashed", color = "grey70") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 10)) + ggtitle(unique(astack$name)) + theme(plot.title = element_text(hjust = 0.5, size = 14)) + scale_color_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2])) ) 

fig1 <- cowplot::plot_grid(plotlist = m_pltagestack, nrow = 6, ncol = 3)


f_ageadj_stack <- ageadj %>% filter(gender == "Female") %>% mutate(name = factor(name, levels = order.of.conditions)) %>% group_by(name) %>% group_split()

f_pltagestack <- lapply(f_ageadj_stack, function(astack) astack %>% filter(gender == "Female") %>% mutate(plateau = ifelse(year <= 2015, "Decline", "Non-Decline")) %>% ggplot(aes(x=year, y=ageadj_excess, color = plateau)) + geom_point(size = 3) + geom_line(size = 0.85, color = "grey65") + theme_minimal() + theme(legend.position = "none") + geom_vline(xintercept = 2016, linetype = "dashed", color = "grey70") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 10)) + ggtitle(unique(astack$name)) + theme(plot.title = element_text(hjust = 0.5, size = 14)) + scale_color_manual(values = c("Decline" = cpal[1], "Non-Decline" = cpal[2])) ) 

fig2 <- cowplot::plot_grid(plotlist = f_pltagestack, nrow = 6, ncol = 3)

fig1 <- plot_grid(ggplot() + theme_void() + ggtitle("Excess AAMR by Top Causes of Death for Males") + theme(plot.title = element_text(hjust = 0.5, size = 14)), fig1, nrow = 2, rel_heights = c(0.02, 1))

fig2 <- plot_grid(ggplot() + theme_void() + ggtitle("Excess AAMR by Top Causes of Death for Females") + theme(plot.title = element_text(hjust = 0.5, size = 14)), fig2, nrow = 2, rel_heights = c(0.02, 1))


ggsave(filename = "results/male-yearly-eaamrs.pdf", plot = fig1, device = cairo_pdf, height = 16, width = 12, units = "in")
ggsave(filename = "results/female-yearly-eaamrs.pdf", plot = fig2, device = cairo_pdf, height = 16, width = 12, units = "in")

