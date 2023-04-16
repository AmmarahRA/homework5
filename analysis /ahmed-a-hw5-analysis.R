pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary)

final.data <- read_tsv('data/output/acs_medicaid.txt')

final.data <- final.data %>% mutate(insured = ins_employer + ins_direct +
                                      ins_medicaid + ins_medicare)
final.data <- final.data %>% mutate(perc_dir = (ins_direct/insured)*100,
                                    perc_empl = (ins_employer/insured)*100,
                                    perc_mcaid = (ins_medicaid/insured)*100,
                                    perc_ins = (insured/adult_pop)*100,
                                    perc_unins = (uninsured/adult_pop)*100)

#1

dir_fig<- final.data %>% group_by(year) %>% summarize(mean=mean(perc_dir)) %>%
  ggplot(aes(x=year,y=mean)) + 
  geom_line() + geom_point() + 
  theme_bw() +
  labs(x="Year", y="Fraction with Direct Purchase", title="Share of Direct Purchase Insurance over Time") +
  geom_vline(xintercept=2013.5, color="red") +
  theme(plot.title = element_text(hjust = 0.5)) 
dir_fig

#3

mcaid_fig <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_mcaid)) %>%
  ggplot(aes(x=year,y=mean)) + 
  geom_line() + geom_point() + 
  theme_bw() +
  labs(x="Year", y="Fraction with Medicaid", title="Share of Medicaid Insurance over Time") +
  geom_vline(xintercept=2013.5, color="red") +
  theme(plot.title = element_text(hjust = 0.5)) 
mcaid_fig

#4

plot_data <- final.data %>% filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarise(mean=mean(perc_unins)) 

unins.plot <- ggplot(data = plot_data, aes(x = year, y = mean, 
                                           group = expand_ever,
                                           linetype = expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = plot_data %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(x = "Year", y = "Fraction Uninsured", title = "Share of Uninsured over Time") +
  theme(plot.title = element_text(hjust = 0.5)) 
unins.plot

#5

data_5 <- final.data %>% filter((year == 2012 | year == 2015)) %>%
  mutate(post = (year >= 2014),
         pre = (year <= 2013),
         treat = post*expand_ever) %>%
  group_by(expand_ever)

tab_5.1 <- data_5 %>% filter(expand_ever == 'TRUE') %>% 
  group_by(year) %>% summarise(avg_unins = mean(perc_unins)) 

tab_5.2 <- data_5 %>% filter(expand_ever == 'FALSE') %>% 
  group_by(year) %>% summarise(avg_unins = mean(perc_unins))

tab_5 <- data.frame(avg_unins = c("Treated", "Control"),
                    after = c(tab_5.1$avg_unins[2], tab_5.2$avg_unins[2]),
                    before = c(tab_5.1$avg_unins[1], tab_5.2$avg_unins[1]))

#6

reg.data <- final.data %>% filter(expand_ever == 'TRUE' & (year == 2012 | year == 2015)) %>%
  mutate(post = (year>=2014),
         treat=post*expand_ever)

m.dd.t <- lm(perc_unins ~ post + expand_ever + treat, data=reg.data)
summary(m.dd.t)

reg.data2 <- final.data %>% filter(expand_ever == 'FALSE' & (year == 2012 | year == 2015)) %>%
  mutate(post = (year>=2014),
         treat=post*expand_ever)

m.dd.f <- lm(perc_unins ~ post + expand_ever + treat, data=reg.data2)
summary(m.dd.f)






