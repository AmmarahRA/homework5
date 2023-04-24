pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary, fixest)

final.data <- read_tsv('data/output/acs_medicaid.txt')

final.data <- final.data %>% filter(State != "Puerto Rico", State != "District of Columbia")
final.data <- final.data %>% mutate(insured = ins_employer + ins_direct +
                                      ins_medicaid + ins_medicare)
final.data <- final.data %>% mutate(perc_dir = (ins_direct/adult_pop)*100,
                                    perc_empl = (ins_employer/adult_pop)*100,
                                    perc_mcaid = (ins_medicaid/adult_pop)*100,
                                    perc_ins = (insured/adult_pop)*100,
                                    perc_unins = (uninsured/adult_pop)*100)

#1

dir_fig<- final.data %>% group_by(year) %>% summarize(mean=mean(perc_dir)) %>%
  ggplot(aes(x=year,y=mean)) + 
  geom_line() + geom_point() + 
  theme_bw() +
  labs(x="Year", y="Fraction with Direct Purchase") +
  geom_vline(xintercept=2013.5, color="red") +
  theme(plot.title = element_text(hjust = 0.5)) 
dir_fig

#3

mcaid_fig <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_mcaid)) %>%
  ggplot(aes(x=year,y=mean)) + 
  geom_line() + geom_point() + 
  theme_bw() +
  labs(x="Year", y="Fraction with Medicaid") +
  geom_vline(xintercept=2013.5, color="red") +
  theme(plot.title = element_text(hjust = 0.5)) 
mcaid_fig

#4

plot_data <- final.data %>% filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarise(mean=mean(perc_unins)) 

unins_fig <- ggplot(data = plot_data, aes(x = year, y = mean, 
                                           group = expand_ever,
                                           linetype = expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = plot_data %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(x = "Year", y = "Fraction Uninsured") +
  theme(plot.title = element_text(hjust = 0.5)) 
unins_fig

#5

data_5 <- final.data %>% filter((year == 2012 | year == 2015) & !is.na(expand_ever)) %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  mutate(post = (year >= 2014),
         pre = (year <= 2013),
         treat = post*expand_ever) %>%
  group_by(expand_ever)

tab_5.1 <- data_5 %>% filter(expand_ever == 'TRUE') %>% 
  group_by(year) %>% summarise(avg_unins = mean(perc_unins)) 

tab_5.2 <- data_5 %>% filter(expand_ever == 'FALSE') %>% 
  group_by(year) %>% summarise(avg_unins = mean(perc_unins))

tab_5 <- data.frame(avg_unins = c("Expansion", "Non-Expansion"),
                    before = c(round(tab_5.1$avg_unins[1], 2), round(tab_5.2$avg_unins[1], 2)),
                    after = c(round(tab_5.1$avg_unins[2], 2), round(tab_5.2$avg_unins[2], 2)))

#6

reg.data <- final.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever)

m.dd <- lm(perc_unins ~ post + expand_ever + treat, data=reg.data)
modelsummary(m.dd)

#7

mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.data)
summary(mod.twfe)

ate_3 <-  feols(perc_unins~ treat | State + year,
                 cluster=~State,
                 data=reg.data)
modelsummary(ate_3)

#8

reg.data3 <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==TRUE, year-expand_year, -1),
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))

mod.twfe2 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.data3)
summary(mod.twfe2)

reg.data3.2 <- final.data %>%mutate(treat = case_when(
  year >= expand_year & !is.na(expand_year) ~ 1, 
  is.na(expand_year) ~ 0,
  year < expand_year & !is.na(expand_year) ~ 0)
)

ate_4 <- feols(perc_unins ~ treat | State + year, data = reg.data3.2)

modelsummary(ate_4)

#9

fig_9 <- iplot(mod.twfe, 
      xlab = 'Time to treatment',
      main = '')
fig_9

#10

fig_10 <- iplot(mod.twfe2, 
               xlab = 'Time to treatment',
               main = '')
fig_10

save.image("homework5.Rdata")

