pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

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

filt_data <- final.data %>% filter(expand_year == 2014) %>%
  group_by(expand_ever, year) %>% summarise(mean=mean(perc_unins) 



final.data %>% filter(expand_year == 2014) %>% 
  group_by(expand_ever, year) %>% 
  summarise(avg_unins = mean(perc_unins)) %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(x = year, y = avg_unins)) +
  labs(title = "Share of Uninsured Population", x = "Year", y = "Fraction Uninsured") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


ins.plot.dat <- final.data %>% filter(expand_year == 2014) %>%
  group_by(expand_ever, year) %>% summarise(mean=mean(perc_unins) 
 
unins.fig <-  ggplot(data=ins.plot.dat, aes(x=year,y=mean,
  geom_line() + geom_point() + theme_bw() +
geom_vline(xintercept=2013.5, color="red") +
geom_text(data = ins.plot.dat %>% filter(year  2016)
 aes(label = c("Non-expansion","Expansion"),  x = year + 1, y = mean)) +
 guides(linetype="none") +
 labs( x="Year", y="Fraction Uninsured",title="Share of Uninsured over Time")                                                                                                  
                                                                                                                                  






