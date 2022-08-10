# The College Scorecard was released at the start of September 2015. Among colleges
# that predominantly grant bachelor’s degrees, did the release of the Scorecard shift
# student interest to high-earnings colleges relative to low-earnings ones (as proxied
# by Google searches for keywords associated with those colleges)?

library(tidyverse)
library(fixest)
library(vtable)
library(Ecdat)
library(wooldridge)
library(ggplot2)
library(ggstance)
library(NHANES)
library(dagitty)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

#   READING SCORECARD & NAMES

scorecard_git <- read_csv('../su-omsba-data_exploration_assignment-lpowell/data/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv') %>%
  rename(opeid = OPEID) %>%
  rename(unitid = UNITID) %>%
  rename(md_earn_wne_p10 = 'md_earn_wne_p10-REPORTED-EARNINGS')
scorecard <- read_csv('C:/Users/loren/OneDrive/Desktop/SeattleU/5300 Applied Econometrics/Data Exploration Assignment/su-omsba-data_exploration_assignment-lpowell/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv') %>%
  rename(opeid = OPEID) %>%
  rename(unitid = UNITID) %>%
  rename(md_earn_wne_p10 = 'md_earn_wne_p10-REPORTED-EARNINGS')

name_link_git <- read_csv('../su-omsba-data_exploration_assignment-lpowell/data/Lab3_Rawdata/id_name_link.csv')  %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1)
name_link <- read_csv('C:/Users/loren/OneDrive/Desktop/SeattleU/5300 Applied Econometrics/Data Exploration Assignment/su-omsba-data_exploration_assignment-lpowell/Lab3_Rawdata/id_name_link.csv')  %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1)



#TUT_file_list <- c(list.files(path = 'C:/Users/loren/OneDrive/Desktop/SeattleU/5300 Applied Econometrics/Data Exploration Assignment/su-omsba-data_exploration_assignment-lpowell/Lab3_Rawdata',
#           pattern = 'trends_up_to_', full.names = TRUE))

TUT_files <- list.files(path = 'C:/Users/loren/OneDrive/Desktop/SeattleU/5300 Applied Econometrics/Data Exploration Assignment/su-omsba-data_exploration_assignment-lpowell/Lab3_Rawdata',
                          pattern = 'trends_up_to_', full.names = TRUE)
TUT_files_git <- list.files(path = '../su-omsba-data_exploration_assignment-lpowell/data/Lab3_Rawdata/Lab3_Rawdata',
                       pattern = 'trends_up_to_', full.names = TRUE)


TUT_map <- map_df(TUT_files, read_csv)


# *** CURRENT BASE DATA SET ***
#str_sub(string = TUT_map$monthorweek, start = 0, end = 10)

TUT_mutate <- TUT_map %>%
    mutate(start_mo_week = ymd(str_sub(monthorweek, start = 0, end = 10))) %>%
    group_by(schname, keyword)
#   mutate(start_mo = floor_date((ymd(str_sub(monthorweek, start = 0, end = 10))), 'month'))

#  Mean & SD of all index's

index_mean <- mean(as.numeric(TUT_mutate$index), na.rm = TRUE)
index_sd <- sd(as.numeric(TUT_mutate$index), na.rm = TRUE)


# Standardized TUT 

TUT_standard <- TUT_mutate %>%
  group_by(schname, keyword) %>%
  mutate(index_stand = ((index - index_mean)/index_sd)) %>%
  mutate(start_year = floor_date(start_mo_week, 'year'))
# summarize(m = mean(as.numeric(TUT_standard$index_stand), na.rm = TRUE))

# BIG MERGE

big_merge <-  inner_join(name_link, TUT_mutate, by = 'schname')
big_merge <- inner_join(big_merge, scorecard, by = 'opeid')

big_merge_stand <- inner_join(name_link, TUT_standard, by = 'schname')
big_merge_stand <- inner_join(big_merge_stand, scorecard, by = 'opeid')

perc_merge <- big_merge_stand %>%
  mutate(inc_group = case_when(
    between(as.numeric(md_earn_wne_p10), 0, 31100) ~ "Low",
    between(as.numeric(md_earn_wne_p10), 31101, 44600) ~ "Med",
    between(as.numeric(md_earn_wne_p10), 44601, 166200) ~ "High",
    FALSE ~ NA_character_)) %>%
  group_by(schname, keyword, inc_group)

merge2013 <- perc_merge %>%
  filter(start_year == '2013-01-01') #%>%

merge2014 <- perc_merge %>%
  filter(start_year == '2014-01-01')# %>%

merge2015 <- perc_merge %>%
  filter(start_year == '2015-01-01')

merge2016 <- perc_merge %>%
  filter(start_year == '2016-01-01')


# Use percentile values to compare index rends
quantile_dat <- as.numeric(big_merge_stand$md_earn_wne_p10)
quant_dat <- na.omit(quantile_dat)
quantile(quant_dat, c( 0.25, 0.75, 1))

# Rough distribution plot of 10 year earnings associated with index scores
quant_plot <- plot(quantile(quant_dat, c( 0.25, 0.75, 1 )))
('../su-omsba-data_exploration_assignment-lpowell/quant_plot.png')

low_inc <- (0:31100)
med_inc <- (31101:44600)
high_inc <- (44601:166200)



# REGRESSION TESTS

#reg1 <- feols(as.numeric(GRAD_DEBT_MDN_SUPP) ~ LOCALE, big_merge)
#reg2 <- feols(as.numeric(NPT4_PRIV) ~ LOCALE, big_merge)
#reg3 <- feols(as.numeric(NPT4_PRIV) ~ STABBR, big_merge)
#reg4 <- feols(as.numeric(md_earn_wne_p10) ~ INSTNM, big_merge)


#etable(reg1, reg2)
#etable(reg3)
#etable(reg4)


# MEDIAN EARNINGS ASSOCIATED W/ DEBT

med_earn_debt_school_df <- distinct(select(big_merge_stand, schname, schid, md_earn_wne_p10, GRAD_DEBT_MDN_SUPP))
med_earn_debt_school_fe_reg <- feols(as.numeric(GRAD_DEBT_MDN_SUPP) ~ md_earn_wne_p10 | index_stand, data = big_merge_stand)
summary(med_earn_debt_school_fe_reg)


#etable(med_earn_debt_school_fe_reg)
#ggplot(med_earn_debt_school_fe_reg)

range(date_index$start_year)


# Index by time
date_index <- select(big_merge_stand, schname, schid, start_mo_week, start_year, index, index_stand) %>%
  mutate(start_year = floor_date(start_mo_week, 'year')) %>%
  group_by(start_year)

date_index_reg <- feols(index_stand ~ start_mo_week + schid, data = date_index)
year_index_reg <- feols(index_stand ~ start_year + schid, data = date_index)
etable(date_index_reg, year_index_reg)


school_index_fe_reg <- feols(index_stand ~ schid | start_year, data = date_index)
school_index_reg <- feols(index_stand ~ schid + start_year, data = date_index)
etable(school_index_reg, school_index_fe_reg)


# YEAR START EARNS WERE A SUCCESS  *****
year_start_earn <- feols(as.numeric(md_earn_wne_p10) ~ start_year, data = big_merge_stand)
year_start_earn_con_ind <- feols(as.numeric(md_earn_wne_p10) ~ start_year | index_stand, data = big_merge_stand)
etable(year_start_earn, year_start_earn_con_ind)
summary(year_start_earn_con_ind)

school_enroll_inc <- feols(schid ~ start_mo_week + index_stand + inc_group, data = perc_merge)
school_enroll_inc_fe <- feols(schid ~ start_mo_week | index_stand + inc_group + GRAD_DEBT_MDN_SUPP, data = perc_merge)
etable(school_enroll_inc, school_enroll_inc_fe)

school_enroll_inc <- feols(schid ~ start_mo_week + index_stand + inc_group + GRAD_DEBT_MDN_SUPP, data = perc_merge)



for schname in perc_merge





ggplot()+
  geom_point(data = big_merge_stand, mapping = aes(x = start_year, y = index_stand, color = schname))
  #geom_line(data = big_merge_stand, mapping = aes(x = start_year, y = md_earn_wne_p10))


ggplot() + 
  geom_point(data = big_merge_stand, mapping = aes(x = start_year, y = mean(as.numeric(index - index_stand))))



date_index_bar <- date_index %>%
  select(index, start_year) %>%
  ggplot(aes(x = start_year)) + geom_bar()

date_index_line <- date_index %>%
  ggplot(aes(x = start_year, y = index-index_stand, color = schname)) + geom_line()



enroll_date_earn <- big_merge_stand %>%
  ggplot(aes(x = start_year, y = md_earn_wne_p10, color = schname)) + geom_line()

enroll_date_earn_bar <- big_merge_stand %>%
  select(schname, start_year, mean(md_earn_wne_p10)) %>%
  ggplot(aes(x = start_year)) + geom_bar()



date_index2013 <- date_index %>%
  filter(start_year == '2013-01-01') #%>%
#ggplot(aes(x = start_year, y = index-index_stand, color = start_year)) + geom_line()

date_index2014 <- date_index %>%
  filter(start_year == '2014-01-01')# %>%
#ggplot(aes(x = start_year, y = index-index_stand, color = start_year)) + geom_line()

date_index2015 <- date_index %>%
  filter(start_year == '2015-01-01')

date_index2016 <- date_index %>%
  filter(start_year == '2016-01-01')


# RUNS AND IS USEFUL BASE BUT UGLY
ggplot() + 
  geom_line(data = date_index2013, mapping = aes(x = month(start_mo_week), y = mean(index_stand), color = '2013'))+
  geom_line(data = date_index2014, mapping = aes(x = month(start_mo_week), y = mean(index_stand), color = '2014'))+
  geom_line(data = date_index2015, mapping = aes(x = month(start_mo_week), y = mean(index_stand), color = '2015'))+
  geom_line(data = date_index2016, mapping = aes(x = month(start_mo_week), y = mean(index_stand), color = '2016'))


# observation count by schoolID  -**
ggplot()+
  geom_bar(data = perc_merge, mapping = aes(x = inc_group), na.rm = FALSE)
ggplot()+
  geom_point(data = perc_merge, mapping = aes(x = month(start_mo_week), y = index_stand), na.rm = FALSE)


inc_group_low<- perc_merge %>%
  filter(inc_group == 'Low') 
inc_group_med<- perc_merge %>%
  filter(inc_group == 'Med')
inc_group_high<- perc_merge %>%
  filter(inc_group == 'High')

ggplot() + 
  geom_line(data = inc_group_low, mapping = aes(x = start_year, y = index_stand, color = 'low'))+
  geom_line(data = inc_group_med, mapping = aes(x = start_year, y = index_stand, color = 'med'))+
  geom_line(data = inc_group_high, mapping = aes(x = start_year, y = index_stand, color = 'high'))


index_enroll_reg_perc <- feols(index_stand ~ start_mo_week| md_earn_wne_p10 + GRAD_DEBT_MDN_SUPP + inc_group, data = perc_merge)
etable(index_enroll_reg_perc)



ggplot() + 
  geom_bar(data = date_index2013, mapping = aes(x = month(start_mo_week), color = '2013'))+
  geom_bar(data = date_index2014, mapping = aes(x = month(start_mo_week), color = '2014'))+
  geom_bar(data = date_index2015, mapping = aes(x = month(start_mo_week), color = '2015'))+
  geom_bar(data = date_index2016, mapping = aes(x = month(start_mo_week), color = '2016'))



date_index2013_2<- date_index %>%
  select(schname, schid, start_year, index_stand) %>%
  filter(start_year == '2013-01-01') #%>%

date_index2014_2 <- date_index %>%
  select(schname, schid, start_year, index_stand) %>%
  filter(start_year == '2014-01-01')# %>%

date_index2015 <- date_index %>%
  filter(start_year == '2015-01-01')

date_index2016 <- date_index %>%
  filter(start_year == '2016-01-01')

ggplot() + 
  geom_bar(data = date_index2013_2, mapping = aes(x = schid, color = '2013'))+
  geom_bar(data = date_index2014_2, mapping = aes(x = schid, color = '2014'))
  #geom_bar(data = date_index2015, mapping = aes(x = month(start_mo_week), color = '2015'))+
  #geom_bar(data = date_index2016, mapping = aes(x = month(start_mo_week), color = '2016'))


#date_index2015
#date_index2016

date_index_line <- date_index %>%
  ggplot(aes(x = start_year, y = mean(index_stand), color = schname)) + geom_line()

# BASIC GRAPH TESTS

ggplot(big_merge_stand, aes(x = GRAD_DEBT_MDN_SUPP, y = md_earn_wne_p10)) + geom_point()
ggplot(big_merge_stand, aes(x = md_earn_wne_p10, y = GRAD_DEBT_MDN_SUPP))

ggplot(big_merge_stand, aes(x = schname, y = as.numeric(md_earn_wne_p10))) + geom_point()



SELECT3 <- distinct(select(big_merge_stand, schname, start_year, md_earn_wne_p10, GRAD_DEBT_MDN_SUPP, index, index_stand,)) %>%
  filter(GRAD_DEBT_MDN_SUPP != 'PrivacySuppressed')

# index ranking X median earnings  no data points 
ggplot(big_merge_stand, aes(x = index_stand, y = mean(md_earn_wne_p10)))+
  geom_point()

