

#***********************************************
# Analyzing ED data 
#***********************************************

library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(GGally)

rm(list = ls())

# read in data: 
df1.ed.data <- read.csv(here("results", 
                             "output from src", 
                             "ed-data.csv")) %>% 
    clean_names() %>% 
    mutate(ed_los_minutes = as.character(ed_los_minutes) %>% 
               as.integer, 
           ad_los_days = as.character(ad_los_days) %>% 
               as.integer, 
           age = as.character(age) %>% 
               as.integer)

str(df1.ed.data)
summary(df1.ed.data)
head(df1.ed.data)



# visualize data: 
p1.pairs <- df1.ed.data %>%
    select(ed_los_minutes, 
           ad_los_days, 
           age, 
           admission_nursing_unit_code) %>% 
    
    ggpairs(cardinality_threshold = 27)

p1.pairs


# pearsons and spearman cor: 
cor(df1.ed.data$ed_los_minutes, 
    df1.ed.data$ad_los_days, 
    method = "pearson",  # measures strength of *linear* relationship 
    use = "complete.obs")


cor(df1.ed.data$ed_los_minutes, 
    df1.ed.data$ad_los_days, 
    method = "spearman",  # more general than pearson
    use = "complete.obs")


# plot ad LOS vs ed los 
p2.ed.and.ad.los <- df1.ed.data %>% 
    ggplot(aes(x = ed_los_minutes, 
               y = ad_los_days)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth() + 
    facet_wrap(~triage_acuity_code); p2.ed.and.ad.los

# why no relationship? 
# Ans. LOS is an "output" var, not an input? 


p3.ed.and.ad.los.age.groups <- df1.ed.data %>% 
    filter(triage_acuity_code == "3") %>% 
    
    ggplot(aes(x = ed_los_minutes, 
               y = ad_los_days)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth() + 
    facet_wrap(~triage_acuity_code); p3.ed.and.ad.los.age.groups
