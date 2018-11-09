

#***********************************************
# Analyzing ED data 
#***********************************************

library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(GGally)

rm(list = ls())

# 1) read in data: ------------------------
df1.ed.data <- read.csv(here("results", 
                             "output from src", 
                             "ed-data.csv")) %>% 
    clean_names() %>% 
    
    # change data types: 
    mutate(ed_los_minutes = as.character(ed_los_minutes) %>% 
               as.integer, 
           ad_los_days = as.character(ad_los_days) %>% 
               as.integer, 
           age = as.character(age) %>% 
               as.integer)

# summary functions: 
str(df1.ed.data)
summary(df1.ed.data)
head(df1.ed.data)



# 2) visualize data: ---------------------

# > 2.1) pairs plot: ----
p1.pairs <- df1.ed.data %>%
    select(ed_los_minutes, 
           ad_los_days, 
           age, 
           admission_nursing_unit_code) %>% 
    
    ggpairs(cardinality_threshold = 27); p1.pairs

# save output 
ggsave(here("results", 
            "output from src", 
            "01_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 


# >> 2.1.1) calculate pearsons and spearman cor: -----
cor(df1.ed.data$ed_los_minutes, 
    df1.ed.data$ad_los_days, 
    method = "pearson",  # measures strength of *linear* relationship 
    use = "complete.obs")


cor(df1.ed.data$ed_los_minutes, 
    df1.ed.data$ad_los_days, 
    method = "spearman",  # more general than pearson
    use = "complete.obs")

# difference between pearson's and spearman's correlations: 
x <- 1:100
y <- exp(x)  # y = e^x, the exponential function 
plot(y~x)  # this is not a ggplot function, it's "base" R. It's useful for quick, simple graphs 
abline(lm(y~x), col = "red")

cor(y,x, method = "pearson")  # 0.252 => gives the impression that there's a weak relationship
cor(y,x, method = "spearman")  # 1.00 ==> actually there is a perfect correlation in one sense: 
                               #    every increase in x is always associated with an increase in y





# > 2.2) Acute LOS vs ED LOS: ------ 
p2.ed.and.ad.los <- df1.ed.data %>% 
    ggplot(aes(x = ed_los_minutes, 
               y = ad_los_days)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth() + 
    facet_wrap(~triage_acuity_code); p2.ed.and.ad.los

# save output 
ggsave(here("results", 
            "output from src", 
            "02_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 

# why no relationship? 
# Ans. LOS is an "output" var, not an input? 


# > 2.3) Acute LOS vs ED LOS by age : ------ 
p3.ed.and.ad.los.age.groups <- df1.ed.data %>% 
    filter(triage_acuity_code == "3", 
           age >= 60, 
           age <= 65) %>% 
    
    ggplot(aes(x = ed_los_minutes, 
               y = ad_los_days)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(aes(group = age), 
                se = FALSE, 
                alpha = 0.2) + 
    
    # zoom in on the x-axis: 
    coord_cartesian(xlim = c(0, 600)) + 
    
    facet_wrap(~triage_acuity_code); p3.ed.and.ad.los.age.groups

# save output 
ggsave(here("results", 
            "output from src", 
            "03_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 



# > 2.4) Acute los vs age: ----- 
p4.age.and.ad.los <- df1.ed.data %>% 
    ggplot(aes(x = age, 
               y=  ad_los_days)) +  # log transform to get 
    geom_jitter(alpha = 0.1) + 
    geom_quantile(); p4.age.and.ad.los  # quantile regression: gives median, 75th and 25th percentiles 

# save output 
ggsave(here("results", 
            "output from src", 
            "04_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 


# > 2.5) log(LOS) vs age: -------
p4.1.logged <- df1.ed.data %>% 
    ggplot(aes(x = age, 
               y=  ad_los_days)) +  # log transform to get 
    # geom_point(alpha = 0.2) + 
    geom_jitter(alpha = 0.1) +
    scale_y_log10(breaks = c(1, 5, 10, 30, 60, 100)) + 
    geom_quantile() + 
    facet_wrap(~triage_acuity_code); p4.1.logged  # quantile regression: gives median, 75th and 25th percentiles 

# save output 
ggsave(here("results", 
            "output from src", 
            "04-01_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 


# 2.6) LOS vs ctas: 
p5.los.ctas <- df1.ed.data %>% 
    ggplot(aes(x = triage_acuity_code, 
               y = ad_los_days)) + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 colour = "firebrick"); p5.los.ctas

# save output 
ggsave(here("results", 
            "output from src", 
            "05_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 


# not very helpful, is it? try logging the y-axis: 

# 2.7) log(LOS) vs ctas: 
p5.1.logged <- df1.ed.data %>% 
    
    mutate(age.group = case_when(
        age < 20 ~ 1, 
        age >=20 & age <60 ~ 2, 
        age >= 60 ~ 3) %>% 
            as.factor) %>% 
    
    # remove NA value of age
    filter(!is.na(age)) %>% 
    
    ggplot(aes(x = triage_acuity_code, 
               y = ad_los_days)) + 
    
    geom_boxplot() + 
    
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 colour = "firebrick") + 
    
    # take logarithm of y-axis: 
    scale_y_log10(breaks = c(1, 2, 3, 4, 5, 10, 30, 60)) +  # by default, labels are in original units, not logged units
    
    # zoom in on y-axis: 
    coord_cartesian(ylim = c(0.1, 60)) + 
    
    facet_wrap(~age.group); p5.1.logged

# note significant positive skews of age.group 1 

# save output 
ggsave(here("results", 
            "output from src", 
            "05-01_ed-date.jpeg"))  # by default ggsave saves the last plot that was called 



# 3) save outputs: --------------------
pdf(here("results", 
         "output from src", 
         "lgh-ed-data-analysis.pdf"))
p1.pairs
p2.ed.and.ad.los
p3.ed.and.ad.los.age.groups
p4.age.and.ad.los
p4.1.logged
p5.los.ctas
p5.1.logged
dev.off()



