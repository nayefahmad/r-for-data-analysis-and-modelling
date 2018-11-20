
#****************************************
# using R for regression 
#****************************************

# create new dataframe 
df1.mtcars <- 
    mtcars %>%
    mutate(cyl = as.factor(cyl)) 

# group by cylinder: 
df1.mtcars %>% 
    group_by(cyl) %>% 
    summarize(mean.mpg = mean(mpg))



# lm( ) for regression models 
m1.mtcars <- lm(mpg ~ cyl, data = df1.mtcars)

# m1.mtcars

# looking into the lm object: 
summary(m1.mtcars)



m2.add.weight <- lm(mpg ~ ., data = df1.mtcars)

summary(m2.add.weight)



df1.mtcars %>% 
    ggplot(aes(x = wt, 
               y = mpg)) + 
    geom_point() + 
    
    # draw lines on plot: 
    stat_smooth(aes(group = cyl, 
                    colour = cyl), 
                method = "lm")
    

# save model outputs
library(broom)

model1 <- broom::tidy(m2.add.weight)

write_csv(model1, 
          here::here("results", 
                     "output from src", 
                     "model.csv"))




# model diagnostics
plot(m1.mpg.vs.cyl)


# print plots in a 2 by 2 grid 
par(mfrow = c(2,2))
plot(m1.mpg.vs.cyl)



# comparing models 
m3 <- lm(mpg ~ cyl + wt, data = df1.mtcars)

anova(m1.mtcars, 
      m3)

?step


summary(lm1 <- lm(Fertility ~ ., data = swiss))
slm1 <- step(lm1)
