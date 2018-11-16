

#*********************************************************
# Function to produce 2 time series - with and without trend 
#*********************************************************

# define function: -------------
generate.ts_function <- function(sd1 = 1, 
                                 sd2 = 4, 
                                 trend2 = .09, 
                                 seed = TRUE){
    # input: standard deviations of 2 time series,  
    #   size of trend slope, and argument to determine 
    #   whether to generate random graph or a specific one 
    
    # output: 2 side by side ggplots 
    
    library(dplyr)
    library(tidyr)
    
    # set the random seed. This ensures we always get same 
    #   result
    if (seed == TRUE){
        set.seed(7)
    }
    
    # creat a random walk without drift: 
    rwalk <- arima.sim(model = list(order = c(0, 0, 0)), 
                       n = 101, 
                       sd = sd1) %>% as.numeric
    
    # rwalk with drift: 
    rwalk.drift <- arima.sim(model = list(order = c(0, 1, 0)), 
                             n = 100, 
                             mean = trend2, 
                             sd = sd2) %>% as.numeric
    
    # create a df with 2 time series: 
    df1 <- data.frame(timeperiod = seq(1, 101), 
                      var1 = rwalk, 
                      var2 = rwalk.drift) %>% 
        
        # gather() moves column names into a "key" column, 
        #   gathering the column values into a single "value" 
        #   column:  
        
        gather(key = "series",
               `var1`, 
               `var2`,
               value = "observations")
        
    
    # create plots: 
    df1 %>% 
        ggplot(aes(x = timeperiod, 
                   y = observations)) + 
        
        geom_line() + 
        
        facet_wrap(~series)
    
    
    
}



# test the function: --------
generate.ts_function(trend2 = .09, sd2 = 4)

# generate.ts_function(trend2 = .09, sd2 = 4, seed = FALSE)
