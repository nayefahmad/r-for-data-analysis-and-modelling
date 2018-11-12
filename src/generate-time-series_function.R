

#*********************************************************
# Function to produce 2 time series - with and without trend 
#*********************************************************

# define function: 
generate.ts_function <- function(sd1 = 2, 
                                 sd2 = 2, 
                                 trend2 = 20){
    # output: 2 side by side ggplots 
    
    library(dplyr)
    library(tidyr)
    
    # create a df with 2 time series: 
    df1 <- data.frame(timeperiod = seq(1, 100), 
                      var1 = rnorm(100, 100, sd1)) %>%  
        mutate(var2 = 100 + trend2*(0:99)) %>% 
        
        # gather moves column names into a "key" column, 
        #   gathering the column values into a single "value" 
        #   column 
        
        gather(key = "series",
               `var1`, 
               `var2`,
               value = "observations")
        
    
    # create plots: 
    df1 %>% 
        ggplot(aes(x = timeperiod, 
                   y = observations)) + 
        
        geom_point() + 
        
        facet_wrap(~series)
    
    
    
}



# test the function: 
generate.ts_function()