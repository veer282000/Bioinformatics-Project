library(tidyverse)


# Make normal distros. Must run set.seed first for reproducibility
set.seed(12345)
norm_ctr_scl = lapply(1:100, function(i){
  distro = data.frame(value = rnorm(1000), outlier = F)
  distro = rbind(distro, data.frame(value = runif(5, min = -15, max = 15), # Bc values are centered/scaled, the values are in std deviations
                                                outlier = T))
  distro %>% filter(!(outlier & abs(value) <= 2))
})

ggplot(norm_ctr_scl[[1]], aes(0, value, color=outlier)) + geom_jitter() + scale_color_manual(values = c('black','red'))

set.seed(1234)
norm_100 = data.frame(value = rnorm(1000, mean = 100, sd = 10), outlier = F) # Generate distribution
norm_100 = rbind(norm_100, data.frame(value = runif(3, 
                                                    min = 120, # 2 SDs above the mean
                                                    max = 250), # 15 SDs above the mean
                                      outlier = T)) # Add outliers
# norm_100 = rbind(norm_100, data.frame(value = runif(nrow(norm_100)*0.005, 
#                                                     min = mean(norm_100$value) + 2*sd(norm_100$value), 
#                                                     max = mean(norm_100$value) + 15*sd(norm_100$value)), 
#                                       outlier = T)) # Add outliers
# Need to filter 'outliers' that fall in main distribution
ggplot(norm_100, aes(0, value, color=outlier)) + geom_jitter() + scale_color_manual(values = c('black','red'))


# Make other distros



