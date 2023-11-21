library(tidyverse)
library(caret)

thyroid = read.csv('annthyroid_21feat_normalised.csv')
ggplot(thyroid, aes(0, Dim_20, color=class)) + geom_jitter() #+ scale_color_manual(values = c('black','red'))

# Bootstrap testing
# library(bootstrap)
# bootstrap(norm_100$value, 2, qq_outliers)

# I think this will end up being easier to do manually. I don't see a way to do subsetting with the above function
for (i in 1:2) {
  x = norm_100 %>% sample_n(5)
  x_out = qq_outliers(x$value)
  x = x %>% mutate(qq_out = x_out$qq_out)
  
  cm = confusionMatrix(factor(x$qq_out, levels = c(T,F)), 
                       factor(x$outlier, levels = c(T,F)))
  
  }
