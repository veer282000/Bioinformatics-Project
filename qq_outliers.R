qq_outliers <- function(vec) {
  
  # Checks
  if (!is.numeric(vec) | !is.vector(vec)) stop("Please provide a numeric vector")
  if (length(vec) < 3) stop("The minimum number of data points required by this function is 3")
  
  
  # Add zscores
  data = data.frame(value = vec, 
                    zscore = scale(vec)) %>% 
                arrange(desc(abs(zscore)))
  
  # Remove points 1 by 1 from largest to smallest and collect qq slope changes
  slopes = data.frame(trim_count=NA,slope=NA)
  for (i in 1:(nrow(data)-1)) {
    x = data[i:nrow(data),]
    probabilities = (1:nrow(x))/(nrow(x)+1)
    norm_quants = qnorm(probabilities, mean(x$value), sd(x$value))
    coeff = lm(sort(x$value) ~ norm_quants)[[1]]
    slopes[i,] = c(i-1,coeff[2])
  }
  
  # Get magnitude of change from slope to slope
  slopes$step = abs(lag(slopes$slope)-slopes$slope)
  
  # Remove trim_count==0. In extreme cases this will remove points at the 
  # low Z end of the data where there is zero variance
  slopes = slopes %>% filter(!is.na(step))
  
  # When all data points are the same, slope is undefined and will be filtered out in the
  #  the above line. This calc will then throw an error
  if (nrow(slopes)) {
    # Get relative influence of each step for associated lm
    slopes$cooksd = cooks.distance(lm(slopes$step ~ slopes$trim_count))
  }
  
  # Join with data
  while (nrow(slopes) < nrow(data)) {
    slopes = slopes %>% add_row(trim_count=NA,slope=NA)
  }
  data = bind_cols(data, slopes)
  
  # Add outlier indicator
  data = data %>% 
              	# Anything with a cook's distance larger than the mean is flagged
                mutate(qq_out = step > mean(cooksd, na.rm = T)) %>% 
              	# Filter to only flag the most extreme values. Once a value is flagged as false (not an outlier), all subsequent points will be ignored
                mutate(qq_out = cumsum(qq_out) == (1:n()))
  
  # Set NAs to FALSE
  data$qq_out[is.na(data$qq_out)] = F
  
  return(data)
}

