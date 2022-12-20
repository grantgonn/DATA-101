## creating vector v 
b = c(3.1, -1, 4.3, 8.2, 7.2, 2.2)

##o-1 transformation
b_min = min(b)
b_max = max(b)
(b- b_min) / (b_max-b_min)

##z score standardization
b_mean = mean(b)
b_sd = sd(b)
(b - b_mean)/ b_sd
