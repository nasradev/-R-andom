library(ggplot2)
library(dplyr)

mtcars %>% ggplot() +
  geom_point(aes(x = mpg, y = hp))

# let's try to determine what the best line that fits the data is
# using simple gradient descent

data <- mtcars %>% select(mpg, hp)

# the line will look like a*x + b
compute_error <- function(a, b, data) {
  total_error <- 0
  # x = data[1], y = data[2]
  data <- data %>% 
    mutate(error = (.[[2]] - (a*.[[1]] + b)) ** 2)
  return (mean(data$error))
}

# l is the learning rate, this needs to be fairly small but not too small
gradient_descent <- function(a, b, data, l) {
  
  # The gradient is the partial derivative of the total error equation, 
  # i.e. the derivative of sum((y - (a*x + b))^2) / N
  # partially on a, this works out like this: (da = partial derivative over a)
  # da sum((y - (a*x + b))^2) / N => sum(da (y - (a*x + b)^2)) / N (using the sum rule of derivatives)
  # => sum(2*(y - (a*x + b)) * da((y - (a*x + b))) / N (using chain rule and then the power rule)
  # Note that we are deriving on a, so in the last derivative above, y and b become 0, and a*x becomes just a (*x^-1=1)
  # => sum(2*(y - (a*x + b)) * a) / N = 2/N * (a * (y - (a*x + b)))
  data <- data %>% 
    mutate(a_gradient = -2/n() * .[[1]] * (.[[2]] - (a*.[[1]] + b))) %>%
    mutate(b_gradient = -2/n() * (.[[2]] - (a*.[[1]] + b)))
  new_a <- a - l * sum(data$a_gradient)
  new_b <- b - l * sum(data$b_gradient)
  return (c(new_a, new_b))
}


# RUN the gradient descent:
learning_rate <- 0.001
num_iterations <- c(1000, 5000, 20000)

# let's see how well we learn by looking at our results after a few thousand iterations
for (num_it in num_iterations) {
  a <- 0.1
  b <- 0.01
  for(i in 1:num_it) {
    res <- gradient_descent(a, b, data, learning_rate)
    a <- res[1]
    b <- res[2]
  }
  print(paste("Error:", round(compute_error(a,b,data),2)))
  
  data %>% ggplot() +
    geom_point(aes(x = .[[1]], y = .[[2]])) +
    geom_point(aes(x = .[[1]], y = a*.[[1]] + b))
}

# what if we used a larger learning rate?

l_rates <- c(0.0001, 0.001, 0.01, 0.1, 1)

for(l in l_rates) {
  a <- 0.1
  b <- 0.01
  for(i in 1:1000) {
    res <- gradient_descent(a, b, data, l)
    a <- res[1]
    b <- res[2]
  }
  print(paste("Error:", round(compute_error(a,b,data),2)))
  
  data %>% ggplot() +
    geom_point(aes(x = .[[1]], y = .[[2]])) +
    geom_point(aes(x = .[[1]], y = a*.[[1]] + b))
}
# We see that a learning rate that's too big not only does not converge, but it overshoots 
# the local minima and spirals the error rate out of control, going upwards and upwards, to really big nums.

