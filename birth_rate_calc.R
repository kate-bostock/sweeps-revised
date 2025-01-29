#### ideas
# reduce variability by taking the square, or higher root of the random number - can therefore remain stable at higher s values...
# or other small random number rather than potentially so large?
# if s is large we approach the limit m so fast that it's not possible to curtail it using the logistic map. 
# if we want to initially grow geometrically, then we can only cut it off at m, there aren't enough points in the chart to curve it up to m


#### input values ####
s_vector <- seq(0.1, 3, 0.1)         # s_driver_birth
b0 <- 1                # initial birth rate
m <- 10               # max birth rate
n <- 10                # number of mutations
#set.seed(1)             # choose seed
randomness_on <- 0   # turn randomness on/off 1= On, 0 = Off
checks_df <- data.frame(matrix(ncol = n + 1, nrow = 0))

#### vectors for birth rate values after each mutation, set the first value to the input base birth rate ####
vec1 <- c(b0)
vec2 <- c(b0)
vec3 <- c(b0)
vec4 <- c(b0)
vec5 <- c(b0)

random_number <- function() {
  r <- 4
  while (r>=4) {
    r <- ifelse(randomness_on == 1, rexp(1,1), 1)
  }
  #print(r)
  r
}

#### functions for calculation of the next birth rate  ####
f1 <- function(b, s, m) b <- b * (1 + s * (1 - b / m) * random_number()) 
f2 <- function(b, s, m) b <- b * (1 + s * ((1 - b / m)^2) * random_number())
f3 <- function(b, s, m) b <- min(m, b * (1 + s * (1 - b / m) * random_number()))
f4 <- function(b, s, m) b <- min(m, b * (1 + s * ((1 - b / m)^2) * random_number()))
#f5 <- function(b, s, m) b <- b * (1 + s * exp(-1 * (b-b0)/(m + 1 - b))*random_number())
#f5 <- function(b, s, m, i) b <- min(m, b * (1 + s * (1 - b / m) + s * 1 * (1 - min(1, i/(log((m/b0)))))))
f5 <- function(b, s, m) b <- b * (1 + s * ((1 - b / m)^3) * random_number())
#f5 <- function(b, s, m, i) b <- b * (1 + s * ((1 - b / m)^((i/2))) * (random_number^(1/i)))
#f5 <- function(b, s, m) b <- m * (1 - s * exp(-2*b))


#### loop through all options for s ####

for (j in 1:length(s_vector)) {
  s <- s_vector[j]

  #### set the initial condition for the birth rate calculations ####
  b1 <- b0
  b2 <- b0
  b3 <- b0
  b4 <- b0
  b5 <- b0
  #### calculate the birth rates after each mutation and store in the vectors, after the initial value i.e. from position 2 #### 
  for(i in 2:n) {
    b1 <- f1(b1, s, m)
    vec1[i] <- b1
    b2 <- f2(b2, s, m)
    vec2[i] <- b2
    b3 <- f3(b3, s, m) 
    vec3[i] <- b3
    b4 <- f4(b4, s, m)
    vec4[i] <- b4
    b5 <- f5(b5, s, m)
    vec5[i] <- b5
  }
  
  #### check if outputs are above or below thresholds ####
  checks1 <- vec5<=m
  checks2 <- vec5>=0
  checks <- checks1*checks2
  checks <- c(s, checks)
  checks_df <- rbind(checks_df, checks)



#### geometric curve of birth rates growing as the rate s (as comparison) ####
vec0 <- b * (1 + s)^(0:(n-1))

#### plotting the charts ####
plot(vec1, ylim = c(-5, 1.5*m), lty = 1, pch = 1, type = "l")  # black circles are the current implementation of the birth rate calculation
lines(vec2, col = "red", pch = 3, lty = 2, type = "l")        # red crosses are the second function f2 with a ^2 element
lines(vec3, col = "purple", pch = 1, lty = 1, type = "l")        # red crosses are the second function f3 with a element and random factor
lines(vec4, col = "green", pch = 3, lty = 1, type = "l")        # d crosses are the second function f4 with a ^2 element and random factor
lines(vec5, col = "brown", pch = 2, lty = 1, type = "l")        # d crosses are the second function f4 with a ^2 element and random factor
lines(vec0, col = "blue", pch = 4 , lty = 1, type = "l")        # blue line is the geometric growth comparison
abline(h = 0, lty = 2)                    # plotting the lower bound dashed line at 0
abline(h = m, lty = 2)                    # plotting the upper bound dashed line at m
legend(5, 5, legend=c("Geometric", 
                       "f1 - original formula", 
                       "f2 - squaring (1-b/m)", 
                       "f3 - original cut off at m", 
                       "f4 - squared & cut off", 
                       "f5 - test option" ),
       col=c("blue", "black", "red", "purple", "green", "brown"), lty = c(1, 1, 2, 1, 1, 1), pch = c(-1, -1, -1, -1, -1, -1), cex = 0.75)
label_text <- paste0("s value = ", s)
text(2, -1, label_text, cex = 1)
}