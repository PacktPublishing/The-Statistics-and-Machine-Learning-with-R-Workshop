# exercise 10.1
# Simulating a Bernoulli trial:
# Set the probability of success
p = 0.6
# Generate a random Bernoulli outcome (1 for success, 0 for failure)
outcome = rbinom(1, size = 1, prob = p)
# Print the outcome
print(outcome)

set.seed(8)
rbinom(1, size = 1, prob = p)

# Simulating multiple Bernoulli trials
# Set the number of trials
n = 5
# Generate multiple Bernoulli outcomes
outcomes = rbinom(n, size = 1, prob = p)
# Print the outcomes
print(outcomes)

# Calculating the mean and variance of a Bernoulli-distributed random variable
# Calculate the mean and variance using the probability of success (p)
mean_bernoulli = p
var_bernoulli = p * (1 - p)
# Print the results
cat("Mean:", mean_bernoulli, "\nVariance:", var_bernoulli)

# Analyzing the results of multiple Bernoulli trials
# Count the number of successes in the outcomes
num_successes = sum(outcomes)
# Calculate the empirical probability of success
empirical_p = num_successes / n
# Print the results
cat("Number of successes:", num_successes, "\nEmpirical probability of success:", empirical_p)

n = 1000
num_successes = sum(rbinom(n, size = 1, prob = p))
empirical_p = num_successes / n
cat("Number of successes:", num_successes, "\nEmpirical probability of success:", empirical_p)

# exercise 10.2
# Simulating and analyzing binomial distribution
# Parameters
n = 10 # Number of trials
p = 0.5 # Probability of success
# Calculate binomial probabilities for each number of successes (0 to 10)
binom_probs = dbinom(0:n, n, p)
binom_probs

# Plot the binomial distribution using the barplot function
# Create a bar plot of the binomial probabilities
barplot(binom_probs, names.arg = 0:n, xlab = "Number of Successes", ylab = "Probability", main = "Binomial Distribution (n = 10, p = 0.5)")

# Calculate the cumulative binomial probabilities using the pbinom function
# Calculate cumulative binomial probabilities for each number of successes (0 to 10)
cum_binom_probs <- pbinom(0:n, n, p)
cum_binom_probs

# Perform a simple analysis: Calculate the probability of obtaining at least 7 successess
# Calculate the probability of obtaining at least 7 successes (1 - P(X <= 6))
prob_at_least_7_successes = 1 - pbinom(6, n, p)
prob_at_least_7_successes

# exercise 10.3
n = 5
p = 0.8
prob_at_least_4_wins = 1 - pbinom(3, n, p)
prob_at_least_4_wins

prob_at_most_3_wins = pbinom(3, n, p)
prob_at_most_3_wins

prob_at_most_3_wins == 1 - prob_at_least_4_wins

# Normal approximation to the binomial distribution
n = 100
p = 0.5
# verify conditions for normal approximation
n*p > 10
n*p*(1-p) > 10
# calculate meana and std
mu = n*p
mu
std = sqrt(n*p*(1-p))
std
# calculate P(lower_limit <= X <= upper_limit)
lower_limit = 40
upper_limit = 60
# equivalent to P(standard_lower_limit <= Z <= standard_upper_limit)
standard_lower_limit = (lower_limit - mu) / std
standard_upper_limit = (upper_limit - mu) / std
standard_lower_limit
standard_upper_limit
# approximation using standard normal cdf
pnorm(standard_upper_limit) - pnorm(standard_lower_limit)
# use binomial distribution
pbinom(upper_limit, n, p) - pbinom(lower_limit, n, p)

# exercise 10.4
# simulating and analyzing Poisson distributed random variables
# Parameters
lambda = 5 # Average rate of occurrence (events per interval)
# Calculate Poisson probabilities for each number of events (0 to 15)
pois_probs = dpois(0:15, lambda)
pois_probs

# Create a bar plot of the Poisson probabilities
barplot(pois_probs, names.arg = 0:15, xlab = "Number of Events", ylab = "Probability", main = "Poisson Distribution (lambda = 5)")

# Calculate cumulative Poisson probabilities for each number of events (0 to 15)
cum_pois_probs = ppois(0:15, lambda)
cum_pois_probs
barplot(cum_pois_probs, names.arg = 0:15, xlab = "Number of Events", ylab = "Cumulative Probability", main = "CDF of Poisson Distribution (lambda = 5)")

# Generate 100 random samples from a Poisson distribution with lambda = 5
pois_samples = rpois(100, lambda)
pois_samples

# poisson approximation to binomial distribution
# Binomial parameters
n = 1000
p = 0.01

# Calculate binomial probability for observing 15 successes
binom_prob = dbinom(15, n, p)
binom_prob

# Poisson parameter (λ) approximated from the binomial distribution
lambda_approx = n * p
lambda_approx

# Calculate Poisson probability for observing 15 successes
pois_approx_prob <- dpois(15, lambda_approx)
pois_approx_prob

# Display the results
cat("Binomial probability:", binom_prob, "\n")
cat("Poisson approximation probability:", pois_approx_prob, "\n")


# exercise 10.5
# work with the geometric distribution, including calculating probabilities, plotting the distribution, and generating random samples
# Calculate geometric probabilities using the dgeom function:
# Parameters
p = 0.25 # Probability of success on each trial
# Calculate geometric probabilities for each number of trials (1 to 10)
geom_probs = dgeom(0:9, p)
geom_probs

# Create a bar plot of the geometric probabilities
barplot(geom_probs, names.arg = 1:10, xlab = "Number of Trials", ylab = "Probability", main = "Geometric Distribution (p = 0.25)")

# Calculate cumulative geometric probabilities for each number of trials (1 to 10)
cum_geom_probs = pgeom(0:9, p)
cum_geom_probs

# Generate 100 random samples from a geometric distribution with p = 0.25
geom_samples = rgeom(100, p)
geom_samples

# exercise 10.6
# Calculate the probability of finding the first bug within the first 5 attempts:
# Parameters
p = 0.1 # Probability of finding a bug on each attempt
# Calculate the cumulative geometric probability for up to 5 attempts
prob_within_5_attempts = pgeom(4, p) # Note that we use 4 since R's dgeom uses zero-based indexing
prob_within_5_attempts
sum(dgeom(0:4, p))

# Calculate the expected number of attempts needed to find the first bug
# Calculate the mean (expected value) of the geometric distribution
mean_attempts <- 1 / p
mean_attempts

# Visualize the probabilities of finding the first bug within different numbers of attempts
# Calculate geometric probabilities for each number of attempts (1 to 20)
geom_probs <- dgeom(0:19, p)
# Create a bar plot of the geometric probabilities
barplot(geom_probs, names.arg = 1:20, xlab = "Number of Attempts", ylab = "Probability", main = "Geometric Distribution (p = 0.1)")

# exercise 10.7
# Calculate the probability density of the normal distribution using the dnorm function
# Parameters
mu = 0      # Mean
sigma = 1   # Standard deviation
# Calculate the probability density for different values of x
x = seq(-4, 4, by = 0.1)
normal_density = dnorm(x, mu, sigma)
normal_density

# Plot the normal distribution using the plot function
# Create a plot of the normal distribution
plot(x, normal_density, type = "l", xlab = "x", ylab = "Probability Density", main = "Normal Distribution (μ = 0, σ = 1)")

# Calculate the cumulative probabilities of the normal distribution using the pnorm function
# Calculate cumulative probabilities for different values of x
normal_cum_prob <- pnorm(x, mu, sigma)
normal_cum_prob

plot(x, normal_cum_prob, type = "l", xlab = "x", ylab = "Cumulative Probability Density", main = "Cumulative Normal Distribution (μ = 0, σ = 1)")

# Generate random samples from a normal distribution using the rnorm function
# Generate 100 random samples from a normal distribution with μ = 0 and σ = 1
normal_samples <- rnorm(100, mu, sigma)
normal_samples

# Find the quantile (inverse cumulative probability) for a given probability using the qnorm function
# Find the quantile corresponding to the 90th percentile
quantile_90 <- qnorm(0.9, mu, sigma)
quantile_90

# exercise 10.8
# Simulate a dataset of 1000 batteries
set.seed(8)
mean_lifespan = 100
sd_lifespan = 10
n = 1000
lifespans = rnorm(n, mean_lifespan, sd_lifespan)

# Calculate the probability that a randomly chosen battery will last more than 120 hours
threshold = 120
probability = 1 - pnorm(threshold, mean_lifespan, sd_lifespan)
probability

# Plot the PDF of the lifespans with the area under the curve above 120 hours shaded
df <- data.frame(lifespan = lifespans)
df_density <- density(lifespans)
df_shaded <- data.frame(x = df_density$x, y = df_density$y)
df_shaded <- df_shaded[df_shaded$x > threshold,]

ggplot(df, aes(x=lifespan)) + 
  geom_density(fill="lightblue") +
  geom_vline(xintercept = threshold, linetype="dashed", color="red") +
  geom_area(data = df_shaded, aes(x=x, y=y), fill="orange", alpha=0.5) +
  theme_minimal() +
  labs(title="Lifespan of batteries", x="Lifespan (hours)", y="Probability Density")


# exercise 10.9
# Generate a random sample of 1000 data points from an exponential distribution with a rate parameter of 0.01.
set.seed(8) # Set seed for reproducibility
lambda = 0.01
sample_size = 1000
exponential_sample = rexp(sample_size, rate = lambda)

# Calculate the probability that the waiting time between events is more than 150 units
threshold = 150
probability_above_threshold = 1 - pexp(threshold, rate = lambda)
probability_above_threshold

# Plot the probability density function (PDF) and shade the area under the curve for waiting times greater than the threshold.
# Create a data frame for the waiting times
waiting_times = seq(0, max(exponential_sample), length.out = 1000)
density_values = dexp(waiting_times, rate = lambda)
df = data.frame(waiting_times, density_values)
# Filter data for the shaded region
df_shaded = df[df$waiting_times > threshold,]
# Plot the PDF of the exponential distribution
ggplot(df, aes(x = waiting_times, y = density_values)) +
  geom_line() +
  geom_area(data = df_shaded, aes(x = waiting_times, y = density_values), fill = "orange", alpha = 0.5) +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Exponential Distribution (λ = 0.01)", x = "Waiting Time", y = "Probability Density")

# exercise 10.10
# Generate a random sample of 10000 data points from a uniform distribution with a lower bound (a) of 2 and an upper bound (b) of 10.
set.seed(8) # Set seed for reproducibility
a = 2
b = 10
sample_size = 10000
uniform_sample = runif(sample_size, min = a, max = b)

# Calculate the probability that a value selected from the uniform distribution is greater than 7.
threshold = 7
probability = 1 - punif(threshold, min = a, max = b)
probability

probability2 = sum(uniform_sample > threshold) / length(uniform_sample)
probability2

# Plot the probability density function (PDF) of the uniform distribution with a lower bound (a) of 2 and an upper bound (b) of 10.
library(ggplot2)

# Create a data frame for the distribution
x_values = seq(a, b, length.out = 1000)
density_values = dunif(x_values, min = a, max = b)
df = data.frame(x_values, density_values)

# Plot the PDF of the uniform distribution
ggplot(df, aes(x = x_values, y = density_values)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Uniform Distribution (a = 2, b = 10)", x = "Value", y = "Probability Density")

# generating normally distributed random samples using the inverse transform method 
set.seed(8) # Set seed for reproducibility

# Define the target normal distribution parameters
mu = 5
sigma = 2

# Generate uniform random variables
n = 5
uniform_sample = runif(n)

# Calculate the corresponding quantiles for the uniform sample using the inverse CDF (quantile function) of the normal distribution
normal_sample = qnorm(uniform_sample, mean = mu, sd = sigma)
normal_sample

normal_sample2 = qnorm(uniform_sample, mean = 0, sd = 1)
normal_sample2 * sigma + mu

# exercise 10.11
set.seed(8) # Set seed for reproducibility

# Define the population parameters
population_mean = 50
population_sd = 10
population_size = 100000

# Generate the population using a normal distribution
population <- rnorm(population_size, mean = population_mean, sd = population_sd)
summary(population)

# Define the sample size in each round
sample_size_per_round = 50

# Function to draw a sample and calculate its mean
get_sample_mean <- function(population, sample_size_per_round) {
  sample <- sample(population, size = sample_size_per_round, replace = FALSE)
  return(mean(sample))
}

get_sample_mean(population, sample_size_per_round)
get_sample_mean(population, sample_size_per_round)

# Generate multiple rounds of sample means
num_rounds = 1000 # the number of rounds to sample
sample_means = replicate(num_rounds, get_sample_mean(population, sample_size))
summary(sample_means)

# Visualize the sampling distribution of the sample mean using a histogram
library(ggplot2)

sampling_distribution_df = data.frame(sample_means)

ggplot(sampling_distribution_df, aes(x = sample_means)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  geom_density(color = "red", lwd = 1.2) +
  theme_minimal() +
  labs(title = "Sampling Distribution of the Sample Mean",
       x = "Sample Mean",
       y = "Density")

# exercise 10.12
# Set a seed for reproducibility
set.seed(8)

# Generate a small sample from a normal distribution
sample_size = 10
mu = 50
sigma = 10
samples = rnorm(sample_size, mean = mu, sd = sigma)
samples

# Calculate the sample mean and standard deviation
sample_mean = mean(samples)
sample_sd = sd(samples)
sample_mean
sample_sd

# Calculate the 95% confidence interval using the t-distribution
alpha = 0.05
t_critical = qt(1 - alpha/2, df = sample_size - 1)  # t-value for a two-tailed test with alpha = 0.05 and df = n - 1
margin_of_error_t = t_critical * (sample_sd / sqrt(sample_size))
ci_t = c(sample_mean - margin_of_error_t, sample_mean + margin_of_error_t)
ci_t

# exercise 10.13
# Sorting a sample
# Generate a random sample
set.seed(8)
samples = rnorm(10, mean = 50, sd = 10)
samples

# Sort the sample in ascending order
sorted_samples = sort(samples)
sorted_samples
sort(samples, decreasing = T)

# Obtaining specific order statistics
# Find the minimum value (1st order statistic)
min_value = sorted_samples[1]
min_value

# Find the maximum value (last order statistic)
max_value = sorted_samples[length(sorted_samples)]
max_value

# Find the k-th order statistic (e.g., k = 3)
k = 3
kth_order_stat = sorted_samples[k]
kth_order_stat

# Calculating quantiles
# Calculate the median (50th percentile)
median_value = median(samples)
median_value
median(sorted_samples)

# Calculate the 25th and 75th percentiles (1st and 3rd quartiles)
quartiles = quantile(samples, probs = c(0.25, 0.75))
quartiles
quantile(sorted_samples, probs = c(0.25, 0.75))


# exercise 10.14
# Set a seed for reproducibility
set.seed(8)

# Generate a random sample of daily returns from a normal distribution
sample_size = 252  # Number of trading days in a year
mu = 0.08          # Mean daily return
sigma = 0.05       # Standard deviation of daily returns
daily_returns = rnorm(sample_size, mean = mu, sd = sigma)
summary(daily_returns)

# Calculate the VaR at a given confidence level
confidence_level = 0.95
portfolio_value = 1000000  # Portfolio value in USD
sorted_returns = sort(daily_returns)
VaR_index = ceiling(sample_size * (1 - confidence_level))
VaR = sorted_returns[VaR_index]
VaR_amount = portfolio_value * (1 - (1 + VaR))
VaR
VaR_amount

# Visualize VaR
library(dplyr)
daily_returns_df <- data.frame(DailyReturns = daily_returns)
# Create the density plot
density_plot <- ggplot(daily_returns_df, aes(x = DailyReturns)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = VaR), linetype = "dashed", color = "red") +
  labs(x = "Daily Returns", y = "Density", title = "Density Plot of Daily Returns with VaR") +
  theme_minimal()

# Add shaded area below the VaR to the density plot
density_data <- ggplot_build(density_plot)$data[[1]] %>%
  as.data.frame() %>%
  filter(x < VaR)

density_plot + 
  geom_ribbon(data = density_data, aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.5)
