# Exercise 14.1 
# set parameters
set.seed(1)
random_prob = runif(1, min = 0, max = 1)
random_prob

prop_success = 0.2
random_prob < prop_success

# generating data
n_samples = 10
data = c()
for(sample_idx in 1:n_samples) {
  data[sample_idx] <- runif(1, min = 0, max = 1) < prop_success
}
data

data = as.numeric(data)
data

set.seed(1)
rbinom(n = n_samples, size = 1, prob = prop_success)

###
set.seed(1)
prop_successes = runif(n_samples, min = 0.0, max = 0.2)
prop_successes

###
library(ggplot2)
# Sample 1000 draws from Beta(35,55) prior
prior_A = rbeta(n = 1000, shape1 = 35, shape2 = 55)

# Store the results in a data frame
prior_sim = data.frame(prior_A)

# Construct a density plot of the prior sample
ggplot(prior_sim, aes(x = prior_A)) + 
  geom_density()

# Sample draws from the Beta(1,1) prior
prior_B = rbeta(n = 1000, shape1 = 1, shape2 = 1)    

# Sample draws from the Beta(100,100) prior
prior_C = rbeta(n = 1000, shape1 = 100, shape2 = 100)

# Combine the results in a single data frame
prior_all <- data.frame(samples = c(prior_A, prior_B, prior_C),
                        priors = rep(c("A","B","C"), each = 1000))

# Plot the 3 priors
ggplot(prior_all, aes(x = samples, fill = priors)) + 
  geom_density(alpha = 0.5)

###
# observed data
x = c(1, 2, 3, 4, 5)
y = c(2, 3, 5, 6, 7)

# parameter value
b = 0.8

# calculate the predicted values
y_pred = b * x

# calculate the residuals
residuals = y - y_pred

# calculate the log-likelihood
log_likelihood = -0.5 * length(y) * log(2 * pi) - 0.5 * sum(residuals^2)
log_likelihood

###
library(ggridges)
# Define a vector of 1000 p values    
p_grid = seq(from = 0, to = 1, length.out = 1000)

# Simulate 10 trials for each p in p_grid, each trial has 1000 samples
sim_result = rbinom(n = 1000, size = 10, prob = p_grid)    

# Collect results in a data frame
likelihood_sim = data.frame(p_grid, sim_result)    

# Density plots of p_grid grouped by sim_result
ggplot(likelihood_sim, aes(x = p_grid, y = sim_result, group = sim_result)) + 
  geom_density_ridges()


# Exercise 14.2
library(rjags)
# define the model
bayes_model = "model{
    # Likelihood model for X
    X ~ dbin(p, n)
    
    # Prior model for p
    p ~ dbeta(a, b)
}"

# compile the model    
bayes_jags = jags.model(textConnection(bayes_model), 
                        data = list(a = 1, b = 1, X = 3, n = 10))

# simulate the posterior
bayes_sim = coda.samples(model = bayes_jags, variable.names = c("p"), n.iter = 10000)

# plot the posterior
plot(bayes_sim, trace = FALSE, xlim = c(0,1), ylim = c(0,3))


# Exercise 14.3
library(coda)
set.seed(1)
mu_true = 2
sd_true = 1
n = 100
data = rnorm(n, mean = mu_true, sd = sd_true)

model_string = "model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu, prec)
    }
    
    mu ~ dnorm(0, 0.1)
    sigma ~ dunif(0, 10)
    
    prec <- pow(sigma, -2)
}"

data_jags = list(y = data, n = n)
model = jags.model(textConnection(model_string), data = data_jags)
update(model, 1000)  # burn-in

params = c("mu", "sigma")
samples = coda.samples(model, params, n.iter = 10000)
# print summary statistics for the posterior samples
summary(samples)

plot(samples)


###
# Store the chains in a data frame
mcmc_chains <- data.frame(samples[[1]], iter = 1:10000)

# Check out the head
head(mcmc_chains)

# Use plot() to construct trace plots 
plot(samples, density = FALSE)

# Use ggplot() to construct a trace plot 
ggplot(mcmc_chains, aes(x = iter, y = mu)) + 
  geom_line()

# Trace plot the first 100 iterations of the mu chain
ggplot(mcmc_chains[1:100, ], aes(x = iter, y = mu)) + 
  geom_line() +
  theme(axis.title.x = element_text(size = 20),  # Increase x-axis label size
        axis.title.y = element_text(size = 20))  # Increase y-axis label size
        

# Use plot() to construct density plots
plot(samples, trace = FALSE)

# Use ggplot() to construct a density plot 
ggplot(mcmc_chains, aes(x = mu)) + 
  geom_density()

model2 = jags.model(textConnection(model_string), data = data_jags, n.chains = 4)

# simulate the posterior    
samples2 <- coda.samples(model = model2, variable.names = params, n.iter = 1000)

# Check out the head 
head(samples2)

# Construct trace plots 
plot(samples2, density = FALSE)

summary(samples2)


# Exercise 14.4
# load the necessary libraries
library(rjags)
library(coda)

# define the model
model = "model{
    # Define model for data Y[i]
    for(i in 1:length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- a + b * X[i]
    }

    # Define the a, b, s priors
    a ~ dnorm(0, 0.5^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20)
}"

# compile the model
model = jags.model(textConnection(model), 
                    data = list(Y = mtcars$wt, X = mtcars$hp), 
                    n.chains = 3, 
                    inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

# burn-in
update(model, 1000)

# generate MCMC samples
samples = coda.samples(model, variable.names = c("a", "b", "s"), n.iter = 5000)

# check convergence using trace plot
plot(samples)

# Get the posterior estimates
posterior_estimates = summary(samples)

# Calculate the mean for each parameter
a_mean = posterior_estimates$statistics["a", "Mean"]
b_mean = posterior_estimates$statistics["b", "Mean"]

# Plot the prediction line
ggplot(mtcars, aes(x = hp, y = wt)) +
  geom_point() +
  geom_abline(intercept = a_mean, slope = b_mean) +
  labs(title = "Bayesian Linear Regression",
       x = "Horsepower",
       y = "Weight") +
  theme(plot.title = element_text(hjust = 0.5))


# Extract samples
a_samples = as.matrix(samples[, "a"])
b_samples = as.matrix(samples[, "b"])

# Calculate credible intervals
a_hpd = coda::HPDinterval(coda::as.mcmc(a_samples))
b_hpd = coda::HPDinterval(coda::as.mcmc(b_samples))

# Plot histograms and credible intervals
par(mfrow=c(2,1))  # Create 2 subplots

# Parameter a
hist(a_samples, freq=FALSE, xlab="a", main="Posterior distribution of a", col="lightgray")
abline(v=a_hpd[1,1], col="red", lwd=2)  # Lower limit of the credible interval
abline(v=a_hpd[1,2], col="red", lwd=2)  # Upper limit of the credible interval

# Parameter b
hist(b_samples, freq=FALSE, xlab="b", main="Posterior distribution of b", col="lightgray")
abline(v=b_hpd[1,1], col="red", lwd=2)  # Lower limit of the credible interval
abline(v=b_hpd[1,2], col="red", lwd=2)  # Upper limit of the credible interval

# make posterior predictions
# Obtain the mean values of the MCMC samples for each parameter
a_mean = mean(samples[[1]][,"a"])
b_mean = mean(samples[[1]][,"b"])

# New input (e.g., horsepower = 120)
new_input = 120

# Prediction
predicted_weight = a_mean + b_mean * new_input
print(predicted_weight)

# Predictive distribution
predicted_weights = samples[[1]][,"a"] + samples[[1]][,"b"] * new_input

# Plot the predictive distribution
hist(predicted_weights, breaks = 30, main = "Posterior predictive distribution", xlab = "Predicted weight")


### 

# define the model
model = "model{
    # Define model for data Y[i]
    for(i in 1:length(Y)) {
      Y[i] ~ dnorm(mu[am[i]+1], s^(-2))
    }

    # Define the mu, s priors
    for(j in 1:2){
      mu[j] ~ dnorm(20, 10^(-2))
    }
    s ~ dunif(0, 20)
}"

# compile the model
model = jags.model(textConnection(model), 
                    data = list(Y = mtcars$mpg, am = mtcars$am), 
                    n.chains = 3, 
                    inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

# burn-in
update(model, 1000)

# generate MCMC samples
samples = coda.samples(model, variable.names = c("mu", "s"), n.iter = 5000)

# check convergence using trace plot
plot(samples)

# Get the posterior estimates
posterior_estimates = summary(samples)

# Calculate the mean for each parameter
mu1_mean = posterior_estimates$statistics["mu[1]", "Mean"]
mu2_mean = posterior_estimates$statistics["mu[2]", "Mean"]

# Plot the prediction line
ggplot(mtcars, aes(x = as.factor(am), y = mpg)) +
  geom_jitter(width = 0.2) +
  geom_hline(aes(yintercept = mu1_mean, color = "Automatic"), linetype = "dashed") +
  geom_hline(aes(yintercept = mu2_mean, color = "Manual"), linetype = "dashed") +
  scale_color_manual(name = "Transmission", values = c("Automatic" = "red", "Manual" = "blue")) +
  labs(title = "Bayesian Linear Regression",
       x = "Transmission (0 = automatic, 1 = manual)",
       y = "Miles Per Gallon (mpg)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
