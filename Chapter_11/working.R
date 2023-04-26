library(dplyr)
library(ggplot2)

# install.packages("socviz")

library(socviz)
data(gss_lon)

# Examine the dimension of the dataset
dim(gss_lon)

# Examine the structure of the dataset
str(gss_lon)
glimpse(gss_lon)


# exercise 11.1
summary(gss_lon$siblings)

gss2016 = gss_lon %>% filter(year == 2016)

ggplot(gss2016, aes(x = siblings)) +
  geom_bar() +
  labs(title = "Frequency count of siblings", x = "Number of siblings", y = "Count") +
  theme(text = element_text(size = 16))

p_hat = gss2016 %>% 
  summarize(prop_2sib = mean(siblings=="2", na.rm=TRUE)) %>% 
  pull()
p_hat


# exercise 11.2
# install.packages("infer")
library(infer)

gss2016 = gss2016 %>% 
  mutate(siblings_two_ind = if_else(siblings=="2","Y","N")) %>% 
  filter(!is.na(siblings_two_ind))

bs = gss2016 %>% 
  specify(response = siblings_two_ind,
          success = "Y") %>% 
  generate(reps = 500,
           type = "bootstrap") %>% 
  calculate(stat = "prop")

bs

ggplot(bs, aes(x = stat)) + 
  geom_density() +
  labs(title = "Density plot of the sample proportions", x = "Sample proportion", y = "Density") + 
  theme(text = element_text(size = 16))

SE = bs %>% 
  summarise(sd(stat)) %>% 
  pull()
SE

c(p_hat - 2*SE, p_hat + 2*SE)

# assuming Bernoulli distribution
SE2 = sqrt(p_hat*(1-p_hat)/nrow(gss2016))
c(p_hat - 2*SE2, p_hat + 2*SE2)

# exercise 11.3
gss2016 %>% 
  ggplot(aes(x = siblings_two_ind)) +
  geom_bar() +
  labs(title = "Frequency count of families with two siblings", x = "Have two siblings", y = "Count") + 
  theme(text = element_text(size = 16))

p_hat = gss2016 %>% 
  summarize(mean(siblings_two_ind=="Y")) %>% 
  pull()
p_hat

null = gss2016 %>% 
  specify(response = siblings_two_ind,
          success = "Y") %>% 
  hypothesise(null = "point",
              p = 0.19) %>% 
  generate(reps = 500,
           type = "draw") %>% 
  calculate(stat = "prop")
null

ggplot(null, aes(x = stat)) + 
  geom_density() +
  geom_vline(xintercept = p_hat,
             color = "red") +
  labs(title = "Density plot using bootstrap", x = "Sample proportion", y = "Density") + 
  theme(text = element_text(size = 16))

null %>% 
  summarise(mean(stat > p_hat)) %>% 
  pull()* 2

# exercise 11.4
gss2016 = gss2016 %>% 
  mutate(higher_degree = if_else(degree %in% c("Bachelor","Graduate"), "Y", "N"))

table(gss2016$higher_degree)
table(gss2016$sex)

ggplot(gss2016, aes(x = sex, fill=higher_degree)) + 
  geom_bar() +
  labs(title = "Frequency count for gender and degree", x = "Gender", y = "Count") + 
  theme(text = element_text(size = 16))

ggplot(gss2016, aes(x = sex, fill=higher_degree)) + 
  geom_bar(position = "fill") +
  labs(title = "Sample proportions for gender and degree", x = "Gender", y = "Ratio") + 
  theme(text = element_text(size = 16))

p_hats = gss2016 %>% 
  group_by(sex) %>% 
  summarise(mean(higher_degree=="Y", na.rm=TRUE)) %>% 
  pull()
p_hats
d_hat = diff(p_hats)
d_hat

gss2016 %>% 
  specify(
    response = higher_degree,
    explanatory = sex,
    success = "Y"
  ) %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 1, type = "permute")

null = gss2016 %>% 
  specify(
    higher_degree ~ sex,
    success = "Y"
  ) %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 500, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("Female", "Male"))
null

ggplot(null, aes(x = stat)) +
  geom_density() + 
  geom_vline(xintercept = d_hat, color = "red") +
  labs(x = "Difference in sample proportion (female - male)", y = "Count") + 
  theme(text = element_text(size = 16))

# Compute two-tailed p-value
null %>%
  summarize(pval = 2 * mean(stat > d_hat)) %>% 
  pull()

############
table(gss2016$degree)

ggplot(gss2016, aes(x = sex, fill=degree)) + 
  geom_bar() +
  labs(title = "Frequency count for gender and degree", x = "Gender", y = "Count") + 
  theme(text = element_text(size = 16))

tab = gss2016 %>% 
  select(sex, degree) %>% 
  table()
tab

# Create one permuted dataset
perm_1 = gss2016 %>%
  # Specify the variables of interest
  specify(degree ~ sex) %>%
  # Set up the null hypothesis
  hypothesize(null = "independence") %>%
  # Generate a single permuted dataset
  generate(reps = 1, type = "permute")
perm_1

# Create null
null_spac = gss2016 %>%
  specify(degree ~ sex) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "Chisq")
null_spac

# calculate expected frequency table
row_totals = rowSums(tab)
col_totals = colSums(tab)
overall_total = sum(tab)
expected = outer(row_totals, col_totals) / overall_total
expected

# Compute chi-square statistic
observed_chi_square = sum((tab - expected)^2 / expected)
observed_chi_square

# Visualize null
ggplot(null_spac, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = observed_chi_square, color = "red") +
  labs(title = "Density curve of bootstrapped chi-square statistic", x = "Chi-square statistic", y = "Density") + 
  theme(text = element_text(size = 16))

# Compute two-tailed p-value
null_spac %>%
  summarize(pval = 2 * mean(stat < observed_chi_square)) %>% 
  pull()

########

# exercise 11.5
data(mtcars)
str(mtcars)

bs <- mtcars %>%
  specify(response = mpg) %>%  
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "median")
bs

# Generate density curve
ggplot(bs, aes(x = stat)) + 
  geom_density() +
  labs(title = "Density plot for bootstrapped median", x = "Median", y = "Probability") + 
  theme(text = element_text(size = 16))

#########
# Calculate bootstrap CI as lower and upper quantiles
bs %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  ) 

SE = bs %>% 
  summarise(sd(stat)) %>% 
  pull()
observed_median = median(mtcars$mpg)
c(observed_median - 2*SE, observed_median + 2*SE)

bs = mtcars %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", med = 16) %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "median")
bs

ggplot(bs, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = median(mtcars$mpg), color = "red") +
  labs(title = "Density curve of bootstrapped median", x = "Sample median", y = "Density") + 
  theme(text = element_text(size = 16))

#########

# exercise 11.6
# P(T < 3) for df = 10
x = pt(3, df = 10)
x

# P(T > 3) for df = 10
y = 1 - x
y

# P(T > 3) for df = 100
z = 1 - pt(3, df = 100)
z

# 95th percentile for df = 10
d = qt(0.95, df = 10)
d

# Upper bound of middle 95th percent for df = 10
e = qt(0.975, df = 10)
e

# Upper bound of middle 95th percent for df = 100
f = qt(0.975, df = 100)
f

mean(mtcars$mpg)

# Construct 95% CI for avg mpg
t.test(mtcars$mpg)

# exercise 11.6
# Define two samples
sample1 = c(10, 12, 14, 16, 18)
sample2 = c(15, 17, 19, 21, 23)

# Combine samples into a data frame
data = tibble(
  value = c(sample1, sample2),
  group = factor(rep(c("Group 1", "Group 2"), each = length(sample1)))
)
data

# Perform bootstrap procedure
bootstrap_results = data %>%
  specify(response = value, explanatory = group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Group 1", "Group 2"))
bootstrap_results

# Calculate the confidence interval
ci = bootstrap_results %>%
  filter(!is.na(stat)) %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
ci

# Perform a two-sample t-test
t_test_result = t.test(sample1, sample2)
t_test_result

t_test_result2 = t.test(value ~ group, data = data)
t_test_result2

######
data(PlantGrowth)
str(PlantGrowth)

anova_results = PlantGrowth %>%
  specify(response = weight, explanatory = group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")
anova_results

p_value = anova_results %>%
  get_p_value(obs_stat = anova_results, direction = "right") %>% 
  pull()
p_value







