library(dplyr)

students = tibble(age = c(26, 30, 28, 31, 25, 29, 30, 29),
                  country = c('SG', 'CN', 'UK', 'UK','CN', 'SG', 'IN', 'SG'),
                  gender = c('F', 'F', 'M', 'M', 'M', 'F', 'F', 'M'),
                  height = c(168, 169, 175, 178, 170, 170, 172, 180))

##### EXERCISE 3.1 #####
students_new = students %>% 
  mutate(country_fullname = recode(country,
                                   "SG"="Singapore",
                                   "CN"="China",
                                   "UK"="United Kingdom",
                                   "IN"="India"))
students_new

students_new = students_new %>% 
  mutate(country_fullname2 = recode_factor(country,
                                   "SG"="Singapore",
                                   "CN"="China",
                                   "UK"="United Kingdom",
                                   "IN"="India"))
students_new

##### EXERCISE 3.2 #####
students_new = students %>% 
  mutate(type = case_when(age >= 30 & country %in% c("SG","IN","CN") ~ "asia_30+",
                             age < 30 & age >= 20 & country %in% c("SG","IN","CN") ~ "asia_20+",
                             TRUE ~ "others"))
students_new

##### EXERCISE 3.3 #####
students_new = students %>% 
  mutate(age_group = cut(x = age,
                    breaks = c(-Inf, 25, 30, Inf),
                    labels = c("<=25", "26-30", ">30")))
students_new

library(tidyverse)
students_new = students %>% 
  mutate(age_group = cut_interval(age, n=3))
students_new
summary(students_new$age_group)

students_new = students %>% 
  mutate(age_group = cut_number(age, n=3))
students_new
summary(students_new$age_group)

##### EXERCISE 3.4 #####
students_wide = students %>% 
  spread(key = country, value = height)
students_wide

avg_height = round(mean(students$height))
students_wide2 = students %>% 
  spread(key = country, value = height, fill = avg_height)
students_wide2

##### EXERCISE 3.5 #####
students_long = students_wide %>% 
  gather(key = "country", value = "height", CN:UK)
students_long

students_long = students_long %>% 
  drop_na(height)
students_long

all_equal(students, students_long, ignore_row_order = T, ignore_col_order = T)

##### EXERCISE 3.6 #####
"statistics workshop"
""statistics" workshop"
'"statistics" workshop'
"\"statistics\" workshop"
writeLines("\"statistics\" workshop")

##### EXERCISE 3.7 #####
format(123000, big.mark = ",")
format(123000, scientific = TRUE)
format(1.256, digits = 3)
round(1.256, digits = 2)

##### EXERCISE 3.8 #####
paste("statistics", "workshop")
paste("statistics", "workshop", sep = "")
paste0("statistics", "workshop")
paste(c("statistics", "workshop"), "course")
paste(c("statistics", "workshop"), "course", collapse = " + ")

##### EXERCISE 3.9 #####
library(stringr)
str_c("statistics", "workshop", sep = " ")
str_c(c("statistics", "workshop"), "course", sep = " ")
str_c(c("statistics", "workshop"), "course", sep = " ", collapse = " + ")

str_length(c("statistics", "workshop"))
nchar(c("statistics", "workshop"))
str_sub(c("statistics", "workshop"), start = 1, end = 3)

##### EXERCISE 3.10 #####
str_detect(c("statistics", "workshop"), "stat")
str_subset(c("statistics", "workshop"), "stat")
str_count(c("statistics", "workshop"), "t")

##### EXERCISE 3.11 #####
str_split(c("statistics & machine leaning workshop"), "&")
str_split(c("statistics & machine leaning workshop"), " & ")
str_split(c("statistics & machine leaning workshop"), " & ")[[1]][2]
str_split(c("statistics & machine leaning workshop", "stats & ml & workshop"), " & ")
str_split(c("statistics & machine leaning workshop", "stats & ml & workshop"), " & ", simplify = TRUE)

str_replace(c("statistics & machine leaning workshop", "stats & ml & workshop"), pattern = "&", replacement = "and")
str_replace_all(c("statistics & machine leaning workshop", "stats & ml & workshop"), pattern = "&", replacement = "and")

##### EXERCISE 3.12 #####
title = "statistics and machine leaning workshop"
title = str_replace(title, pattern = "and", replacement = "&")
title
a = str_split(title, " & ")
a
b = str_c(str_sub(a[[1]][1], 1, 4), str_sub(a[[1]][1], -1, -1))
b
c = unlist(str_split(a[[1]][2], " "))
c
d = str_c(str_sub(c[1], 1, 1), str_sub(c[2], 1, 1))
d
e = str_c(b, "&", d, c[3], sep = " ")
e

# regular expressions
library(rebus)
str_detect(c("statistics", "machine learning"), pattern = START %R% "s")
START
str_view(c("statistics", "machine learning"), pattern = START %R% "s")

##### EXERCISE 3.13 #####
texts = c("stats 101", "machine learning", "R 101 ABC workshop", "101 R workshop")
str_subset(texts, pattern = "learning" %R% END)
str_subset(texts, pattern = ANY_CHAR %R% "101")
ANY_CHAR
str_subset(texts, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% "a")
str_subset(texts, pattern = START %R% or("stats", "R"))
str_subset(texts, pattern = one_or_more(char_class("aA")))

##### EXERCISE 3.14 #####
library(tidytext)
texts = c("stats 101", "Machine Learning", "R and ML workshop", "R workshop & Statistics with R")
texts_df = tibble(id = 1:length(texts),
                  text = texts)
texts_df

tidy_df <- texts_df %>% 
  unnest_tokens(unit_token, text)
tidy_df

tidy_df2 <- texts_df %>% 
  unnest_tokens(unit_token, text, token = "ngrams", n = 2)
tidy_df2

tidy_df %>%
  count(unit_token, sort = TRUE)

get_stopwords()
tidy_df2 = tidy_df %>%
  filter(!(unit_token %in% get_stopwords()$word)) %>% 
  count(unit_token, sort = TRUE)
tidy_df2

##### EXERCISE 3.15 #####
count_df = tidy_df %>% 
  group_by(id, unit_token) %>% 
  summarise(count=n())
count_df

library(tm)
dtm = count_df %>%
  cast_dtm(id, unit_token, count)
dtm
as.data.frame(as.matrix(dtm), stringsAsFactors=False)

tidy_dtm = tidy(dtm)
tidy_dtm
