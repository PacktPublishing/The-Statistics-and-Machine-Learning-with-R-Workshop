##### EXERCISE 2.01 #####
install.packages("tidyverse")
library(dplyr)

data("iris")
class(iris)

iris_tbl = as_tibble(iris)
class(iris_tbl)

iris_tbl

##### EXERCISE 2.02 #####
unique(iris_tbl$Species)

iris_tbl_subset = iris_tbl %>% 
  filter(Species == "setosa")
iris_tbl_subset
unique(iris_tbl_subset$Species)
unique(as.character(iris_tbl_subset$Species))

iris_tbl_subset = iris_tbl %>% 
  filter(Species == "setosa",
         Sepal.Length <= 5)
max(iris_tbl_subset$Sepal.Length)
dim(iris_tbl_subset)

##### EXERCISE 2.03 #####
iris_tbl_sorted = iris_tbl %>% 
  arrange(Sepal.Length)
iris_tbl_sorted

iris_tbl_sorted = iris_tbl %>% 
  arrange(desc(Sepal.Length))
iris_tbl_sorted

iris_tbl_subset_sorted = iris_tbl %>% 
  filter(Species == "setosa",
         Sepal.Length <= 5) %>% 
  arrange(desc(Sepal.Length),desc(Sepal.Width))
iris_tbl_subset_sorted

##### EXERCISE 2.04 #####
paste("Before:", class(iris_tbl$Species))
iris_tbl = iris_tbl %>%
  mutate(Species = as.character(Species))
paste("After:", class(iris_tbl$Species))

iris_tbl = iris_tbl %>% 
  mutate(ind = Sepal.Width > Petal.Length)
iris_tbl
table(iris_tbl$ind)

iris_tbl_subset = iris_tbl %>%
  filter(ind==TRUE)
table(iris_tbl_subset$ind)

iris_tbl_subset2 = iris_tbl %>%
  filter(Sepal.Width > Petal.Length)
nrow(iris_tbl_subset2)

##### EXERCISE 2.05 #####
rst = iris_tbl %>%
  select(Sepal.Length, Sepal.Width, Petal.Length)
rst

rst = iris_tbl %>%
  select(Sepal.Length:Petal.Length)
rst

rst = iris_tbl %>%
  select(contains("length"))
rst

rst = iris_tbl %>%
  select(starts_with("petal"))
rst

# Introducing other verbs
rst = iris_tbl %>%
  select(Sepal.Length, Sepal_Width=Sepal.Width)
rst

rst = iris_tbl %>%
  transmute(Species, Diff = abs(Sepal.Length - Petal.Length))
rst

##### EXERCISE 2.06 #####
rst = iris_tbl %>%
  top_n(1, Sepal.Length)
rst

rst = iris_tbl %>%
  arrange(desc(Sepal.Length)) %>% 
  head(1) 
rst

rst = iris_tbl %>%
  group_by(Species) %>% 
  top_n(1, Sepal.Length) %>% 
  select(Species, Sepal.Length)
rst

rst = iris_tbl %>%
  group_by(Species) %>% 
  summarize(max_sepal_length = max(Sepal.Length))
rst

rst = iris_tbl %>%
  group_by(Species) %>% 
  summarize(max_sepal_length = max(Sepal.Length)) %>% 
  top_n(1, max_sepal_length)
rst


#### EXERCISE 2.07 #####
# rst = iris_tbl %>%
#   arrange(desc(Sepal.Length)) %>%
#   head(80) %>%
#   # top_n(100, Sepal.Length) %>%
#   filter(Sepal.Width > Petal.Length) %>% 
#   mutate(Diff = abs(Sepal.Length - Petal.Length)) %>% 
#   select(Diff) %>% 
#   colMeans()
# rst

rst = iris_tbl %>%
  top_n(80, Sepal.Length) %>%
  filter(Sepal.Width > Petal.Length) %>% 
  mutate(Diff = abs(Sepal.Length - Petal.Length)) %>% 
  select(Diff) %>% 
  colMeans()
rst

##### EXERCISE 2.08 #####
rst = iris_tbl %>%
  count(Species)
rst

rst = iris_tbl %>%
  filter(abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  count(Species, sort=TRUE)
rst

##### EXERCISE 2.09 #####
rst = iris_tbl %>%
  group_by(Species) %>% 
  summarise(n=n())
rst

rst = iris_tbl %>%
  filter(abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
rst

rst = iris_tbl %>%
  mutate(ind = abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species, ind) %>% 
  summarise(mean_sepal_length=mean(Sepal.Length))
rst

rst = iris_tbl %>%
  mutate(ind = abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species, ind) %>% 
  summarise(mean_sepal_length=mean(Sepal.Length)) %>% 
  ungroup()
rst

##### EXERCISE 2.10 #####
a = 1:3
tbl_A = tibble(key_A=a, col_A=2*a)
tbl_B = tibble(key_B=a+1, col_B=3*a)
tbl_A
tbl_B

# inner join
rst = tbl_A %>% 
  inner_join(tbl_B, by=c("key_A"="key_B"))
rst

# left join
rst = tbl_A %>% 
  left_join(tbl_B, by=c("key_A"="key_B"))
rst

# left join with duplicate
tbl_C = tbl_B %>% 
  bind_rows(tbl_B[1,])
tbl_C[nrow(tbl_C),"col_B"] = 10
tbl_C

rst = tbl_A %>% 
  left_join(tbl_C, by=c("key_A"="key_B"))
rst

# right join
rst = tbl_A %>% 
  right_join(tbl_B, by=c("key_A"="key_B"))
rst

# full join
rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B"))
rst

library(tidyr)
rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B")) %>% 
  drop_na()
rst

rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B")) %>% 
  replace_na(list(col_A=0, col_B=0))
rst

##### EXERCISE 2.11 #####
library(readr)
df_questions = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/questions.csv")
df_questions
summary(df_questions$score)

df_question_tags = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/question_tags.csv")
df_question_tags

df_tags = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/tags.csv")
df_tags

df_all = df_questions %>% 
  left_join(df_question_tags, by=c("id"="question_id"))
df_all

df_all = df_all %>% 
  left_join(df_tags, by=c("tag_id"="id"))
df_all

df_all = df_all %>% 
  filter(!is.na(tag_name))
rst = df_all %>% 
  count(tag_name, sort = TRUE)
rst

library(lubridate)
rst = df_all %>% 
  mutate(year = year(creation_date)) %>% 
  count(year)
rst

max(df_all$creation_date)

df_all = df_all %>% 
  mutate(month = month(creation_date),
         year_month = format(creation_date, "%Y%m")) 
df_all

rst1 = df_all %>% 
  count(year_month, month)
rst1

rst2 = rst1 %>% 
  group_by(month) %>% 
  summarise(avg_num_tag = mean(n))
rst2

rst = df_all %>% 
  group_by(tag_name) %>% 
  summarise(count = n(),
            min_score = min(score),
            mean_score = mean(score),
            max_score = max(score)) %>% 
  arrange(desc(count))
rst

