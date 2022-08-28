library(readr)
library(dplyr)

df = read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/marvel-wikia-data.csv")
df

###### EXERCISE 5.1 ######
unique(df$ALIGN)
unique(df$SEX)

df = df %>% 
  filter(!is.na(ALIGN),
         !is.na(SEX))
dim(df)
sum(is.na(df$ALIGN))
sum(is.na(df$SEX))

table(df$ALIGN, df$SEX)

library(ggplot2)
ggplot(df, aes(x=SEX, fill=ALIGN)) +
  geom_bar() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.position = c(0.2, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20)) 

df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=SEX, fill=ALIGN)) +
  geom_bar()


###### EXERCISE 5.2 ######
options(scipen=999, digits=3)

count_df = table(df$ALIGN, df$SEX)

prop.table(count_df)

sum(prop.table(count_df))

# condition on rows
prop.table(count_df, margin=1)
rowSums(prop.table(count_df, margin=1))

# condition on columns
prop.table(count_df, margin=2)
colSums(prop.table(count_df, margin=2))

# bar chart using proportions
df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=SEX, fill=ALIGN)) +
  geom_bar(position="fill") +
  ylab("proportion") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20)) 

df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=ALIGN, fill=SEX)) +
  geom_bar(position="fill") +
  ylab("proportion") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20)) 


# Marginal distribution and faceted bar chart
colSums(count_df)
table(df$SEX)

df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=SEX)) +
  geom_bar() +
  facet_wrap(~ALIGN) + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        strip.text.x = element_text(size = 30)) 

df$ALIGN = factor(df$ALIGN, levels = c("Bad Characters", "Neutral Characters", "Good Characters"))
df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=SEX)) +
  geom_bar() +
  facet_wrap(~ALIGN) + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        strip.text.x = element_text(size = 30))

###### EXERCISE 5.3 ######
summary(df$Year)

ggplot(df, aes(x=Year)) +
  geom_dotplot(dotsize=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) 

ggplot(df, aes(x=Year)) +
  geom_histogram() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) 

ggplot(df, aes(x=Year)) +
  geom_density() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) 

ggplot(df, aes(x=Year)) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) 

ggplot(df, aes(x=Year)) +
  geom_boxplot() +
  facet_wrap(~SEX) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size = 30)) 

ggplot(df, aes(x=APPEARANCES)) +
  geom_boxplot()


###### EXERCISE 5.4 ######
df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=Year)) +
  geom_density() +
  facet_grid(ALIGN ~ SEX, labeller = label_both) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 12)) 

df %>% 
  filter(!(SEX %in% c("Agender Characters", "Genderfluid Characters"))) %>% 
  ggplot(aes(x=Year)) +
  geom_histogram() +
  facet_grid(ALIGN ~ SEX, labeller = label_both) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 12)) 

###### EXERCISE 5.5 ######
mean(df$APPEARANCES)
summary(df$APPEARANCES)
mean(df$APPEARANCES, na.rm = TRUE)
median(df$APPEARANCES, na.rm = TRUE)

mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(df$APPEARANCES)

df %>% 
  group_by(ALIGN) %>% 
  summarise(mean_appear = mean(APPEARANCES, na.rm=TRUE),
            median_appear = median(APPEARANCES, na.rm=TRUE))

###### EXERCISE 5.6 ######
tmp = df$APPEARANCES[!is.na(df$APPEARANCES)]
pop_var = sum((tmp - mean(tmp))^2)/length(tmp)
formatC(pop_var, digits = 2, format = "f")

sample_var = sum((tmp - mean(tmp))^2)/(length(tmp)-1)
formatC(sample_var, digits = 2, format = "f")

formatC(var(tmp), digits = 2, format = "f")

sd(tmp)

IQR(tmp)
summary(tmp)

tmp2 = tmp[tmp != max(tmp)]
sd(tmp2)
IQR(tmp2)

df %>% 
  group_by(ALIGN) %>% 
  summarise(sd_appear = sd(APPEARANCES, na.rm=TRUE),
            IQR_appear = IQR(APPEARANCES, na.rm=TRUE),
            count = n())


###### EXERCISE 5.7 ######
tmp = df %>% 
  filter(Year >= 2000)

ggplot(tmp, aes(x=APPEARANCES, fill=ALIGN)) + 
  geom_density(alpha=0.2) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))

tmp = tmp %>% 
  filter(APPEARANCES <= quantile(APPEARANCES, 0.9, na.rm=TRUE))

ggplot(tmp, aes(x=APPEARANCES, fill=ALIGN)) + 
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))

ggplot(tmp, aes(x=log(APPEARANCES), fill=ALIGN)) + 
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))


###### EXERCISE 5.8 ######
install.packages("yfR")
library(yfR)

first_date = as.Date("2021-01-01")
last_date = as.Date("2022-01-01")
my_ticker <- c('META', 'NFLX', 'GOOG', 'AMZN', 'MSFT')

df <- yf_get(tickers = my_ticker, 
                         first_date = first_date,
                         last_date = last_date)
str(df)

###### EXERCISE 5.9 ######
ggplot(df, 
       aes(x = ref_date, y = price_adjusted,
           color = ticker)) + 
  geom_line() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

ggplot(df, aes(x=price_adjusted, fill=ticker)) +
  geom_histogram(bins=100) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

ggplot(df, aes(x=price_adjusted, fill=ticker)) +
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

ggplot(df, aes(ticker, price_adjusted, fill=ticker)) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

df %>% 
  group_by(ticker) %>% 
  summarise(mean = mean(price_adjusted, na.rm=TRUE),
            sd = sd(price_adjusted, na.rm=TRUE),
            IQR = IQR(price_adjusted, na.rm=TRUE),
            count = n())

###### EXERCISE 5.10 ######
library(tidyr)
wide_df <- df %>%
  select(ref_date, ticker, price_adjusted) %>% 
  spread(ticker, price_adjusted)
head(wide_df)

install.packages("corrplot")
library(corrplot)
cor_table = cor(wide_df[,-1])
corrplot(cor_table, method = "circle")
cor_table
