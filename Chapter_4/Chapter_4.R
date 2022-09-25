##### EXERCISE 4.1 #####
library(ggplot2)
str(mtcars)

ggplot(mtcars, aes(x=cyl, y=mpg)) +
  geom_point()

unique(mtcars$cyl)

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot()

##### EXERCISE 4.2 #####
ggplot(mtcars, aes(factor(cyl), mpg, color=disp)) +
  geom_point()

ggplot(mtcars, aes(factor(cyl), mpg, color=disp, size=hp)) +
  geom_point()

##### EXERCISE 4.3 #####
ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(alpha=0.6) +
  geom_smooth()

plt = ggplot(mtcars, aes(hp, mpg)) +
  geom_point(alpha=0.6)
plt

plt = plt +
  geom_point(aes(color=disp)) +
  geom_smooth()
plt

##### EXERCISE 4.4 #####
ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(shape=1, size=4)

ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(shape=2, size=2)

ggplot(mtcars, aes(hp, mpg, fill = factor(cyl))) +
  geom_point(shape = 21, size = 5, alpha = 0.6)


##### EXERCISE 4.5 #####
ggplot(mtcars, aes(hp, mpg)) +
  geom_text(label=row.names(mtcars))

ggplot(mtcars, aes(hp, mpg)) +
  geom_text(label=row.names(mtcars),
            fontface = "bold",
            position=position_jitter(width=20,height=20))

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_jitter()

##### EXERCISE 4.6 #####
ggplot(mtcars, aes(hp, mpg, color=factor(cyl))) +
  geom_point()

ggplot(mtcars, aes(hp, mpg)) +
  geom_point(aes(col=factor(cyl)))

library(dplyr)
tmp = mtcars %>% 
  group_by(factor(cyl)) %>% 
  summarise_all(mean)
tmp

ggplot(mtcars, aes(x=hp, y=mpg, color=factor(cyl))) +
  geom_point() +
  geom_point(data=tmp, shape=15, size=6)

ggplot(mtcars, aes(x=hp, y=mpg, color=factor(cyl))) +
  geom_point() +
  geom_point(data=tmp[,c("mpg","disp")], shape=15, size=6)


##### EXERCISE 4.7 #####
ggplot(mtcars, aes(x=hp)) +
  geom_histogram()

ggplot(mtcars, aes(x=hp)) +
  geom_histogram(binwidth=40)

ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="stack")

ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="dodge")

ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="fill") + 
  ylab("Proportion")

##### EXERCISE 4.8 #####
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="stack")

ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="fill")

ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="dodge")

ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position = position_dodge(width=0.2))

##### EXERCISE 4.8 #####
library(zoo)
JohnsonJohnson2 = data.frame(qtr_earning=as.matrix(JohnsonJohnson), 
           date=as.Date(as.yearmon(time(JohnsonJohnson))))
head(JohnsonJohnson2, n=3)

library(dplyr)
JohnsonJohnson2 = JohnsonJohnson2 %>% 
  mutate(ind = if_else(date >= as.Date("1975-01-01"), TRUE, FALSE),
         qtr = quarters(date))
head(JohnsonJohnson2, n=3)

ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning)) +
         geom_line()

ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=ind)) +
  geom_line()

ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line()

##### EXERCISE 4.9 #####
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20)) 

tmp = ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme(legend.position=c(0.1,0.8),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))
tmp

tmp = tmp + 
  theme(
    axis.title=element_text(color="blue"),
    axis.line = element_line(color = "black", linetype = "solid")
  )
tmp

tmp = tmp + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
tmp
 
##### EXERCISE 4.10 #####
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_classic() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

install.packages("ggthemes")
library(ggthemes)

ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))

ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_tufte() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size=20))
