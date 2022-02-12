#### EXERCISE 1.01 #####
# String assignment
test = "I am a string"
print(test)
# Simple calculation
test2 = 1 + 2
print(test2)

ls()

#### EXERCISE 1.02 #####
a = 5
b = 2
a + b
a - b
a * b
a / b
a ^ b
a %% b

#### EXERCISE 1.03 #####
a = 1.0; b = 1; c = "test"; d = TRUE; e = factor("test")
class(a); class(b); class(c); class(d); class(e)

a + b
class(a + b)

a + c

a + d
class(a + d)
a + !d

a == d

class(as.numeric(b))

as.integer(1.8)
round(1.8)

as.factor(a)
as.factor(c)

#### EXERCISE 1.04 #####
vec_a = c(1,2,3)
vec_b = c(1,1,1)
sum(vec_a)
mean(vec_a)
vec_a + vec_b
vec_a + 1
vec_a + c(1,2)
vec_a > vec_b
vec_a == vec_b

#### EXERCISE 1.05 #####
vec_a[1]
vec_a[c(1,3)]
vec_a[c(1,2,3)]
vec_a[1:3]
vec_a[vec_a > vec_b]

#### EXERCISE 1.06 #####
mtx_a = matrix(c(vec_a,vec_b), nrow=2, byrow=TRUE)
mtx_a

rownames(mtx_a) = c("r1", "r2")
colnames(mtx_a) = c("c1", "c2", "c3")
mtx_a

#### EXERCISE 1.07 #####
mtx_a[1,2]
mtx_a[1:2,c(2,3)]
mtx_a[2,]
mtx_a[rownames(mtx_a)=="r2",]
mtx_a[,3]
mtx_a[,colnames(mtx_a)=="c3"]

#### EXERCISE 1.08 #####
mtx_b = mtx_a * 2
mtx_b
mtx_a / mtx_b
rowSums(mtx_a)
colSums(mtx_a)
rowMeans(mtx_a)
colMeans(mtx_a)

#### EXERCISE 1.09 #####
cbind(mtx_a, mtx_b)
rbind(mtx_a, mtx_b)

#### EXERCISE 1.10 #####
data("iris")
dim(iris)
head(iris)
tail(iris)
str(iris)

df_a = data.frame("a"=vec_a, "b"=vec_b)

#### EXERCISE 1.11 #####
df_a[,2]
df_a[1:3,2]
df_a[,"b"]
df_a$b

subset(df_a, a>2)
subset(df_a, a>2, select="b")

#### EXERCISE 1.12 #####
vec_c = c(5,1,10)
order(vec_c)
vec_c[order(vec_c)]

df_a[order(-df_a$a),]

#### EXERCISE 1.13 #####
ls_a = list(a, vec_a, df_a)
ls_a
ls_a[[2]]

names(ls_a) <- c("a", "vec_a", "df_a")
ls_a
ls_a[['vec_a']]
ls_a$vec_a

ls_a[['new_entry']] = "test"
ls_a

ls_a[['df_a']] = NULL
ls_a

ls_a[['vec_a']] = c(1,2)
ls_a

#### EXERCISE 1.14 #####
1 == 2
"statistics" == "calculus"
TRUE == TRUE
TRUE == FALSE

1 != 2
"statistics" != "calculus"
TRUE != TRUE
TRUE != FALSE

1 < 2
"statistics" > "calculus"
TRUE > FALSE

1 >= 2
2 <= 2

(1 > 2) | (1 == 2)
(2 < 2) | (2 == 2)

vec_a > 1

#### EXERCISE 1.15 #####
TRUE & FALSE
TRUE & TRUE
FALSE & FALSE
1 > 0 & 1 < 2

TRUE | FALSE
TRUE | TRUE
FALSE | FALSE
1 < 0 | 1 < 2

!TRUE
!FALSE
!(1<0)

c(TRUE, FALSE) & c(TRUE, TRUE)
c(TRUE, FALSE) | c(TRUE, TRUE)
!c(TRUE, FALSE)

c(TRUE, FALSE) && c(FALSE, TRUE)
c(TRUE, FALSE) || c(FALSE, TRUE)

#### EXERCISE 1.16 #####
x = 1
if(x > 0){
  print("positive")
} else {
  print("not positive")
}

x = 0
if(x > 0){
  print("positive")
} else if(x == 0){
  print("zero")
} else {
  print("negative")
}

#### EXERCISE 1.17 #####
x = 2
while(x < 10){
  x = x^2
  print(x)
}
x

x = 2
while(x < 10){
  x = x^2
  if(x > 10){
    break
  }
  print(x)
}
x

#### EXERCISE 1.18 #####
string_a = c("statistics","and","calculus")
for(i in string_a){
  print(i)
}

for(i in 1:length(string_a)){
  print(string_a[i])
}

for(i in string_a){
  if(i == "and"){
    break
  }
  print(i)
}

for(i in string_a){
  if(i == "and"){
    next
  }
  print(i)
}

#### EXERCISE 1.19 #####
test_func = function(x, cap=FALSE){
  msg = paste(x,"is fun!")
  if(cap){
    msg = toupper(msg)
  }
  return(msg)
}
test_func("r")
test_func("r",cap=TRUE)
test_func()

