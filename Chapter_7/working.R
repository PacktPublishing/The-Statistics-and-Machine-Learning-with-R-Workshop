data(sleep)
head(sleep)

# Exercise 7.1 #
x = c(1, 2, 3, 4, 5, 6)
x

x = c(1:6)
x

x = seq(1, 6, by=1)
x

y = rep(1, 6)
y


# Exercise 7.2 #
x[2] = 20
x

x*2

x + 1
x + y


# Exercise 7.3 #
X = matrix(data=2, nrow=3, ncol=2)
X
X[1,2] 
class(X)

matrix(x, nrow=3, ncol=2, byrow=TRUE)

matrix(x, nrow=3, ncol=2, byrow=FALSE)

X[1,1] = 10
X

# Exercise 7.4 #
X %*% c(1,1)

X %*% c(1,1,1)

X[1,] %*% c(1,1)

sum(X[1,] * c(1,1))

X[2,] %*% c(1,1)
sum(X[2,] * c(1,1))

X * 2
X * matrix(2, 3, 2)


# Exercise 7.5 #
matrix(1:4, 2, 2) %*% matrix(2, 2, 2)

matrix(2, 2, 2) %*% matrix(1:4, 2, 2)

matrix(1:4, 2, 2) * matrix(2, 2, 2)

# Exercise 7.6 #
diag(2)
diag(c(1,2,3), nrow=3, ncol=3)

matrix(1:4, 2, 2) %*% diag(2)
matrix(1:4, 2, 2)

diag(2) %*% matrix(1:4, 2, 2)

# Exercise 7.7 #
X = matrix(1:4, 2, 2)
X

t(X)

t(t(X))

all.equal(X, t(t(X)))


# Exercise 7.8 #
solve(diag(2))

Xinv = solve(X)
Xinv

Xinv %*% X

X %*% Xinv

# Exercise 7.9 #
X = matrix(c(1:4), nrow=2, ncol=2)
X

w = c(1,1)
w

y = X %*% w
y

w[1] * X[,1] + w[2] * X[,2]

# analysis
plot(x=1, y=1, xlab="w1", ylab="w2", xlim=c(0, 5), ylim=c(0, 5))
abline(a=4/3, b=-1/3)
abline(a=3/2, b=-1/2)

plot(x=1, y=1, xlab="w1", ylab="w2", xlim=c(0, 5), ylim=c(0, 5))
abline(a=4/3, b=-1/3)
abline(a=7/3, b=-1/3)

solve(X)
det(X)

# Exercise 7.10 #
X = matrix(c(1:4), nrow=2, ncol=2)
X
y = c(4, 6)
y

w_hat = solve(X) %*% y
w_hat

X %*% w_hat





