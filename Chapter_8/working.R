# calculate determinant
A <- matrix(c(2, 6, 1, 8), nrow=2, byrow=TRUE)
A
det(A)

# obtain rank
library(Matrix)
A = matrix(c(1,2,3,3,2,4), nrow=2, byrow=TRUE)
A
rankMatrix(A)
rankMatrix(A)[1]

# compute trace
A = as.matrix(data.frame("c1"=c(1,2,3),"c2"=c(2,5,2),"c3"=c(-1,8,3)))
A
diag(A)
sum(diag(A))

trace <- function(A) {
  # get matrix dimension
  n = dim(A)[1] 
  # track trace value
  tr = 0 
  # add diagonal elements to trace
  for(k in 1:n) {
    l = A[k,k]
    tr = tr + l
  }
  return(tr[[1]])
}

trace(A)

B = as.matrix(data.frame("c1"=c(1,0,1),"c2"=c(1,1,2),"c3"=c(-1,2,0)))
B

# property 1
trace(A + B) == trace(A) + trace(B)
# property 2
trace(A) == trace(t(A))
t(A)
# property 3
trace(2*A) == 2*trace(A)
# property 4
trace(A %*% B) == trace(B %*% A)
# property 5
trace(A) == trace(crossprod(crossprod(B,A),solve(B)))

# caculate vector norm
a = as.matrix(c(1,2,3))
a
# l1 norm
norm(a)
norm(a, type="1")

# l2 norm
norm(a, type="2")

# max norm
norm(a, type="I")

# caculate matrix norm
X = as.matrix(data.frame("c1"=c(1,2,3),"c2"=c(2,5,2),"c3"=c(-1,8,3)))
X

# l1 norm
norm(X, type="1")
colSums(abs(X))

# l2 norm
norm(X, type="f")
sqrt(sum(X^2))

# max norm
norm(X, type="I")
max(rowSums(abs(X)))

# scalar vector multiplication 
v = c(1,2,3)
lambda = 2
lambda * v

# introduce identity matrix
I = diag(3)
I
lambda * I
(lambda * I) %*% v

# verify eigenvalue and eigenvector
A = matrix(c(2,3,0,1), byrow=TRUE, nrow=2)
A  
lambda = 2
v = c(1,0)

A%*%v
lambda*v
A%*%v - lambda*v

# scalar multiplies of eigenvectors are also eigenvectors
A%*%(0.5*v)- lambda*(0.5*v)

# compute eigenvalue and eigenvector
eigen(A)
eigen(A)$values[1]
eigen(A)$vectors
eigen(A)$vectors[,1]

# verify eigenvalue and eigenvector
eigen_rst = eigen(A)
det(eigen_rst$values[1] * diag(2) - A)
det(eigen_rst$values[2] * diag(2) - A)

A%*%eigen_rst$vector[,1] - eigen_rst$values[1]*eigen_rst$vector[,1]
A%*%eigen_rst$vector[,2] - eigen_rst$values[2]*eigen_rst$vector[,2]

# calculate variance-covariance matrix
X = matrix(c(1:5,2*(1:5)), byrow=FALSE, nrow=5)
X

X[,1] = X[,1] - mean(X[,1])
X[,2] = X[,2] - mean(X[,2])
X

t(X)%*%X / (nrow(X)-1)

var(X[,2])
cov(X[,1], X[,2])

eigen(t(X)%*%X / (nrow(X)-1))

prcomp(X)

# perform PCA
X = iris[,c(1:4)]
head(X)
X_pca = prcomp(X)
X_pca

library(factoextra)
fviz_eig(X_pca)

fviz_pca_ind(X_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(X_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
