# HW4

# Q1
k = 5; n = 10; m = 3
A = matrix(runif(50,0,100),k,n)
B = matrix(runif(30,0,100),n,m)
ans1 = t(A%*%B)
ans2 = t(B)%*%t(A)
compare = ans1-ans2

# Q2

# * and %*% between numbers
a = 4; b = 10
a*b
a%*%b

# between vectors
a = array(1:4); b = array(1:4)
a*b
a%*%t(b)

#between matrices
A = matrix(1:9,3,3)
B = matrix(11:19,3,3)
A*B
A%*%B

# Q3
# Ax = b, solve for x, use solve(A,b)
A = matrix(c(2,-1,1,-1,-2,1,2,1,-1),3,3)
b = matrix(c(5,3,2),3,1)
x = solve(A,b)
# Thus a=3.5, b=-5, c=-3.5

# Q4
library(rvest)
# SOCR Knee Pain dataset
wiki_url = read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_KneePainData_041409")
html_nodes(wiki_url, "#content")
socrdata = html_table(html_nodes(wiki_url, "table")[[2]])
socrdata = as.data.frame(socrdata)

rb = subset(socrdata,socrdata$View=='RB')
lsfit = lm(rb$Y~rb$x)
summary(lsfit)

plot(rb$x,rb$Y)
abline(lsfit,col='red')
# The model captures the main trend of x and y

# Q5
library(MASS)
a = seq(1,15,length=6)
A = matrix(a,3,2)
# add a row, 3:4
A = rbind(A,3:4)
# add two columns, 5:8, 1:4
C = cbind(A,10:13)
C = cbind(C,1:4)
# diagonal matrix D
D = diag(rnorm(4),4,4)
# element wise calculation
C+D
C-D
C*D
D/C
# matrix multiplication D and C
D%*%C
# inverse of C
solve(C) #cannot solve C
ginv(C) # can give the general inverse

# Q6
library(matrixStats)
# SOCR Data Iris Sepal Petal Classes
wiki_url = read_html("http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_052511_IrisSepalPetalClasses")
html_nodes(wiki_url, "#content")
irisdata = html_table(html_nodes(wiki_url, "table")[[3]])
irisdata = as.data.frame(irisdata)

setosa = subset(irisdata, irisdata$Iris_Class == 'Iris-setosa')
setosa = setosa[,-5]
colMeans(setosa)
colVars(as.matrix(setosa))
cov(setosa$`Sepal_Length(cm)`, setosa$`Sepal_Width(cm)`)
cor(setosa$`Sepal_Length(cm)`, setosa$`Sepal_Width(cm)`)

# Q7
set.seed(2017)
A = matrix(runif(9),nrow = 3)
eigA = eigen(A)
# verify
lamda1 = eigA$values[1]
lamda2 = eigA$values[2]
lamda3 = eigA$values[3]
v1 = eigA$vectors[,1]
v2 = eigA$vectors[,2]
v3 = eigA$vectors[,3]

A%*%v1
lamda1%*%v1

A%*%v2
lamda2%*%v2

A%*%v3
lamda3%*%v3




