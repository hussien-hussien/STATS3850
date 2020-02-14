Advertising <- read.csv("Advertising.csv")
fit1<- lm(data = Advertising, Sales~TV)
summary(fit1)

### Example of simple regression
n=20;
x<-rnorm(n);
y<-2+3*x+rnorm(n,0,2)
fit_1<-lm(y~x)
summary(fit_1)


#the standard deviation of beta
b=c();
for (j in 1:500)
{
  # x<-rnorm(20);
  y<-2+3*x+rnorm(n,0,2)
  fit_t<-lm(y~x)
  b<-c(b,coef(fit_t)[2])
}
mb<-mean(b)
mb
vb<-var(b)
(sqrt(vb))

# p-value

t1<-summary(fit_1)$coefficients[2,3]
pt(t1,n-2, lower.tail=F)*2

# Practice  Question 1: Numerically evaluate the covariance matrix of the coefficient

# Practice  Question 2: Evaluate the p-vlaue numerically

# training MSE

train.mse<- mean(residuals(fit_1)^2)

# how to estimate tesing MSE?

# generate new data

xt<-rnorm(100);
yt<-2+3*x+rnorm(n,0,2)
pre_y<-predict(fit_1, newdata = data.frame(x=xt))

test.mse<- mean((pre_y-yt)^2)

### Testing and Training MSE-2020

n=50;
set.seed(1)
x<-rnorm(n,1,1);
y<-1+ x + 2* sin(x)+rnorm(n, 0, 1)

plot(x,y)

fit_1<-lm(y~x)
fit_2<-lm(y~poly(x,2))
fit_3<-lm(y~poly(x,3))
fit_4<-lm(y~poly(x,4))
fit_5<-lm(y~poly(x,5))
fit_6<-lm(y~poly(x,6))
fit_7<-lm(y~poly(x,7))
fit_8<-lm(y~poly(x,8))
fit_9<-lm(y~poly(x,9))
fit_10<-lm(y~poly(x,10))


training_e1<-mean(residuals(fit_1)^2)
training_e2<-mean(residuals(fit_2)^2)
training_e3<-mean(residuals(fit_3)^2)
training_e4<-mean(residuals(fit_4)^2)
training_e5<-mean(residuals(fit_5)^2)
training_e6<-mean(residuals(fit_6)^2)
training_e7<-mean(residuals(fit_7)^2)
training_e8<-mean(residuals(fit_8)^2)
training_e9<-mean(residuals(fit_9)^2)
training_e10<-mean(residuals(fit_10)^2)

training_e<-c(training_e1,training_e2,training_e3,training_e4,training_e5,training_e6,training_e7,training_e8,training_e9,training_e10)
training_e


n=50;
x1t<- rnorm(n,1,1);
yt<- 1+ x1t + 2* sin(x1t)+rnorm(n, 0, 1)

ndata_1<-data.frame(x=x1t)
pre_1<-predict(fit_1, ndata_1)
pre_2<-predict(fit_2, ndata_1)
pre_3<-predict(fit_3, ndata_1)
pre_4<-predict(fit_4, ndata_1)
pre_5<-predict(fit_5, ndata_1)
pre_6<-predict(fit_6, ndata_1)
pre_7<-predict(fit_7, ndata_1)
pre_8<-predict(fit_8, ndata_1)
pre_9<-predict(fit_9, ndata_1)
pre_10<-predict(fit_10, ndata_1)

testing_e1<-mean((yt-pre_1)^2)
testing_e2<-mean((yt-pre_2)^2)
testing_e3<-mean((yt-pre_3)^2)
testing_e4<-mean((yt-pre_4)^2)
testing_e5<-mean((yt-pre_5)^2)
testing_e6<-mean((yt-pre_6)^2)
testing_e7<-mean((yt-pre_7)^2)
testing_e8<-mean((yt-pre_8)^2)
testing_e9<-mean((yt-pre_9)^2)
testing_e10<-mean((yt-pre_10)^2)

testing_e<-c(testing_e1,testing_e2,testing_e3,testing_e4,testing_e5,testing_e6,testing_e7,testing_e8,testing_e9,testing_e10)
testing_e


lm.d<-seq(1,10) 
plot(lm.d, training_e)
lines(lm.d, testing_e)

lm.d
#### Sales example

require(ISLR)
# Interpretation of the regression coefficients


Advertising <- read.csv("Advertising.csv")
View(Advertising)
fit_a<-lm(sales~ newspaper, data=Advertising)
summary(fit_a)

fit_b<-lm(sales~ newspaper+TV+radio, data=Advertising)
summary(fit_b)

# please explain with the coeffient of newspaper change
fit_c<-lm(sales~ newspaper+TV, data=Advertising)
summary(fit_c)

fit_d<-lm(sales~ newspaper+radio, data=Advertising)
summary(fit_d)



### Qualitative predictors
require(ISLR)
# you need to use your own directory
Credit <- read.csv("~/SS3850-2018/r-notes/Credit.csv")
View(Credit)

m1<-lm(Balance~ Student, data = Credit)
summary(m1)

m2<-lm(Balance~ Rating+Student, data = Credit)
summary(m2)

m3<-lm(Balance~ Rating+Student+Rating:Student, data = Credit)
summary(m3)


##KNN regression

rm(list=ls())
set.seed(100)
n=50;
x<-rnorm(n);
y<-2+3*x+rnorm(n,0,2)
train_r<- data.frame(x=x)

set.seed(300)
xt<-rnorm(n);
yt<-2+3*xt+rnorm(n,0,2)
test_r<- data.frame(x=xt)

library(FNN)
kr1<-knn.reg(train_r, train_r, y,k=1)
kr2<-knn.reg(train_r, train_r, y,k=2)
kr3<-knn.reg(train_r, train_r, y,k=3)
kr4<-knn.reg(train_r, train_r, y,k=4)
kr5<-knn.reg(train_r, train_r, y,k=5)

k_train_e_1=sum((y-kr1$pred)^2)
k_train_e_2=sum((y-kr2$pred)^2)
k_train_e_3=sum((y-kr3$pred)^2)
k_train_e_4=sum((y-kr4$pred)^2)
k_train_e_5=sum((y-kr5$pred)^2)

k_train_e=c(k_train_e_1, k_train_e_2,k_train_e_3,k_train_e_4,k_train_e_5)
k_train_e


kr1<-knn.reg(train_r, test_r, y,k=1)
kr2<-knn.reg(train_r, test_r, y,k=2)
kr3<-knn.reg(train_r, test_r, y,k=3)
kr4<-knn.reg(train_r, test_r, y,k=4)
kr5<-knn.reg(train_r, test_r, y,k=5)

k_test_e_1=sum((yt-kr1$pred)^2)
k_test_e_2=sum((yt-kr2$pred)^2)
k_test_e_3=sum((yt-kr3$pred)^2)
k_test_e_4=sum((yt-kr4$pred)^2)
k_test_e_5=sum((yt-kr5$pred)^2)


k_test_e=c(k_test_e_1, k_test_e_2,k_test_e_3,k_test_e_4,k_test_e_5)
k_test_e

##compare LM and KNN example 1

rm(list=ls())
set.seed(100)
n=50;
x<-rnorm(n);
y<-2+3*x+rnorm(n,0,1)
train_r<- data.frame(x=x)

set.seed(300)
xt<-rnorm(n);
yt<-2+3*xt+rnorm(n,0,1)
test_r<- data.frame(x=xt)


lm_fit_1<-lm(y~x)
lm_fit_2<-lm(y~poly(x,2))
lm_fit_3<-lm(y~poly(x,3))
ndata_1<-data.frame(x=xt)
lm_pre_1<-predict(lm_fit_1, ndata_1)
lm_pre_2<-predict(lm_fit_2, ndata_1)
lm_pre_3<-predict(lm_fit_3, ndata_1)

lm_testing_e1<-sum((yt-lm_pre_1)^2)
lm_testing_e2<-sum((yt-lm_pre_2)^2)
lm_testing_e3<-sum((yt-lm_pre_3)^2)

lm_testing_e<- c(lm_testing_e1,lm_testing_e2,lm_testing_e3)
lm_testing_e

library(FNN)
kr1<-knn.reg(train_r, test_r, y,k=1)
kr2<-knn.reg(train_r, test_r, y,k=2)
kr3<-knn.reg(train_r, test_r, y,k=3)
kr4<-knn.reg(train_r, test_r, y,k=4)
kr5<-knn.reg(train_r, test_r, y,k=5)

k_test_e_1=sum((yt-kr1$pred)^2)
k_test_e_2=sum((yt-kr2$pred)^2)
k_test_e_3=sum((yt-kr3$pred)^2)
k_test_e_4=sum((yt-kr4$pred)^2)
k_test_e_5=sum((yt-kr5$pred)^2)
k_test_e=c(k_test_e_1, k_test_e_2,k_test_e_3,k_test_e_4,k_test_e_5)
k_test_e


##compare LM and KNN example 2

rm(list=ls())
set.seed(100)
n=50;
x<-rnorm(n);
y<- 3*sin(x)+rnorm(n,0,1)
train_r<- data.frame(x=x)

set.seed(300)
xt<-rnorm(n);
yt<-2+3*sin(xt)+rnorm(n,0,1)
test_r<- data.frame(x=xt)


lm_fit_1<-lm(y~x)
lm_fit_2<-lm(y~poly(x,2))
lm_fit_3<-lm(y~poly(x,3))
ndata_1<-data.frame(x=xt)
lm_pre_1<-predict(lm_fit_1, ndata_1)
lm_pre_2<-predict(lm_fit_2, ndata_1)
lm_pre_3<-predict(lm_fit_3, ndata_1)

lm_testing_e1<-sum((yt-lm_pre_1)^2)
lm_testing_e2<-sum((yt-lm_pre_2)^2)
lm_testing_e3<-sum((yt-lm_pre_3)^2)

lm_testing_e<- c(lm_testing_e1,lm_testing_e2,lm_testing_e3)
lm_testing_e

library(FNN)
kr1<-knn.reg(train_r, test_r, y,k=1)
kr2<-knn.reg(train_r, test_r, y,k=2)
kr3<-knn.reg(train_r, test_r, y,k=3)
kr4<-knn.reg(train_r, test_r, y,k=4)
kr5<-knn.reg(train_r, test_r, y,k=5)

k_test_e_1=sum((yt-kr1$pred)^2)
k_test_e_2=sum((yt-kr2$pred)^2)
k_test_e_3=sum((yt-kr3$pred)^2)
k_test_e_4=sum((yt-kr4$pred)^2)
k_test_e_5=sum((yt-kr5$pred)^2)
k_test_e=c(k_test_e_1, k_test_e_2,k_test_e_3,k_test_e_4,k_test_e_5)
k_test_e


lm_testing_e<- c(lm_testing_e1,lm_testing_e2,lm_testing_e3)
lm_testing_e


names(kr)
kr$residuals
