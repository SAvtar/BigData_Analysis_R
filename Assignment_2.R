# 1(a) H0:B0 = 0 & B1 = 0 & B2 = 0 & B3 = 0
R <- rbind(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1)); r <- c(0,0,0,0)

# 1(b) H0:B0 = 0 & B1 = 0
R <- rbind(c(1,0),c(0,1)); r <- c(0,0)

# 1(c) H0:B0 = 1 & B1 = 1
R <- rbind(c(1,1),c(1,1)); r <- c(1,1)

# 1(d) H0:B1 = 0
R <- rbind(c(0,1)); r <- c(0)

# 1(e) H0:B1+B2 = 1
R <- rbind(c(0,1)); r <- c(1)

'library(AER)
 data(CPS1985)
 Before transformation we need to check the normlal
 distribution of the data and prior to this first create the models
 1. Y = exp(X*beta+U)
 2. Y = X*beta+U and Y is wage.' 

library(AER)
data(CPS1985)
library(lmtest)  # for testing the Regression model
library(sandwich)  # for robust covariance matrices

# 2(a) First we need to find the regression of both the models
reg_01 <- lm(log(wage) ~ education + married + gender + I(experience), data = CPS1985)
model_1 <- rexp(reg_01)
model_1
# Beta Estimates
Y <- CPS1985$wage
X1 <- CPS1985$education
X2 <- CPS1985$married
X3 <- CPS1985$gender
X4 <- CPS1985$experience

X <- cbind(1, X1, X2, X3, X4)

beta_h <- solve(t(X)%*%X)%*%t(X)%*%Y
beta_h
reg_02 <- lm(log(wage) ~ education + married + gender + I(experience) ^ 3, data = CPS1985)
model_2 <- reg_02
model_2

# T- Stats
y_h <- X%*%beta_h
U_h <- Y - y_h
n <- (length(Y))
K <- ncol(X) - 1
s_2 <- sum(U_h^2)/(n-K-1)
SE <- sqrt(s_2*diag(solve(t(X)%*%X)))
t_0 <- beta_h/SE
t_0

# 2(b) likhti hai: is equation da asr othe painda jitha sample space bhut 
# choti hundi wa. 

# P values for 2(c)
2*pt(t_0,df=(n-K-1),lower.tail=F)
# Heteroskedacity 
v_b_model_1 <- vcovHC(reg_01)
v_b_model_1
v_b_model_2 <- vcovHC(model_2)
v_b_model_2

# Robust Results
coeftest(reg_01,vcov=v_b_model_1)
coeftest(reg_02,vcov=v_b_model_2)

# Matrices for Joint Hypothesis
R <- rbind(c(1,0,0,0,0), c(0,1,0,0,0), c(0,0,1,0,0), c(0,0,0,1,0), c(0,0,0,0,1)); r <- c(0,0,0,0,0)

##Find p value
pf(f_val, df1=Q, df2= (n-K-1),lower.tail=F)
summary(reg)

# F-test
q <- nrow(R)
S_2 <- sum(U_h^2)/(n-K-1)
# F-test for Joint-test
linearHypothesis(reg_01, hypothesis.matrix = R, rhs = r)


## 2(d) Outliers
set.seed(7); n<-15
#I generate some X-varibles and construct some
#correlation between them
x1<-rexp(n);x2<-runif(n)+0.5*x1;x3<-runif(n)+x2-x1
#I generate Y through a linear model
Y<-0.5+2*x1+2*x2-2*x3+rchisq(n,df=2)#rnorm(n,0,1)
#I run the regression
reg2<-lm(Y~x1+x2+x3)
#Create graphical diagnostics
par(mfrow=c(1,2))
plot(fitted(reg2),resid(reg2),xlab="Fitted values",
     ylab="Residuals")
plot(reg2,which=4)

data<-data.frame(Y,x1,x2,x3)
data_sub<-data[-7,]
reg3<-lm(Y~x1+x2+x3,data=data_sub)
par(mfrow=c(1,2))
plot(fitted(reg3),resid(reg3),xlab="Fitted values",
     ylab="Residuals")
plot(reg3,which=4)
summary(reg2)
summary(reg3)

# 3 Higher degree models or non linear models
# 3(a,c,d) likhti ne, jediya calculations apaa upr kr k aa rhe
# aa unaa naal tulna krni ae, te dsna k keda sb toh vdiya wa

educ_cube <- CPS1985$experience^3
educ <- CPS1985$experience^3
educ_penta <- CPS1985$experience^5

reg_03 <- lm(log(wage) ~ education+married+gender+I(experience) + educ_cube + educ_penta, data=CPS1985)
reg_04 <- lm(log(wage) ~ education+married:gender + educ, data=CPS1985)
summary(reg_03)

R <- rbind(c(1,0,0,0,0,0,0), c(0,1,0,0,0,0,0), c(0,0,1,0,0,0,0), c(0,0,0,1,0,0,0), c(0,0,0,0,1,0,0), c(0,0,0,0,0,1,0), c(0,0,0,0,0,0,1)); r <- c(0,0,0,0,0,0,0)

nlr <- vcovHC(reg_03)
# 3(b) joint testing
linearHypothesis(reg_03, hypothesis.matrix = R, rhs = r,
                 test=c("Chisq"), vcov.=nlr)
# 4(a)
reg_04
# 4(b) haa frk painda wa, mtlb k wadhdi wa