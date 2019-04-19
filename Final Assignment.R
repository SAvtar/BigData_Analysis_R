#1(a)
problem<- function(suits, cards){
suits <- c("Heart", "Diamond", "Club", "Spade")
cards <- c("Ace", "King", "Queen", "Jack", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
deck <- outer(suits, cards, paste)
deck[5]
while (numOfDeck == 1){
  for (i in suits){
    for (j in cards){
      deck <- rbind.data.frame(deck, cbind.data.frame(j, i))
    }
  }
  numOfDeck = numOfDeck + 1
}
}
hand <- sample (1:52, 5, prob = NULL)
replicate(n = 30, expr = sample(deck[hand]))
a <- rnorm(n = 5,  mean = hand/13)
plot(a)
probabilities <- rnorm(n = 50,  mean = hand/13)
rexp(probabilities)
plot(probabilities)
#1(b) and (c)
probs<- rnorm(n = 500,  mean = hand/13)
rexp(probs)
plot(probs)
# 2(a)
library(ff)
library(ffbase)
airlines_2008 <- (ffx07<- read.csv.ffdf(file = "C:\\Users\\dell\\Desktop\\new2008.csv",header=TRUE, na.string=c("",NA),colClasses=
                                         c(DayOfWeek="factor", UniqueCarrier = "factor", Distance = "factor")))
airlines_2008
#2(b)
df_02 <- subset(ffx07, Month == 12)
df_02
#2(c)
df <- as.ffdf(airlines_2008$DayOfWeek, airlines_2008$UniqueCarrier, airlines_2008$Distance)
df
#2(d)
samplesize = length(df)
samplesize
#2(e)
library(biglm)
formula <- ActualElapsedTime ~ DayOfWeek + UniqueCarrier + Distance
reg <- biglm(formula, data = airlines_2008)
summary(reg)

#Bob's Data
df_bob <- data.frame(df$DayOfWeek == 4, df$UniqueCarrier== '9E', df$Distance == 1000)
# Susanna's Data
df_susanna <- data.frame(df$DayOfWeek == 6 | df$DayOfWeek == 'Saturday', df$UniqueCarrier== '9E', df$Distance == 1000)
# Predicted Average time
avg_time_bob <- predict(beta_h = reg$coef, data = df_bob)
avg_time_susanna <- predict(beta_h = reg$coef, data = df_susanna)
summary(reg, mean(time(ArrTime)))
#2(f)
avg_time_bob
#2(g)
avg_time_susanna

#2(h)
V_b<-vcovHC(reg) 
coeftest(reg,vcov=V_b)
#2(i)
# The predicted avg time significantly varies on the bases of
# week day which is almost negligible.

#3(a)
airlines_2007 <- read.csv(file="C:\\Users\\dell\\Desktop\\new2007.csv", header = TRUE, stringsAsFactors = FALSE)
samplesize_07 = length(airlines_2007)
samplesize_07
#3(b)
formula_07 <- ArrDelay ~ Origin + Month + Distance
reg_07 <- biglm(formula, data = airlines_2007)
reg_07
formula_08 <- ArrDelay ~ Origin + Month + Distance
reg_08 <- biglm(formula, data = airlines_2008)
#3(c)
df_sue <- subset(df$DayOfMonth[12,25], df$UniqueCarrier== 'EWR', df$Distance == 10000)
reg_sue <-  biglm(formula, data = df_sue)
pred_sue <- predict(beta_h = reg_sue$coef, data = df_sue)
summary(pred_sue)
#3(d)
#This function produces a data frame including predicted values for all
#factor levels in a Chunk_lm regression. The numerical variables are held
#at the sample averages. 


#ports: The name of the factor of the airports
#reqd_prmtrs: The names of all other X-variables in the regression
#beta_h: the regression coefficients
#data: the data used for the regression
df_airlines07 <- subset(airlines_2007, Month == 12)
airports <- read.csv(file="C:\\Users\\dell\\Desktop\\airports.csv", stringsAsFactors = FALSE)
airports
#df_03 <- subset(airports)
#datatotal <- merge(df_02,df_03, by = c(colnames(do.NULL = TRUE)))
reg_airports <- biglm(formula_08, data = airports)

# Regression Using Chunk_lm function for Big Data
Chunk_lm <- function(formula, data, chunkSize = 1000, sand = FALSE){ 
  for (i in chunk(data, by=chunkSize)){ 
    if (i[1]==1)
      biglmfit <- biglm(formula, data=data[i,],sandwich=sand) 
    else{
      message("Next Chunk is: ", i[[1]],":",i[[2]])
      biglmfit <- update(biglmfit, data[i,],sandwich=sand)}
  }
  
  biglmfit}
system.time(reg_01 <- Chunk_lm(formula, data = ffx07, chunkSize = 70000))
system.time(reg_02 <- Chunk_lm(formula, data = airports, chunkSize = 70000))

pred_airports<-function(ports="Origin",reqd_prmtrs=c("Month","Distance","Year"),beta_h,data){
  #To select variable ports
  arg<-names(airports)==ports
  #To select ports coefficients
  if (length(rownames(beta_h <- reg_airports$coef))>0)
    coef_nam<-rownames(beta_h)
  else
    coef_nam<-names(beta_h)
  
  
  arg2<-grepl(ports, coef_nam)
  arg2[coef_nam =="(Intercept)"]<-TRUE
  #ports names in the coefficinet names 
  nam<-gsub(ports,"", coef_nam)
  #find reference airport
  arg3<-levels(data[,arg])%in%nam
  ref_level<-levels(data[,arg])[!arg3]
  if (length(ref_level)>1){
    nam[nam=="(Intercept)"]<-ref_level[1]
    print("There are several candidates as reference level")
  }
  else{
    nam[nam=="(Intercept)"]<-ref_level
  }
  #Create the X-matrix for each airport
  dummies<-diag(1,(length(nam[arg2])))
  dummies[,1]<-1
  #Compute the part of the predicted values due to airport
  preds<-data.frame(nam[arg2],pred=dummies%*%beta_h[arg2])
  nam_ports<-nam[arg2]
  names(preds)[1]<-ports
  # Identify factors among X-variables
  arg4<-names(data)%in%reqd_prmtrs
  data2<-subset(data,c(TRUE,TRUE,FALSE))
  X_factor<-sapply(data2[,],is.factor)[arg4]
  
  #Compute average for numerical X-variables 
  k<-length(X_factor[!X_factor])
  if (k>0){
    if (k>1){
      mean_num<-NULL
      for (i in 1:k){
        mean_num<-c(mean_num,mean(data[,arg4][,!X_factor][,i],na.rm=TRUE))
      }
    }
    else
      mean_num<-mean(data[,arg4][,!X_factor],na.rm=TRUE)
    
    mean_num<-as.matrix(mean_num)
    #Add the averages of the numerical X-variables to preds
    arg5<-coef_nam%in%names(data)[arg4][!X_factor]
    preds$pred<-preds$pred+sum(t(mean_num)%*%beta_h[arg5])
  }
  
  predy<-preds$pred
  
  #Add predicted values for factor X-variables to preds
  #At the moment this only works with one factor except for ports
  k<-length(X_factor[X_factor])
  nam_factors<-names(X_factor)[X_factor]
  if (k>1)
    stop("Works with only one factor except for ports")
  
  if (k>0){
    mean_num<-NULL
    for (i in 1:k){
      expr1<-parse(text=paste("levels(data$",nam_factors[i],")",sep=""))
      lev<-eval(expr1)
      n_l<-length(lev) 
      
      nam2<-gsub(nam_factors[i],"", coef_nam)
      arg_l<-nam2%in%lev
      arg_l_ref<-!lev%in%nam2
      preds<-data.frame(lev[arg_l_ref],preds)
      names(preds)[1]<-c(nam_factors[i])
      for (l in 1:(n_l-1)){
        arg_coef_l<-nam2==lev[!arg_l_ref][l]
        pre<-predy+beta_h[arg_coef_l]
        df<-data.frame(lev[!arg_l_ref][l],nam_ports,pre)
        names(df)<-names(preds)
        preds<-rbind(preds,df)
      }
      #predy<-preds$pred
      #expr2<-parse(text=paste("preds$",ports,sep=""))
      #nam_ports<-eval(expr2)
    }
  }
  preds
}
pred_data<-data.frame(Origin=c(names(pred07),names(pred08)),
                      rbind(data.frame(pred=pred07,year=2007),data.frame(pred=pred08,year=2008)))
names(pred_data)<-c("Origin","pred","year")

