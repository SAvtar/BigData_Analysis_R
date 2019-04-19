rm(list = ls())
library(ff)
library(ffbase)
df_01 <- system.time(ffx07 <- read.csv.ffdf(file="C:\\Users\\satinder singh\\Downloads\\2007.csv.bz2", header=TRUE, na.string=c("",NA),colClasses= c(Month="factor",DayOfWeek="factor", Year="factor")))
df_01
df_02 <- system.time(ffx08 <- read.csv.ffdf(file="C:\\Users\\satinder singh\\Downloads\\2008.csv.bz2", header=TRUE, na.string=c("",NA),colClasses= c(Month="factor",DayOfWeek="factor", Year="factor")))
df_02
save.ffdf(ffx07, dir = "~/df_01", clone = FALSE, relativepath = TRUE,overwrite = TRUE)
save.ffdf(ffx08, dir = "~/df_02", clone = FALSE, relativepath = TRUE,overwrite = TRUE)
load.ffdf(dir = "~/df_01")
load.ffdf(dir = "~/df_02")
subset_df_01 <- subset(ffx07, month.name = December)
subset_df_01
subset_df_02 <- subset(ffx08, Month == 12)
subset_df_02
drop_01 <- subset_df_01$Month<-droplevels(subset_df_01$Month)
drop_01
drop_02 <- subset_df_02$Month<-droplevels(subset_df_02$Month)
drop_02
library(biglm)
formula <- ArrDelay ~ Origin + Distance
system.time(reg <- biglm(formula, data = ffx07,sandwich = FALSE))
system.time(reg <- biglm(formula, data = ffx08,sandwich = FALSE))


Chunk_lm <- function(formula, data, chunkSize = 1000, sand = FALSE){ 
    for (i in chunk(data, by=chunkSize)){ 
      if (i[1]==1)
        biglmfit <- biglm(formula, data=data[i,],sandwich=sand) 
      else{
        message("Next Chunk is: ", i[[1]],":",i[[2]])
        biglmfit <- update(biglmfit, data[i,],sandwich=sand)}
    }
  
      biglmfit}
    system.time(reg_01 <- Chunk_lm(formula, data = ffx07, chunkSize = 75000))
      system.time(reg_02 <- Chunk_lm(formula, data = ffx08, chunkSize = 75000))

# Predicted values are the beta coefficient values which will be calculated from
# T test and joint hypothesis or robust testing
summary(reg_01)
summary(reg_02)
beta_h_01 <- reg_01$coef
beta_h_01
beta_h_02 <- reg_02$coef
beta_h_02
K_01 <- length(beta_h_01); K_01
K_02 <- length(beta_h_02); K_02
R_01<-rbind(numeric(K_01)); r<-c(0) 
R_02<-rbind(numeric(K_02)); r<-c(0) 
#R[,rownames(beta_h)=="Month3"]<-1 
#R[,rownames(beta_h)=="Month12"]<--1 
#Wald-value (V is V(beta_hat)/n) 
W_01<-t(R_01%*%beta_h_01-r)%*%solve(R_01%*%reg_01$V%*%t(R_01))%*% (R_01%*%beta_h_01-r) 
W_02<-t(R_02%*%beta_h_02-r)%*%solve(R_02%*%reg_02$V%*%t(R_02))%*% (R_02%*%beta_h_02-r) 
#P-value 
pchisq(W_01, df=length(r), lower.tail = FALSE)
pchisq(W_02, df=length(r), lower.tail = FALSE)


#############################################################################################################
#coordinates US boarders (included in the ggplot2 package) 
library(ggplot2)
airports <- read.csv(file="C:\\Users\\satinder singh\\Downloads\\airports.csv", stringsAsFactors = FALSE)
airports
plot_data <- data.frame(airports$IATA_CODE, airports$AIRPORT, long <- airports$LONGITUDE, lat <- airports$LATITUDE)
plot_data
map.us <- map_data(map = "state") 
p1 <- ggplot() 
p1 <- p1 + geom_polygon(data = map.us, 
                        aes(x = long, y = lat,group=group),fill = grey(0.5)) 
p1
p1 <- p1+geom_point (data = plot_data, 
                   aes (x = long, y = lat),pch = 16) 
p1 
#Split in to two plots based on year 
p1<-p1 + facet_grid(. ~ year)
p1
p1 <- p1+geom_point ( data = plot_sub,
                    aes ( x = long, 
                          y = lat, 
                          colour =pred, #ifelse(pred>0,"Delay","Ahead") ),
                          pch = 16)+ 
                      theme(legend.position=c(.5, .175))+labs(colour="Color")+ 
                      scale_colour_gradient(low = "#56B1F7", high = "#132B43")) 
p1
#Add text to points with predicted delays 
p1<-p1+geom_text(data = plot_sub,aes ( 
  x = long, 
  y = lat,label=round(pred,0)
  ),size=3.2,vjust=0.6) 
p1
plot_sub2<-subset(plot_sub,pred>=quantile(pred, probs = 0.99)) 
p1<-p1+geom_text(data = plot_sub2,aes ( 
  x = long, 
  y = lat,label=round(pred,0) ),size=3.2,vjust=0.6) 
p1                          
#Table number of flights from each airport in the data
numb07<-table(sub_07$Origin[])
numb08<-table(sub_08$Origin[])
#Make a data.frame with the information from table() 
flights<-data.frame(c(dimnames(numb07)[[1]],
                      dimnames(numb08)[[1]]), 
                      rbind(cbind(as.numeric(numb07),2007),
                            cbind(as.numeric(numb08),2008))) 
names(flights)<-c("Origin","numb","year") 
#Merge with plot_sub 
plot_sub2<-merge(plot_sub,flights,by.x=c("iata","year"), 
                 by.y=c("Origin","year"))
rm(p1) 
p1 <- ggplot() 
p1 <- p1 + geom_polygon(data=map.us,
                        aes(x = long, y = lat,group=group),fill = grey(0.5)) 
p1<-p1+geom_point ( 
  data = plot_sub2, 
  aes (x = long, y = lat,colour = pred,size =numb/1000), 
  pch = 16)+labs(size = "Flights (k)",colour="Delay")+ 
  theme(legend.position=c(0.5, .25))+ 
  scale_colour_gradient(low = "#56B1F7", high = "#132B43") 
p1<-p1 + facet_grid(. ~ year) 
p1
ggsave("~/map.pdf", plot =p1)



