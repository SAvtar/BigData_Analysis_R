#This function produces a data frame including predicted values for all
#factor levels in a Chunk_lm regression. The numerical variables are held
#at the sample averages. 


#ports: The name of the factor of the airports
#Other_X: The names of all other X-variables in the regression
#beta_h: the regression coefficients
#data: the data used for the regression



pred_airports<-function(ports="Origin",Other_X=c("Month","Distance"),beta_h,data){
  #To select variable ports
  arg<-names(data)==ports
  #To select ports coefficients
  if (length(rownames(beta_h))>0)
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
  arg4<-names(data)%in%Other_X
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
    stop("At the moment this function only works with one factor except for ports")
  
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