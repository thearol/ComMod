### Exercises W14
# 1.
# When we have a high taget and a lot of small targets, the small targets will be overshot.
# When we have a low taget and a lot of high targets, the small targets will be overshot.
# When we have all the same tagets we will not see any effect

# 2.
setwd(paste(getwd(),'/Models of perception and action/21', sep = '')) #setting working directory

t1<-read.delim('21_1.txt',sep = ',', header = F) #loading data

colnames(t1)<-c('sample','condition','x','y') #naming variables


# 3.
library(ggplot2)
ggplot(data=t1, aes(x=x,y=y)) + #plotting data
  geom_point()

# 4.
ggplot(data=t1, aes(x=x,y=y)) + #plotting data with y and x limits
  geom_point() +
  ylim(0,600) +
  xlim(0,1900)

# 5.
distance<-function(df,add=0){ #creating a function for distance calcualtion
  for (i in (1:nrow(df))){
    if (i!=nrow(df))
    add=add+sqrt(((df[i,'x']-df[i+1,'x'])^2)+((df[i,'y']-df[i+1,'y'])^2)) #distance calcualtion
  }
  paste(add) #write output
}
distance(t1) #running the funtion

# 6.
time <- seq(from=0,to=7.28,by=0.0025) #making a time seqence with the length of the dataframe with 0.0025 increments
t1<-cbind(t1,time) #adding that seqence to the dataframe

# 7.
library(dplyr)
t1$v<-((t1$y-dplyr::lag(t1$y, n= 1L))/0.0025) #calculating velocity
t1$a<-((t1$v-dplyr::lag(t1$v, n= 1L))/0.0025) #calculating acceleration

# 8.
t1[is.na(t1)] <- 0 # changing NA's to 0

# velocity
lowpass.spline <- smooth.spline(t1$time,t1$v, spar = 0.3) #running a spline low pass filter

plot(t1$time, t1$v, type="l", lwd = 5, col = "green") #plotting unfiltered  data
lines(predict(lowpass.spline, t1$time), col = "red", lwd = 2) #plotting filtered data
sv<-predict(lowpass.spline, t1$time) #running the filter on the data
t1<-cbind(sv$y,t1) #saving the filtered data


# acceleration
lowpass.spline <- smooth.spline(t1$time,t1$a, spar = 0.3) #running a spline low pass filter

plot(t1$time, t1$a, type="l", lwd = 5, col = "green") #plotting unfiltered  data
lines(predict(lowpass.spline, t1$time), col = "red", lwd = 2) #plotting filtered data
sa<-predict(lowpass.spline, t1$time) #running the filter on the data
t1<-cbind(sa$y,t1) #saving the filtered data

# 9.
colnames(t1)<-c('sa','sv','sample','condition','x','y','time','v','a') #naming variables

t1$updn <- c(0, diff(sign(t1$sa))) #finding were the sign switches in acceleration
t1$updn<-ifelse(t1$updn==0,0,1) #making it a 1 where then sign switches
t1$updn <-as.factor(t1$updn) #making it a factor variable 

ggplot(data=t1, aes(x=x,y=y,color=updn)) + #plotting the data again
  geom_point() +
  ylim(0,600) +
  xlim(0,1900) +
  scale_color_discrete(name = "", labels = c("x-y positions", "velocity extreme"))

# 10.
t1$updn <- c(0, diff(sign(t1$sv))) #finding were the sign switches velocity
table<-which(t1$updn!=0) #save the sample numbers of the sign switches

t1$imp<-ifelse(t1$sample<table[16] | t1$sample>table[18],0,1) #marking samples with the important 6th movement
t1$imp<-ifelse(t1$updn==0,t1$imp,2) #adding peaks to the 6th movement data to the dataframe
t1$imp <-as.factor(t1$imp) #making it a factor

ggplot(data=t1, aes(x=x,y=y,color=imp, alpha = imp)) + #plotting the data
  geom_point() +
  ylim(0,600) +
  xlim(0,1900) +
  scale_alpha_manual(guide='none', values = list('0' = 0.1, '1' = 0.1, '2' = 1))+
  scale_color_discrete(name = "", labels = c("Non-important trial", "Important trail", "y velocity changing direction"))

# Bonus

# Here is a function that will run the analysis in question 10 for any set of data
mousePlot <-function(df){
  
  time <- seq(from=0,to=(nrow(df)-1)*0.0025,by=0.0025) #making a time seqence with the length of the dataframe with 0.0025 increments
  df<-cbind(df,time)
  
  colnames(df)<-c('sample','condition','x','y','time') #naming variables
  
  df$v<-((df$y-dplyr::lag(df$y, n= 1L))/0.0025) #calculating velocity
  
  df[is.na(df)] <- 0 
  
  lowpass.spline <- smooth.spline(df$time,df$v, spar = 0.3) #running a spline low pass filter
  sv<-predict(lowpass.spline, df$time) #running the filter on the data
  df<-cbind(sv$y,df) #saving the filtered data
  
  colnames(df)<-c('sv','sample','condition','x','y','time','v') #naming variables
  
  df$updn <- c(0, diff(sign(df$sv))) #finding were the sign switches velocity
  table<-which(df$updn!=0) #save the sample numbers of the sign switches

  for (n in (1:length(table))){ #finding the 6th movement
    if (df[table[n],'x']>1100 & df[table[n],'x']<1263)
      high<-n
  }
  
  df$imp<-ifelse(df$sample<table[high-1] | df$sample>table[high+1],0,1) #marking samples with the important 6th movement
  df$imp<-ifelse(df$updn==0,df$imp,2) #adding peaks to the 6th movement data to the dataframe
  df$imp <-as.factor(df$imp) #making it a factor
  
  ggplot(data=df, aes(x=x,y=y,color=imp, alpha = imp)) + #plotting the data
    geom_point() +
    ylim(0,600) +
    xlim(0,1900) +
    scale_alpha_manual(guide='none', values = list('0' = 0.1, '1' = 0.1, '2' = 1))+
    scale_color_discrete(name = "", labels = c("Non-important trial", "Important trail", "y velocity changing direction"))+
    labs(title=paste('Condition:' ,df[1,'condition']))
}

mousedata<-read.delim('21_2.txt',sep = ',', header = F) #loading data
mousePlot(mousedata) #running the function
