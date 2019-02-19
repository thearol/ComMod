
# Setting the wd
setwd(paste(getwd(),"/1_eyetracking",sep = ''))

# Loading data
df<-read.csv("samples2018_2019.csv")

# Setting the right variable type
df$RightEye_PositionX<-as.numeric(df$RightEye_PositionX)
df$RightEye_PositionY<-as.numeric(df$RightEye_PositionY)
df$RightEye_PupilSize<-as.numeric(df$RightEye_PupilSize)
df$RightEye_MeanVelocityX<-as.numeric(df$RightEye_MeanVelocityX)
df$RightEye_MeanVelocityY<-as.numeric(df$RightEye_MeanVelocityY)
df$RightEye_MeanAccellerationX<-as.numeric(df$RightEye_MeanAccellerationX)
df$RightEye_MeanAccellerationY<-as.numeric(df$RightEye_MeanAccellerationY)
df$LeftEye_PositionX<-as.numeric(df$LeftEye_PositionX)
df$LeftEye_PositionY<-as.numeric(df$LeftEye_PositionY)
df$LeftEye_PupilSize<-as.numeric(df$LeftEye_PupilSize)
df$LeftEye_MeanVelocityX<-as.numeric(df$LeftEye_MeanVelocityX)
df$LeftEye_MeanVelocityY<-as.numeric(df$LeftEye_MeanVelocityY)
df$LeftEye_MeanAccellerationX<-as.numeric(df$LeftEye_MeanAccellerationX)
df$LeftEye_MeanAccellerationY<-as.numeric(df$LeftEye_MeanAccellerationY)
