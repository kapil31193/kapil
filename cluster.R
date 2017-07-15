getwd()

setwd("E:\\R\\final case study +interview preparation\\unrar final case\\files")

#cleaning the Environment
rm(list=ls())

#importing Data
CC<-read.table(file="CC GENERAL.csv",header = T,sep = ",",stringsAsFactors = F )
View(CC)
str(CC)

#Data Cleaning

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


diag_stats<-t(data.frame(apply(CC[,-1], 2, mystats)))

write.csv(diag_stats, "diag_stats.csv")

#Outliers #capping at 95prctl
CC$BALANCE[CC$BALANCE>5909.111808]<- 5909.111808
CC$PURCHASES[CC$PURCHASES>3998.619500]<-3998.619500
CC$ONEOFF_PURCHASES[CC$ONEOFF_PURCHASES>2671.094000]<-2671.094000
CC$INSTALLMENTS_PURCHASES[CC$INSTALLMENTS_PURCHASES>1750.087500]<-1750.087500
CC$CASH_ADVANCE[CC$CASH_ADVANCE>4647.169122]<-4647.169122
CC$PURCHASES_TRX[CC$PURCHASES_TRX>57.000000]<-57.000000
CC$CREDIT_LIMIT[CC$CREDIT_LIMIT>12000.000000]<-12000.000000
CC$PAYMENTS[CC$PAYMENTS>6082.090595]<-6082.090595
CC$MINIMUM_PAYMENTS[CC$MINIMUM_PAYMENTS>2766.563310]<-2766.563310

View(CC)

# Missing value immputaion

sum(is.na(CC)) #checking if there is any Missing value or not.
summary(CC)
CC$CREDIT_LIMIT[is.na(CC$CREDIT_LIMIT)==TRUE]<-mean(CC$CREDIT_LIMIT,na.rm = T)
CC$MINIMUM_PAYMENTS[is.na(CC$MINIMUM_PAYMENTS)==TRUE]<-mean(CC$MINIMUM_PAYMENTS,na.rm = T)
sum(is.na(CC))

# Factor Loading to remove Multicollinearity

corr<-cor(CC[,-1])

require(psych)
require(GPArotation)

eigen(corr)$value

require(dplyr)
eigenvalue<-mutate(data.frame(eigen(corr)$value),cum.sum.eigen=cumsum(eigen(corr)$value),
                     per_var=eigen(corr)$value/sum(eigen(corr)$value),
                     cum_per_var=cumsum(per_var))
fa<-fa(r=corr,6,rotate="varimax",fm="ml")

print(fa)
sort_fa<-fa.sort(fa)
sort_fa$loadings
loading<-data.frame(sort_fa$loadings[1:ncol(CC[,-1]),])

write.csv(loading,"C:\\Users\\PARUL\\Desktop\\Analytixlab\\inter\\factor.csv")

var<-c("PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_FREQUENCY","INSTALLMENTS_PURCHASES",
       "ONEOFF_PURCHASES","PURCHASES","BALANCE","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT",
       "CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","TENURE","ONEOFF_PURCHASES_FREQUENCY"
       )

inputdata_final <-CC[,var]

#Prepare final Data
#standardizing the data
inputdata_final = scale(inputdata_final)
View(inputdata_final)
#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

CC_new<-cbind(CC,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(CC_new)
