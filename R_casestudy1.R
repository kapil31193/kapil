setwd("E:\\R\\R files")

### Q1 ###
customers<-read.csv(file="Customers.csv",header=TRUE,stringsAsFactors = FALSE)

### Q2 ###
View(customers)
head(custumers)
tail(customers)
str(customers)
names(customers)
nrow(customers)
ncol(customers)
summary(customers)
duplicated(customers)
dim(customers)
library(psych)     # library(hmisc)
describe(customers)

### Q3 ###
sum(is.na(customers$Customer.Value))/length(customers$Customer.Value)*100
OR
mean(is.na(customers$Customer.Value))*100

### Q4 ###
customers_dupicate<- customers[duplicated(customers),]
customers_unique<-customers[!duplicated(customers),]

### Q5 ###
customer_subset<-subset(customers,Customer.Value>10000)

### Q6 ###
customers$customer_value_segment<- cut(customers$Customer.Value,breaks = c(-Inf,10000,25000,Inf),
                 labels=c("Low Value Segment" , "Mediam Value Segment" , "High Value Segment"))
OR
customers$customer_value_segment[customers$Customer.Value<10000]<- "Low Value Segment"
customers$customer_value_segment[10000<customers$Customer.Value<25000]<- "Mediam Value Segment"
customers$customer_value_segment[25000<customers$Customer.Value]<- "High Value Segment"

### Q7 ###
customers<- transform(customers,average.revenue.per.trip=Customer.Value/buy.times,
                      balance.points=Points.earned - Points.redeemed)
OR
customers<- mutate(customers,average.revenue.per.trip=Customer.Value/buy.times,
                      balance.points=Points.earned - Points.redeemed)
OR
customers<- transmute(customers,average.revenue.per.trip=Customer.Value/buy.times,
                      balance.points=Points.earned - Points.redeemed)

### Q8 ###
customers$recent.date<-as.Date(as.character(customers$recent.date),format="%Y%m%d")
day.difference<-c(customers$recent.date- Sys.Date())

### Q9 ###
yy<-aggregate(Customer.Value~Last_city+Last_region+Last_state,data=customers,FUN = sum)
total<-sum(yy$Customer.Value)
yy<-transform(yy,sale_per=Customer.Value/total*100)

### 10 ###
z<-group_by(customers,Last_state,Last_city)
zz<-summarise(z,count=n(),avr_no.Purchases=mean(buy.times,na.rm=T),avg_purchase_tran=mean(Customer.Value/buy.times,na.rm=T))
