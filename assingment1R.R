###ASSingment 1 ###
#Q1#
sessionInfo()

#Q2#
abc<-3

#Q3
a <- 5
b <- analtix
c <- as.logical(a)     #or c <- FALSE

#4
ls()      #list of all the obeject in current session#

#5
x <- c(4,4,5,6,7,2,9)
 #A  
length(x)
mean(x)
sum(x)
max(x)
min(x)
var(x)
 #B 
x[3]
x[c(1,3,5,7)]
x[2:6]

#6 
m <- matrix(data=1:24,nrow=6,ncol=4)
View(m)

#7 
 storeID <- c(111,208,113,408)
 Tenure <- c(25,34,28,52)
 storeType<- c("Type1","TYpe2","Type1","Type1")
 status <- c("Poor","Improved","Excellent","Poor")
 Df<-data.frame(storeID,Tenure,storeType,status)
 
 #8
 Df[c("storeID","Tenure")]  #a
 Df[c("storeID","status")]  #b
 Df$Tenure                  #c
 
 #9
 ethnicity <- factor( c("White","African American","White","Asian"))
 status <- factor(c("Poor","Improved","Excellent","Poor"), levels = c("Poor","Improved","Excellent"),ordered = TRUE)
 outcome <- factor(c(1,3,2,4,3,1,1),levels = c(1,2,3,4),labels = c("Poor","Average","Good","Excellent"),ordered=TRUE)
 View(outcome)
 
 #10 
 g<- "My First List"
 h <- c(25,26,18,39)
 j<-matrix(1:10,5,2)
 k<- c("one","two","three")
 
 mylist<- list( title=g,ages=h ,j,k)
 
 mylist[[2]][2]
 mylist[[3]][2,]
 mylist[c(1,2)]
 mylist[[4]][1]
 mylist[c(2,3)]
 
#11#
 getwd()

 stores <- read.csv("E:/R/R files/stores.csv")
 
 ##12##
 with(stores,summary(OperatingCost)) ##summary using "with" function .
 summary(stores$OperatingCost)       ##summary without using with function..
                                     ##Both are giving same result #No difference.
 ###13###
 
 class(stores)
 names(stores)
 length(stores)
 dim(stores)
 str(stores)
 head(stores)
 tail(stores)
 ###14###
 stores$New1<- stores$OperatingCost+stores$AcqCostPercust ##Creating new column directly by calculation & assinging
 
 stores<- transform(stores,new2=OperatingCost+AcqCostPercust) ###using transform fuction.
 
 ###15### Creating new column class with subdivisions in it.
 stores$class[stores$TotalSales<120]<- "Low perform store"
 stores$class[120<stores$TotalSales<240]<- "Average perform store"
 stores$class[stores$TotalSales>240]<- "High perform store"
 
            OR
stores$class<- cut(stores$TotalSales,breaks=c(-Inf,120,240,Inf),labels=c("low perform store","Average perform store","High perform store"))            
    
 
 ####16####
 names(stores)[names(stores)=="AcqCostPercust"]<-"AcqCost" ##Renaming AcqCostPerCus to AcqCost
 
 ###17###
x<-  is.na(data frame name) #to find the NA values.
stores[is.na(data frame name)==TRUE]<- "0" ##to replace NA values with 0
stores.NO.NA<-na.omit(stores)

###18### sorting
newstores<- stores[order(stores$StoreType),] ##using variable storetype
newstores<- stores[order(stores$Location,-(stores$TotalSales)),] ##by location in ascending &totalsales in decsending

###19###
date1 <- c("2014-06-22","2014-02-13")
date1 <- as.Date(date1)
class(q19)
#b
date2 <- c("01/05/1965","08/16/1975")
date2 <- as.Date(date2,format = '%m/%d/%Y')

###20###
part_a <- store[,c(5,7,8,9)]
part_b <- store[,-c(5,7,8,9)]
part_c <- head(store,10)
part_d <- store[store$StoreType == 'Apparel' & store$TotalSales > 100]
part_e <- with(store,subset(store,subset = TotalSales >100 & TotalSales < 300,select=c(StoreCode,StoreName,Location,TotalSales)))
part_f <- store[store$StoreType == 'Electronincs' & store$TotalSales > 100,c(1:10)]
 