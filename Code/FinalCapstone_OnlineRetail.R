onlineretail.df <- read.csv("/Users/vinemerline/Desktop/CapstoneProject/Uncleaned_OnlineRetail.csv")

#********************************* BEGIN CLEANING AND PREPARATION *****************************************************************
# Ignoring null values
retail <- na.omit(onlineretail.df)
dim(retail)
summary(retail)
head(retail)
tail(retail)
names(retail)

# check duplicates
duplicated(retail)
sum(duplicated(retail))
dim(retail)

#Drop duplicates
retail <- retail[!duplicated(retail),]
dim(retail)

# Drop transactions with price zero
retail<-subset(retail, UnitPrice!=0)
dim(retail)

# Convert datetime to proper datatype
datesFormat1 <-  as.POSIXct(retail$InvoiceDate, format = "%d/%m/%y %H:%M")
datesFormat2 <-  as.POSIXct(retail$InvoiceDate, format = "%d-%m-%Y %H:%M")
datesFormat1[is.na(datesFormat1)] <- datesFormat2[!is.na(datesFormat2)] # Combine both formats
retail$formattedInvoiceDate <-datesFormat1 #store it back as column in the data-frame
retail$InvoiceDate <-date(retail$formattedInvoiceDate)

head(retail)

retail$PurchaseAmount = retail$Quantity*retail$UnitPrice
head(retail$PurchaseAmount)
head(retail)
tail(retail)
############################################# Explore Data/ Data Visualization ##################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(coop)

head(retail$InvoiceDate)

##Time Series Analysis
NumInvoices <- retail %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(NumOrders=n_distinct(InvoiceNo))


options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(NumInvoices, aes(x=InvoiceDate, y=NumOrders)) +
  geom_line(color="darkgreen") +
  labs(title="Number of Orders over Time") +
  ylab("Number of Orders")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))

NumInvoices <- retail %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(NumOrders=n_distinct(InvoiceNo))

#plotting again
ggplot(NumInvoices, aes(x=InvoiceDate, y=NumOrders)) +
  geom_line(color="darkgreen") +
  labs(title="Number of Orders over Time") +
  ylab("Number of Orders")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
#calculate the sales column
retail <- retail %>%
  mutate(Sales=Quantity*UnitPrice)

Revenue <- retail %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(Sales=sum(Sales))

ggplot(Revenue, aes(x=InvoiceDate, y=Sales)) +
  geom_line(color="darkgreen") +
  labs(title="Revenue over Time") +
  ylab("Sales")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))

#aggregating data so that one row represents one purchase order
Invoice <- retail %>%
  group_by(InvoiceNo, InvoiceDate) %>%
  summarize(CustomerID=max(CustomerID), Sales=sum(Sales))

#aggregating data into months
InvoiceCustomer <- Invoice %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"), CustomerID) %>%
  summarise(Count=n_distinct(InvoiceNo), Sales=sum(Sales))

#filter out customer with 1 order and NA CustomerId
RepeatCustomers <-InvoiceCustomer%>%
  filter(!is.na(CustomerID)) %>%
  filter(Count>1)

RepeatCustomers <- RepeatCustomers %>%
  group_by(InvoiceDate) %>%
  summarize(Count=n_distinct(CustomerID), Sales=sum(Sales))

#total number of monthly customers
UniqueCustomers <- retail %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(Count=n_distinct(CustomerID))

#find the percentage of monthly revenue that are attributed to the repeat customers
RepeatCustomers$Perc <- RepeatCustomers$Sales/Revenue$Sales*100.0
#append unique customers
RepeatCustomers$Total <- UniqueCustomers$Count
#visualize repeat customers data
ggplot(RepeatCustomers) +
  geom_line(aes(x=InvoiceDate, y=Total), stat="identity", color="navy") +
  geom_line(aes(x=InvoiceDate, y=Count), stat="identity", color="orange") +
  geom_bar(aes(x=InvoiceDate, y=Perc*20), stat="identity", color="gray", alpha=0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name="Percentage (%)")) +
  labs(title="Number of Unique vs Repeat & Revenue from Repeat Customers") +
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
#number of items sold for each product for each period
PopularProducts <- retail %>%
  group_by(InvoiceDate = floor_date(InvoiceDate, "month"), StockCode) %>%
  summarise(Quantity=sum(Quantity))

#let's find top 5 items sold in November 2011 (most recent trends)
Top5 <- PopularProducts %>%
  filter(InvoiceDate=="2011-11-01") %>%
  arrange(desc(Quantity)) %>%
  top_n(5)

#here we take the data from PopularProducts data frame with the stock codes in Top5
Top5monthly <- PopularProducts[
  which(PopularProducts$StockCode %in% Top5$StockCode),]

ggplot(Top5monthly, aes(x=InvoiceDate, y=Quantity, color=StockCode)) +
  geom_line() +
  labs(title="Top 5 Popular Products over Time") +
  ylab("Number of purchases")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
################################## RMF Analysis ####################################################################################
# 1. Calcualte Recency
# Compute the maximum date to know the last transaction date
max_date<-head(max(retail$formattedInvoiceDate))
max_date

# Compute the difference between max date and transaction date
retail$Diff = max_date - retail$formattedInvoiceDate
retail$Diff <- as.numeric(difftime(max_date, retail$formattedInvoiceDate), units = "days")
retail$Diff<-trunc(retail$Diff, units = "days")

# Compute last transaction date to get the recency of customers
recency <- aggregate(x = retail$Diff, 
                     by = retail['CustomerID'], 
                     FUN = min) 

# 2. Calcualte monetary
monetary <- aggregate(x = retail$PurchaseAmount, 
                      by = retail['CustomerID'], 
                      FUN = sum) 

# 3. Calcualte frequency
freq <- aggregate(x = retail$InvoiceNo, 
                  by = retail['CustomerID'], 
                  FUN = length)# 

# Merging the three columns monetary, freq and recency
df<-merge(monetary,freq, by='CustomerID')
rmfdf<-merge(df,recency, by='CustomerID')
# Rename column where names is x.y and x.y as Monetary, Freq and Recency
names(rmfdf)[names(rmfdf) == "x.x"] <- "Monetary"
names(rmfdf)[names(rmfdf) == "x.y"] <- "Frequency"
names(rmfdf)[names(rmfdf) == "x"] <- "Recency"
head(rmfdf)

#write.csv(rmfdf,"/Users/vinemerline/Desktop/CapstoneProject/rmfdf.csv", row.names = FALSE)

library(e1071)
#If the skewness is less than -1 or greater than 1, the data are highly skewed.
skewness(rmfdf$Monetary) 
# 21.68765
skewness(rmfdf$Frequency) 
#18.08154
skewness(rmfdf$Recency)
#1.248497

## Boxplot to check for outliers
boxplot(rmfdf$Monetary, ylab = "Monetary")
boxplot(rmfdf$Frequency, ylab = "Frequency")
boxplot(rmfdf$Recency, ylab = "Recency")

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(rmfdf$Monetary, .25)
Q3 <- quantile(rmfdf$Monetary, .75)
IQR <- IQR(rmfdf$Monetary)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
rmfdf <- subset(rmfdf, rmfdf$Monetary> (Q1 - 1.5*IQR) & rmfdf$Monetary< (Q3 + 1.5*IQR))

## boxplot to check for outliers
boxplot(rmfdf$Monetary, ylab = "Monetary")
boxplot(rmfdf$Frequency, ylab = "Frequency")
boxplot(rmfdf$Recency, ylab = "Recency")

skewness(rmfdf$Monetary) # 1.357965
skewness(rmfdf$Frequency) #3.837673
skewness(rmfdf$Recency) #1.13742

#	Use ggplot2 package to draw a scatter plot between Monetary and Frequency, color the dots based on the sales_high
# Set the color according to the Sales
ggplot(sales.df, aes(x=Monetary, y=Frequency)) + geom_point(aes(col=sales_high), size=3)
ggplot(sales.df, aes(x=Recency, y=Monetary)) + geom_point(aes(col=sales_high), size=3) 
ggplot(sales.df, aes(x=Recency, y=Frequency)) + geom_point(aes(col=sales_high), size=3)

################################ Customer Lifetime Value(CLTV):########################################################################
# Drop cancelled transactions -> Removing the rows that starts with C 
#load dplyr package
library(dplyr) 
retail<-filter(retail, !grepl("C",InvoiceNo))
head(retail)
dim(retail)

# Find total transaction, total unit, total price
# cltv_df dataframe is created in order for ease of calculation and observation
InvoiceNo <- aggregate(x = retail$InvoiceNo, 
                       by = retail['CustomerID'], 
                       FUN = length)


Quantity <- aggregate(x = retail$Quantity, 
                      by = retail['CustomerID'], 
                      FUN = sum)


UnitPrice <- aggregate(x = retail$UnitPrice, 
                       by = retail['CustomerID'], 
                       FUN = sum)

# Merging  Quantity,InvoiceNo and UnitPrice
df<-merge(Quantity,InvoiceNo, by='CustomerID')
df<-merge(df,UnitPrice, by='CustomerID')

# Rename column where names is x, x.y and x.y 
names(df)[names(df) == "x.x"] <- "total_unit"
names(df)[names(df) == "x.y"] <- "total_transaction"
names(df)[names(df) == "x"] <- "total_price"

#total_sales
df$total_sales<-df$total_unit*df$total_price
head(df)

#AOV calculation
df$avg_order_value= df$total_price/df$total_transaction
head(df)

#Find No of Customer
list<-dim(df)
list[1]

#purchase_frequency
df$purchase_frequency = df$total_transaction/list[1]
head(df)


#repeat rate
repeat_rate <- dim(df[df$total_transaction > 1, ])[1]/list[1]
repeat_rate

#churn rate
churn_rate = 1- repeat_rate
churn_rate

# assuming profit margin : 5 %
df$profit_margin = df$total_price* 0.05
head(df)

# Customer Value(CV):
df$CV = df$avg_order_value * df$purchase_frequency
head(df)

#Customer Lifetime Value(CLTV):
df$CLTV = (df$CV /churn_rate) * df$profit_margin
cltv<-df
head(cltv)
summary(cltv)


#considering  columns Monetary, Freq, Recency  and cltv 
complete.df<-merge(rmfdf,cltv, by='CustomerID')
head(complete.df)

library(e1071)
#If the skewness is less than -1 or greater than 1, the data are highly skewed.
skewness(complete.df$CLTV) #18.30551

## boxplot to check for outliers
boxplot(complete.df$CLTV, ylab = "CLTV")

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(complete.df$CLTV, .25)
Q3 <- quantile(complete.df$CLTV, .75)
IQR <- IQR(complete.df$CLTV)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
complete.df <- subset(complete.df, complete.df$CLTV> (Q1 - 1.5*IQR) & complete.df$CLTV< (Q3 + 1.5*IQR))
## boxplot to check for outliers
boxplot(complete.df$CLTV, ylab = "CLTV")
skewness(complete.df$CLTV) #  1.754201

dim(complete.df) 
head(complete.df) 
head(complete.df[c(2,3,4,13)])
boxplot(complete.df$Frequency, ylab = "Frequency")
boxplot(complete.df$Monetary, ylab = "Monetary")
boxplot(complete.df$Recency, ylab = "Recency")
boxplot(complete.df$CLTV, ylab = "CLTV")

################################ K means algorithm - hierarchical clustering  ###############################################
head(complete.df) 
#write.csv(complete.df,"/Users/vinemerline/Desktop/CapstoneProject/complete.csv", row.names = FALSE)

kmeanshc.df<-complete.df[c(1,2,3,4)]
head(kmeanshc.df)
summary(kmeanshc.df)


plot(kmeanshc.df$Frequency ~ kmeanshc.df$Monetary, xlab ="Monetary", ylab = "Frequency")
plot(kmeanshc.df$Recency ~ kmeanshc.df$Monetary, xlab ="Monetary", ylab = "Recency")
plot(kmeanshc.df$Recency ~ kmeanshc.df$Frequency, xlab ="Frequency", ylab = "Recency")

# set row names to the column
kmeanshc.df[,1]
row.names(kmeanshc.df) <- kmeanshc.df[,1]
summary(kmeanshc.df)
row.names(kmeanshc.df)

kmeanshc1.df <- kmeanshc.df[,-1]
summary(kmeanshc1.df)
row.names(kmeanshc1.df)

# normalize input variables. 
library(caret)
kmeanshc.norm <- preProcess(kmeanshc1.df, method=c("center", "scale"))
kmeanshc.norm.df <- predict(kmeanshc.norm, kmeanshc1.df)
summary(kmeanshc.norm.df)

# Label rows for your normalized data
row.names(kmeanshc.norm.df)

# compute normalized distance based on all 3 variables
kmeanshcdnorm <- dist(kmeanshc.norm.df[,], method = "euclidean")
kmeanshcdnorm

# hclust() set argument method = "ward.D", "single", "complete", "average", "median", or "centroid"
##########Use  method complete
kmeanshchc1 <- hclust(kmeanshcdnorm, method = "complete")
plot(kmeanshchc1, hang = -1, ann = FALSE)
#Get membership numbers : 
kmeanshcmemb1 <- cutree(kmeanshchc1, k = 6)
kmeanshcmemb1[1:100]
#Get membership numbers : 
kmeanshcmemb2 <- cutree(kmeanshchc1, k = 3)
kmeanshcmemb2[1:100]
###
aggregate(kmeanshc.norm.df, list(kmeanshcmemb1),mean)
aggregate(kmeanshc.norm.df, list(kmeanshcmemb2),mean)

# set labels as cluster membership and utility name
row.names(kmeanshc.norm.df) <- paste(kmeanshcmemb1, ": ", row.names(kmeanshc.df), sep = "")
row.names(kmeanshc1.df) <- paste(kmeanshcmemb1, ": ", row.names(kmeanshc.df), sep = "")
# plot heatmap 
heatmap(as.matrix(kmeanshc.norm.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
heatmap(as.matrix(kmeanshc1.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

# set labels as cluster membership and utility name
row.names(kmeanshc.norm.df) <- paste(kmeanshcmemb2, ": ", row.names(kmeanshc.df), sep = "")
row.names(kmeanshc1.df) <- paste(kmeanshcmemb2, ": ", row.names(kmeanshc.df), sep = "")
# plot heatmap 
heatmap(as.matrix(kmeanshc.norm.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
heatmap(as.matrix(kmeanshc1.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

##########Use  method average
kmeanshchc2 <- hclust(kmeanshcdnorm, method = "average") # among all the distance what is the maximum distance- we get a different dendogram
plot(kmeanshchc2, hang = -1, ann = FALSE)
#Get membership numbers : 
kmeanshcmemb <- cutree(kmeanshchc2, k = 6)
kmeanshcmemb
# set labels as cluster membership and  name
row.names(kmeanshc.norm.df) <- paste(kmeanshcmemb, ": ", row.names(kmeanshc.df), sep = "")
row.names(kmeanshc1.df) <- paste(kmeanshcmemb, ": ", row.names(kmeanshc.df), sep = "")
# plot heatmap 
heatmap(as.matrix(kmeanshc.norm.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
heatmap(as.matrix(kmeanshc1.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

########Use  method centroid
kmeanshchc3 <- hclust(kmeanshcdnorm, method = "centroid") # among all the distance what is the maximum distance- we get a different dendogram
plot(kmeanshchc3, hang = -1, ann = FALSE)
#Get membership numbers : 
kmeanshcmemb <- cutree(kmeanshchc3, k = 6)
kmeanshcmemb
# set labels as cluster membership and  name
row.names(kmeanshc.norm.df) <- paste(kmeanshcmemb, ": ", row.names(kmeanshc.df), sep = "")
row.names(kmeanshc1.df) <- paste(kmeanshcmemb, ": ", row.names(kmeanshc.df), sep = "")
# plot heatmap 
heatmap(as.matrix(kmeanshc.norm.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
heatmap(as.matrix(kmeanshc1.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

######################## K means algorithm - non hierarchial clustering  ###############################################
head(complete.df) 
kmeans.non.hc.df<-complete.df[c(1,2,3,4)]
head(kmeans.non.hc.df)


# set row names to the utilities column
row.names(kmeans.non.hc.df) <- kmeans.non.hc.df[,1]
summary(kmeans.non.hc.df)

kmeans.non.hc.df1 <- kmeans.non.hc.df[,-1]
summary(kmeans.non.hc.df1)
row.names(kmeans.non.hc.df1)

# normalized distance:
kmeans.non.hc.norm <- preProcess(kmeans.non.hc.df1, method=c("center", "scale"))
kmeans.non.hc.norm.df <- predict(kmeans.non.hc.norm, kmeans.non.hc.df1)
summary(kmeans.non.hc.norm.df)

# Label rows for your normalized data
row.names(kmeans.non.hc.norm.df) <- row.names(kmeans.non.hc.df)
row.names(kmeans.non.hc.df)

# run kmeans algorithm 
set.seed(2)
kmeans.non.hc.km <- kmeans(kmeans.non.hc.norm.df, 6)

# show cluster membership --- shows cluster members
kmeans.non.hc.km$cluster[1:100]

# centroids
kmeans.non.hc.km$centers

##### plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(kmeans.non.hc.km$centers), max(kmeans.non.hc.km$centers)), xlim = c(0, 3))
# label x-axes
axis(1, at = c(1:3), labels = names(kmeans.non.hc.df1))

# plot centroids
for (i in c(1:6)){
  lines(kmeans.non.hc.km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                                     "black", "dark grey"))}
# name clusters
text(x = 0.5, y = kmeans.non.hc.km$centers[, 1], labels = paste("Cluster", c(1:6)))


#### 
dist(kmeans.non.hc.km$centers)
kmeans.non.hc.km
kmeans.non.hc.km$withinss
#303.3968 461.8821 256.4259 366.1651 328.2193 600.5810
sum(kmeans.non.hc.km$withinss)
#2316.67
mean(kmeans.non.hc.km$withinss)
#386.1117
kmeans.non.hc.km1 <- kmeans(kmeans.non.hc.norm.df, 1)
kmeans.non.hc.km1$withinss
#10233

## Effect of varying k -  This time the measure can be average within SS rather than accuracy
kmeans.non.hc.kvsWithinSS.df <- data.frame(k = seq(1, 10, 1), AvgWithinSS = rep(0, 10)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
kmeans.non.hc.kvsWithinSS.df

# compute AvgWithinSS for different k
for(i in 1:10) {
  kmeans.non.hc.kvsWithinSS.df[i,2] <-  mean(kmeans(kmeans.non.hc.norm.df, i)$withinss)
}
plot(kmeans.non.hc.kvsWithinSS.df$AvgWithinSS ~ kmeans.non.hc.kvsWithinSS.df$k, xlab="k", ylab = "Average WithinSS", lty=2, type="l")

summary(kmeans.non.hc.df)
kmeans.non.hc.df$cluster <- kmeans.non.hc.km$cluster
head(kmeans.non.hc.df)
aggregate(kmeans.non.hc.df$Monetary, by=list(kmeans.non.hc.df$cluster), FUN=mean)
aggregate(kmeans.non.hc.df$Frequency, by=list(kmeans.non.hc.df$cluster), FUN=mean)
aggregate(kmeans.non.hc.df$Recency, by=list(kmeans.non.hc.df$cluster), FUN=mean)
######################################## predict sales greater than means sales - knn algorithm ########################################################
head(complete.df) 
summary(sales.df)
sales.df<-complete.df[c(1,2,3,4,8,13)]
head(sales.df)
sales.df$Frequency<-as.numeric(sales.df$Frequency)

sales.df[,"total_sales"]
sales.df$sales_high<-ifelse(sales.df[,"total_sales"] > mean(sales.df[,"total_sales"]),"1","0")
sales.df$sales_high
sales.df$sales_high<-as.factor(sales.df$sales_high)
head(sales.df)
sales.df<-sales.df[c(1,2,3,4,6,7)]
head(sales.df)

sales.df <- sales.df[,-c(1)]
names(sales.df)
head(sales.df)
dim(sales.df)
summary(sales.df)

new.df <- data.frame("Monetary"=500,"Frequency"=35, "Recency"=50, "CLTV"=3)
new.df
dim(new.df)

set.seed(1)
## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(sales.df), dim(sales.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(sales.df), train.rows), 
                     dim(sales.df)[1]*0.3)
test.rows <- setdiff(rownames(sales.df), union(train.rows, valid.rows))
train.df <- sales.df[train.rows, ]
dim(train.df)
valid.df <- sales.df[valid.rows, ]
dim(valid.df)
test.df <- sales.df[test.rows, ]
dim(test.df)


# initialize normalized training, validation data, assign (temporarily) data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
sales.norm.df <- sales.df
head(train.norm.df)
summary(train.norm.df)
new.norm.df <- new.df
new.norm.df

# use preProcess() from the caret package to normalize the variables.
library(caret)
norm.values <- preProcess(train.df[ , c(1:4, 5)], method=c("center", "scale"))
head(norm.values)
train.norm.df[ , c(1:4, 5)] <- predict(norm.values, train.df[ ,c(1:4, 5)])
head(train.norm.df)
##Similarly scale valid data, bank data and the test data
valid.norm.df[ ,c(1:4, 5)] <- predict(norm.values, valid.df[ , c(1:4, 5)])
sales.norm.df[ , c(1:4, 5)] <- predict(norm.values, sales.df[ , c(1:4, 5)])
test.norm.df[ , c(1:4, 5)] <- predict(norm.values, test.df[ , c(1:4, 5)])
new.norm.df[ , c(1:4)] <- predict(norm.values, new.df[ , c(1:4)])
new.norm.df

# initialize a data frame with two columns: k, and calculating accuracy for valid dataset
sales_accuracy1.df <- data.frame(k = seq(1, 20 , 1), accuracy = rep(0, 20)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
sales_accuracy1.df
library(FNN)
#install.packages("forecast")
library(forecast)
# compute knn for different k on validation.
for(i in 1:20) {
  #Use knn function with k=i and predict for valid dataset
  sales_knn1.pred <- knn(train = train.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                         cl = train.norm.df[, 5], k = i)
  sales_accuracy1.df[i, 2] <- confusionMatrix(sales_knn1.pred, as.factor(valid.df[, 5]))$overall[1] 
}
sales_accuracy1.df
#    k  accuracy
#1   1 0.9433040
#2   2 0.9452590
#3   3 0.9579668
#4   4 0.9569892
#5   5 0.9569892
#6   6 0.9530792
#7   7 0.9521017
#8   8 0.9560117
#9   9 0.9569892
#10 10 0.9550342
#11 11 0.9560117
#12 12 0.9540567
#13 13 0.9540567
#14 14 0.9550342
#15 15 0.9569892
#16 16 0.9540567
#17 17 0.9560117
#18 18 0.9560117
#19 19 0.9579668
#20 20 0.9579668

#accuracy on valid data with best k (k=3) using training data to train and valid data as test
sales_knn_valid.pred <- knn(train = train.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                            cl = train.norm.df[, 5], k = 3)
confusionMatrix(sales_knn_valid.pred, as.factor(valid.df[, 5]))
#Confusion Matrix and Statistics
#Reference
#Prediction   0   1
#0 731  24
#1  19 249

#Accuracy : 0.958              
#95% CI : (0.9438, 0.9694)   
#No Information Rate : 0.7331             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8919             

#Mcnemar's Test P-Value : 0.5419             
                                             
#            Sensitivity : 0.9747             
#            Specificity : 0.9121             
#         Pos Pred Value : 0.9682             
#         Neg Pred Value : 0.9291             
#             Prevalence : 0.7331             
#         Detection Rate : 0.7146             
#   Detection Prevalence : 0.7380             
#      Balanced Accuracy : 0.9434             
                                             
#       'Positive' Class : 0    

#accuracy on valid data with best k (k=3) using full dataset to train and valid data as test
sales_knn_valid.pred <- knn(train = sales.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                            cl = sales.norm.df[, 5], k = 3)
confusionMatrix(sales_knn_valid.pred, as.factor(valid.df[, 5]))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 738  16
#1  12 257

#Accuracy : 0.9726             
#95% CI : (0.9607, 0.9817)   
#No Information Rate : 0.7331             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.9297             

#Mcnemar's Test P-Value : 0.5708             
                                             
#            Sensitivity : 0.9840             
#            Specificity : 0.9414             
#         Pos Pred Value : 0.9788             
#         Neg Pred Value : 0.9554             
#             Prevalence : 0.7331             
#         Detection Rate : 0.7214             
#   Detection Prevalence : 0.7370             
#      Balanced Accuracy : 0.9627             
                                             
#       'Positive' Class : 0  

#accuracy on valid data with best k (k=3) using training data to train and test data as test
sales_knn_valid.pred <- knn(train = train.norm.df[ , c(1:4)], test = test.norm.df[ , c(1:4)], 
                            cl = train.norm.df[, 5], k = 3)
confusionMatrix(sales_knn_valid.pred, as.factor(test.df[, 5]))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
  #0 472  17
#1  16 178

#Accuracy : 0.9517             
#95% CI : (0.9328, 0.9665)   
#No Information Rate : 0.7145             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8814             

#Mcnemar's Test P-Value : 1                  
                                             
#            Sensitivity : 0.9672             
#            Specificity : 0.9128             
#         Pos Pred Value : 0.9652             
#         Neg Pred Value : 0.9175             
#             Prevalence : 0.7145             
#         Detection Rate : 0.6911             
#   Detection Prevalence : 0.7160             
#      Balanced Accuracy : 0.9400             
                                             
#       'Positive' Class : 0     

#accuracy on test data with best k (k=3)
sales_knn_test.pred <- knn(train = sales.norm.df[ , c(1:4)], test = test.norm.df[ , c(1:4)], 
                           cl = sales.df[, 5], k = 3)
confusionMatrix(sales_knn_test.pred, as.factor(test.df[, 5]))
#summary(sales_knn_test.pred)
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 479   8
#1   9 187

#Accuracy : 0.9751             
#95% CI : (0.9604, 0.9854)   
#No Information Rate : 0.7145             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.9391             

#Mcnemar's Test P-Value : 1                  
                                             
#            Sensitivity : 0.9816             
#            Specificity : 0.9590             
#         Pos Pred Value : 0.9836             
#         Neg Pred Value : 0.9541             
#             Prevalence : 0.7145             
#         Detection Rate : 0.7013             
#   Detection Prevalence : 0.7130             
#      Balanced Accuracy : 0.9703             
                                             
#       'Positive' Class : 0     


#new data
sales_knn_test.pred <- knn(train = sales.norm.df[ , c(1:4)], test = new.norm.df, 
                           cl = sales.df[, 5], k = 3)
row.names(sales.df)[attr(sales_knn_test.pred, "nn.index")]
##Display the row names
#[1] "1782" "3358" "143" 
sales.df[attr(sales_knn_test.pred, "nn.index"),]
#Monetary Frequency Recency     CLTV sales_high
#1782   476.96        31      53 2.419933          0
#3358   541.53        36      39 2.613554          0
#143    519.61        34      39 1.419725          0
#summary(sales_knn_test.pred)
######################################## predict sales greater than mean sales - decision trees ##############

sales.df<-complete.df[c(1,2,3,4,8,13)]
head(sales.df)
sales.df$Frequency<-as.numeric(sales.df$Frequency)

#total sales > mean total sales = 1 , #total sales < mean total sales = 0
sales.df[,"total_sales"]
sales.df$sales_high<-ifelse(sales.df[,"total_sales"] > mean(sales.df[,"total_sales"]),"1","0")
sales.df$sales_high
sales.df$sales_high<-as.factor(sales.df$sales_high)
head(sales.df)
sales.df<-sales.df[c(1,2,3,4,6,7)]
head(sales.df)

sales.df <- sales.df[,-c(1)]
names(sales.df)
head(sales.df)
dim(sales.df)
summary(sales.df)

##########
ksct.df<-sales.df
options(scipen = 999)

# Partition your dataset into 3 like in KNN. 
set.seed(1)
## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(ksct.df), dim(ksct.df)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(ksct.df), train.rows), 
                     dim(ksct.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(ksct.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows 
kstraining.df <- ksct.df[train.rows, ]
ksvalidation.df <- ksct.df[valid.rows, ]
kstesting.df <- ksct.df[test.rows, ]
head(kstraining.df)
head(ksvalidation.df)
head(kstesting.df)

library(rpart)
library(rpart.plot)
# run the classification tree, using default
ksdefault.tree <- rpart(sales_high ~ ., data = kstraining.df, 
                        method = "class")

# plot tree
prp(ksdefault.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 
#predict train
default.pred.train <- predict(ksdefault.tree,kstraining.df,type = "class")
confusionMatrix(default.pred.train, as.factor(kstraining.df[, 5]))
#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0 1205   60
#1   21  420

#Accuracy : 0.9525               
#95% CI : (0.9413, 0.9621)     
#No Information Rate : 0.7186               
#P-Value [Acc > NIR] : < 0.00000000000000022

#Kappa : 0.8796               

#Mcnemar's Test P-Value : 0.00002419           
                                               
#            Sensitivity : 0.9829               
#            Specificity : 0.8750               
#         Pos Pred Value : 0.9526               
#         Neg Pred Value : 0.9524               
#             Prevalence : 0.7186               
#         Detection Rate : 0.7063               
#   Detection Prevalence : 0.7415               
#      Balanced Accuracy : 0.9289               
                                               
#       'Positive' Class : 0 

default.pred.valid <- predict(ksdefault.tree,ksvalidation.df,type = "class")#predict valid
confusionMatrix(default.pred.valid, as.factor(ksvalidation.df$sales_high))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 732  34
#1  18 239

#Accuracy : 0.9492              
#95% CI : (0.9339, 0.9618)    
#No Information Rate : 0.7331              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8676              

#Mcnemar's Test P-Value : 0.03751             
                                              
#            Sensitivity : 0.9760              
#            Specificity : 0.8755              
#         Pos Pred Value : 0.9556              
#         Neg Pred Value : 0.9300              
#             Prevalence : 0.7331              
#         Detection Rate : 0.7155              
#   Detection Prevalence : 0.7488              
#      Balanced Accuracy : 0.9257              
                                              
#       'Positive' Class : 0  
default.pred.test <- predict(ksdefault.tree,kstesting.df,type = "class")#predict test
confusionMatrix(default.pred.test, as.factor(kstesting.df$sales_high))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 477  24
#1  11 171

#Accuracy : 0.9488              
#95% CI : (0.9294, 0.9641)    
#No Information Rate : 0.7145              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8718              

#Mcnemar's Test P-Value : 0.04252             
                                              
#            Sensitivity : 0.9775              
#            Specificity : 0.8769              
#         Pos Pred Value : 0.9521              
#         Neg Pred Value : 0.9396              
#             Prevalence : 0.7145              
#         Detection Rate : 0.6984              
#   Detection Prevalence : 0.7335              
#      Balanced Accuracy : 0.9272              
                                              
#       'Positive' Class : 0  


#######Create a deeper tree, go all the way by mentioning cp=0.
ksdeeper.ct <- rpart(sales_high ~ ., data = kstraining.df,method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(ksdeeper.ct$frame$var[ksdeeper.ct$frame$var == "<leaf>"])
#76
# plot tree
#prp(ksdeeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
#box.col=ifelse(ksdeeper.ct$frame$var == "<leaf>", 'gray', 'white'))  
deeper.ct <- rpart(sales_high ~ ., data = kstraining.df, method = "class",cp = 0, minsplit = 1)

library(caret)
kstraining.df[1,]
# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
ksdefault.pred.train <- predict(ksdefault.tree,kstraining.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(ksdefault.pred.train, as.factor(kstraining.df[, 5]))

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0 1205   60
#1   21  420

#Accuracy : 0.9525               
#95% CI : (0.9413, 0.9621)     
#No Information Rate : 0.7186               
#P-Value [Acc > NIR] : < 0.00000000000000022

#Kappa : 0.8796               

#Mcnemar's Test P-Value : 0.00002419           
                                               
#            Sensitivity : 0.9829               
#            Specificity : 0.8750               
#         Pos Pred Value : 0.9526               
#         Neg Pred Value : 0.9524               
#             Prevalence : 0.7186               
#         Detection Rate : 0.7063               
#   Detection Prevalence : 0.7415               
#      Balanced Accuracy : 0.9289               
                                               
#       'Positive' Class : 0                

        

### repeat the code for the validation set, and the deeper tree

ksdefault.pred.valid <- predict(ksdefault.tree,ksvalidation.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(ksdefault.pred.valid, as.factor(ksvalidation.df$sales_high))

#Confusion Matrix and Statistics for the validation data

#Reference
#Prediction   0   1
#0 732  34
#1  18 239

#Accuracy : 0.9492              
#95% CI : (0.9339, 0.9618)    
#No Information Rate : 0.7331              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8676              

#Mcnemar's Test P-Value : 0.03751             
                                              
#            Sensitivity : 0.9760              
#            Specificity : 0.8755              
#         Pos Pred Value : 0.9556              
#         Neg Pred Value : 0.9300              
#             Prevalence : 0.7331              
#         Detection Rate : 0.7155              
#   Detection Prevalence : 0.7488              
#      Balanced Accuracy : 0.9257              
                                              
#       'Positive' Class : 0 
    

### repeat the code for the test set, and the deeper tree
ksdefault.pred.test <- predict(ksdefault.tree,kstesting.df,type = "class")
# generate confusion matrix for test data
confusionMatrix(ksdefault.pred.test , as.factor(kstesting.df$sales_high))   
#Confusion Matrix and Statistics on test data 

#Reference
#Prediction   0   1
#0 477  24
#1  11 171

#Accuracy : 0.9488              
#95% CI : (0.9294, 0.9641)    
#No Information Rate : 0.7145              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8718              

#Mcnemar's Test P-Value : 0.04252             
                                              
#            Sensitivity : 0.9775              
#            Specificity : 0.8769              
#         Pos Pred Value : 0.9521              
#         Neg Pred Value : 0.9396              
#             Prevalence : 0.7145              
#         Detection Rate : 0.6984              
#   Detection Prevalence : 0.7335              
#      Balanced Accuracy : 0.9272              
                                              
#       'Positive' Class : 0           


#######cross validation###

# argument xval refers to the number of folds to use in rpart's built-in cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
kscv.ct <- rpart(sales_high ~ ., data = kstraining.df, method = "class", 
                 cp = 0.00001, minsplit = 10,xval=5)

# use printcp() to print the table. 
#reading the output. Root node error is the error with just one node (no splits). 
#Rel error is the error in the training data set relative to the root node error
#xerror is the relative error (average) in the valid data
#xstd std dev. of xerror through the different folds
printcp(kscv.ct)

#Best CP picked is at split 10
#10 0.00208333     16  0.093750 0.18750 0.019236


# prune by lower cp
kspruned.ct <- prune(kscv.ct, 
                     cp = kscv.ct$cptable[which.min(kscv.ct$cptable[,"xerror"]),"CP"])

#Find the length of the pruned tree
length(kspruned.ct$frame$var[kspruned.ct$frame$var == "<leaf>"])
#15

#Plot the pruned tree
prp(kspruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(kspruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

kspruned.pred.valid <- predict(ksdefault.tree,ksvalidation.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(kspruned.pred.valid, as.factor(ksvalidation.df$sales_high))

#Confusion Matrix and Statistics for the pruned tree

#Reference
#Prediction   0   1
#0 732  34
#1  18 239

#Accuracy : 0.9492              
#95% CI : (0.9339, 0.9618)    
#No Information Rate : 0.7331              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8676              

#Mcnemar's Test P-Value : 0.03751             
                                              
#            Sensitivity : 0.9760              
#            Specificity : 0.8755              
#         Pos Pred Value : 0.9556              
#         Neg Pred Value : 0.9300              
#             Prevalence : 0.7331              
#         Detection Rate : 0.7155              
#   Detection Prevalence : 0.7488              
#      Balanced Accuracy : 0.9257              
                                              
#       'Positive' Class : 0     



kspruned.pred.valid <- predict(ksdefault.tree,kstesting.df,type = "class")
# generate confusion matrix for test data
confusionMatrix(kspruned.pred.valid, as.factor(kstesting.df$sales_high))
#Confusion Matrix and Statistics on test data
#Reference
#Prediction   0   1
#0 477  24
#1  11 171

#Accuracy : 0.9488              
#95% CI : (0.9294, 0.9641)    
#No Information Rate : 0.7145              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8718              

#Mcnemar's Test P-Value : 0.04252             
                                              
#            Sensitivity : 0.9775              
#            Specificity : 0.8769              
#         Pos Pred Value : 0.9521              
#         Neg Pred Value : 0.9396              
#             Prevalence : 0.7145              
#         Detection Rate : 0.6984              
#   Detection Prevalence : 0.7335              
#      Balanced Accuracy : 0.9272              
                                              
#      'Positive' Class : 0   


##########

#Find a better parsimonous pruned tree
kspruned.ct <- prune(kscv.ct, 
                     cp =  0.00208333)


#Plot the pruned tree
prp(kspruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(kspruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

kspruned1.pred.valid <- predict(ksdefault.tree,ksvalidation.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(kspruned1.pred.valid, as.factor(ksvalidation.df$sales_high))

#Confusion Matrix and Statistics for better parsimonious tree
#Reference
#Prediction   0   1
#0 732  34
#1  18 239

#Accuracy : 0.9492              
#95% CI : (0.9339, 0.9618)    
#No Information Rate : 0.7331              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8676              

#Mcnemar's Test P-Value : 0.03751             
                                              
#            Sensitivity : 0.9760              
#            Specificity : 0.8755              
#         Pos Pred Value : 0.9556              
#         Neg Pred Value : 0.9300              
#             Prevalence : 0.7331              
#         Detection Rate : 0.7155              
#   Detection Prevalence : 0.7488              
#      Balanced Accuracy : 0.9257              
                                              
#       'Positive' Class : 0                   
                           
            
kspruned1.pred.test <- predict(ksdefault.tree,kstesting.df,type = "class")
# generate confusion matrix for test data
confusionMatrix(kspruned1.pred.test, as.factor(kstesting.df$sales_high))
#Confusion Matrix and Statistics on test data

#Reference
#Prediction   0   1
#0 477  24
#1  11 171

#Accuracy : 0.9488              
#95% CI : (0.9294, 0.9641)    
#No Information Rate : 0.7145              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8718              

#Mcnemar's Test P-Value : 0.04252             
                                              
#            Sensitivity : 0.9775              
#            Specificity : 0.8769              
#         Pos Pred Value : 0.9521              
#         Neg Pred Value : 0.9396              
#             Prevalence : 0.7145              
#         Detection Rate : 0.6984              
#   Detection Prevalence : 0.7335              
#      Balanced Accuracy : 0.9272              
                                              
#       'Positive' Class : 0              

#######RandomForest Trees

library(randomForest)
set.seed(1)
## random forest
ksrf <- randomForest(as.factor(sales_high) ~ ., data = kstraining.df,ntree = 100,
                     mtry = 4, nodesize = 5, importance = TRUE)  


## variable importance plot
varImpPlot(ksrf, type = 1)

## confusion matrix of the random forest above, use the valid data

ksRF.pred.valid <- predict(ksrf,ksvalidation.df,type = "class")
# generate confusion matrix for valid data

confusionMatrix(ksRF.pred.valid, as.factor(ksvalidation.df$sales_high))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 737  23
#1  13 250

#Accuracy : 0.9648             
#95% CI : (0.9516, 0.9752)   
#No Information Rate : 0.7331             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.909              

#Mcnemar's Test P-Value : 0.1336             
                                             
#            Sensitivity : 0.9827             
  #            Specificity : 0.9158             
#         Pos Pred Value : 0.9697             
#         Neg Pred Value : 0.9506             
#             Prevalence : 0.7331             
#         Detection Rate : 0.7204             
#   Detection Prevalence : 0.7429             
#      Balanced Accuracy : 0.9492             
                                             
#       'Positive' Class : 0            
    


ksRF.pred.test <- predict(ksrf,kstesting.df,type = "class")
# generate confusion matrix for test data

confusionMatrix(ksRF.pred.test, as.factor(kstesting.df$sales_high))

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 473  15
#1  15 180

#Accuracy : 0.9561             
#95% CI : (0.9379, 0.9702)   
#No Information Rate : 0.7145             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8923             

#Mcnemar's Test P-Value : 1                  
                                             
#            Sensitivity : 0.9693             
#            Specificity : 0.9231             
#         Pos Pred Value : 0.9693             
#         Neg Pred Value : 0.9231             
#             Prevalence : 0.7145             
#         Detection Rate : 0.6925             
#   Detection Prevalence : 0.7145             
#      Balanced Accuracy : 0.9462             
                                             
#       'Positive' Class : 0              

  



###### Boosted Trees
#install.packages("adabag")
library(adabag)
library(rpart) 
library(caret)


kstraining.df$sales_high <- as.factor(kstraining.df$sales_high)

set.seed(1)
ksboost1<-boosting(sales_high ~ ., data = kstraining.df,boos = TRUE, mfinal = 10, control = (minsplit = 0))

ksboost1.pred.valid <- predict(ksboost1,ksvalidation.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(as.factor(ksboost1.pred.valid$class), as.factor(ksvalidation.df$sales_high))

#Confusion Matrix and Statistics
              
#Reference
#Prediction   0   1
#0 739  20
#1  11 253

#Accuracy : 0.9697             
#95% CI : (0.9573, 0.9793)   
#No Information Rate : 0.7331             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.9217             

#Mcnemar's Test P-Value : 0.1508             
                                             
#            Sensitivity : 0.9853             
#            Specificity : 0.9267             
#         Pos Pred Value : 0.9736             
#         Neg Pred Value : 0.9583             
#             Prevalence : 0.7331             
#         Detection Rate : 0.7224             
#   Detection Prevalence : 0.7419             
#      Balanced Accuracy : 0.9560             
                                             
#       'Positive' Class : 0
              
   

ksboost2.pred.test <- predict(ksboost1,kstesting.df,type = "class")
# generate confusion matrix for testing data
confusionMatrix(as.factor(ksboost2.pred.test$class), as.factor(kstesting.df$sales_high))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 473  15
#1  15 180

#Accuracy : 0.9561             
#95% CI : (0.9379, 0.9702)   
#No Information Rate : 0.7145             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8923             

#Mcnemar's Test P-Value : 1                  
                                             
#            Sensitivity : 0.9693             
#            Specificity : 0.9231             
#         Pos Pred Value : 0.9693             
#         Neg Pred Value : 0.9231             
#             Prevalence : 0.7145             
#         Detection Rate : 0.6925             
#   Detection Prevalence : 0.7145             
#      Balanced Accuracy : 0.9462             
                                             
#       'Positive' Class : 0 
                  

################################## predict sales greater than mean sales: Logit ######################################################################
sales.df<-complete.df[c(1,2,3,4,8,13)]
head(sales.df)
sales.df$Frequency<-as.numeric(sales.df$Frequency)


#If sr value is above mean code 1 or 0
sales.df[,"total_sales"]
sales.df$sales_high<-ifelse(sales.df[,"total_sales"] > mean(sales.df[,"total_sales"]),"1","0")
sales.df$sales_high<-as.numeric(sales.df$sales_high)
sales.df<-sales.df[c(1,2,3,4,6,7)]
head(sales.df)
summary(sales.df)

sales.df <- sales.df[,-c(1)]
names(sales.df)
head(sales.df)
dim(sales.df)
summary(sales.df)

#################

kslog.df<-sales.df
# Partition your dataset into 3 like in KNN. 
set.seed(1)
## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(kslog.df), dim(kslog.df)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(kslog.df), train.rows), 
                     dim(kslog.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(kslog.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows 
kstrain.df <- kslog.df[train.rows, ]
ksvalid.df <- kslog.df[valid.rows, ]
kstest.df <- kslog.df[test.rows, ]


# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic regression.
kslogit.reg <- glm(sales_high ~ ., data = kstrain.df, family = "binomial") 
options(scipen=999)
summary(kslogit.reg)
# Monetary, Frequency and CLTV are significant since p value is less than 0.05
#Coefficients:
#  Estimate Std. Error z value             Pr(>|z|)    
#(Intercept) -9.2762547  0.6129798 -15.133 < 0.0000000000000002 ***
#  Monetary     0.0044934  0.0003423  13.128 < 0.0000000000000002 ***
#  Frequency    0.0833223  0.0073926  11.271 < 0.0000000000000002 ***
#  Recency     -0.0004390  0.0014476  -0.303             0.761667    
#CLTV         0.0286634  0.0077644   3.692             0.000223 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

############Accuracy on validation data##############

# use predict() with type = "response" to compute predicted probabilities. 3th column is the state # response variable
kslogit.valid.pred <- predict(kslogit.reg, ksvalid.df[, -5], type = "response") 


# first 5 actual and predicted records
data.frame(actual = ksvalid.df$sales_high[1:5], predicted = kslogit.valid.pred[1:5])
#actual    predicted
#2585      0 0.0046970074
#1922      0 0.0003478757
#2489      0 0.0008564080
#3732      0 0.0047124194
#2624      0 0.0006635067

####
library(gains)
ksgain_valid <- gains(ksvalid.df$sales_high, kslogit.valid.pred, groups=10)
ksgain_valid
ksgain_valid$obs

# > ksgain_valid
#Depth                            Cume   Cume Pct                     Mean
#of           Cume     Mean      Mean   of Total    Lift   Cume     Model
#File     N      N      Resp      Resp      Resp    Index   Lift     Score
#-------------------------------------------------------------------------
#10   102    102      1.00      1.00      37.4%     375    375      1.00
#20   102    204      0.95      0.98      72.9%     356    366      0.97
#30   102    306      0.59      0.85      94.9%     220    317      0.58
#40   103    409      0.12      0.66      99.3%      44    248      0.12
#50   102    511      0.02      0.53     100.0%       7    200      0.02
#60   102    613      0.00      0.45     100.0%       0    167      0.01
#70   103    716      0.00      0.38     100.0%       0    143      0.00
#80   102    818      0.00      0.33     100.0%       0    125      0.00
#90   102    920      0.00      0.30     100.0%       0    111      0.00
#100   103   1023      0.00      0.27     100.0%       0    100      0.00

#Lift Index is Mean Resp / Cum MEan Resp for the last row*100

# plot lift chart
plot(c(0,ksgain_valid$cume.pct.of.total*sum(ksvalid.df$state))~c(0,ksgain_valid$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(ksvalid.df$state))~c(0, dim(ksvalid.df)[1]), lty=2)

summary(as.factor(ksvalid.df$sales_high))
#0   1 
#750 273  

# compute deciles and plot decile-wise chart
heights <- ksgain_valid$mean.resp/mean(ksvalid.df$sales_high)
mean(ksvalid.df$sales_high)
ksgain_valid$mean.resp
heights
decileplot <- barplot(heights, names.arg = ksgain_valid$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response/Overall Mean", main = "Decile-wise lift chart")

# add labels to columns
text(decileplot, heights+0.4, labels=round(heights, 1), cex = 0.8)

#generate confusion matrix
kslogit.valid.pred
confusionMatrix(as.factor(ifelse(kslogit.valid.pred>0.5,1,0)), as.factor(ksvalid.df[, 5]))
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 727  36
#1  23 237

#Accuracy : 0.9423             
#95% CI : (0.9262, 0.9558)   
#No Information Rate : 0.7331             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8503             

#Mcnemar's Test P-Value : 0.1182             
                                             
#            Sensitivity : 0.9693             
#            Specificity : 0.8681             
#         Pos Pred Value : 0.9528             
#         Neg Pred Value : 0.9115             
#             Prevalence : 0.7331             
#         Detection Rate : 0.7107             
#   Detection Prevalence : 0.7458             
#      Balanced Accuracy : 0.9187             
                                             
#       'Positive' Class : 0                  
                                

############Accuracy on test data##############
kslogit.test.pred <- predict(kslogit.reg, kstest.df[, -5], type = "response") 

# first 5 actual and predicted records
data.frame(actual = kstest.df$sales_high[1:5], predicted = kslogit.test.pred[1:5])

#actual     predicted
#1       1 0.00009588398
#2       1 0.88018523274
#21      0 0.05692187016
#23      1 0.99452552518
#25      1 0.99999930557

ksgain_test <- gains(kstest.df$sales_high, kslogit.test.pred, groups=10)
ksgain_test
ksgain_test$obs

#>ksgain_test
#Depth                            Cume   Cume Pct                     Mean
#of           Cume     Mean      Mean   of Total    Lift   Cume     Model
#File     N      N      Resp      Resp      Resp    Index   Lift     Score
#-------------------------------------------------------------------------
#10    68     68      1.00      1.00      34.9%     350    350      1.00
#20    68    136      0.97      0.99      68.7%     340    345      0.97
#30    68    204      0.68      0.88      92.3%     237    309      0.68
#40    69    273      0.17      0.70      98.5%      61    246      0.17
#50    68    341      0.01      0.57      99.0%       5    198      0.03
#60    68    409      0.00      0.47      99.0%       0    165      0.01
#70    69    478      0.00      0.40      99.0%       0    141      0.00
#80    68    546      0.00      0.35      99.0%       0    124      0.00
#90    68    614      0.00      0.31      99.0%       0    110      0.00
#100    69    683      0.03      0.29     100.0%      10    100      0.00

#Lift Index is Mean Resp / Cum MEan Resp for the last row*100

# plot lift chart
plot(c(0,ksgain_test$cume.pct.of.total*sum(kstest.df$sales_high))~c(0,ksgain_test$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(kstest.df$sales_high))~c(0, dim(kstest.df)[1]), lty=2)

summary(as.factor(kstest.df$sales_high))
#0   1 
#488 195 

# compute deciles and plot decile-wise chart
heights <- ksgain_test$mean.resp/mean(kstest.df$sales_high)
mean(kstest.df$sales_high)
ksgain_test$mean.resp
heights
decileplot <- barplot(heights, names.arg = ksgain_test$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response/Overall Mean", main = "Decile-wise lift chart")

# add labels to columns
text(decileplot, heights+0.4, labels=round(heights, 1), cex = 0.8)

#generate confusion matrix
kslogit.test.pred
confusionMatrix(as.factor(ifelse(kslogit.test.pred>0.5,1,0)), as.factor(kstest.df[, 5]))
# Confusion Matrix and Statistics
# 
#Reference
#Prediction   0   1
#0 471  19
#1  17 176

#Accuracy : 0.9473             
#95% CI : (0.9278, 0.9628)   
#No Information Rate : 0.7145             
#P-Value [Acc > NIR] : <0.0000000000000002

#Kappa : 0.8704             

#Mcnemar's Test P-Value : 0.8676             
                                             
#            Sensitivity : 0.9652             
#            Specificity : 0.9026             
#         Pos Pred Value : 0.9612             
#         Neg Pred Value : 0.9119             
#             Prevalence : 0.7145             
#         Detection Rate : 0.6896             
#   Detection Prevalence : 0.7174             
#      Balanced Accuracy : 0.9339             
                                             
#       'Positive' Class : 0

################################# predict sales :  SVM algorithm ####################
sales.df<-complete.df[c(1,2,3,4,8,13)]
head(sales.df)

sales.df[,"total_sales"]
sales.df$sales_high<-ifelse(sales.df[,"total_sales"] > mean(sales.df[,"total_sales"]),"1","0")
sales.df$sales_high
head(sales.df)
sales.df<-sales.df[c(2,3,4,6,7)]
head(sales.df)

sales.df$Frequency<-as.numeric(sales.df$Frequency)
sales.df$sales_high<-as.factor(sales.df$sales_high)

#	Use linear kernel function to train a svm classifier to predict sales_high based on all explanatory variables. 
# use set.seed() to get the same partitions when re-running the R code.
###### Partition  dataset into 3 like in KNN. 
set.seed(1)
## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(sales.df), dim(sales.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(sales.df), train.rows), dim(sales.df)[1]*0.3)
test.rows <- setdiff(rownames(sales.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows 
train.data <- sales.df[train.rows, ]
valid.data <- sales.df[valid.rows, ]
test.data <- sales.df[test.rows, ]
head(train.data)
head(valid.data)
head(test.data)

###### SVM classification
library('e1071')
#Compare a radial kernel with large γ to a radial kernel with small γ (0.1 vs 0.5). 
# low gamma -> tend to under fit
svmlg<-svm(sales_high ~ ., data=train.data, kernel='radial',  gamma=0.1)
library(forecast)
#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmlg, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmlg, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy

# high gamma -> tend to over fit
svmhg<-svm(sales_high ~ ., data=train.data, kernel='radial',  gamma=0.5)
library(forecast)
#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmhg, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmhg, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy

#Visualize
plot(svmlg,sales.df,Monetary ~ Frequency) 
plot(svmhg,sales.df,Monetary ~ Frequency) 

#compare a large misclassification cost (100,000) to a small misclassification cost (1). 
svmLowCost1<-svm(sales_high ~ ., data=train.data,kernel='radial', cost=1,gamma=0.1)
svmLowCost2<-svm(sales_high ~ ., data=train.data,kernel='radial', cost=1,gamma=0.5)
svmHighCost1<-svm(sales_high ~ ., data=train.data,kernel='radial', cost=100000,gamma=0.1)
svmHighCost2<-svm(sales_high ~ ., data=train.data,kernel='radial', cost=100000,gamma=0.5)

#Visualize
plot(svmLowCost1,sales.df,Monetary ~ Frequency) 
plot(svmLowCost2,sales.df,Monetary ~ Frequency) 
plot(svmHighCost1,sales.df,Monetary ~ Frequency) 
plot(svmHighCost2,sales.df,Monetary ~ Frequency)

#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmLowCost1, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmLowCost1, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy


#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmLowCost2, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmLowCost2, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy

#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmHighCost1, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmHighCost1, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy


#Predict on validation set with the trained model and calculate the prediction accuracy
pred = predict(svmHighCost2, newdata = valid.data)
confusion=table(pred,valid.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#Predict on testing set with the trained model and calculate the prediction accuracy
pred = predict(svmHighCost2, newdata = test.data)
confusion=table(pred,test.data$sales_high)
accuracy=sum(diag(confusion))/100# compute accuracy on prediction set
accuracy
#############################################Aprior Algorithm #################################################################### 
library(arules)
#Choosing France
retailAprior <- retail[retail$Country == "France",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
itemFrequencyPlot(retailAprior.trans[1:15,1:15]) #Frequency of various transactions

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.07, conf = 0.8, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                      rhs                                support confidence   coverage     lift count
#[1] {ALARM CLOCK BAKELIKE GREEN}          => {ALARM CLOCK BAKELIKE RED }     0.07969152  0.8157895 0.09768638 8.576814    31
#[2] {ALARM CLOCK BAKELIKE RED }           => {ALARM CLOCK BAKELIKE GREEN}    0.07969152  0.8378378 0.09511568 8.576814    31
#[3] {SET/20 RED RETROSPOT PAPER NAPKINS ,                                                                                   
#  SET/6 RED SPOTTY PAPER CUPS}         => {SET/6 RED SPOTTY PAPER PLATES} 0.10025707  0.9750000 0.10282776 7.585500    39
#[4] {SET/20 RED RETROSPOT PAPER NAPKINS ,                                                                                   
#  SET/6 RED SPOTTY PAPER PLATES}       => {SET/6 RED SPOTTY PAPER CUPS}   0.10025707  0.9750000 0.10282776 7.023611    39
#[5] {SET/6 RED SPOTTY PAPER PLATES}       => {SET/6 RED SPOTTY PAPER CUPS}   0.12339332  0.9600000 0.12853470 6.915556    48
#[6] {SET/6 RED SPOTTY PAPER CUPS}         => {SET/6 RED SPOTTY PAPER PLATES} 0.12339332  0.8888889 0.13881748 6.915556    48

# filter out rules like this
subRules<-retailApriorRules[quality(retailApriorRules)$confidence<=0.9 & quality(retailApriorRules)$support <= 0.08]
inspect(subRules)
summary(subRules) 
# convert rules to a dataframe
rules.tbl = data.frame(
  lhs = labels(lhs(subRules)),
  rhs = labels(rhs(subRules)), 
  subRules@quality)
#filtering in a dataframe
rules.tbl1<-sum(rules.tbl[rules.tbl$support <= 0.08 & rules.tbl$confidence <= 0.9,]$count)
rules.tbl1

############################################################################################################################################
library(arules)
#Choosing Germany
retailAprior <- retail[retail$Country == "Germany",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
itemFrequencyPlot(retailAprior.trans[1:15,1:15]) #Frequency of various transactions

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.04, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                rhs                             support    confidence coverage   lift      count
#[1] {SET/6 RED SPOTTY PAPER PLATES} => {SET/6 RED SPOTTY PAPER CUPS}   0.04595186 0.8076923  0.05689278 15.379808 21   
#[2] {SET/6 RED SPOTTY PAPER CUPS}   => {SET/6 RED SPOTTY PAPER PLATES} 0.04595186 0.8750000  0.05251641 15.379808 21   
#[3] {RED RETROSPOT CHARLOTTE BAG}   => {WOODLAND CHARLOTTE BAG}        0.05908096 0.8437500  0.07002188  6.648168 27   
#[4] {WOODLAND CHARLOTTE BAG}        => {RED RETROSPOT CHARLOTTE BAG}   0.05908096 0.4655172  0.12691466  6.648168 27   
#[5] {JUMBO BAG RED RETROSPOT}       => {JUMBO BAG WOODLAND ANIMALS}    0.04814004 0.6111111  0.07877462  6.071256 22   
#[6] {JUMBO BAG WOODLAND ANIMALS}    => {JUMBO BAG RED RETROSPOT}       0.04814004 0.4782609  0.10065646  6.071256 22   
 
############################################################################################################################################
library(arules)
#Choosing Denmark
retailAprior <- retail[retail$Country == "Denmark",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
#itemFrequencyPlot(retailAprior.trans) #Frequency of various transactions
itemFrequencyPlot(retailAprior.trans[1:15,1:15])

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.09, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                  rhs                               support   confidence coverage  lift count
#[1] {FOLDING BUTTERFLY MIRROR RED  }  => {BOX OF 6 MINI 50'S CRACKERS}     0.1111111 1          0.1111111 9    2    
#[2] {BOX OF 6 MINI 50'S CRACKERS}     => {FOLDING BUTTERFLY MIRROR RED  }  0.1111111 1          0.1111111 9    2    
#[3] {HOLIDAY FUN LUDO}                => {LUNCH BOX I LOVE LONDON}         0.1111111 1          0.1111111 9    2    
#[4] {LUNCH BOX I LOVE LONDON}         => {HOLIDAY FUN LUDO}                0.1111111 1          0.1111111 9    2    
#[5] {HOLIDAY FUN LUDO}                => {AIRLINE BAG VINTAGE JET SET RED} 0.1111111 1          0.1111111 9    2    
#[6] {AIRLINE BAG VINTAGE JET SET RED} => {HOLIDAY FUN LUDO}                0.1111111 1          0.1111111 9    2   

############################################################################################################################################

library(arules)
#Choosing Italy
retailAprior <- retail[retail$Country == "Italy",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
#itemFrequencyPlot(retailAprior.trans) #Frequency of various transactions
itemFrequencyPlot(retailAprior.trans[1:15,1:15])

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.09, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                      rhs                                     support confidence  coverage lift count
#[1] {LUNCH BAG CARS BLUE}                 => {LUNCH BAG WOODLAND}                  0.1052632        1.0 0.1052632  9.5     4
#[2] {LUNCH BAG WOODLAND}                  => {LUNCH BAG CARS BLUE}                 0.1052632        1.0 0.1052632  9.5     4
#[3] {RETROSPOT TEA SET CERAMIC 11 PC ,                                                                                      
#  SET OF 20 KIDS COOKIE CUTTERS}       => {GINGERBREAD MAN COOKIE CUTTER}       0.1052632        1.0 0.1052632  9.5     4
#[4] {SET OF TEA COFFEE SUGAR TINS PANTRY} => {VINTAGE CREAM DOG FOOD CONTAINER}    0.1052632        0.8 0.1315789  7.6     4
#[5] {SET OF 20 KIDS COOKIE CUTTERS}       => {GINGERBREAD MAN COOKIE CUTTER}       0.1052632        0.8 0.1315789  7.6     4
#[6] {VINTAGE CREAM DOG FOOD CONTAINER}    => {SET OF TEA COFFEE SUGAR TINS PANTRY} 0.1052632        1.0 0.1052632  7.6     4

########################################################################################################################
library(arules)
#Choosing Norway
retailAprior <- retail[retail$Country == "Norway",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
#itemFrequencyPlot(retailAprior.trans) #Frequency of various transactions
itemFrequencyPlot(retailAprior.trans[1:15,1:15])

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.09, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                      rhs                                   support   confidence coverage  lift count
#[1] {TRAVEL CARD WALLET PANTRY}           => {GROW A FLYTRAP OR SUNFLOWER IN TIN}  0.1111111 1          0.1111111 9    4    
#[2] {GROW A FLYTRAP OR SUNFLOWER IN TIN}  => {TRAVEL CARD WALLET PANTRY}           0.1111111 1          0.1111111 9    4    
#[3] {TRAVEL CARD WALLET PANTRY}           => {CHOCOLATE HOT WATER BOTTLE}          0.1111111 1          0.1111111 9    4    
#[4] {CHOCOLATE HOT WATER BOTTLE}          => {TRAVEL CARD WALLET PANTRY}           0.1111111 1          0.1111111 9    4    
#[5] {TRAVEL CARD WALLET PANTRY}           => {SMALL FOLDING SCISSOR(POINTED EDGE)} 0.1111111 1          0.1111111 9    4    
#[6] {SMALL FOLDING SCISSOR(POINTED EDGE)} => {TRAVEL CARD WALLET PANTRY}           0.1111111 1          0.1111111 9    4 
# filter out rules like this
################################################################################################################################################

library(arules)
#Choosing Finland
retailAprior <- retail[retail$Country == "Finland",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
#itemFrequencyPlot(retailAprior.trans) #Frequency of various transactions
itemFrequencyPlot(retailAprior.trans[1:15,1:15])

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.09, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
inspect(retailApriorRules)
#lhs                                    rhs                                  support confidence   coverage     lift count
#[1] {CHILDRENS CUTLERY POLKADOT GREEN } => {CHILDRENS CUTLERY POLKADOT BLUE} 0.12195122  1.0000000 0.12195122 5.857143     5
#[2] {CHILDRENS CUTLERY POLKADOT GREEN } => {CHILDRENS CUTLERY POLKADOT PINK} 0.09756098  0.8000000 0.12195122 3.644444     4
#[3] {CHILDRENS CUTLERY POLKADOT BLUE}   => {CHILDRENS CUTLERY POLKADOT PINK} 0.14634146  0.8571429 0.17073171 3.904762     6
#[4] {CHILDRENS CUTLERY POLKADOT BLUE,                                                                                       
#  CHILDRENS CUTLERY POLKADOT GREEN } => {CHILDRENS CUTLERY POLKADOT PINK} 0.09756098  0.8000000 0.12195122 3.644444     4
#[5] {CHILDRENS CUTLERY POLKADOT GREEN ,                                                                                     
#  CHILDRENS CUTLERY POLKADOT PINK}   => {CHILDRENS CUTLERY POLKADOT BLUE} 0.09756098  1.0000000 0.09756098 5.857143     4

####################################################################################################################################################
library(arules)
#Choosing Netherlands
retailAprior <- retail[retail$Country == "Netherlands",  ] 
retailAprior<-aggregate(Quantity ~ InvoiceNo + Description, data = retailAprior, FUN = sum)
head(retailAprior)

library(reshape) 
library(reshape2) 
retailAprior<-dcast(retailAprior, InvoiceNo~Description, fill=0)

#remove first column 
retailAprior <- retailAprior[,-1]
dim(retailAprior)

library(dplyr)
retailAprior <- select (retailAprior,-c('POSTAGE'))
dim(retailAprior)

#create a binary incidence matrix. First convert the columns to be binary. 1 if purchase (irrespective of the number) 0 if no purhase
#Then convert it to matrix form
retailAprior <- ifelse(retailAprior>0,1,0)
summary(retailAprior)

# convert to matrix, use as.matrix function
retailAprior.mat <- as.matrix(retailAprior)
retailAprior.mat[1:5,1:5]

# convert the binary incidence matrix into a transactions database
retailAprior.trans <- as(retailAprior.mat, "transactions")
retailAprior.trans[1:5,1:5]

inspect(retailAprior.trans) 
#itemFrequencyPlot(retailAprior.trans) #Frequency of various transactions
itemFrequencyPlot(retailAprior.trans[1:15,1:15])

## get rules # when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
retailApriorRules <- apriori(retailAprior.trans, parameter = list(supp = 0.09, conf = 0.08, target = "rules", maxtime=50))
inspect(retailApriorRules) 

# inspect the first six rules, sorted by their lift
inspect(head(sort(retailApriorRules, by = "lift"), n = 6)) 
#lhs                                      rhs                                    support confidence  coverage     lift count
#[1] {PLASTERS IN TIN WOODLAND ANIMALS}    => {PLASTERS IN TIN SPACEBOY}          0.09574468  0.9000000 0.1063830 7.050000     9
#[2] {PLASTERS IN TIN WOODLAND ANIMALS,                                                                                         
#  ROUND SNACK BOXES SET OF4 WOODLAND } => {PLASTERS IN TIN SPACEBOY}          0.09574468  0.9000000 0.1063830 7.050000     9
#[3] {PLASTERS IN TIN SPACEBOY}            => {PLASTERS IN TIN WOODLAND ANIMALS}  0.09574468  0.7500000 0.1276596 7.050000     9
#[4] {PLASTERS IN TIN SPACEBOY,                                                                                                 
#  ROUND SNACK BOXES SET OF4 WOODLAND } => {PLASTERS IN TIN WOODLAND ANIMALS}  0.09574468  0.7500000 0.1276596 7.050000     9
#[5] {LUNCH BOX WITH CUTLERY RETROSPOT }   => {STRAWBERRY LUNCH BOX WITH CUTLERY} 0.09574468  0.9000000 0.1063830 6.042857     9
#[6] {STRAWBERRY LUNCH BOX WITH CUTLERY}   => {LUNCH BOX WITH CUTLERY RETROSPOT } 0.09574468  0.6428571 0.1489362 6.042857     9

#################################### Predict Sales Amount -KNN ################################################################
head(complete.df) 
sales.df<-complete.df[c(1,2,3,4,8,13)]
head(sales.df)
summary(sales.df)
sales.df$Frequency<-as.numeric(sales.df$Frequency)

sales.df[,"total_sales"]
sales.df$total_sales <- findInterval(sales.df$total_sales, c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000), rightmost.closed = TRUE)
sales.df$total_sales
sales.df$total_sales = as.factor(sales.df$total_sales)
head(sales.df)
sales.df<-sales.df[c(1,2,3,4,6,5)]
head(sales.df)

sales.df <- sales.df[,-c(1)]
head(sales.df)
names(sales.df)
head(sales.df)
dim(sales.df)
summary(sales.df)

new.df <- data.frame("Monetary"=1000,"Frequency"=45, "Recency"=90, "CLTV"=1)
new.df
dim(new.df)

set.seed(1)
## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(sales.df), dim(sales.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(sales.df), train.rows), 
                     dim(sales.df)[1]*0.3)
test.rows <- setdiff(rownames(sales.df), union(train.rows, valid.rows))
train.df <- sales.df[train.rows, ]
dim(train.df)
head(train.df)
valid.df <- sales.df[valid.rows, ]
dim(valid.df)
test.df <- sales.df[test.rows, ]
dim(test.df)


# initialize normalized training, validation data, assign (temporarily) data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
sales.norm.df <- sales.df
head(train.norm.df)
summary(train.norm.df)
new.norm.df <- new.df
new.norm.df

# use preProcess() from the caret package to normalize the variables.
library(caret)
#train.df$total_sales = as.factor(train.df$total_sales)
norm.values <- preProcess(train.df[ , c(1:4, 5)], method=c("center", "scale"))
head(norm.values)
norm.values
train.norm.df[ , c(1:4, 5)] <- predict(norm.values, train.df[ ,c(1:4, 5)])
head(train.norm.df)
##Similarly scale valid data, bank data and the test data
valid.norm.df[ ,c(1:4, 5)] <- predict(norm.values, valid.df[ , c(1:4, 5)])
sales.norm.df[ , c(1:4, 5)] <- predict(norm.values, sales.df[ , c(1:4, 5)])
test.norm.df[ , c(1:4, 5)] <- predict(norm.values, test.df[ , c(1:4, 5)])
head(new.df[ , c(1:4)])
norm.values
new.norm.df[ , c(1:4)] <- predict(norm.values, new.df[ , c(1:4)])
new.norm.df

# initialize a data frame with two columns: k, and calculating accuracy for valid dataset
sales_accuracy1.df <- data.frame(k = seq(1, 40 , 1), accuracy = rep(0, 40)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
sales_accuracy1.df
library(FNN)
#install.packages("forecast")
library(forecast)
# compute knn for different k on validation.
for(i in 1:40) {
  #Use knn function with k=i and predict for valid dataset
  sales_knn1.pred <- knn(train = train.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                         cl = train.norm.df[, 5], k = i)
  
  sales_accuracy1.df[i, 2] <- confusionMatrix(sales_knn1.pred, as.factor(valid.df[, 5]))$overall[1] 
}  
sales_accuracy1.df

#accuracy on valid data with best k (k=22) using training data to train and valid data as test
sales_knn_valid.pred <- knn(train = train.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                            cl = train.norm.df[, 5], k = 22)
confusionMatrix(sales_knn_valid.pred, as.factor(valid.df[, 5]))
             

#accuracy on valid data with best k (k=22) using full dataset to train and valid data as test
sales_knn_valid.pred <- knn(train = sales.norm.df[ , c(1:4)], test = valid.norm.df[ , c(1:4)], 
                            cl = sales.norm.df[, 5], k = 22)
confusionMatrix(sales_knn_valid.pred, as.factor(valid.df[, 5]))
         

#accuracy on valid data with best k (k=22) using training data to train and test data as test
sales_knn_valid.pred <- knn(train = train.norm.df[ , c(1:4)], test = test.norm.df[ , c(1:4)], 
                            cl = train.norm.df[, 5], k = 22)
confusionMatrix(sales_knn_valid.pred, as.factor(test.df[, 5]))
            

#accuracy on test data with best k (k=22)
sales_knn_test.pred <- knn(train = sales.norm.df[ , c(1:4)], test = test.norm.df[ , c(1:4)], 
                           cl = sales.df[, 5], k = 22)
confusionMatrix(sales_knn_test.pred, as.factor(test.df[, 5]))
           
#new data
sales_knn_test.pred <- knn(train = sales.norm.df[ , c(1:4)], test = new.norm.df, 
                           cl = sales.df[, 5], k = 22)
row.names(sales.df)[attr(sales_knn_test.pred, "nn.index")]
 
sales.df[attr(sales_knn_test.pred, "nn.index"),]

