##importing dataset
customers <- read.csv("./archive/olist_customers_dataset.csv")

 geolocation <- read.csv("./archive/olist_geolocation_dataset.csv")

 order_items <- read.csv("./archive/olist_order_items_dataset.csv")

 order_payments <- read.csv("./archive/olist_order_payments_dataset.csv")
 order_reviews <- read.csv("./archive/olist_order_reviews_dataset.csv")
 orders <- read.csv("./archive/olist_orders_dataset.csv")
 products <- read.csv("./archive/olist_products_dataset.csv")
 sellers <- read.csv("./archive/olist_sellers_dataset.csv")
 product_category_name <- read.csv("./archive/product_category_name_translation.csv")

#fix the column nbame
   names(product_category_name)[1]<-"product_category_name"
#Data Pre-processing 
#Translate Product Category: 
  
library(tidyverse)
product = left_join(products,product_category_name,  'product_category_name')
product = product[,-2]
   
#Convert Timestamp Data
order_date=c("order_purchase_timestamp" , "order_approved_at"  , "order_delivered_carrier_date" , "order_delivered_customer_date","order_estimated_delivery_date")
library(lubridate) 
for(i in 1:length(order_date)){
orders[,order_date[i]]<-as.Date(orders[,order_date[i]])
}
str(orders)
# creating master dataframe 

df1 = full_join(order_payments,order_items,"order_id")

df2 = full_join(df1,product,"product_id")

df3 = full_join(df2,sellers,"seller_id")

df4 = full_join(df3,order_reviews, 'order_id')

df5 = full_join(df4,orders,'order_id')

df6 =full_join( df5,customers, 'customer_id')
#cleaning up and re-engineering some columns
df6[,'order_purchase_year'] = year(df6[,'order_purchase_timestamp'])

df6[,'order_purchase_month'] = month(df6[,'order_purchase_timestamp'])

df6[,'order_purchase_dayofweek'] = wday(df6[,'order_purchase_timestamp'])

df6[,'order_purchase_hour'] = hour(df6[,'order_purchase_timestamp'])

df6[,'order_purchase_day'] = if_else(df6[,'order_purchase_dayofweek']==0,'Mon',if_else(df6[,'order_purchase_dayofweek']==1,'Tue',if_else(df6[,'order_purchase_dayofweek']==2,'Wed',if_else(df6[,'order_purchase_dayofweek']==3,'Thu',if_else(df6[,'order_purchase_dayofweek']==4,'Fri',if_else(df6[,'order_purchase_dayofweek']==5,'Sat','Sun'))))))

df6[,'order_purchase_mon'] = if_else(df6[,'order_purchase_month']==1,'Jan',if_else(df6[,'order_purchase_month']==2,'Feb',if_else(df6[,'order_purchase_month']==3,'Mar',if_else(df6[,'order_purchase_month']==4,'Apr',if_else(df6[,'order_purchase_month']==5,'May',if_else(df6[,'order_purchase_month']==6,'Jun',if_else(df6[,'order_purchase_month']==7,'Jul',if_else(df6[,'order_purchase_month']==8,'Aug',if_else(df6[,'order_purchase_month']==9,'Sep',if_else(df6[,'order_purchase_month']==10,'Oct',if_else(df6[,'order_purchase_month']==11,'Nov','Dec')))))))))))

df6[, "order_count"] = 1 

df6[,'year_month'] =format(df6[,'order_purchase_timestamp'], format = "%Y-%m")
 
df6[,'ship_duration']=difftime(df6[,'order_delivered_customer_date'], df6[,'order_purchase_timestamp'], units="hours")


df6[,'tocarrier_duration']=difftime(df6[,'order_delivered_carrier_date'],df6[,'order_purchase_timestamp'], units="hours")


df6[,'lastmile_duration']=difftime(df6[,'order_delivered_customer_date'],df6[,'order_delivered_carrier_date'], units="hours")


df6[,'expected_vs_shipdate']=difftime(df6[,'order_estimated_delivery_date'],df6[,'order_delivered_customer_date'],units="hours")


df6['expected_duration']=difftime(df6[,'order_estimated_delivery_date'],df6[,'order_purchase_timestamp'],units="hours")

#dropping non-needed columns
df6 = df6[,!(names(df6) %in% c("product_name_lenght", "product_description_lenght", "product_photos_qty", "product_length_cm", "product_height_cm", "product_width_cm", "product_length_cm", "review_id","review_comment_title", "review_comment_message", "product_category_name"))]
# displaying missing value counts
colSums(is.na(df6))

##dropping missing data
df6<-df6[complete.cases(df6),]
# displaying first 3 rows of master dataframe
head(df6,3)
##Analysis Direction: We will discover the reasons for retailer churn by analyzing three dimensions:
  
#Customers Analysis: New User, Retention (repeated purchases) - Any trends YoY? Anythings standout by Geo?
#Order Analysis: Order volume (take into account seasonality) & value
#Review Analysis: Any significant degrade in CSAT

##Orders Volume and Sales Analysis

# Creating new datasets for each year
df_2016 = df6[which(df6$order_purchase_year=="2016"),]
df_2017 = df6[which(df6$order_purchase_year=="2017"),]
df_2018 = df6[which(df6$order_purchase_year=="2018"),]

library(plotly)
par(mfrow=c(1,1))
p <- ggplot(df6, aes(x=factor(order_purchase_year))) +
  geom_bar(width = 0.25) + 
  labs(title = 'Total Order Purchase by Year',x="Order Purchase Year")+
  theme_bw()
p
p1<- ggplot(df_2017, aes(x=factor(order_purchase_month))) +
  geom_bar(width = 0.25) + 
  labs(title = 'Number of Orders by Month in 2017',x="Order Purchase Month")+
  theme_bw()
p1
p2<- ggplot(df_2018, aes(x=factor(order_purchase_month))) +
  geom_bar(width = 0.25) + 
  labs(title = 'Number of Orders by Month in 2018',x="Order Purchase Month")+
  theme_bw()
p2
p3<- ggplot(df_2016, aes(x=factor(order_purchase_month))) +
  geom_bar(width = 0.25) + 
  labs(title = 'Number of Orders by Month in 2016',x="Order Purchase Month")+
  theme_bw()
p3
#Observations: Order purchase was on a rise throughout 2017, and suddenly came to a plateau in 2018 and has been on a downward trend. Both order's volume and order's value have reduced in 2018
#This reduction on order volume and sales happened very sudden at the start of 2018, across all geos.


df_ytsales = df6 %>% 
  group_by('order_purchase_year', 'order_purchase_month','year_month') %>% 
  mutate(sum_pv=sum( payment_value))

#aggregate

sales_by_year <- aggregate(payment_value ~order_purchase_year , data = df6, FUN = sum)

p4 <- ggplot(sales_by_year, aes(x=factor(order_purchase_year),y=payment_value)) +
  geom_bar(stat="identity",width = 0.25) + 
  labs(title = 'Sales by Year',x="Order Purchase Year")+
  theme_bw()
p4

sales_by_month_2016<-aggregate(payment_value ~order_purchase_month , data = df_2016, FUN = sum)

p5 <- ggplot(sales_by_month_2016, aes(x=factor(order_purchase_month),y=payment_value)) +
  geom_bar(stat="identity",width = 0.25) + 
  labs(title = 'Sales by Month 2016',x="Order Purchase Month")+
  theme_bw()
p5

sales_by_month_2017<-aggregate(payment_value ~order_purchase_month , data = df_2017, FUN = sum)

p6 <- ggplot(sales_by_month_2017, aes(x=factor(order_purchase_month),y=payment_value)) +
  geom_bar(stat="identity",width = 0.25) + 
  labs(title = 'Sales by Month 2017',x="Order Purchase Month")+
  theme_bw()

p6

sales_by_month_2018<-aggregate(payment_value ~order_purchase_month , data = df_2018, FUN = sum)

p7 <- ggplot(sales_by_month_2018, aes(x=factor(order_purchase_month),y=payment_value)) +
  geom_bar(stat="identity",width = 0.25) + 
  labs(title = 'Sales by Month 2018',x="Order Purchase Month")+
  theme_bw()
p7


sales_by_year_month <- aggregate(payment_value ~year_month , data = df6, FUN = sum)

p8 <- ggplot(sales_by_year_month, aes(x=factor(year_month),y=payment_value)) +
  geom_bar(stat="identity",width = 0.25) + 
  geom_line(aes(group=1))+
  labs(title ='Brazilian E-Commerce Monthly Sales from 2016 to 2018',x="Year-Month")+
  theme_bw()
p8

order_by_year_month <- aggregate(order_count ~year_month , data = df6, FUN = sum)

p9 <- ggplot(order_by_year_month, aes(x=factor(year_month),y=order_count)) +
  geom_bar(stat="identity",width = 0.25) + 
  geom_line(aes(group=1))+
  labs(title ='Brazilian E-Commerce Purchase Orders from 2016 to 2018',x="Year-Month")+
  theme_bw()
p9

#1. Analyze total customers by month-year: apart from seasonal factors, is there any problem?
#2. Analyze churn by states (geo)
# Grouping by customer state
df_cus_count = df6 %>%
  group_by(order_purchase_year, order_purchase_month,year_month)%>%
  select(order_purchase_year, order_purchase_month,year_month,customer_unique_id,seller_id) %>%
  unique()
head(df_cus_count,10)

customer_by_year_month <- df_cus_count%>%
  group_by(year_month)%>%
  summarise(count = n_distinct(customer_unique_id))


p10 <- ggplot(customer_by_year_month, aes(x=factor(year_month),y=count)) +
  geom_bar(stat="identity",width = 0.25) + 
  geom_line(aes(group=1))+
  labs(title ='Brazilian E-Commerce Number of Customers from 2016 to 2018',x="Year-Month")+
  theme_bw()
p10

df_cus_state_year_month =df6 %>%
  group_by(year_month, customer_state) %>%
  summarise(sum(payment_value))
  
df_cus_state_year<-df6 %>%
  group_by(order_purchase_year, customer_state) %>%
  summarise(sum(payment_value))
df_cus_state_year

#Based on the above table, the biggest five states: SP, RJ, MG, RS, and PR Their sale patterns are similar, on the decline. SP dominates the overall sale figure, so let's isolate it out.
top5 = c('SP', 'RJ','MG','RS','PR')
df_top5_state = df_cus_state_year_month[df_cus_state_year_month$customer_state%in%top5,]
df_top5_state

p11 <- ggplot(df_top5_state, aes(x=year_month,y=as.numeric(`sum(payment_value)`),color = factor(customer_state),group=factor(customer_state)))+
  geom_point() +
  geom_line()+
  labs(title ='Sales from the top 5 states from 2016 to 2018',x="Year-Month",y="Payment Value")+
  theme_bw()


p11

top4_noSP = c('RJ','MG','RS','PR')
df_top4_noSP = df_cus_state_year_month[df_cus_state_year_month$customer_state%in%top4_noSP,]
df_top4_noSP

p12 <- ggplot(df_top4_noSP, aes(x=year_month,y=as.numeric(`sum(payment_value)`),group=factor(customer_state),color = factor(customer_state)))+
  geom_point() +
  geom_line()+
  labs(title ='Sales from the top 4 states from 2016 to 2018, excluding SP',x="Year-Month",y="Payment Value")+
  theme_bw()


p12

#All other top 4 states suffer from the same decline as SP. Since the sale decline happened in 2018 across all states, there could be only two reasons:
  
#Competitors launch something in Brazil, attracting customers/sales to their sites. The top 3 states are all trending down in the past 3 to 4 months.
#There are some serious bugs/business strategy changes
#Since the case assumed no major bugs, the first hypothesis is the most reasonable one. This hypothesis is further confirmed here "The company [Amazon] made its first big move into merchandise in October 2017, when it began offering the use of its Brazilian website to third-party merchants to sell electronics." Source: https://www.reuters.com/article/us-amazon-com-brazil/amazon-com-starts-direct-sales-of-merchandise-in-brazil-after-delays-idUSKCN1PG0AG

#Sellers
#The number of sellers is still increasing MoM, despite a reduced in order volume
sellers_by_year_month <- df_cus_count%>%
  group_by(year_month)%>%
  summarise(count = n_distinct(seller_id))


p13 <- ggplot(sellers_by_year_month, aes(x=factor(year_month),y=count)) +
  geom_bar(stat="identity",width = 0.25) + 
  geom_line(aes(group=1))+
  labs(title ='Brazilian E-Commerce Number of Sellers from 2016 to 2018',x="Year-Month")+
  theme_bw()
p13

#Churn Analysis

#Create a dataframe to count how many times a customer shop 
df_order = df6 %>% 
  group_by(customer_id) %>%
  summarise(count = sum(order_count))

order_count=names(table(df_order$count))

num_customer=table(df_order$count)

percentage=table(df_order$count)/sum(table(df_order$count))*100

df_count_cust<-as.data.frame(cbind(order_count,num_customer=as.numeric(num_customer),percentage=as.numeric(percentage)))
head(df_count_cust)

#86% of customers only buy with Olist once, which is a big problem.


#Check the values per year

df_order_year = df6 %>% 
  group_by(customer_id,order_purchase_year) %>%
  summarise(count = sum(order_count))

order_count=names(table(df_order_year$count[df_order_year$order_purchase_year==2016]))

num_customer=table(df_order_year$count[df_order_year$order_purchase_year==2016])

percentage=table(df_order_year$count[df_order_year$order_purchase_year==2016])/sum(table(df_order_year$count[df_order_year$order_purchase_year==2016]))*100

df_count_cust_2016<-as.data.frame(cbind(order_count,num_customer=as.numeric(num_customer),percentage=as.numeric(percentage)))
head(df_count_cust_2016)

order_count=names(table(df_order_year$count[df_order_year$order_purchase_year==2017]))

num_customer=table(df_order_year$count[df_order_year$order_purchase_year==2017])

percentage=table(df_order_year$count[df_order_year$order_purchase_year==2017])/sum(table(df_order_year$count[df_order_year$order_purchase_year==2017]))*100

df_count_cust_2017<-as.data.frame(cbind(order_count,num_customer=as.numeric(num_customer),percentage=as.numeric(percentage)))
head(df_count_cust_2017)

order_count=names(table(df_order_year$count[df_order_year$order_purchase_year==2018]))

num_customer=table(df_order_year$count[df_order_year$order_purchase_year==2018])

percentage=table(df_order_year$count[df_order_year$order_purchase_year==2018])/sum(table(df_order_year$count[df_order_year$order_purchase_year==2018]))*100

df_count_cust_2018<-as.data.frame(cbind(order_count,num_customer=as.numeric(num_customer),percentage=as.numeric(percentage)))
head(df_count_cust_2018)


#Rootcause Analysis
#Why 86% of customers shop with us only once? Key factors include

#Price
#Customer Experience - proxy by Review
#Delivery Duration

#Since we have review scores and order delivery time, we will focus here first.

df_quality<-df6 %>%
  group_by(order_purchase_year,year_month) %>%
summarise(mean(expected_duration),mean(ship_duration), mean(tocarrier_duration), mean(lastmile_duration),mean(expected_vs_shipdate),mean(review_score))
head(df_quality,10)

df_quality_year<-df6 %>%
  group_by(order_purchase_year) %>%
  summarise(mean(expected_duration),mean(ship_duration), mean(tocarrier_duration), mean(lastmile_duration),mean(expected_vs_shipdate),mean(review_score))
head(df_quality_year,10)

#Although our review score is not too bad, we have a very long end-to-end ship duration. The review score is high because our expected delivery date is almost a month, so we beat it everytime!

#'Last mile lead-time: from carriers to customers (hours)'
par(mfrow=c(2,2))
hist(as.numeric(df6$expected_duration), main = "Expected Ship Duration") # Frequency
hist(as.numeric(df6$tocarrier_duration),main= "Middle mile lead-time: from retailers to carriers")
hist(as.numeric(df6$ship_duration),main = "End to End Ship Duration")
hist(as.numeric(df6$lastmile_duration),main="Last mile lead-time: from carriers to customers")
par(mfrow=c(1,1))

hist(as.numeric(df6$expected_vs_shipdate),main='Difference between expected ship date and delivered date' ) 
#This delivery performance won't keep any customers. Average duration to ship is between 12-35 days!
hist(df6$review_score,main="Review Score 2016-2018")


##Now we're going to build a model that can predict if a customer would churn or not

#first we create a variable churn of YES/No based on the total number of order by customer

df_order$Churn<-if_else(df_order$count==1,"Yes","No")

df6<-left_join(df6,df_order,"customer_id")

#I’ll first split the data into training and testing data. Normally, statisticians normally go with an 80/20 split
library(caret)
split<- createDataPartition(df6$Churn,p=0.8,list=FALSE)
 set.seed(2021)
TR<- df6[split,] #training data
TS<- df6[-split,] #testing data
##decision tree model
library(rpart.plot)

#check dimensions of train & test set
dim(TR); dim(TS);


set.seed(3333)

#model formula
colnames(TR)

mymodel <- as.formula(as.factor(Churn) ~ payment_sequential + payment_type + payment_installments +  payment_value + price + freight_value +seller_city + seller_state + review_score +customer_city + customer_state + order_status +order_purchase_year + order_purchase_hour + order_purchase_day + order_purchase_mon +  ship_duration + tocarrier_duration + lastmile_duration + expected_vs_shipdate +expected_duration )

library(rpart)
tree_fit<-rpart(mymodel,TR,method = "class")
plotcp(tree_fit)
printcp(tree_fit)

tree_fit_prune<-prune(tree_fit,cp=0.01)

#rpart rules
rpart.rules(tree_fit_prune,"wide", roundint = FALSE,extra=4)
prp(tree_fit_prune, box.palette = "Blues", tweak = 1.2)
unTRs<-unique(TR$seller_city)
TS<-TS[TS$seller_city%in% unTRs,]
unTRc<-unique(TR$customer_city)
TS<-TS[TS$customer_city %in% unTRc,]
tree_prediction<-predict(tree_fit_prune,TS,"class")
confusionMatrix(as.factor(tree_prediction),as.factor(TS$Churn))

##Random Forest
library(randomForest)
rfFit <- randomForest(mymodel,  data = TR)
rfFit

varImpPlot(rfFit)

rf_prediction<-predict(rfFit,TS,"class")
confusionMatrix(as.factor(rf_prediction),as.factor(TS$Churn))


