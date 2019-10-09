#set Directory####
setwd("path")

#load library####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(corrplot)
library(caTools)
library(xgboost)
library(caret)
library(h2o)
library(DMwR)
library(pROC)
library(ROSE)


#no scientific notation####
options(scipen=999)

#load data####
train <- read.csv("Data/train_data/train.csv")
campaign_data <- read.csv("Data/train_data/campaign_data.csv")
coupon_item_mapping <- read.csv("Data/train_data/coupon_item_mapping.csv")
customer_demographics <- read.csv("Data/train_data/customer_demographics.csv")
customer_transaction_data <- read.csv("Data/train_data/customer_transaction_data.csv")
item_data <- read.csv("Data/train_data/item_data.csv")

test <- read.csv("Data/test.csv")

#Explore train data ####
str(train) #id variable (can't be used for model building)
summary(train)

sapply(train, function(x){sum(is.na(x))}) #no missing values

table(train$redemption_status)
prop.table(table(train$redemption_status)) #Highly Skewed data (99.07% : 0.93%)

length(unique(train$campaign_id)) #18 campaigns
length(unique(train$coupon_id)) #866 coupons
length(unique(train$customer_id)) #1428 customers (Demographics available for 760)

#Explore campaign_data####
str(campaign_data)
summary(campaign_data)

sapply(campaign_data, function(x){sum(is.na(x))}) #no missing values

#2 types of campaign
table(campaign_data$campaign_type) #6 types of camp X, 22 of camp Y
#join this variable to train data

#get dates in correct format
campaign_data$start_date = format(as.Date(campaign_data$start_date, "%d/%m/%y"), "20%y-%m-%d")
campaign_data$start_date = as.Date(campaign_data$start_date)
campaign_data$end_date = format(as.Date(campaign_data$end_date, "%d/%m/%y"), "20%y-%m-%d")
campaign_data$end_date = as.Date(campaign_data$end_date)

min(campaign_data$start_date) #2012-08-12
max(campaign_data$start_date) #2013-10-21

min(campaign_data$end_date) #2012-09-21
max(campaign_data$end_date) #2013-12-20

#New Variable -> No. of days the campaign ran
campaign_data$campaign_duration <- as.numeric(campaign_data$end_date - campaign_data$start_date)
#join this variable to train data

str(campaign_data)

#Join campaign data to train and test####
train1 <- merge(train,campaign_data,by = "campaign_id",all.x = T)
test1 <- merge(test,campaign_data,by = "campaign_id",all.x = T)

#Explore coupon_item_mapping####
str(coupon_item_mapping) #only ids, need item table mapping

sapply(coupon_item_mapping, function(x){sum(is.na(x))}) #no missing values

table(coupon_item_mapping$coupon_id) #a coupon is valid for multiple items

length(unique(coupon_item_mapping$coupon_id)) #1116 coupons
length(unique(coupon_item_mapping$item_id)) #36289 items had a promotion at least once

#Explore item_data####
str(item_data)

sapply(item_data, function(x){sum(is.na(x))}) #no missing values

length(unique(item_data$item_id)) #74066 items present
#48.9% of items were under promotion at least once

length(unique(item_data$brand)) #5528 brands
unique(item_data$brand_type) #2 brand types
unique(item_data$category) #19 categories

#Join coupon_item_mapping and item_data ####
coupon_info <- merge(coupon_item_mapping,item_data,by = "item_id",all.x= T)

str(coupon_info)

#New Variable -> no. of items coupon is valid on
#New Variable -> no. of brands coupon is valid on
#New Variable -> no. of brand type coupon is valid on
#New Variable -> no. of category coupon is valid on

coupon_info_aggregate <- aggregate(coupon_info[,-2],by = list(coupon_info$coupon_id),function(x) length(unique(x)))
names(coupon_info_aggregate) <- c("coupon_id","no_items_on_discount","no_brands_on_discount",
                                  "no_brand_type_on_discount","no_category_on_discount")

#join coupon_info_aggregate to train and test ####
train2 <- merge(train1,coupon_info_aggregate,by="coupon_id",all.x = T)
test2 <- merge(test1,coupon_info_aggregate,by="coupon_id",all.x = T)

#Explore customer_demographics####
str(customer_demographics)

sapply(customer_demographics, function(x){sum(is.na(x))}) #no missing values

table(customer_demographics$age_range) #map to train data
table(customer_demographics$marital_status)#map to train data

#treat missing values for marital status
#View(customer_demographics[customer_demographics$marital_status == "",])

customer_demographics$marital_status <- as.character(customer_demographics$marital_status)
unique(customer_demographics$marital_status)

customer_demographics[customer_demographics$marital_status == "" & customer_demographics$family_size == 1,"marital_status"] <- "Single"
unique(customer_demographics$marital_status)

customer_demographics[customer_demographics$marital_status == "" & customer_demographics$family_size == 4 & customer_demographics$no_of_children == "3+","marital_status"] <- "Single"
customer_demographics[customer_demographics$marital_status == "" & customer_demographics$family_size == 3 & customer_demographics$no_of_children == 2,"marital_status"] <- "Single"
customer_demographics[customer_demographics$marital_status == "" & customer_demographics$family_size == 2 & customer_demographics$no_of_children == 1,"marital_status"] <- "Single"

table(customer_demographics$marital_status)

#replace with mode for rest
customer_demographics[customer_demographics$marital_status == "" ,"marital_status"] <- "Married"

customer_demographics$marital_status <- as.factor(customer_demographics$marital_status)

table(customer_demographics$marital_status)

table(customer_demographics$rented)#map to train data

customer_demographics$rented <- as.factor(customer_demographics$rented)

table(customer_demographics$family_size)#map to train data
table(customer_demographics$no_of_children)#map to train data


#treat missing values in no_of_children
customer_demographics$no_of_children <- as.character(customer_demographics$no_of_children)
unique(customer_demographics$no_of_children)

#View(customer_demographics[customer_demographics$no_of_children == "",]) #family size either 1 or 2

customer_demographics[customer_demographics$no_of_children == "" & customer_demographics$family_size == 2 & customer_demographics$marital_status == "Married","no_of_children"] <- 0
customer_demographics[customer_demographics$no_of_children == "" & customer_demographics$family_size == 1,"no_of_children"] <- 0
#replace with mode for rest
table(customer_demographics$no_of_children)
customer_demographics[customer_demographics$no_of_children == "" ,"no_of_children"] <- 0

customer_demographics$no_of_children <- as.factor(customer_demographics$no_of_children)

table(customer_demographics$no_of_children)

customer_demographics$no_of_children <- as.factor(customer_demographics$no_of_children)

table(customer_demographics$income_bracket)#map to train data
#higher income braket corresponds to higher income (can be used as continous variable)

#join customer_demographics to train and test ####
train3 <- merge(train2,customer_demographics,by="customer_id",all.x = T)
test3 <- merge(test2,customer_demographics,by="customer_id",all.x = T)

#NAs introduced for missing customer demographics

#Explore customer_transaction_data####
str(customer_transaction_data)

sapply(customer_transaction_data, function(x){sum(is.na(x))}) #no missing values

#correct date format
customer_transaction_data$date <- as.Date(customer_transaction_data$date,format = "%Y-%m-%d")

length(unique(customer_transaction_data$item_id)) #74063
#3 items were never sold during the data period

#join customer_transaction_data and item_data ####
customer_transaction_info <- merge(customer_transaction_data,item_data,by = "item_id",all.x= T)

str(customer_transaction_info)

sapply(customer_transaction_info, function(x){sum(is.na(x))}) #no missing values

length(which(customer_transaction_info$coupon_discount != 0))# 21286 coupons were redeemed

revenue_from_customer <- aggregate(customer_transaction_info$selling_price,by = list(customer_transaction_info$customer_id),sum)
names(revenue_from_customer) <- c("customer_id","revenue_from_customer")

purchased_by_customer <- aggregate(customer_transaction_info[,c("item_id","brand","brand_type","category")],by = list(customer_transaction_info$customer_id),function(x){length(unique(x))})
names(purchased_by_customer) <- c("customer_id","no_items_purchased","no_brands_purchased",
                                  "no_brand_type_purchased","no_category_purchased")

coupon_disc_purchase_by_customer <- aggregate(customer_transaction_info[customer_transaction_info$coupon_discount != 0,c("item_id","brand","brand_type","category")],by = list(customer_transaction_info[customer_transaction_info$coupon_discount != 0,"customer_id"]),function(x){length(unique(x))})
names(coupon_disc_purchase_by_customer) <- c("customer_id","no_items_purchased_cd","no_brands_purchased_cd",
                                             "no_brand_type_purchased_cd","no_category_purchased_cd")


#join transaction info with train and test data ####
train4 <- merge(train3,revenue_from_customer,by = "customer_id",all.x = T)
test4 <- merge(test3,revenue_from_customer,by = "customer_id",all.x = T)

train5 <- merge(train4,purchased_by_customer,by = "customer_id",all.x = T)
test5 <- merge(test4,purchased_by_customer,by = "customer_id",all.x = T)

train6 <- merge(train5,coupon_disc_purchase_by_customer,by = "customer_id",all.x = T)
test6 <- merge(test5,coupon_disc_purchase_by_customer,by = "customer_id",all.x = T)

#Coupon Discount Transactions ####
coupon_discount_transactions <- customer_transaction_info[customer_transaction_info$coupon_discount != 0,]

str(coupon_discount_transactions)

table(coupon_discount_transactions$customer_id) #a customer did multiple coupon discount transactions

#Pending


# # seasonal behaviour ####
# train_shortlist_camp <- train6[train6$campaign_id %in% c(4,5,3,6,7,8,9,10,12,11,13),] #62040
# 
# table(train_shortlist_camp$redemption_status)
# 
# prop.table(table(train_shortlist_camp$redemption_status))
# 
# nrow(unique(train_shortlist_camp[,-1])) #60229
# 
# customer_transaction_data$transactions <- 1
# 
# #train data
# campaigns_train <- unique(train_shortlist_camp$campaign_id)
# 
# campaign_season_train <- list()
# campaign_season_train_customer <- list()
# 
# for(i in 1:length(campaigns_train)){
#   print(i)
#   campaign_season_train[[i]] <- customer_transaction_data[customer_transaction_data$date >= (campaign_data[campaign_data$campaign_id == campaigns_train[i],"start_date"]-365) & customer_transaction_data$date <= (campaign_data[campaign_data$campaign_id == campaigns_train[i],"end_date"]-365),]
#   campaign_season_train_customer[[i]] <- aggregate(campaign_season_train[[i]][c(4:8)],by = list(campaign_season_train[[i]][,3]),sum)
#   names(campaign_season_train_customer[[i]]) <- c("customer_id","seasonal_quantity","seasonal_selling_price",
#                                                   "seasonal_other_discount","seasonal_coupon_discount","seasonal_transaction")
#   campaign_season_train_customer[[i]]$campaign_id <- campaigns_train[i]
# }
# 
# names(campaign_season_train)  <- campaigns_train
# names(campaign_season_train_customer) <- campaigns_train
# 
# #test data
# campaigns_test <- unique(test$campaign_id)
# 
# campaign_season_test <- list()
# campaign_season_test_customer <- list()
# 
# for(i in 1:length(campaigns_test)){
#   print(i)
#   campaign_season_test[[i]] <- customer_transaction_data[customer_transaction_data$date >= (campaign_data[campaign_data$campaign_id == campaigns_test[i],"start_date"]-365) & customer_transaction_data$date <= (campaign_data[campaign_data$campaign_id == campaigns_test[i],"end_date"]-365),]
#   campaign_season_test_customer[[i]] <- aggregate(campaign_season_test[[i]][c(4:8)],by = list(campaign_season_test[[i]][,3]),sum)
#   names(campaign_season_test_customer[[i]]) <- c("customer_id","seasonal_quantity","seasonal_selling_price",
#                                                  "seasonal_other_discount","seasonal_coupon_discount","seasonal_transaction")
#   campaign_season_test_customer[[i]]$campaign_id <- campaigns_test[i]
# }
# 
# names(campaign_season_test)  <- campaigns_test
# names(campaign_season_test_customer) <- campaigns_test
# 
# campaign_season_train_customer_data <- do.call(rbind,campaign_season_train_customer)
# campaign_season_test_customer_data <- do.call(rbind,campaign_season_test_customer)
# 
# #Join seasonal behaviour to train and test ####
# 
# train7 <- merge(train_shortlist_camp,campaign_season_train_customer_data,by = c("customer_id","campaign_id"),all.x = T)
# test7 <- merge(test6,campaign_season_test_customer_data,by = c("customer_id","campaign_id"),all.x = T)
# 

# max and min and avg SP of each item and then coupon id####

customer_transaction_info$SP_per_unit <- customer_transaction_info$selling_price/customer_transaction_info$quantity

max_sp_item <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$item_id),max)
names(max_sp_item) <- c("item_id","max_sp_item")

min_sp_item <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$item_id),min)
names(min_sp_item) <- c("item_id","min_sp_item")

mean_sp_item <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$item_id),mean)
names(mean_sp_item) <- c("item_id","mean_sp_item")

coupon_sp <- merge(coupon_item_mapping,max_sp_item,by = "item_id",all.x = T)

coupon_sp <- merge(coupon_sp,min_sp_item,by = "item_id",all.x = T)

coupon_sp <- merge(coupon_sp,mean_sp_item,by = "item_id",all.x = T)

max_sp_coupon <- aggregate(coupon_sp$max_sp_item,by = list(coupon_sp$coupon_id),max)
names(max_sp_coupon) <- c("coupon_id","max_sp_coupon")

min_sp_coupon <- aggregate(coupon_sp$min_sp_item,by = list(coupon_sp$coupon_id),min)
names(min_sp_coupon) <- c("coupon_id","min_sp_coupon")

mean_sp_coupon <- aggregate(coupon_sp$mean_sp_item,by = list(coupon_sp$coupon_id),mean)
names(mean_sp_coupon) <- c("coupon_id","mean_sp_coupon")

# max and min SP of each customer####
max_sp_customer <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$customer_id),max)
names(max_sp_customer) <- c("customer_id","max_sp_customer")

min_sp_customer <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$item_id),min)
names(min_sp_customer) <- c("customer_id","min_sp_customer")

mean_sp_customer <- aggregate(customer_transaction_info$SP_per_unit,by = list(customer_transaction_info$item_id),mean)
names(mean_sp_customer) <- c("customer_id","mean_sp_customer")

# join sp with train and test data ####

train61 <- merge(train6,max_sp_coupon,by = "coupon_id",all.x = T)
test61 <- merge(test6,max_sp_coupon,by = "coupon_id",all.x = T)

train62 <- merge(train61,min_sp_coupon,by = "coupon_id",all.x = T)
test62 <- merge(test61,min_sp_coupon,by = "coupon_id",all.x = T)

train63 <- merge(train62,min_sp_customer,by = "customer_id",all.x = T)
test63 <- merge(test62,min_sp_customer,by = "customer_id",all.x = T)

train64 <- merge(train63,max_sp_customer,by = "customer_id",all.x = T)
test64 <- merge(test63,max_sp_customer,by = "customer_id",all.x = T)

train65 <- merge(train64,mean_sp_coupon,by = "coupon_id",all.x = T)
test65 <- merge(test64,mean_sp_coupon,by = "coupon_id",all.x = T)

train66 <- merge(train65,mean_sp_customer,by = "customer_id",all.x = T)
test66 <- merge(test65,mean_sp_customer,by = "customer_id",all.x = T)

#new variables out of max and min sp ####
train66$diff_max_sp <- train66$max_sp_customer - train66$max_sp_coupon
train66$diff_min_sp <- train66$min_sp_customer - train66$min_sp_coupon

test66$diff_max_sp <- test66$max_sp_customer - test66$max_sp_coupon
test66$diff_min_sp <- test66$min_sp_customer - test66$min_sp_coupon

train66$diff_mean_sp <- train66$mean_sp_customer - train66$mean_sp_coupon
test66$diff_mean_sp <- test66$mean_sp_customer - test66$mean_sp_coupon

# historical item purchase by customer ####

customer_coupon_info <- merge(customer_transaction_data,coupon_item_mapping,by = "item_id",all.x = T)

customer_coupon_redemption_chance <- customer_coupon_info[!is.na(customer_coupon_info$coupon_id),c("customer_id","coupon_id")]
customer_coupon_redemption_chance$redemption_chance <- 1

#2101361
customer_coupon_redemption_chance <- unique(customer_coupon_redemption_chance) #226598

#join redemption chance with train and test####

train67 <- merge(train66,customer_coupon_redemption_chance,by = c("customer_id","coupon_id"),all.x = T)
test67 <- merge(test66,customer_coupon_redemption_chance,by = c("customer_id","coupon_id"),all.x = T)


# discount percent info for customer ####
customer_transaction_info$MRP_per_unit <- (customer_transaction_info$selling_price - customer_transaction_info$other_discount - customer_transaction_info$coupon_discount)/customer_transaction_info$quantity
customer_transaction_info$other_discount_perc <- -customer_transaction_info$other_discount/(customer_transaction_info$quantity*customer_transaction_info$MRP_per_unit)
customer_transaction_info$coupon_discount_perc <- -customer_transaction_info$coupon_discount/((customer_transaction_info$quantity*customer_transaction_info$MRP_per_unit)+customer_transaction_info$other_discount)

coupon_discount_transactions$MRP_per_unit <- (coupon_discount_transactions$selling_price - coupon_discount_transactions$other_discount - coupon_discount_transactions$coupon_discount)/coupon_discount_transactions$quantity
coupon_discount_transactions$other_discount_perc <- -coupon_discount_transactions$other_discount/(coupon_discount_transactions$quantity*coupon_discount_transactions$MRP_per_unit)
coupon_discount_transactions$coupon_discount_perc <- -coupon_discount_transactions$coupon_discount/((coupon_discount_transactions$quantity*coupon_discount_transactions$MRP_per_unit)+coupon_discount_transactions$other_discount)

avg_cust_coupon_disc_perc <- aggregate(coupon_discount_transactions$coupon_discount_perc,by = list(coupon_discount_transactions$customer_id),mean)
names(avg_cust_coupon_disc_perc) <- c("customer_id","avg_cust_coupon_disc_perc")

# join avg coupon disc perc for customer with train and test ####
train68 <- merge(train67,avg_cust_coupon_disc_perc,by = "customer_id",all.x = T)
test68 <- merge(test67,avg_cust_coupon_disc_perc,by = "customer_id",all.x = T)

# discount percent info for coupon ####
avg_item_coupon_disc_perc <- aggregate(coupon_discount_transactions$coupon_discount_perc,by = list(coupon_discount_transactions$item_id),mean)
names(avg_item_coupon_disc_perc) <- c("item_id","avg_coupon_disc_perc")

coupon_item_disc_perc <- merge(coupon_item_mapping,avg_item_coupon_disc_perc,by = "item_id",all.x = T)
coupon_item_disc_perc <- coupon_item_disc_perc[!is.na(coupon_item_disc_perc$avg_coupon_disc_perc),]

coupon_mean_disc_perc  <- aggregate(coupon_item_disc_perc$avg_coupon_disc_perc,by = list(coupon_item_disc_perc$coupon_id),mean)
names(coupon_mean_disc_perc) <- c("coupon_id","mean_coupon_disc_perc")

# join avg coupon disc perc for coupon with train and test ####
train69 <- merge(train68,coupon_mean_disc_perc,by = "coupon_id",all.x = T)
test69 <- merge(test68,coupon_mean_disc_perc,by = "coupon_id",all.x = T)

#Schema Exploration Finished####

train8 <- train69[,c("id","customer_id","campaign_id","coupon_id","campaign_type","campaign_duration","no_items_on_discount","no_brands_on_discount",
                     "no_brand_type_on_discount","no_category_on_discount","age_range",
                     "marital_status","rented","family_size","no_of_children","income_bracket",
                     "revenue_from_customer","no_items_purchased","no_brands_purchased",
                     "no_brand_type_purchased","no_category_purchased","no_items_purchased_cd",
                     "no_brands_purchased_cd","no_brand_type_purchased_cd","no_category_purchased_cd",
                     "diff_max_sp","diff_min_sp","diff_mean_sp","redemption_chance",
                     "avg_cust_coupon_disc_perc","mean_coupon_disc_perc","redemption_status")]

test8 <- test69[,c("id","customer_id","campaign_id","coupon_id","campaign_type","campaign_duration","no_items_on_discount","no_brands_on_discount",
                   "no_brand_type_on_discount","no_category_on_discount","age_range",
                   "marital_status","rented","family_size","no_of_children","income_bracket",
                   "revenue_from_customer","no_items_purchased","no_brands_purchased",
                   "no_brand_type_purchased","no_category_purchased","no_items_purchased_cd",
                   "no_brands_purchased_cd","no_brand_type_purchased_cd","no_category_purchased_cd",
                   "diff_max_sp","diff_min_sp","diff_mean_sp","redemption_chance",
                   "avg_cust_coupon_disc_perc","mean_coupon_disc_perc")]

#Plotting Functions ####
Categorical <- function(data, source_var, target_var){
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar() +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  
  p2 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar(position = "fill") +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  x11() 
  grid.arrange(p1, p2)
  
}

Numeric <- function(data, source_var, target_var){
  
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
    theme_gdocs() + scale_fill_tableau(name = target_var) + geom_density(alpha = 0.3) +
    labs(x = source_var, y = "density") 
  
  p2 <- ggplot(data, aes(x = data[,c(target_var)], y = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_boxplot() + theme_gdocs() + scale_fill_tableau(name = target_var) + 
    labs(x = target_var, y = source_var)
  
  x11() 
  grid.arrange(p1, p2)
  
}

#EDA ####
str(train8)
summary(train8)

train8$redemption_status <- as.factor(train8$redemption_status)


redemption_status  <-  train8$redemption_status
train8$redemption_status   <-  NULL

data <- rbind(train8,test8)

train8$redemption_status <- redemption_status

#campaign_type
#Categorical(train8,"campaign_type","redemption_status")
prop.table(table(train8[,c("campaign_type","redemption_status")]),1) #Redemption 2.5 times for X type

dummy <- as.data.frame(model.matrix(~campaign_type, data = data))
data <- cbind(data[,-which("campaign_type" == names(data))], dummy[,-1])
names(data)[ncol(data)] <- names(dummy)[2]

#age_range
#Categorical(train8,"age_range","redemption_status")  
#NA's present as customer demographic missing

train8$age_range <- as.character(train8$age_range)
unique(train8$age_range)
train8[is.na(train8$age_range),"age_range"] <- "Unknown"
train8$age_range <- as.factor(train8$age_range)

data$age_range <- as.character(data$age_range)
unique(data$age_range)
data[is.na(data$age_range),"age_range"] <- "Unknown"
data$age_range <- as.factor(data$age_range)

prop.table(table(train8[,c("age_range","redemption_status")]),1) 
#need to combine age 26-70

data$age_range <- as.character(data$age_range)
data[data$age_range %in% c("26-35","36-45","46-55","56-70"),"age_range"] <- "26-70"
data$age_range <- as.factor(data$age_range)

dummy <- as.data.frame(model.matrix(~age_range, data = data))
data <- cbind(data[,-which("age_range" == names(data))], dummy[,-1])

#marital_status
#Categorical(train8,"marital_status","redemption_status")  
#NA's present as customer demographic missing

train8$marital_status <- as.character(train8$marital_status)
unique(train8$marital_status)
train8[is.na(train8$marital_status),"marital_status"] <- "Unknown" #creating new category
train8$marital_status <- as.factor(train8$marital_status)

data$marital_status <- as.character(data$marital_status)
unique(data$marital_status)
data[is.na(data$marital_status),"marital_status"] <- "Unknown" #creating new category
data$marital_status <- as.factor(data$marital_status)


prop.table(table(train8[,c("marital_status","redemption_status")]),1) 
#missing customer demographic has lowest redemption rate (already taken care of by age_range)
#remove variable

train8 <- train8 %>% dplyr::select(-marital_status)
data <- data %>% dplyr::select(-marital_status)

#family_size
#Categorical(train8,"family_size","redemption_status")  
#NA's present as customer demographic missing

train8$family_size <- as.character(train8$family_size)
unique(train8$family_size)
train8[is.na(train8$family_size),"family_size"] <- "Unknown" #creating new category
train8$family_size <- as.factor(train8$family_size)

data$family_size <- as.character(data$family_size)
unique(data$family_size)
data[is.na(data$family_size),"family_size"] <- "Unknown" #creating new category
data$family_size <- as.factor(data$family_size)

prop.table(table(train8[,c("family_size","redemption_status")]),1)
#family size 1 and 2 together in a bin

data$family_size <- as.character(data$family_size)
unique(data$family_size)

data[data$family_size %in% c("1","2"),"family_size"] <- "<3"
data$family_size <- as.factor(data$family_size)

dummy <- as.data.frame(model.matrix(~family_size, data = data))
data <- cbind(data[,-which("family_size" == names(data))], dummy[,-1])

#no_of_children
#Categorical(train8,"no_of_children","redemption_status")  
#NA's present as customer demographic missing

train8$no_of_children <- as.character(train8$no_of_children)
unique(train8$no_of_children)
train8[is.na(train8$no_of_children),"no_of_children"] <- "Unknown" #creating new category
train8$no_of_children <- as.factor(train8$no_of_children)

data$no_of_children <- as.character(data$no_of_children)
unique(data$no_of_children)
data[is.na(data$no_of_children),"no_of_children"] <- "Unknown" #creating new category
data$no_of_children <- as.factor(data$no_of_children)

prop.table(table(train8[,c("no_of_children","redemption_status")]),1)
#keep

dummy <- as.data.frame(model.matrix(~no_of_children, data = data))
data <- cbind(data[,-which("no_of_children" == names(data))], dummy[,-1])

#rented
#Categorical(train8,"rented","redemption_status")  
#rented has missing values due to missing demographics

train8$rented <- as.character(train8$rented)
unique(train8$rented)
train8[is.na(train8$rented),"rented"] <- "Unknown" #creating new category
train8$rented <- as.factor(train8$rented)

data$rented <- as.character(data$rented)
unique(data$rented)
data[is.na(data$rented),"rented"] <- "Unknown" #creating new category
data$rented <- as.factor(data$rented)

prop.table(table(train8[,c("rented","redemption_status")]),1)
#keep

dummy <- as.data.frame(model.matrix(~rented, data = data))
data <- cbind(data[,-which("rented" == names(data))], dummy[,-1])

#income_bracket
#Numeric(train8,"income_bracket","redemption_status") 
#seems important
#income_bracket has missing values due to missing demographics

unique(train8$income_bracket)
train8[is.na(train8$income_bracket),"income_bracket"] <- "Unknown" #creating new category
train8$income_bracket <- as.factor(train8$income_bracket)

unique(data$income_bracket)
data[is.na(data$income_bracket),"income_bracket"] <- "Unknown" #creating new category
data$income_bracket <- as.factor(data$income_bracket)

table(train8[,c("income_bracket","redemption_status")])
prop.table(table(train8[,c("income_bracket","redemption_status")]),1) #Redemption double for X type
#Needs binning

#frequency
income_freq <- data.frame(prop.table(table(train8[,c("income_bracket")]))[order(prop.table(table(train8[,c("income_bracket")])))])
names(income_freq) <- c("income_bracket","freq")
#response rate
income_rr <- data.frame(prop.table(table(train8[,c("income_bracket","redemption_status")]),1)[order(prop.table(table(train8[,c("income_bracket","redemption_status")]),1)[,2]),])[14:26,-2]
names(income_rr) <- c("income_bracket","RR")

#freq and rr
income <- merge(income_rr,income_freq,by="income_bracket")
income <- income[order(income$RR),]
income$cumfreq <- cumsum(income$freq)

#New Categories
data$new_income_bracket <- ifelse((as.character(data$income_bracket) == 11 | as.character(data$income_bracket) == 12),"ib1",
                                  ifelse((as.character(data$income_bracket) == 2 | as.character(data$income_bracket) == 4 | as.character(data$income_bracket) == 7),"ib2",
                                         ifelse((as.character(data$income_bracket) == 3 | as.character(data$income_bracket) == 8 | as.character(data$income_bracket) == 1),"ib3",
                                                ifelse((as.character(data$income_bracket) == 9 | as.character(data$income_bracket) == 10),"ib4",
                                                       ifelse((as.character(data$income_bracket) == 5 | as.character(data$income_bracket) == 6),"ib5",
                                                              "Unknown")))))

data <- data[,-which("income_bracket" == names(data))]

dummy <- as.data.frame(model.matrix(~new_income_bracket, data = data))
data <- cbind(data[,-which("new_income_bracket" == names(data))], dummy[,-1])

#campaign_duration
#Numeric(train8,"campaign_duration","redemption_status") 
#Campaign running for less days have less redemption rate

#no_items_on_discount
#Numeric(train8,"no_items_on_discount","redemption_status") 
#might need binning or outlier treatment (look at woe)

#no_brands_on_discount
#Numeric(train8,"no_brands_on_discount","redemption_status") 
#does not seem important (look at woe)

#no_brand_type_on_discount
#Numeric(train8,"no_brand_type_on_discount","redemption_status") 
#very important variable (make it categorical)

data$no_brand_type_on_discount <- as.factor(data$no_brand_type_on_discount)
#Categorical(train8,"no_brand_type_on_discount","redemption_status")

dummy <- as.data.frame(model.matrix(~no_brand_type_on_discount, data = data))
data <- cbind(data[,-which("no_brand_type_on_discount" == names(data))], dummy[,-1])
names(data)[ncol(data)] <- names(dummy)[2]

#no_category_on_discount
#Numeric(train8,"no_category_on_discount","redemption_status") 
#seems important (look at woe)

#revenue_from_customer
#Numeric(train8,"revenue_from_customer","redemption_status") 
#very important

#no_items_purchased
#Numeric(train8,"no_items_purchased","redemption_status") 
#very important

#no_brands_purchased
#Numeric(train8,"no_brands_purchased","redemption_status") 
#very important

#no_brand_type_purchased
#Numeric(train8,"no_brand_type_purchased","redemption_status") 
unique(train8$no_brand_type_purchased) #drop

train8 <- train8 %>% dplyr::select(-no_brand_type_purchased)
data <- data %>% dplyr::select(-no_brand_type_purchased)

#no_category_purchased
#Numeric(train8,"no_category_purchased","redemption_status") 
#very important

#no_items_purchased_cd
#Numeric(train8,"no_items_purchased_cd","redemption_status") 
#very important

data[is.na(data$no_items_purchased_cd),"no_items_purchased_cd"] <- 0

#no_brands_purchased_cd
#Numeric(train8,"no_brands_purchased_cd","redemption_status") 
#very important

data[is.na(data$no_brands_purchased_cd),"no_brands_purchased_cd"] <- 0

#no_brand_type_purchased_cd
#Numeric(train8,"no_brand_type_purchased_cd","redemption_status") 
#very important

data[is.na(data$no_brand_type_purchased_cd),"no_brand_type_purchased_cd"] <- 0

#no_category_purchased_cd
#Numeric(train8,"no_category_purchased_cd","redemption_status") 
#very important

data[is.na(data$no_category_purchased_cd),"no_category_purchased_cd"] <- 0

#diff_max_sp
#Numeric(train8,"diff_max_sp","redemption_status") 
#very important

data[is.na(data$diff_max_sp),"diff_max_sp"] <- mean(data$diff_max_sp,na.rm = T)

# train8 <- train8 %>% dplyr::select(-diff_max_sp)
# data <- data %>% dplyr::select(-diff_max_sp)

#diff_min_sp
#Numeric(train8,"diff_min_sp","redemption_status") 
#very important

data[is.na(data$diff_min_sp),"diff_min_sp"] <- mean(data$diff_min_sp,na.rm = T)

# train8 <- train8 %>% dplyr::select(-diff_min_sp)
# data <- data %>% dplyr::select(-diff_min_sp)

#diff_mean_sp
#Numeric(train8,"diff_mean_sp","redemption_status") 
#very important

data[is.na(data$diff_mean_sp),"diff_mean_sp"] <- mean(data$diff_mean_sp,na.rm = T)

# train8 <- train8 %>% dplyr::select(-diff_mean_sp)
# data <- data %>% dplyr::select(-diff_mean_sp)

#avg_cust_coupon_disc_perc
#Numeric(train8,"avg_cust_coupon_disc_perc","redemption_status") 
#very important

data[is.na(data$avg_cust_coupon_disc_perc),"avg_cust_coupon_disc_perc"] <- 0

#mean_coupon_disc_perc
#Numeric(train8,"mean_coupon_disc_perc","redemption_status") 
#very important

data[is.na(data$mean_coupon_disc_perc),"mean_coupon_disc_perc"] <- mean(data$mean_coupon_disc_perc,na.rm = T)

#redemption_chance
#Categorical(train8,"redemption_chance","redemption_status") 
#very important

data[is.na(data$redemption_chance),"redemption_chance"] <- 0

#customer_id
#Numeric(train8,"customer_id","redemption_status") 
#try

#campaign_id
#Numeric(train8,"campaign_id","redemption_status") 
#very important

train8 <- train8 %>% dplyr::select(-campaign_id)
data <- data %>% dplyr::select(-campaign_id)

#coupon_id
#Numeric(train8,"coupon_id","redemption_status") 
#try

#split back to train and test ####

data_train <- data[which(data$id %in% train$id),]
data_test <- data[which(data$id %in% test$id),]

# data_train <- data[1:nrow(train8),]
# data_test <- data[(nrow(train8)+1):nrow(data),]

data_train$redemption_status <- redemption_status

data_train$redemption_status <- as.numeric(as.character(data_train$redemption_status))

#Correlation ####

corr <- data.frame(cor(data_train))

#remove redundant variables
data_train <- data_train %>% dplyr::select(-age_rangeUnknown)
data_train <- data_train %>% dplyr::select(-family_sizeUnknown)
data_train <- data_train %>% dplyr::select(-no_of_childrenUnknown)
data_train <- data_train %>% dplyr::select(-rentedUnknown)

data_test <- data_test %>% dplyr::select(-age_rangeUnknown)
data_test <- data_test %>% dplyr::select(-family_sizeUnknown)
data_test <- data_test %>% dplyr::select(-no_of_childrenUnknown)
data_test <- data_test %>% dplyr::select(-rentedUnknown)

corr <- data.frame(cor(data_train))

# #SMOTE ####
# 
# data_train$redemption_status  <- redemption_status
# 
# table(data_train$redemption_status)
# 
# 
# newData <- SMOTE(redemption_status ~ ., data_train, perc.over = 1000,perc.under=1000)
# 
# table(newData$redemption_status)
# prop.table(table(newData$redemption_status))
# 
# data_train<- newData

# #UnderSampling ####
# 
# data_train$redemption_status <-redemption_status
# 
# table(data_train$redemption_status)
# 
# undersample_index <- sample(which(data_train$redemption_status == 0),20000)
# 
# newData <- rbind(data_train[undersample_index,],data_train[data_train$redemption_status == 1,])
# 
# table(newData$redemption_status)
# prop.table(table(newData$redemption_status))
# 
# data_train <- newData
# 
# redemption_status <- data_train$redemption_status

#Standardization ####

View(data_train)

standard_vars <- c(2:18,20,21)

var_mean <- sapply(data_train[,standard_vars],mean)
var_sd <- sapply(data_train[,standard_vars],sd)

for(i in names(data_train)[standard_vars]){
  data_train[,i] <- (data_train[,i]-var_mean[i])/var_sd[i]
  data_test[,i] <- (data_test[,i]-var_mean[i])/var_sd[i]
}

#Model ####

nrow(unique(data_train[,-1]))#78327

data_train <- unique(data_train[,-1])

data_train$redemption_status <- as.factor(data_train$redemption_status)

redemption_status <- data_train$redemption_status

#data_train$redemption_status <- redemption_status
label = as.integer(data_train$redemption_status)-1
data_train$redemption_status = NULL

#Parameters
num_class = length(levels(redemption_status))

# params = list(
#   booster="gbtree",
#   eta=0.001,
#   max_depth=9,
#   gamma=3,
#   subsample=0.75,
#   colsample_bytree=1,
#   objective="multi:softmax",
#   eval_metric="mlogloss",
#   num_class=num_class
# )

params = list(
  booster="gbtree",
  eta=0.01,
  max_depth=7,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="binary:logistic",
  eval_metric="logloss"
  # ,
  # num_class=num_class
)

#seed
set.seed(123)

#Cross validation
folds = createFolds(redemption_status, k = 5)

cv = lapply(folds, function(x) {
  
  train.data = as.matrix(data_train[-x,])
  train.label = label[-x]
  valid.data = as.matrix(data_train[x,])
  valid.label = label[x]
  
  xgb.train = xgb.DMatrix(data=train.data,label=train.label)
  xgb.valid = xgb.DMatrix(data=valid.data,label=valid.label)
  test.data <- as.matrix(data_test[,-1])
  
  xgb.fit=xgb.train(
    params=params,
    data=xgb.train,
    nrounds=10000,
    nthreads=1,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.valid)
  )
  
  xgb.fitted = predict(xgb.fit,train.data,reshape=T)
  xgb.fitted = as.data.frame(xgb.fitted)
  colnames(xgb.fitted) = levels(redemption_status)[2]
  xgb.fitted = cbind.data.frame(xgb.fitted,actual = train.label)
  
  xgb.pred = predict(xgb.fit,valid.data,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(redemption_status)[2]
  xgb.pred = cbind.data.frame(xgb.pred,actual = valid.label)
  
  xgb.test = predict(xgb.fit,test.data,reshape=T)
  xgb.test = as.data.frame(xgb.test)
  
  return(list(xgb.fitted,xgb.pred,xgb.fit,xgb.test))
  
})

auc(cv$Fold1[[1]][,2], cv$Fold1[[1]][,1])
auc(cv$Fold1[[2]][,2], cv$Fold1[[2]][,1])

auc(cv$Fold2[[1]][,2], cv$Fold2[[1]][,1])
auc(cv$Fold2[[2]][,2], cv$Fold2[[2]][,1])

auc(cv$Fold3[[1]][,2], cv$Fold3[[1]][,1])
auc(cv$Fold3[[2]][,2], cv$Fold3[[2]][,1])

auc(cv$Fold4[[1]][,2], cv$Fold4[[1]][,1])
auc(cv$Fold4[[2]][,2], cv$Fold4[[2]][,1])

auc(cv$Fold5[[1]][,2], cv$Fold5[[1]][,1])
auc(cv$Fold5[[2]][,2], cv$Fold5[[2]][,1])

output_prob <- cbind(cv$Fold1[[4]][,1],cv$Fold2[[4]][,1],cv$Fold3[[4]][,1],cv$Fold4[[4]][,1],cv$Fold5[[4]][,1])

output <- rowMeans(output_prob)

#Submissions ####

Submission32 <- cbind.data.frame(id = data_test$id,redemption_status = output)

Submission32 <- Submission32[order(Submission32$id),]

write.csv(Submission32,"Outputs/Submission32.csv",row.names=F)





#save image ####
save.image("Outputs/Submission32.RData")











