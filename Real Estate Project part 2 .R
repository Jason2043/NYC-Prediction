# Load packages used:
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(data.table)
library(factoextra)
library(cluster)


# Read in data:
nyc_transactions <- read_csv("./NYC Database/NYC_TRANSACTION_DATA.csv")
neighborhood <- read_csv("./NYC Database/NEIGHBORHOOD.csv")
building_class <- read_csv("./NYC Database/BUILDING_CLASS.csv")
neighborhood <- read.csv("./NYC Database/NEIGHBORHOOD.csv")
glimpse(nyc_transactions)

select <- c(54,57,58,62,66)

# Question 1

# Residential Units Sold 
# useful sale
#data_with_0_Gross<- subset(nycdata,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
#                                    as.Date(SALE_DATE) <= as.Date("2020-12-31")&
#                                    SALE_PRICE>10&
#                                    NEIGHBORHOOD_ID%in%c(246,247,248,251,252))
#data_without_0_Gross <- subset(data_with_0_Gross,GROSS_SQUARE_FEET>10)
data <- subset(nyc_transactions,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                 as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                 SALE_PRICE>10&
                 GROSS_SQUARE_FEET>10&
                 NEIGHBORHOOD_ID%in%c(54,57,58,62,66))
sale=data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(sales=n())
print(sale)

#Question 2 

#Sale Price

mean_price = data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Mean_Price = mean(SALE_PRICE))
sd_price   = data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Sd_Price=sd(SALE_PRICE))
print(mean_price)
print(sd_price)
#Question 3 

#Gross Sqr. Footage
Gross = data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Mean_Grass = mean(GROSS_SQUARE_FEET,na.rm=TRUE))
print(Gross)
#Question 4

#Descriptive Summary of Sale Price and Gross SQR. Footage

Five_Price=data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(minimum       = min(SALE_PRICE),
                                                        low_quartile  = quantile(SALE_PRICE, 0.25),
                                                        median        = median(SALE_PRICE),
                                                        high_quartile = quantile(SALE_PRICE, 0.75),
                                                        maximum       = max(SALE_PRICE))
print(Five_Price)
Five_Gross=data%>%group_by(NEIGHBORHOOD_ID)%>%summarise (minimum      = min(GROSS_SQUARE_FEET,na.rm=TRUE),
                                                         low_quartile  = quantile(GROSS_SQUARE_FEET, 0.25,na.rm=TRUE),
                                                         median        = median(GROSS_SQUARE_FEET,na.rm=TRUE),
                                                         high_quartile = quantile(GROSS_SQUARE_FEET, 0.75,na.rm=TRUE),
                                                         maximum       = max(GROSS_SQUARE_FEET,na.rm=TRUE))
print(Five_Gross)
#Question 5

#Distribution of Properties Sold by Type: What is the proportion of units sold of residential, commercial, and mixed type in each of your chosen neighborhoods during 2011-2020?

data_with_type = merge(data,building_class,
                       by.x  = "BUILDING_CLASS_FINAL_ROLL",
                       by.y  = "BUILDING_CODE_ID" )
#write.csv(data_with_type,'C:/Users/Administrator/Desktop/data2.csv')

Res_data <- data_with_type%>%group_by(NEIGHBORHOOD_ID)%>%
  filter(TYPE =="RESIDENTIAL" )%>%
  summarise(RES = n())
Res_data_merge = merge(Res_data,sale)
Res_data_merge$RESIDENTIAL<-Res_data_merge$RES/Res_data_merge$sales
print(Res_data_merge)

COM_data <- data_with_type%>%group_by(NEIGHBORHOOD_ID)%>%
  filter(TYPE =="COMMERCIAL" )%>%
  summarise(COM = n())
COM_data_merge = merge(COM_data,sale)
COM_data_merge$COMMERICAL<-COM_data_merge$COM/COM_data_merge$sales
print(COM_data_merge)

OTHER_data <- data_with_type%>%group_by(NEIGHBORHOOD_ID)%>%
  filter(TYPE =="OTHER" )%>%
  summarise(OTHER = n())
OTHER_data_merge = merge(OTHER_data,sale)
OTHER_data_merge$RESIDENTIAL<-OTHER_data_merge$OTHER/OTHER_data_merge$sales
print(OTHER_data_merge)


#Question 6

#Sold: What is the correlation between sale price and gross square feet for residential properties in each of your chosen neighborhoods during 2011-2020?

Corre<-data%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Correlation=cor(SALE_PRICE,GROSS_SQUARE_FEET,method = "pearson", use = "complete.obs"))
print(Corre)

#Question 7

#Clustering Analysis

data_with_0_Gross <- subset(nyc_transactions,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                              as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                              SALE_PRICE>10)
data_w0g_merge = merge(data_with_0_Gross,building_class,
                       by.x = "BUILDING_CLASS_FINAL_ROLL",by.y="BUILDING_CODE_ID")#,all.y = TRUE

#Number of Sales for residential properties sold
data_1=data_w0g_merge%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(sales=n())%>%ungroup()
data1 <- na.omit(data_1)
data1 <- scale(data1)

pi1 = fviz_nbclust(data1,kmeans,method = "wss") + geom_vline(xintercept = 6,linetype =2)
print(pi1)

#So we know that the best center is 6
km1 <- kmeans(data_1, centers = 6, nstart=25)
si1=silhouette(km1$cluster, dist(data_1$sales))
plot(si1)
result1 = data.frame(cluster=km1$cluster,nbhd1=data_1$NEIGHBORHOOD_ID)%>%
  filter(nbhd1 %in% select)
result1 = merge(result1,data_1,
                by.x = "nbhd1",by.y="NEIGHBORHOOD_ID")
print(result1)

#Median Sale Price for residential properties sold

data_2=data_w0g_merge%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(mid_sales=median(SALE_PRICE,na.rm=TRUE)/1)%>%ungroup()
data2 <- na.omit(data_2)
data2 <- scale(data2)

pi2 = fviz_nbclust(data2,kmeans,method = "wss") + geom_vline(xintercept = 3,linetype =2)
print(pi2)

#So we know that the best center is 3
km2 <- kmeans(data2, centers = 3, nstart=25)
si2=silhouette(km2$cluster, dist(data_2$mid_sales))
plot(si2)


result2 = data.frame(cluster=km2$cluster,nbhd2=data_2$NEIGHBORHOOD_ID)%>%
  filter(nbhd2 %in% select)
result2 = merge(result2,data_2,
                by.x = "nbhd2",by.y="NEIGHBORHOOD_ID")
print(result2)
#Standard deviation of sales of residential properties sold

data_3=data_w0g_merge%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(s_sales=sd(SALE_PRICE,na.rm=TRUE))
data_3 <- na.omit(data_3)
data3 <- scale(data_3)

pi3 = fviz_nbclust(data3,kmeans,method = "wss") + geom_vline(xintercept = 5,linetype =2)
print(pi3)
#So we know that the best center is 5
km3 <- kmeans(data3, centers = 5, nstart=25)
si3=silhouette(km3$cluster, dist(data_3$s_sales))
plot(si3)

result3 = data.frame(cluster=km3$cluster,nbhd3=data_3$NEIGHBORHOOD_ID)%>%
  filter(nbhd3 %in% select)
result3 = merge(result3,data_3,
                by.x = "nbhd3",by.y="NEIGHBORHOOD_ID")
print(result3)


#Proportion of residential units sold

data_all = dim(data_w0g_merge)[1]
data_4=data_w0g_merge%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(per=n()/data_all)%>%ungroup()

data4 <- na.omit(data_4)
data4 <- scale(data4)

pi4 = fviz_nbclust(data2,kmeans,method = "wss") + geom_vline(xintercept = 4,linetype =2)
print(pi4)
#So we know that the best center is 4
km4 <- kmeans(data4, centers = 4, nstart=25)
si4=silhouette(km4$cluster, dist(data_4$per))
plot(si4)

result4 = data.frame(cluster=km4$cluster,nbhd4=data_4$NEIGHBORHOOD_ID)%>%
  filter(nbhd4 %in% select)
result4 = merge(result4,data_4,
                by.x = "nbhd4",by.y="NEIGHBORHOOD_ID")
print(result4)



#Sale Price of 1 gross square foot of residential real estate 

data_before_5 = subset(data_w0g_merge,GROSS_SQUARE_FEET>10)

data_5 = data_before_5%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Mean_Price = mean(SALE_PRICE))%>%ungroup()
data_gross_5 = data_before_5%>%filter(TYPE=='RESIDENTIAL')%>%group_by(NEIGHBORHOOD_ID)%>%summarise(Mean_Price = mean(GROSS_SQUARE_FEET))%>%ungroup()
data_5$Mean_Price = data_5$Mean_Price/data_gross_5$Mean_Price

data5 <- na.omit(data_5)
data5 <- scale(data5)


pi5 = fviz_nbclust(data5,kmeans,method = "wss") + geom_vline(xintercept = 7,linetype =2)
print(pi5)
#So we know that the best center is 7
km5 <- kmeans(data5, centers = 7, nstart=25)
si5=silhouette(km5$cluster, dist(data_5$Mean_Price))
mean(si5[, 3])
plot(si5)

result5 = data.frame(cluster=km5$cluster,nbhd5=data_5$NEIGHBORHOOD_ID)%>%
  filter(nbhd5 %in% select)
result5 = merge(result5,data_5,
                by.x = "nbhd5",by.y="NEIGHBORHOOD_ID")

print(result5)

#Question 8

#Comparative Analysis of Neighborhoods Across Clusters

#MidTown of 160,161,162 was selected as the adjacent community and the Upper West Side of 246,247,248 was compared

mid_town <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(160,161,162))


data_54 <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(54))

n54 = t.test(data_54$SALE_PRICE/data_54$GROSS_SQUARE_FEET,
              mid_town$SALE_PRICE/mid_town$GROSS_SQUARE_FEET,
              mu=0,paired=FALSE,conf.level = 0.95)
print(n54)

data_57 <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(57))

n57 = t.test(data_57$SALE_PRICE/data_57$GROSS_SQUARE_FEET,
              mid_town$SALE_PRICE/mid_town$GROSS_SQUARE_FEET,
              mu=0,paired=FALSE,conf.level = 0.95)
print(n57)

data_58 <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(58))

n58 = t.test(data_58$SALE_PRICE/data_58$GROSS_SQUARE_FEET,
              mid_town$SALE_PRICE/mid_town$GROSS_SQUARE_FEET,
              mu=0,paired=FALSE,conf.level = 0.95)
print(n58)
# Select 118,119,120 Harlem as the neighborhood and compare the Washington Height of 251,252
Harlem <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                   as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                   SALE_PRICE>10&
                   TYPE == "RESIDENTIAL"&
                   GROSS_SQUARE_FEET>10&
                   NEIGHBORHOOD_ID%in%c(118,119,120))


data_62 <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(62))

n62 = t.test(data_62$SALE_PRICE/data_62$GROSS_SQUARE_FEET,
              Harlem$SALE_PRICE/Harlem$GROSS_SQUARE_FEET,
              mu=0,paired=FALSE,conf.level = 0.95)
print(n62)

data_66 <- subset(data_w0g_merge,as.Date(SALE_DATE) >= as.Date("2010-01-01")&
                     as.Date(SALE_DATE) <= as.Date("2020-12-31")&
                     SALE_PRICE>10&
                     TYPE == "RESIDENTIAL"&
                     GROSS_SQUARE_FEET>10&
                     NEIGHBORHOOD_ID%in%c(66))

n66 = t.test(data_66$SALE_PRICE/data_66$GROSS_SQUARE_FEET,
              Harlem$SALE_PRICE/Harlem$GROSS_SQUARE_FEET,
              mu=0,paired=FALSE,conf.level = 0.95)
print(n66)

# Question 9
# According to the above analysis, 66 May be more suitable for real estate company construction. 
# Even if the mean price in 66 is not the highest, but the sold unit is way higher than other neighborhoods.


