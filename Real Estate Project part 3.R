# Load packages used:
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(data.table)
library(cluster)
library(forecast)


# Read in data:
nycdata <- read_csv("./NYC Database/NYC_TRANSACTION_DATA.csv")
borough <- read_csv("./NYC Database/BOROUGH.csv")
buildingclass <- read_csv("./NYC Database/BUILDING_CLASS.csv") 
neighborhood <- read_csv("./NYC Database/NEIGHBORHOOD.csv")
nycdata      <-  merge(nycdata,buildingclass,
                       by.x  = "BUILDING_CLASS_FINAL_ROLL",
                       by.y  = "BUILDING_CODE_ID" )
glimpse(nycdata)

select <- c(54,57,58,62,66)

# The community capacity is selected as follows:
# The volume of sales is known by Assignment_3         The maximum size is 246
# If you exclude all Gross_Square_Feet = 0, the minimum number is 251
# If not excluded, the minimum is 248

#The data is the grouping of the selection
{
data_high    <- subset(nycdata,as.Date(SALE_DATE) >= as.Date("2011-01-01")&
                       SALE_PRICE>10&
                       NEIGHBORHOOD_ID%in%c(54))
data_high_no <- subset(nycdata,as.Date(SALE_DATE) >= as.Date("2011-01-01")&
                       SALE_PRICE>10&
                       GROSS_SQUARE_FEET>10&
                       NEIGHBORHOOD_ID%in%c(54))
data_low     <- subset(nycdata,as.Date(SALE_DATE) >= as.Date("2011-01-01")&
                       SALE_PRICE>10&
                       NEIGHBORHOOD_ID%in%c(57))
data_low_no  <- subset(nycdata,as.Date(SALE_DATE) >= as.Date("2011-01-01")&
                       SALE_PRICE>10&
                       GROSS_SQUARE_FEET>10&
                       NEIGHBORHOOD_ID%in%c(66))
}

#kpi 
#These data are KPIs for the corresponding groups
{
kpi_high    = data_high    %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                               filter(TYPE =="RESIDENTIAL" )%>%
                               summarize(sum_of_sales = sum(SALE_PRICE)/1)

kpi_high_no = data_high_no %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                               filter(TYPE =="RESIDENTIAL" )%>%
                               summarize(sum_of_sales = sum(SALE_PRICE)/1)

kpi_low     = data_low     %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                               filter(TYPE =="RESIDENTIAL" )%>%
                               summarize(sum_of_sales = sum(SALE_PRICE)/1)

kpi_low_no  = data_low_no  %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                               filter(TYPE =="RESIDENTIAL" )%>%
                               summarize(sum_of_sales = sum(SALE_PRICE)/1)
}
# Part One
#These data are time series corresponding to KPIs

{
tsales_kpi_high    <- ts(kpi_high$sum_of_sales   ,start = c(2011,1),frequency = 12)
plot(tsales_kpi_high,main="tsales_kpi_high")

tsales_kpi_high_no <- ts(kpi_high_no$sum_of_sales,start = c(2011,1),frequency = 12)
plot(tsales_kpi_high_no,main="tsales_kpi_high_no")

tsales_kpi_low     <- ts(kpi_low$sum_of_sales    ,start = c(2011,1),frequency = 12)
plot(tsales_kpi_low,main="tsales_kpi_low")

tsales_kpi_low_no  <- ts(kpi_low_no$sum_of_sales ,start = c(2011,1),frequency = 12)
plot(tsales_kpi_low_no,main="tsales_kpi_low_no")
}
#Data smoothing (not used)
smooth <- function(tsales_kpi){
smooth1 <- ma(tsales_kpi,3)
plot(smooth1,main="Simple Moving Averages (k=3)")
}

#The data needs to take into account seasonal characteristics, seasonal characteristics
#Test models such as suitable seasons
add_fit <- function(sales_k) {
  fit <- stl(sales_k,s.window = "period")
  monthplot(sales_k,xlab="",ylab="")
  seasonplot(sales_k,year.labels="TRUE",main="")
  plot(fit)
}

log_fit <- function(sales_k){
  lsales_k <- log(sales_k)
  monthplot(lsales_k,xlab="",ylab="")
  seasonplot(lsales_k,year.labels="TRUE",main="")
  fit <- stl(lsales_k,s.window = "period")
  plot(fit)
}

add_fit(tsales_kpi_high)
log_fit(tsales_kpi_high)
add_fit(tsales_kpi_high_no)
log_fit(tsales_kpi_high_no)
add_fit(tsales_kpi_low)
log_fit(tsales_kpi_low)
add_fit(tsales_kpi_low_no)
log_fit(tsales_kpi_low_no)

#The way the data is predicted needs to be validated and justified 
#There is no trend, there is seasonality

mod <- function(tsales_kpi) {
model <- ets(y=tsales_kpi,model='MNM')
fit <- predict(model,32)
plot(fit)
print(fit)
lines(tsales_kpi,col='red')
}
mod(tsales_kpi_high)
mod(tsales_kpi_high_no)
mod(tsales_kpi_low)
mod(tsales_kpi_low_no)
# Part Two
# Here are the sales figures
{
sales_high    = data_high    %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                                 summarize(sales=n())
sales_high_no = data_high_no %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                                 summarize(sales=n())
sales_low     = data_low     %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                                 summarize(sales=n())
sales_low_no  = data_low_no  %>% group_by(month = lubridate::floor_date(SALE_DATE, 'month')) %>%
                                 summarize(sales=n())
}
#plot(sales_high$month,sales_high$sales)
#Model 1
R1 <- function(sales_k) {
  fit <- lm(sales~month,data = sales_k)
  plot(sales_k$month,sales_k$sales)
#  kk <- summary(fit)
#  print(kk)
  abline(fit)
}
#Model 2
R2 <- function(sales_k) {
  tsales_k   <- ts(sales_k$sales  ,start = c(2011,1),frequency = 12)
  tt <- decompose(tsales_k)
  tt <- as.numeric(tt$seasonal)
  fit <- lm(sales~month+I(tt),data = sales_k)
 #kk <- summary(fit)
 #print(kk)
  plot(sales_k$month,sales_k$sales)
  lines(sales_k$month,fitted(fit))
}
R1(sales_high)
R2(sales_high)
R1(sales_high_no)
R2(sales_high_no)
R1(sales_low)
R2(sales_low)
R1(sales_low_no)
R2(sales_low_no)


# Part Three
R3 <- function(sales_k) {
  high_no <- subset(sales_k,select = c(SALE_PRICE,SALE_DATE,TYPE,GROSS_SQUARE_FEET,RESIDENTIAL_UNITS,YEAR_BUILT))
  high_no[,c(3)][high_no[,c(3)] == "RESIDENTIAL"] = 1
  high_no[,c(3)][high_no[,c(3)] == "COMMERCIAL"] = 2
  high_no[,c(3)][high_no[,c(3)] == "MIXED"] = 3
  high_no[,c(3)][high_no[,c(3)] == "OTHER"] = 4
  ti <- as.numeric(high_no$SALE_DATE)
  high_no[,c(2)] = ti
  high_no[,c(1)] = high_no$SALE_PRICE/1
  ty <- as.numeric(high_no$TYPE)
  high_no[,c(3)] = ty
  cor(high_no)
  scatterplotMatrix(high_no,main="Scatter Plot Matrix")
  fit <- lm(SALE_PRICE~GROSS_SQUARE_FEET+RESIDENTIAL_UNITS+SALE_DATE+TYPE+YEAR_BUILT,data=high_no)
  #kk <- summary(fit)
  #print(kk)
  #print(fit)
}





