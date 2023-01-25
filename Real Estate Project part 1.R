

# Load packages used:
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(data.table)
library(factoextra)
library(cluster)

# Read in data:
nycdata <- read_csv("./NYC Database/NYC_TRANSACTION_DATA.csv")
borough <- read_csv("./NYC Database/BOROUGH.csv")
buildingclass <- read_csv("./NYC Database/BUILDING_CLASS.csv") 
neighborhood <- read_csv("./NYC Database/NEIGHBORHOOD.csv")
glimpse(nycdata)

# Question 1

# Filter to 5 residential neighborhoods in BROOKLYN

residentialtable <- nycdata %>%
  left_join(y=neighborhood,by="NEIGHBORHOOD_ID") %>%
  left_join(y=borough,by="BOROUGH_ID") %>%
  filter(BOROUGH_NAME=="BROOKLYN") %>%
  left_join(y=buildingclass,by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) %>%
  filter(TYPE=="RESIDENTIAL") 

selectedneighborhood <- residentialtable%>%
  select(NEIGHBORHOOD_ID) %>%
  unique() %>%
  top_n(5) #selected neighborhoods are 54, 57, 58, 62, 66

filteredneighborhood <-residentialtable %>% 
  filter(NEIGHBORHOOD_ID==54|
           NEIGHBORHOOD_ID==57|
           NEIGHBORHOOD_ID==58| 
           NEIGHBORHOOD_ID==62|
           NEIGHBORHOOD_ID==66) %>%
  group_by(year=year(SALE_DATE), NEIGHBORHOOD_ID)
  
aveprice_squarefoot <-summarise(filteredneighborhood, averageprice_squarefoot=mean(SALE_PRICE/GROSS_SQUARE_FEET))
# Check for range of years:
# range(year(nycdata$SALE_DATE))

# Question 2 
sum(is.na(filteredneighborhood$GROSS_SQUARE_FEET)) #6815 cases
sum(filteredneighborhood$GROSS_SQUARE_FEET==0,na.rm=TRUE) #54793 cases
#NA and 0 in GROSS_SQUARE_FEET create NA in averageprice_squarefoot 


# Question 3 

adequate_neighborhoods<-filteredneighborhood %>%
  #filter out NA and 0 for GROSS_SQUARE_FEET
  filter(!is.na(GROSS_SQUARE_FEET)&GROSS_SQUARE_FEET!=0) %>%
  #filter out 0 for SALE_PRICE
  filter(SALE_PRICE!=0) #64154 rows filtered out


# Question 4 
n1 <- merged %>% 
  mutate(SALE_YEAR=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_ID==54, TYPE=="RESIDENTIAL") %>% 
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(SALE_YEAR) %>%
  summarize(AVG_PRICE_PER_FOOT=mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm=T))
view(n1)

n2 <- merged %>% 
  mutate(SALE_YEAR=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_ID==57, TYPE=="RESIDENTIAL") %>% 
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(SALE_YEAR) %>%
  summarize(AVG_PRICE_PER_FOOT=mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm=T))
view(n2)

n3 <- merged %>% 
  mutate(SALE_YEAR=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_ID==58, TYPE=="RESIDENTIAL") %>% 
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(SALE_YEAR) %>%
  summarize(AVG_PRICE_PER_FOOT=mean(SALE_PRICE/GROSS_SQUARE_FEET))
view(n3)

n4 <- merged %>% 
  mutate(SALE_YEAR=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_ID==62, TYPE=="RESIDENTIAL") %>% 
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(SALE_YEAR) %>%
  summarize(AVG_PRICE_PER_FOOT=mean(SALE_PRICE/GROSS_SQUARE_FEET))
view(n4)

n5 <- merged %>% 
  mutate(SALE_YEAR=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_ID==66, TYPE=="RESIDENTIAL") %>% 
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(SALE_YEAR) %>%
  summarize(AVG_PRICE_PER_FOOT=mean(SALE_PRICE/GROSS_SQUARE_FEET))
view(n5)

#to extract table to word 
write.table(new_aveprice_squarefoot, file = "q4.txt", sep = ",", quote = FALSE, row.names = F)

# Question 5
summary(new_aveprice_squarefoot) #mean of 564.89 
summary(new_aveprice_squarefoot %>% filter(NEIGHBORHOOD_ID==54))
summary(new_aveprice_squarefoot %>% filter(NEIGHBORHOOD_ID==57)) 
summary(new_aveprice_squarefoot %>% filter(NEIGHBORHOOD_ID==58))
summary(new_aveprice_squarefoot %>% filter(NEIGHBORHOOD_ID==62))   
summary(new_aveprice_squarefoot %>% filter(NEIGHBORHOOD_ID==66))    

# Question 6
#Join NEIGHBORHOOD_NAME for clearer labelling in plot
plot<-left_join(new_aveprice_squarefoot, neighborhood)

plot %>%
  ggplot(aes(x=year, y=averageprice_squarefoot, col = NEIGHBORHOOD_NAME)) + 
  geom_line() + 
  labs(x='Year', y='Average Price of 1sqft of Real Estate', title='Average Price of 1 Square Foot of Real Estate in Five Neighborhoods in BROOKLYN')
