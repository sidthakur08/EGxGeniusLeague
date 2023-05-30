library(ggplot2)
library(lme4)
library(dplyr)
library(tidyverse)
library(broom)

#ReadFileIn
vendor_df<- read.csv("Q3_citi_vendors.csv")
vendor_df$vendor<-factor(vendor_df$vendor)
vendor_df$day<-factor(vendor_df$day)
vendor_df$game<-factor(vendor_df$game)
vendor_df$section<-factor(vendor_df$section)

summary(vendor_df)
str(vendor_df)

#Checking for correlation
corrplot(cor(vendor_df[, !names(vendor_df) %in% c("vendor")]))
#Checking for normalcy
hist(vendor_df$hot_dogs_sold)#Seems normal, though a tail exists on the right


#Plotting scatterplot for each variable with hotdogssold
ggplot(vendor_df, aes(x = game, y = hot_dogs_sold)) +
  geom_point() 
ggplot(vendor_df, aes(x = day, y = hot_dogs_sold)) +
  geom_point() 
ggplot(vendor_df, aes(x = section, y = hot_dogs_sold)) +
  geom_point() 
ggplot(vendor_df, aes(x = vendor, y = hot_dogs_sold)) +
  geom_point() 

#Building a mixed effect model with the rest of the variable as fixed effect and vendor IDs being an intercept level random effect
#This is done to measure given similar fixed variables how does each vendor move the line. The variance gives us an idea of ranking. 
mixed_lmer <- lmer(hot_dogs_sold ~ day + section +game + (1|vendor) , data = vendor_df, verbose = 1)
summary(mixed_lmer)

#Use the effect and each of the vendor's intercept variance to rank each of the vendor. Positive number means more boost to sales.
vendor_effect <- mixed_lmer %>% 
  ranef() 
as.data.frame(vendor_p$vendor)
vendorRanking<-as.data.frame(vendor_effect$vendor)
colnames(vendorRanking)<-c("VendorEffectToSales")
vendorRanking<-rownames_to_column(vendorRanking, var="VendorID")
vendorRanking<-vendorRanking[order(-vendorRanking$VendorEffectToSales),] 

