#--------------#
#Initialization#
#--------------#



install.packages("vctrs")
install.packages("readr")
library(readr)
data <- read.csv("mva_data.csv",  na = c("", "NA", "-", "null"))

head(data)
str(data)
summary(data)
(colSums(is.na(data))/nrow(data))*100
raw_data <- data




#-------------------------------#
#identifying type of missingness#
#-------------------------------#




(colSums(is.na(data))/nrow(data))*100

#Test for MCAR - Visual Logical Evidence
install.packages("naniar")
install.packages("dplyr")
library(naniar)
library(dplyr)
vis_miss(data)

#Test for MAR - Relationship based Evidence
#Chi_Square Test - p<0.05 = Significant
#                  p>0.05 = No Significant       
data$Age_missing <- ifelse(is.na(data$Age), 1, 0) #creating missing indicator

table(data$Age_missing, data$Gender)
chisq.test(table(data$Age_missing, data$Gender))

table(data$Age_missing, data$City)
chisq.test(table(data$Age_missing, data$City))

table(data$Age_missing, data$Campaign_Channel)
chisq.test(table(data$Age_missing, data$Campaign_Channel))

#Test for MNAR - Visual Logical Evidence
install.packages("naniar")
install.packages("dplyr")
library(naniar)
library(dplyr)
vis_miss(data)




#---------------------------------------------#
#Selection of method for missingness treatment#
#---------------------------------------------#



#-----------------Numeric Data----------------#
str(data)
(colSums(is.na(data))/nrow(data))*100

num_cols <- sapply(data, is.numeric)
num_with_na <- names(data)[num_cols & colSums(is.na(data)) > 0]
num_with_na

plot(density(data$'Age', na.rm = TRUE), col = "blue", lwd = 2)
median_value <- median(data$`Age`, na.rm = TRUE)
data$`Age`[is.na(data$`Age`)] <- median_value

plot(density(data$'Ad_Spend', na.rm = TRUE), col = "blue", lwd = 2)
median_value <- median(data$'Ad_Spend', na.rm = TRUE)
data$'Ad_Spend'[is.na(data$'Ad_Spend')] <- median_value

plot(density(data$'Clicks', na.rm = TRUE), col = "blue", lwd = 2)
median_value <- median(data$'Clicks', na.rm = TRUE)
data$'Clicks'[is.na(data$'Clicks')] <- median_value

plot(density(data$'Conversions', na.rm = TRUE), col = "blue", lwd = 2)
median_value <- median(data$'Conversions', na.rm = TRUE)
data$'Conversions'[is.na(data$'Conversions')] <- median_value

plot(density(data$'Revenue', na.rm = TRUE), col = "blue", lwd = 2)
median_value <- median(data$'Revenue', na.rm = TRUE)
data$'Revenue'[is.na(data$'Revenue')] <- median_value

#---------------Categorical Data--------------#
(colSums(is.na(data))/nrow(data))*100

cat_cols <- sapply(data, is.character)
cat_with_na <- names(data)[cat_cols & colSums(is.na(data)) > 0]
cat_with_na

mode_impute <- function(x) {
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_val
  return(x)
}

data$Gender <- mode_impute(data$Gender)
data$City <- mode_impute(data$City)
data$Campaign_Channel <- mode_impute(data$Campaign_Channel)

data$Doctor_Consulted_missing <- ifelse(is.na(data$Doctor_Consulted), 1, 0)
data$Doctor_Consulted <- mode_impute(data$Doctor_Consulted)




#---------------------------------#
# Outlier detection and treatment #
#---------------------------------#

num_cols <- sapply(data, is.numeric)
num_data <- data[, num_cols]
colnames(num_data)

boxplot(data$Age)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Ad_Spend)
plot(density(data$Ad_Spend, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Clicks)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Conversions)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Revenue)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_0)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_1)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_2)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_3)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_4)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_5)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_6)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_7)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_8)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Feature_9)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Age_missing)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)
boxplot(data$Doctor_Consulted_missing)
plot(density(data$Age, na.rm = TRUE), col = "blue", lwd = 2)

outlier_percent <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  outliers <- sum(x < lower | x > upper, na.rm = TRUE)
  total <- sum(!is.na(x))
  
  percent <- (outliers / total) * 100
  
  return(percent)
}

num_cols <- sapply(data, is.numeric)
num_cols["Patient_ID"] <- FALSE

sapply(data[num_cols], outlier_percent)

cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  return(x)
}

data$Ad_Spend <- cap_outliers(data$Ad_Spend)
data$Revenue <- cap_outliers(data$Revenue)

uneven_num_cols <- sapply(data, is.numeric)
uneven_num_cols[c("Patient_ID", "Age_missing", "Doctor_Consulted_missing")] <- FALSE

data[uneven_num_cols] <- scale(data[uneven_num_cols])
summary(data[uneven_num_cols])