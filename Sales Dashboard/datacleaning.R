install.packages("readxl") #package installation    
library(readxl) #calling library

data <- read_excel("E:/SBUP/SEM-2/EXTRA/Personal Project/Sales Dashboard/dataset.xlsx", 
                   sheet = "Sheet1", na = c("", "NA", "-", "null")) 
      #reading excel file
      #sheet1 has been selected
      #missing values are unified in 'NA'          

head(data) #first 100 rows
str(data) #data type
summary(data) # summarize

#MISSING VALUE TREATMENT
(colSums(is.na(data))/5600)*100 #missing value in %


raw_data <- data #copying data in temp location

install.packages("dplyr")
library(dplyr)

data <- data[, !(names(data) %in% c("Delivered Quantity Set/Pcs",
                                    "Product Category 3"))]   #column deletion


cols <- c("Product Category 1", "Product Category 2", "Product Main","Customer Category", 
          "Unit Cabinet Category", "Cluster","Product All")

for (col in cols) {
  mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_val  #mode imputation
}

plot(density(data$'Cluster unique', na.rm = TRUE), col = "blue", lwd = 2)

median_value <- median(data$`Cluster unique`, na.rm = TRUE)
data$`Cluster unique`[is.na(data$`Cluster unique`)] <- median_value

getwd()

install.packages("writexl")   # only once
library(writexl)
write_xlsx(data, "cleaned_data.xlsx")
