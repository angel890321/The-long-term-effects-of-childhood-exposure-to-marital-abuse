library(dplyr)
trauma <- read.csv("C:/Users/映均/視覺化分析/trauma(總分).csv", header=TRUE, fileEncoding = "UTF-8-BOM")


View(trauma)
library(dplyr)
sex1_data <- subset(trauma,sex=="男")
mean(sex1_data$passive_attitude_score,na.rm = TRUE)# 5.40
mean(sex1_data$physical_and_mental_symptom_score,na.rm = TRUE)# 13.24
mean(sex1_data$repeated_experience_score,na.rm = TRUE)# 7.43

sex2_data <- subset(trauma,sex=="女")
mean(sex2_data$passive_attitude_score,na.rm = TRUE)# 4.74
mean(sex2_data$physical_and_mental_symptom_score,na.rm = TRUE)# 12.95
mean(sex2_data$repeated_experience_score,na.rm = TRUE)#  7.68

