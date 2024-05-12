data = read.csv("/Users/sahirbandali/Downloads/Housing.csv")
names(data)[names(data) == "CRIM"] <- "crime_rate_pcap"
names(data)[names(data) == "ZN"] <- "prop_res_land_over_25k_sqft"
names(data)[names(data) == "INDUS"] <- "prop_non_retail_acres"
names(data)[names(data) == "CHAS"] <- "house_by_river"
names(data)[names(data) == "NOX"] <- "nitric_oxides_ppm"
names(data)[names(data) == "RM"] <- "rooms_per_house"
names(data)[names(data) == "AGE"] <- "prop_units_pre_1940"
names(data)[names(data) == "DIS"] <- "dist_to_5_emp_centres"
names(data)[names(data) == "RAD"] <- "index_access_to_highways"
names(data)[names(data) == "TAX"] <- "tax_rate_per_10k"
names(data)[names(data) == "PTRATIO"] <- "pupil_teacher_ratio"
names(data)[names(data) == "B"] <- "black_prop"
names(data)[names(data) == "LSTAT"] <- "pct_lower_status_pop"
names(data)[names(data) == "MEDV"] <- "home_median_val_1000s"
names(data_outliers)[names(data_outliers) == "log_home_median_val_1000s"] <- "log_home_median_val_10000s"

data= subset(data, select = -c(12))

colnames(data)
data <- na.omit(data)
data$log_home_median_val_1000s <- log(data$home_median_val_1000s)

data_model <- lm(home_median_val_1000s ~ crime_rate_pcap + prop_res_land_over_25k_sqft 
                 + prop_non_retail_acres + house_by_river + nitric_oxides_ppm 
                 + rooms_per_house + prop_units_pre_1940 + dist_to_5_emp_centres 
                 + index_access_to_highways + tax_rate_per_10k + pupil_teacher_ratio 
                 + pct_lower_status_pop + black_prop, data = data)
log_data_model<- lm(log_home_median_val_1000s ~ crime_rate_pcap + prop_res_land_over_25k_sqft 
                    + prop_non_retail_acres + house_by_river + nitric_oxides_ppm 
                    + rooms_per_house + prop_units_pre_1940 + dist_to_5_emp_centres 
                    + index_access_to_highways + tax_rate_per_10k + pupil_teacher_ratio 
                    + pct_lower_status_pop + black_prop, data = data)
summary(data_model)
summary(log_data_model)

# 1. Normal Probability Plot
qqnorm(residuals(data_model), col='dark green', pch = 19, cex=1.7)
qqline(residuals(data_model))

# 2. Studentized residuals vs Index
plot(1:length(residuals(data_model)), rstudent(data_model), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(data_model), residuals(data_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(data_model), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)

#Log
# 1. Normal Probability Plot
qqnorm(residuals(log_data_model), col='dark green', pch = 19, cex=1.7)
qqline(residuals(log_data_model))

# 2. Studentized residuals vs Index
plot(1:length(residuals(log_data_model)), rstudent(log_data_model), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(log_data_model), residuals(log_data_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(log_data_model), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)

studentized_residuals <- rstudent(log_data_model)
outliers <- which(abs(studentized_residuals) > 3.5)

independent_vars <- c("crime_rate_pcap", "prop_res_land_over_25k_sqft", "prop_non_retail_acres", 
                      "house_by_river", "nitric_oxides_ppm", "rooms_per_house", 
                      "prop_units_pre_1940", "dist_to_5_emp_centres", "index_access_to_highways", 
                      "black_prop", "tax_rate_per_10k", "pupil_teacher_ratio", 
                      "pct_lower_status_pop")

boston_housing_data= subset(data, select = -c(14))
# backward Selection using AIC
model.housing.basic<-lm(log_home_median_val_10000s~1,data=boston_housing_data) # Null model
model.housing.full<-lm(log_home_median_val_10000s~.,data=boston_housing_data) # Full model
select.backward.AIC<-stepAIC(model.housing.full,scope=formula(model.housing.full),direction="backward")
select.backward.AIC$anova

#Forward
select.forward.AIC<-stepAIC(model.housing.basic,scope=formula(model.housing.full),direction="forward")
select.forward.AIC$anova

final_boston_model <- lm(log_home_median_val_1000s ~ pct_lower_status_pop 
                         + pupil_teacher_ratio + crime_rate_pcap + rooms_per_house 
                         + dist_to_5_emp_centres + nitric_oxides_ppm  
                         + index_access_to_highways + tax_rate_per_10k 
                         + house_by_river + prop_res_land_over_25k_sqft, data = data)
summary(final_boston_model)

# 1. Normal Probability Plot
qqnorm(residuals(final_boston_model), col='dark green', pch = 19, cex=1.7)
qqline(residuals(final_boston_model))

# 2. Studentized residuals vs Index
plot(1:length(residuals(final_boston_model)), rstudent(final_boston_model), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(final_boston_model), residuals(final_boston_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(final_boston_model), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)

#Outliers
studentized_residuals <- rstudent(final_boston_model)
outliers2 <- which(abs(studentized_residuals) > 3.5)
rstudent(final_boston_model)[outliers2]

Leverage_values <- hatvalues(final_boston_model)
# Identify the two oblack_propservations with the largest Leverage
top_Leverage_indices <- order(Leverage_values, decreasing = TRUE)[1]
top_Leverage_observations <- data[top_Leverage_indices, ]

# Print the identified oblack_propservations
print(top_Leverage_observations)
rstudent(final_boston_model)[top_Leverage_indices]
#381 
#1.574725 

data_outliers <- data[-outliers2,]
outliers_boston_model <- lm(log_home_median_val_1000s ~ pct_lower_status_pop 
                            + pupil_teacher_ratio + crime_rate_pcap 
                            + rooms_per_house + dist_to_5_emp_centres
                            + nitric_oxides_ppm + index_access_to_highways 
                            + tax_rate_per_10k + house_by_river 
                            + prop_res_land_over_25k_sqft, data = data_outliers)
summary(outliers_boston_model4)

# 1. Normal Probability Plot
qqnorm(residuals(outliers_boston_model), col='dark green', pch = 19, cex=1.7)
qqline(residuals(outliers_boston_model))

# 2. Studentized residuals vs Index
plot(1:length(residuals(outliers_boston_model)), rstudent(outliers_boston_model), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(outliers_boston_model), residuals(outliers_boston_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(outliers_boston_model), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)

studentized_residuals <- rstudent(outliers_boston_model3)
outliers <- which(abs(studentized_residuals) > 3.5)
rstudent(outliers_boston_model3)[outliers]

data_outliers4 <- data_outliers3[-outliers,]
outliers_boston_model4 <- lm(log_home_median_val_1000s ~ pct_lower_status_pop + pupil_teacher_ratio + crime_rate_pcap + rooms_per_house + dist_to_5_emp_centres + nitric_oxides_ppm + index_access_to_highways + 
                               tax_rate_per_10k + house_by_river + prop_res_land_over_25k_sqft, data = data_outliers4)
summary(outliers_boston_model3)

# 1. Normal Probability Plot
qqnorm(residuals(outliers_boston_model4), col='dark green', pch = 19, cex=1.7)
qqline(residuals(outliers_boston_model4))

# 2. Studentized residuals vs Index
plot(1:length(residuals(outliers_boston_model4)), rstudent(outliers_boston_model4), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(outliers_boston_model4), residuals(outliers_boston_model4), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(outliers_boston_model4), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#Interactions
data2 = subset(data_outliers, select = -c(3, 7, 14))

# Assuming your data is in a dataframe called 'data'
variables <- c("pct_lower_status_pop", "pupil_teacher_ratio", "crime_rate_pcap", "rooms_per_house", "dist_to_5_emp_centres", "nitric_oxides_ppm", "index_access_to_highways", "tax_rate_per_10k", "prop_res_land_over_25k_sqft")

for (var in variables) {
  data_outliers[paste(var, "house_by_river", sep="_")] <- data_outliers[var] * data$house_by_river
}

test_boston_model <- lm(log_home_median_val_10000s ~ pct_lower_status_pop 
                        + pupil_teacher_ratio + crime_rate_pcap + rooms_per_house 
                        + dist_to_5_emp_centres + nitric_oxides_ppm 
                        + index_access_to_highways + tax_rate_per_10k 
                        + house_by_river + prop_res_land_over_25k_sqft 
                        + rooms_per_house_house_by_river, data = data_outliers)
summary(test_boston_model)

studentized_residuals <- rstudent(test_boston_model)
outliers <- which(abs(studentized_residuals) > 3.5)
rstudent(test_boston_model)[outliers]

Leverage_values <- hatvalues(test_boston_model)
# Identify the two observations with the largest Leverage
top_Leverage_indices <- order(Leverage_values, decreasing = TRUE)[1]
top_Leverage_observations <- data3[top_Leverage_indices, ]

# Print the identified oblack_propservations
print(top_Leverage_observations)
rstudent(test_boston_model)[top_Leverage_indices]
data6 <- data5[-top_Leverage_indices,]

#rooms_per_house ., crime_rate_cap * 
# 1. Normal Probability Plot
qqnorm(residuals(test_boston_model), col='dark green', pch = 19, cex=1.7)
qqline(residuals(test_boston_model))

# 2. Studentized residuals vs Index
plot(1:length(residuals(test_boston_model)), rstudent(test_boston_model), 
     xlab = "Index", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Index", col='dark green', pch = 19, cex=1.7)

# 3. Residuals vs Fitted values
plot(fitted(test_boston_model), residuals(test_boston_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col='dark green', pch = 19, cex=1.7)

# 4. Leverage vs Index
plot(hatvalues(test_boston_model), 
     xlab = "Index", ylab = "Leverage",
     main = "Leverage vs Index", col='dark green', pch = 19, cex=1.7)