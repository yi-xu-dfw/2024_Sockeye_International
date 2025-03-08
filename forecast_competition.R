rm(list = ls())
setwd("C:/Work/Sockeye_International_Data_April25/")

library(tidyverse)

# Function to find N years with the most similar escapement numbers
find_nearest_years <- function(data, Brood.Year,N) {
  
  data_use <- data[data$Brood.Year<Brood.Year,] # remove data more than the brood year, only use historical data not future data
  idx <- which(data$Brood.Year==Brood.Year)
  target_esc <- data$Escapement[idx]
  
  # find the abs of the difference
  esc_diff <- abs(data_use$Escapement-target_esc)
  esc_order <- order(esc_diff) # order them
  
  closestN <- rbind(data_use[esc_order[1:N],],data[idx,])
  
  return(closestN)
}

Forecast.Year <- 2024

# read data in
raw_data <- read_csv("combined_brood.csv") 
# read age conversion table
age_table <- read_csv("age_conversion.csv")

age_table <- age_table |>
             mutate(Brood.Year = Forecast.Year - TotalAge)

stocks <- unique(raw_data$stock)

for(istock in 1:length(stocks)) {

stock <- raw_data |>
                filter(stock == stocks[istock] & Brood.Year > 1990)

# find the Brood years to forecast (only forecast dominate ages))
target_age <- stock |>
               pivot_longer(cols = X0.1:X3.4, 
                            values_to = "recruit", names_to = "ages") |>
               rename(total_recruits = Recruits) |>
               mutate(perc = recruit/total_recruits)  |>
               filter(recruit >= 1 & perc > 0.05) |># at least 5 years that are more than 5%
               count(ages) |>
               filter(n>5) |>
               left_join(age_table, by = c("ages"="EUAgeName")) |>
               select(Age = ages, Brood.Year) |>
               mutate(Forecast = NA)

 brief <- stock |>
               select(Brood.Year,target_age$Age,Escapement,Recruits)
 
 # methodology: find two nearest neighbor years, then do the average for target age, need to calculate for different age
 
 # Step 1 find nearest two years based on escapement
 N <- 2  # Set the number of years to find
 s = 0 # sum of all forecast
 
 forecast <- data.frame(Brood.Year = unique(target_age$Brood.Year), 
                        Forecast = NA)
 
 for (iyear in unique(target_age$Brood.Year)){

 # Apply the function to your data
 nearest_years <- find_nearest_years(brief, iyear, N)
 
 # Step 2. average the N years of target age, then predict using the same R/S rate
 idx_age <- target_age$Age[target_age$Brood.Year == iyear] # find the age

 total_recruit <- sum(nearest_years[1:N,idx_age])
 total_esc <- sum(nearest_years$Escapement[1:N])
 forecast_age <- nearest_years$Escapement[N+1]*total_recruit/total_esc
 forecast$Forecast[forecast$Brood.Year==iyear] <- forecast_age
 s = s + forecast_age
 
 }
 #print(forecast)
 print(paste0(stocks[istock]," forecast is ",round(s)))
 
 }
