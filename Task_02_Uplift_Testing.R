---------------------------------
  Task - 2 
---------------------------------
  Step - 1: Monthly Aggregation
---------------------------------
  #### Load libraries and data
  library(data.table)
library(ggplot2)
library(tidyr)

filePath <- "/Users/saimhussain/Downloads/"
data <- fread(paste0(filePath, "QVI_data.csv"))

head(data)

#### Create Monthly Metrics 
# Create YEARMONTH column
data[, YEARMONTH := format(DATE, "%Y%m")]

# Calculate metrics per store per month
measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTransactions = .N,
  nChips = sum(PROD_QTY),
  avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)
), by = .(STORE_NBR, YEARMONTH)][order(STORE_NBR, YEARMONTH)]

#### Filter for stores with full 12 months of data [cite: 42]
storesWithFullObs <- measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR]
measureOverTime <- measureOverTime[STORE_NBR %in% storesWithFullObs]

# Define Pre-Trial Period
preTrialMeasures <- measureOverTime[YEARMONTH < "201902"]
--------------------------------------
  Step - 2: Selecting the Control Store (Function)
--------------------------------------
  #### Function to calculate Correlation and Magnitude [cite: 65, 84]
  calculateControlScore <- function(trial_store, metric_col) {
    
    trial_data <- preTrialMeasures[STORE_NBR == trial_store, get(metric_col)]
    
    # Calculate Correlation
    calc_corr <- preTrialMeasures[STORE_NBR != trial_store, .(
      correlation = cor(trial_data, get(metric_col))
    ), by = STORE_NBR]
    
    # Calculate Magnitude Distance
    calc_dist <- preTrialMeasures[STORE_NBR != trial_store, .(
      dist = mean(abs(trial_data - get(metric_col)))
    ), by = STORE_NBR]
    
    # Standardize distance (1 is closest, 0 is furthest)
    calc_dist[, dist_score := 1 - (dist - min(dist)) / (max(dist) - min(dist))]
    
    # Final Score: Average of Correlation and Distance
    final_score <- merge(calc_corr, calc_dist, by = "STORE_NBR")
    final_score[, score := (correlation + dist_score) / 2]
    
    return(final_score[order(-score)])
  }

#### Example: Find Control for Store 77
# Based on the template, Store 233 is often the best match for 77 [cite: 98]
control_77 <- calculateControlScore(77, "totSales")
print(head(control_77))
------------------------------------
  Step - 3: Assessment of All Trial Stores
------------------------------------
  #### Define Trial Stores and their matched Controls 
  trial_stores <- c(77, 86, 88)
control_stores <- c(233, 155, 237)

#### Loop through each pair to calculate Uplift 
for (i in 1:3) {
  trial <- trial_stores[i]
  control <- control_stores[i]
  
  # 1. Calculate Scaling Factor (Pre-trial Sales Ratio) 
  scalingFactor <- preTrialMeasures[STORE_NBR == trial, sum(totSales)] / 
    preTrialMeasures[STORE_NBR == control, sum(totSales)]
  
  # 2. Apply Scaling to Control Store 
  scaledControlSales <- measureOverTime[STORE_NBR == control]
  scaledControlSales[, controlSales := totSales * scalingFactor]
  
  # 3. Calculate Percentage Difference 
  percentageDiff <- merge(measureOverTime[STORE_NBR == trial, .(YEARMONTH, totSales)],
                          scaledControlSales[, .(YEARMONTH, controlSales)], 
                          by = "YEARMONTH")
  percentageDiff[, diff := (totSales - controlSales) / controlSales]
  
  # 4. T-Test for Significance (Trial months: Feb, Mar, Apr) 
  stdDev <- sd(percentageDiff[YEARMONTH < "201902", diff])
  percentageDiff[, tValue := diff / stdDev]
  
  print(paste("Results for Trial Store", trial, "vs Control Store", control))
  print(percentageDiff[YEARMONTH %in% c("201902", "201903", "201904"), .(YEARMONTH, totSales, controlSales, tValue)])
}
-----------------------------
  Visualizing the Results
-----------------------------
  #### Example Visualization for Store 77 
  # Combine trial and scaled control data for plotting
  plotData <- merge(measureOverTime[STORE_NBR == 77, .(YEARMONTH, totSales, Store_type = "Trial")],
                    scaledControlSales[, .(YEARMONTH, totSales = controlSales, Store_type = "Control")],
                    by = c("YEARMONTH", "totSales", "Store_type"), all = TRUE)

ggplot(plotData, aes(x = YEARMONTH, y = totSales, color = Store_type, group = Store_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Sales: Store 77 (Trial) vs Store 233 (Control)",
       y = "Monthly Sales", x = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
------------------------------
  #### Example Visualization for Store 86 
  # Combine trial and scaled control data for plotting
  plotData <- merge(measureOverTime[STORE_NBR == 86, .(YEARMONTH, totSales, Store_type = "Trial")],
                    scaledControlSales[, .(YEARMONTH, totSales = controlSales, Store_type = "Control")],
                    by = c("YEARMONTH", "totSales", "Store_type"), all = TRUE)

ggplot(plotData, aes(x = YEARMONTH, y = totSales, color = Store_type, group = Store_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Sales: Store 86 (Trial) vs Store 155 (Control)",
       y = "Monthly Sales", x = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
--------------------------------
  #### Example Visualization for Store 88 
  # Combine trial and scaled control data for plotting
  plotData <- merge(measureOverTime[STORE_NBR == 88, .(YEARMONTH, totSales, Store_type = "Trial")],
                    scaledControlSales[, .(YEARMONTH, totSales = controlSales, Store_type = "Control")],
                    by = c("YEARMONTH", "totSales", "Store_type"), all = TRUE)

ggplot(plotData, aes(x = YEARMONTH, y = totSales, color = Store_type, group = Store_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Sales: Store 88 (Trial) vs Store 237 (Control)",
       y = "Monthly Sales", x = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
