#### Load required libraries
library(data.table) # Efficient data manipulation [cite: 49]
library(ggplot2)    # Main visualization tool 
library(readr)      # For parsing numbers from strings [cite: 52]

#### 1. Load Datasets
# Setting the file path to your Downloads folder
filePath <- "/Users/saimhussain/Downloads/" 
transactionData <- fread(paste0(filePath, "QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath, "QVI_purchase_behaviour.csv"))

#### 2. Examine and Clean Transaction Data
# Convert DATE column: Excel dates begin on 30 Dec 1899 [cite: 74, 75]
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# Remove Salsa products (we only want chips) [cite: 97, 101]
# Note: changed 'lower' to 'tolower' for standard R syntax
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

# Remove the outlier: Customer buying 200 packets [cite: 107, 120]
# Filtering out the specific loyalty card number associated with the bulk purchase [cite: 122]
transactionData <- transactionData[PROD_QTY < 100]

#### 3. Feature Engineering
# Extract Pack Size from product names [cite: 158]
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

# Extract Brand Name (using the first word) [cite: 167, 171]
transactionData[, BRAND := toupper(sub(" .*", "", PROD_NAME))]

# Clean Brand Names for consistency [cite: 175, 177]
transactionData[BRAND == "RED", BRAND := "RRD"] # Red Rock Deli [cite: 175, 178]
transactionData[BRAND == "SNCCB", BRAND := "SUNBITES"] 
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]

#### 4. Merge Data
# Merge transaction data with customer segments using a left join [cite: 189, 191]
data <- merge(transactionData, customerData, all.x = TRUE, by = "LYLTY_CARD_NBR")

#### 5. Data Analysis & Visualizations (Using ggplot2)
# Calculate Total Sales by LIFESTAGE and PREMIUM_CUSTOMER [cite: 210, 212]
sales_summary <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Create the Sales Plot using ggplot2 bar charts [cite: 213]
ggplot(sales_summary, aes(x = LIFESTAGE, weight = SALES, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = "dodge") +
  coord_flip() + # Horizontal bars are easier to read for long lifestage names
  labs(title = "Total Sales by Customer Segment", 
       subtitle = "Analysis for Chip Category Strategy",
       x = "Lifestage", 
       y = "Total Sales ($)") +
  theme_minimal()

#### 6. Statistical Testing (T-Test)
# Testing if Mainstream Young/Midage spend more per unit than others [cite: 241, 243]
mainstream_unit_price <- data[PREMIUM_CUSTOMER == "Mainstream" & 
                                (LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES"), 
                              TOT_SALES / PROD_QTY]

non_mainstream_unit_price <- data[!(PREMIUM_CUSTOMER == "Mainstream" & 
                                      (LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES")), 
                                  TOT_SALES / PROD_QTY]

# Perform independent t-test [cite: 241]
t_test_results <- t.test(mainstream_unit_price, non_mainstream_unit_price, alternative = "greater")
print(t_test_results)

#### 7. Final Step: Save Cleaned Data
# Save the dataset as a CSV for Task 2 analysis [cite: 198, 200]
fwrite(data, paste0(filePath, "QVI_data.csv"))
