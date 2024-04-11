# Part 1: importing libraries and data

if(!require("readxl")) install.packages("readxl")
library(haven)
library(dplyr)
library(tidyr)
if(!require("lubridate")) install.packages("lubridate")
library(ggplot2)
if (!require("countrycode")) {install.packages("countrycode")}
library(countrycode)

# Set the default working directory
setwd('C:/Users/Source/OneDrive/Bureau/R Project/')

# Read SAS files
userdata <- read_sas('RawDataIIUserDailyAggregation.sas7bdat')
PokerStats <- read_sas('RawDataIIIPokerChipConversions.sas7bdat')
demographics <- read_sas('RawDataIDemographics.sas7bdat')

# Read Excel files
countries <- read_excel(path = 'countries.xlsx')
languages <- read_excel(path = 'languages.xlsx')
applications <- read_excel(path = 'applications.xlsx')
products <- read_excel(path = 'products.xlsx')

# Merge data frames based on ID columns
demographics <- demographics %>%
  left_join(countries, by = "Country") %>%
  left_join(languages, by = "Language") %>%
  left_join(applications, by = "ApplicationID")

demographics <- demographics %>%
  select(-Country, -Language, -ApplicationID)

# Update ambiguous cases directly in the 'CountryName' column
demographics$CountryName[demographics$CountryName == "Moldavia"] <- "Moldova"
demographics$CountryName[demographics$CountryName == "Serbia and Montenegro"] <- "Serbia"
demographics$CountryName[demographics$CountryName == "Tunesia"] <- "Tunisia"

# Create a new column 'Continent' based on the country names
demographics$Continent <- countrycode(
  sourcevar = demographics$CountryName,
  origin = "country.name",
  destination = "continent",
  warn = TRUE  # Display warnings for ambiguous cases
)

demographics$Continent[demographics$CountryName == "Diego Garcia"] <- "Asia"


userdata <- userdata %>% left_join(products, by = "ProductID") %>% select(-ProductID)


options(digits=2)

# Part 2: Data summary and cleaning 

# 1: Inspecting the datasets

# using the following code we got the Data summary
str(userdata)
head(userdata)
str(PokerStats)
head(PokerStats)
str(demographics)
head(demographics)

# Missing values detection
print(sapply(userdata, function(x) sum(is.na(x))))
print(sapply(PokerStats, function(x) sum(is.na(x))))
print(sapply(demographics, function(x) sum(is.na(x))))

# Replacing the missing gender with the mode
demographics$Gender[is.na(demographics$Gender)] <- 
  names(sort(table(demographics$Gender), decreasing = TRUE))[1]

# Removing duplicates
userdata <- unique(userdata)
PokerStats <- unique(PokerStats)
demographics <- unique(demographics)

# Data transformation and correction 
PokerStats$TransType <- ifelse(PokerStats$TransType == 24, "Buy", "Sell")
demographics$Gender <- ifelse(demographics$Gender == 0, "Female", "Male")


userdata$Date <- as.Date(userdata$Date, format = "%Y%m%d")
demographics$RegDate <- as.Date(demographics$RegDate)
demographics$FirstPay <- as.Date(demographics$FirstPay, format = "%Y%m%d")
demographics$FirstAct <- as.Date(demographics$FirstAct, format = "%Y%m%d")
demographics$FirstSp <- as.Date(demographics$FirstSp, format = "%Y%m%d")
demographics$FirstCa <- as.Date(demographics$FirstCa, format = "%Y%m%d")
demographics$FirstGa <- as.Date(demographics$FirstGa, format = "%Y%m%d")
demographics$FirstPo <- as.Date(demographics$FirstPo, format = "%Y%m%d")

PokerStats$TransDateTime <- ymd_hms(PokerStats$TransDateTime)
PokerStats$TransDate <- date(PokerStats$TransDateTime)
PokerStats$TransMonth <- month(PokerStats$TransDateTime)
PokerStats$TransDay <- day(PokerStats$TransDateTime)
PokerStats$TransHour <- hour(PokerStats$TransDateTime)

# Outliers detection
boxplot(PokerStats$TransAmount)
hist(PokerStats$TransAmount)
boxplot(userdata$Stakes)
boxplot(userdata$Winnings)
boxplot(userdata$Bets)

# Part 3: Creating new indicators

# For Demographics Table

demographics <- demographics %>%
  mutate(FirstPurchaseDuration = difftime(FirstPay, RegDate, units = "days"),  # First Purchase Duration: Time taken from registration to the first payment
         TimeToEngage = difftime(FirstAct, RegDate, units = "days"),  # Time to Engage: Duration from registration to the first active play
         ConversionTime = difftime(FirstPay, FirstAct, units = "days"))  # Conversion Time: Time from the first active play date to the first paying date

# For UserDailyAggregation Table

# Calculating user metrics and aggregates
usermetrics <- userdata %>%
  group_by(UserID) %>%
  summarise(
    FirstTransaction = min(Date),
    LastTransaction = max(Date),
    total_winnings = sum(Winnings),
    total_stakes = sum(Stakes),
    total_bets = sum(Bets),
    max_winnings = max(Winnings),
    max_stakes = max(Stakes),
    max_bets = max(Bets),
    min_winnings = min(Winnings),
    min_stakes = min(Stakes),
    min_bets = min(Bets),
    bets_frequency = n(),
    winning_rate = total_winnings / total_stakes,
    average_stake_per_bet = total_stakes / total_bets,
    profit_or_loss_per_bet = total_winnings - (total_stakes / total_bets),
    CustomerLifespan = difftime(LastTransaction, FirstTransaction, units = "days")
  ) %>%
  ungroup()

# Favorite product
product_counts <- userdata %>%
  group_by(UserID, ProductDescription) %>%
  summarise(product_count = n()) %>%
  slice_max(product_count, with_ties = FALSE) %>%
  ungroup() %>%
  rename(favorite_product = ProductDescription) %>%
  select(UserID, favorite_product)

# Join the result with usermetrics
usermetrics <- usermetrics %>%
  left_join(product_counts, by = "UserID")

# Calculating average daily/weekly/monthly bets/stakes/winnings for each user
# Daily, Weekly, and Monthly bets
avg_bets <- userdata %>%
  group_by(UserID, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_bets_in_day = sum(Bets),
    total_bets_in_week = sum(Bets),
    total_bets_in_month = sum(Bets)
  ) %>%
  group_by(UserID) %>%
  summarise(
    avg_daily_bets = mean(total_bets_in_day),
    avg_weekly_bets = mean(total_bets_in_week),
    avg_monthly_bets = mean(total_bets_in_month)
  )

# Daily, Weekly, and Monthly stakes
avg_stakes <- userdata %>%
  group_by(UserID, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_stakes_in_day = sum(Stakes),
    total_stakes_in_week = sum(Stakes),
    total_stakes_in_month = sum(Stakes)
  ) %>%
  group_by(UserID) %>%
  summarise(
    avg_daily_stakes = mean(total_stakes_in_day),
    avg_weekly_stakes = mean(total_stakes_in_week),
    avg_monthly_stakes = mean(total_stakes_in_month)
  )

# Daily, Weekly, and Monthly winnings
avg_winnings <- userdata %>%
  group_by(UserID, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_winnings_in_day = sum(Winnings),
    total_winnings_in_week = sum(Winnings),
    total_winnings_in_month = sum(Winnings)
  ) %>%
  group_by(UserID) %>%
  summarise(
    avg_daily_winnings = mean(total_winnings_in_day),
    avg_weekly_winnings = mean(total_winnings_in_week),
    avg_monthly_winnings = mean(total_winnings_in_month)
  )

# Daily, Weekly, and Monthly bets
avg_bets_product <- userdata %>%
  group_by(UserID, ProductDescription, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_bets_in_day = sum(Bets),
    total_bets_in_week = sum(Bets),
    total_bets_in_month = sum(Bets)
  ) %>%
  group_by(UserID, ProductDescription) %>%
  summarise(
    avg_daily_bets = mean(total_bets_in_day),
    avg_weekly_bets = mean(total_bets_in_week),
    avg_monthly_bets = mean(total_bets_in_month)
  ) %>%
  pivot_wider(
    id_cols = c(UserID),
    names_from = ProductDescription,
    values_from = c(avg_daily_bets, avg_weekly_bets, avg_monthly_bets)
  )

# Daily, Weekly, and Monthly stakes
avg_stakes_product <- userdata %>%
  group_by(UserID, ProductDescription, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_stakes_in_day = sum(Stakes),
    total_stakes_in_week = sum(Stakes),
    total_stakes_in_month = sum(Stakes)
  ) %>%
  group_by(UserID, ProductDescription) %>%
  summarise(
    avg_daily_stakes = mean(total_stakes_in_day),
    avg_weekly_stakes = mean(total_stakes_in_week),
    avg_monthly_stakes = mean(total_stakes_in_month)
  ) %>%
  pivot_wider(
    id_cols = c(UserID),
    names_from = ProductDescription,
    values_from = c(avg_daily_stakes, avg_weekly_stakes, avg_monthly_stakes)
  )

# Daily, Weekly, and Monthly winnings
avg_winnings_product <- userdata %>%
  group_by(UserID, ProductDescription, Day = as.Date(Date), Week = format(Date, "%Y-%U"), Month = format(Date, "%Y-%m")) %>%
  summarise(
    total_winnings_in_day = sum(Winnings),
    total_winnings_in_week = sum(Winnings),
    total_winnings_in_month = sum(Winnings)
  ) %>%
  group_by(UserID, ProductDescription) %>%
  summarise(
    avg_daily_winnings = mean(total_winnings_in_day),
    avg_weekly_winnings = mean(total_winnings_in_week),
    avg_monthly_winnings = mean(total_winnings_in_month)
  ) %>%
  pivot_wider(
    id_cols = c(UserID),
    names_from = ProductDescription,
    values_from = c(avg_daily_winnings, avg_weekly_winnings, avg_monthly_winnings)
  )

# Merge the three tables
merged_averages <- avg_bets_product %>%
  left_join(avg_stakes_product, by = "UserID") %>%
  left_join(avg_winnings_product, by = "UserID")

# Calculating aggregates based on UserID and ProductDescription

products <- userdata %>%
  group_by(UserID, ProductDescription) %>%
  summarise(
    tot_stakes = sum(Stakes),
    tot_winnings = sum(Winnings),
    tot_bets = sum(Bets),
    max_winnings_prod = max(Winnings),
    max_stakes_prod = max(Stakes),
    max_bets_prod = max(Bets),
    min_winnings_prod = min(Winnings),
    min_stakes_prod = min(Stakes),
    min_bets_prod = min(Bets),
    frequency = n(),
    winning_rate_prod = tot_winnings / tot_stakes,
    average_stake = tot_stakes / tot_bets,
    profit_or_loss_per_bet_prod = tot_winnings - (tot_stakes / tot_bets),
    FirstDate= min(Date),
    LastDate = max(Date)
  ) %>%
  pivot_wider(
    id_cols = UserID,
    names_from = ProductDescription,
    values_from = c("tot_stakes", "tot_winnings", "tot_bets", 
                    "max_winnings_prod", "max_stakes_prod", "max_bets_prod",
                    "min_winnings_prod", "min_stakes_prod", "min_bets_prod",
                    "frequency", "winning_rate_prod", 
                    "average_stake", "profit_or_loss_per_bet_prod","FirstDate","LastDate")
  )

# Merging metrics 
usermetrics <- merge(usermetrics, products, by = "UserID", all.x = TRUE)
usermetrics <- merge(usermetrics, merged_averages, by = "UserID", all.x = TRUE)

# Handling missing values in the products data
products %>%
  mutate(across(starts_with("total_stakes_"), ~ifelse(is.na(.), 0, .))) -> products

products %>%
  mutate(across(starts_with("total_bets_"), ~ifelse(is.na(.), 0, .))) -> products

products %>%
  mutate(across(starts_with("total_winnings_"), ~ifelse(is.na(.), -1, .))) -> products

# Checking for missing values in the updated product dataset
print(sapply(products, function(x) sum(is.na(x))))


# For PokerChipConversion Table

poker_metrics <- PokerStats %>% 
  group_by(UserID, TransType) %>% 
  summarise(
    FirstPokerDate = min(TransDate), 
    LastPokerDate = max(TransDate),
    TotalPokerAmount = sum(TransAmount),
    AvgPokerAmount = mean(TransAmount),
    MaxPokerAmount= max(TransAmount),
    MinPokerAmount = min(TransAmount),
    poker_count = n(),
    ActiveDays = ifelse(LastPokerDate == FirstPokerDate, 1, as.numeric(difftime(LastPokerDate, FirstPokerDate, units = "days")))
  ) %>% pivot_wider(
    id_cols = UserID,
    names_from = TransType,
    values_from = c(FirstPokerDate, LastPokerDate, TotalPokerAmount, AvgPokerAmount, MaxPokerAmount, MinPokerAmount, poker_count, ActiveDays)
  )


# Calculating the buy percentage and join with transaction_count

poker_metrics$Poker_Balance <- poker_metrics$TotalPokerAmount_Buy - poker_metrics$TotalPokerAmount_Sell

# Transactions per period

Trans_period <- PokerStats %>%
  mutate(  TransactionPeriod = case_when(
           between(TransHour, 6, 11) ~ "Morning",
           between(TransHour, 12, 17) ~ "Afternoon",
           TRUE ~ "Night"
         ))


# Pivot for TransactionPeriod
pivoted_period <- Trans_period %>%
  group_by(UserID, TransType, TransactionPeriod) %>%
  summarise(NumTransactions = n(),
            TotalAmount = sum(TransAmount)) %>%
  pivot_wider(
    id_cols = c(UserID, TransType),
    names_from = TransactionPeriod,
    values_from = c(NumTransactions, TotalAmount)
  )

pivoted_type <- pivoted_period %>%
  pivot_wider(
    id_cols = c(UserID),
    names_from = TransType,
    values_from = -c(UserID, TransType)
  )

poker_metrics <- poker_metrics %>%
  left_join(pivoted_type, by = "UserID")


# Part 4: Merging the data

# Merging with a left join on 'UserID' to keep all UserIDs in demographics and create NAs in the other tables
MarketingDataMart <- merge(demographics, usermetrics, by = "UserID", all.x = TRUE)
MarketingDataMart <- merge(MarketingDataMart, avg_bets, by = "UserID", all.x = TRUE)
MarketingDataMart <- merge(MarketingDataMart, avg_stakes, by = "UserID", all.x = TRUE)
MarketingDataMart <- merge(MarketingDataMart, avg_winnings, by = "UserID", all.x = TRUE)
MarketingDataMart <- merge(MarketingDataMart, poker_metrics, by = "UserID", all.x = TRUE)

MarketingDataMart$total_profit <- (
  replace(MarketingDataMart$total_winnings, is.na(MarketingDataMart$total_winnings), 0) + 
    replace(MarketingDataMart$TotalPokerAmount_Buy, is.na(MarketingDataMart$TotalPokerAmount_Buy), 0)
) - (
  replace(MarketingDataMart$total_stakes, is.na(MarketingDataMart$total_stakes), 0) + 
    replace(MarketingDataMart$TotalPokerAmount_Sell, is.na(MarketingDataMart$TotalPokerAmount_Sell), 0)
)


MarketingDataMart <- MarketingDataMart %>% mutate_at(vars(-Poker_Balance), ~ifelse(is.na(.), 0, .))

write.csv(MarketingDataMart, file = "C:/Users/Source/OneDrive/Bureau/R Project/MarketingDataMart.csv", row.names = FALSE)

# Part 5: Visualizations

#plot1: Gender distribution 
ggplot(MarketingDataMart, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

#plot2: frequency of each product:
cols_to_impute <- c("frequency_1", "frequency_2", "frequency_4", "frequency_5", "frequency_6", "frequency_7", "frequency_8")
MarketingDataMart[cols_to_impute] <- lapply(MarketingDataMart[cols_to_impute], function(x) replace(x, is.na(x), 0))
frequencies <- sapply(MarketingDataMart[cols_to_impute], sum)
product_frequencies <- data.frame(
  Product = factor(c("Product 1", "Product 2", "Product 4", "Product 5", "Product 6", "Product 7", "Product 8"), 
                   levels = c("Product 1", "Product 2", "Product 4", "Product 5", "Product 6", "Product 7", "Product 8")),
  Frequency = frequencies)
colors <- c("red", "green", "blue", "orange", "purple", "brown", "pink")
ggplot(product_frequencies, aes(x = Product, y = Frequency, fill = Product)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.3, color = "black") +
  scale_fill_manual(values = setNames(colors, levels(product_frequencies$Product))) +
  labs(title = "Product Frequencies", x = "Product", y = "Frequency")


#Plot 3: Segmentation by country:
top_countries <- MarketingDataMart %>%
  group_by(CountryName) %>%
  summarize(NumberOfUsers = n()) %>%
  arrange(desc(NumberOfUsers)) %>%
  top_n(10, NumberOfUsers)

# Plotting the number of users for the top 10 countries
ggplot(top_countries, aes(x = reorder(CountryName, NumberOfUsers), y = NumberOfUsers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(title = "Top 10 Countries by Number of Users", x = "Country", y = "Number of Users")


#Plot 4: Seeing the distributuion with regards to the application I

# Group by ApplicationID, count the number of users, and get the top 8 applications
top_apps <- MarketingDataMart %>%
  group_by(ApplicationDescription) %>%
  summarize(NumberOfUsers = n()) %>%
  arrange(desc(NumberOfUsers)) %>%
  top_n(5, NumberOfUsers)

# Convert ApplicationID to a factor
top_apps$ApplicationDescription <- as.factor(top_apps$ApplicationDescription)

# Define pastel colors
pastel_colors <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC")

# Plotting the number of users for the top 5 applications with pastel colors
ggplot(top_apps, aes(x = reorder(ApplicationDescription, NumberOfUsers), y = NumberOfUsers, fill = ApplicationDescription)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pastel_colors) +
  geom_text(aes(label = NumberOfUsers), vjust = -0.3, color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 5 Applications by Number of Users", x = "Application ID", y = "Number of Users")


#plot 5: Map fir the favorite product in each country

#PLOT 7: total stakes vs total winning:
data_long <- MarketingDataMart %>%
  select(tot_stakes_1, tot_winnings_1, tot_stakes_2, tot_winnings_2, 
         tot_stakes_4, tot_winnings_4, tot_stakes_5, tot_winnings_5, 
         tot_stakes_6, tot_winnings_6, tot_stakes_7, tot_winnings_7, 
         tot_stakes_8, tot_winnings_8) %>%
  pivot_longer(cols = everything(), 
               names_to = "TransactionType", 
               values_to = "Amount") %>%
  mutate(Category = gsub("\\D+", "", TransactionType), # Extract category number
         Transaction = if_else(grepl("stakes", TransactionType), "Stakes", "Winnings"))

# Step 2: Create the grouped bar plot
ggplot(data_long, aes(x = Category, y = Amount, fill = Transaction)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Stakes" = "blue", "Winnings" = "red")) +
  theme_minimal() +
  labs(x = "Category", y = "Amount", title = "Stakes and Winnings by Category") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for readability

#PLOT 6: Poker Buy and Sell Transaction for each time of the day
MarketingDataMart %>%
  gather(key = "TransactionType", value = "Count", NumTransactions_Morning_Buy, NumTransactions_Afternoon_Buy, NumTransactions_Night_Buy, NumTransactions_Morning_Sell, NumTransactions_Afternoon_Sell, NumTransactions_Night_Sell) %>%
  mutate(TimeOfDay = case_when(
    grepl("Morning", TransactionType) ~ "Morning",
    grepl("Afternoon", TransactionType) ~ "Afternoon",
    grepl("Night", TransactionType) ~ "Night"
  ),
  TransactionAction = if_else(grepl("Buy", TransactionType), "Buy", "Sell")) %>%
  ggplot(aes(x = TimeOfDay, y = Count, fill = TransactionAction)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Buy" = "blue", "Sell" = "red")) +
  labs(title = "Poker Transactions Count by Time of Day", x = "Time of Day", y = "Transactions Count") +
  theme_minimal()

#PLOT 8: poker count of sell and buy 
ggplot() +
  geom_bar(data = MarketingDataMart, aes(x = factor(1), y = poker_count_Buy, fill = "Buy"), stat = "identity") +
  geom_bar(data = MarketingDataMart, aes(x = factor(2), y = poker_count_Sell, fill = "Sell"), stat = "identity") +
  scale_fill_manual(values = c("Buy" = "blue", "Sell" = "red")) +
  labs(title = "Transaction Count Comparison", x = "", y = "Count") +
  theme_minimal()

#PLOT 9: Scatter plot for days active vs transaction amount

combined_data <- MarketingDataMart %>%
  filter(TotalPokerAmount_Buy <= 100000, TotalPokerAmount_Sell <= 100000) %>%
  mutate(TransactionType = "Buy") %>%
  select(TransactionType, ActiveDays = ActiveDays_Buy, TotalAmount = TotalPokerAmount_Buy) %>%
  bind_rows(
    MarketingDataMart %>%
      filter(TotalPokerAmount_Buy <= 100000, TotalPokerAmount_Sell <= 100000) %>%
      mutate(TransactionType = "Sell") %>%
      select(TransactionType, ActiveDays = ActiveDays_Sell, TotalAmount = TotalPokerAmount_Sell)
  )

# Creating the combined scatter plot
ggplot(combined_data, aes(x = ActiveDays, y = TotalAmount, color = TransactionType)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Buy" = "red", "Sell" = "blue")) +
  labs(title = "Combined Buy and Sell Transactions: Active Days vs Total Amount (Capped at 100000)",
       x = "Active Days",
       y = "Total Transaction Amount",
       color = "Transaction Type") +
  theme_minimal()






