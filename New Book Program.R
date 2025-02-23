library(tidyverse)
install.packages("lubridate")
library(lubridate)
book <- read.csv("sales2019.csv")

#Understanding the data
head(book)
str(book)
glimpse(book)

#checking missing data
missing_percentage <- colSums(is.na(book)) / nrow(book) * 100
print(missing_percentage)

#We have significant missing data
#We will do away with them inorder to get a good analysis report and workout

# a. Changing to date format

book1 <- book %>%
  select(date, total_purchased, customer_type, user_submitted_review) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

#converting to M/Y to help in summing monthly sales
book2 <- book1 %>%
  mutate(month = format(date, "%Y-%m"))

str(book2$month)
glimpse(book2)
print(book2)
  
  
# Summing the sales per month
 book3<- book2 %>%
  group_by(month)%>%
  summarise(purchased=sum(total_purchased,na.rm = TRUE))
   
 print(book3)
 
 
 #checking the trend
 
 ggplot(book3, aes(x = factor(month, levels = unique(month)), y = purchased)) +
   geom_col(fill = "blue") +
   coord_flip() +
   labs(title = "Total Sales by Month",
        x = "Month",
        y = "Total Sales") +
   theme_minimal()
 
 #Line Graph
 ggplot(book3, aes(x = as.Date(paste0(month, "-01")), y = purchased, group = 1)) +
   geom_line(color = "blue", size = 1) +  # Line color and thickness
   geom_point(color = "red", size = 2) +  # Add points for clarity
   labs(title = "Total Sales Over Time",
        x = "Month",
        y = "Total Sales") +
   theme_minimal() +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Format x-axis
   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
 
 
 # Confirming the mean of sales before and after July
 #Before that, we should change the date format to MM/YY
 book4 <- book3 %>%
   mutate(month = as.Date(paste0("20", substr(month, 3, 7), "-01"))) 
 str(book4)
 
 
 #Applying After and Before July
 book5 <- book4 %>%
   mutate(period = ifelse(month < as.Date("2019-07-01"), "Before", "After"))
 print(book5)
 
 #Summarizing the descriptive statistics for BEFORE and AFTER
 summary_stats <- book5 %>%
   group_by(period) %>%
   summarise(
     total_sales = sum(purchased, na.rm = TRUE),
     avg_sales = mean(purchased, na.rm = TRUE),
     sd_sales = sd(purchased, na.rm = TRUE),
     median_sales = median(purchased, na.rm = TRUE),
     min_sales = min(purchased, na.rm = TRUE),
     max_sales = max(purchased, na.rm = TRUE),
     count = n()
   )
 
 print(summary_stats)
 
 
 #Line Graph after descritive stat
 library(ggplot2)
 
 ggplot(book5, aes(x = month, y = purchased, group = 1)) +
   geom_line(color = "blue", size = 1) +  # Blue line for sales trend
   geom_point(aes(color = period), size = 3) +  # Different colors for Before/After
   geom_vline(xintercept = as.Date("2019-07-01"), linetype = "dashed", color = "red") +  # Mark July 2019
   labs(title = "Sales Trends Before and After the New Book Program",
        x = "Month",
        y = "Total Sales") +
   theme_minimal() +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Show all months
   scale_color_manual(values = c("Before" = "black", "After" = "red")) +  # Colors for periods
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

 
 #3. Independent t-Test (Compare Mean Sales Before vs. After July)
 
 t_test_result <- t.test(purchased ~ period, data = book5, var.equal = TRUE)  
 
 print(t_test_result) 
 
 #p-value = 0.6367 → This is much greater than 0.05, meaning we fail to reject the null hypothesis. There is no significant difference in sales before and after July.
 #Mean sales (Before) = 1440.33, Mean sales (After) = 1420.83 → Sales actually decreased slightly after July.
 # 95% Confidence Interval: (-108.7, 69.7) → This range includes 0, meaning there is no clear increase or decrease in sales due to the new book program.
 
 #4. Regression Analysis (Account for Other Factors)
 # Convert month to numerical format for regression
 book6 <- book5 %>%
   mutate(month_numeric = as.numeric(format(month, "%m")),  # Extract month as a number
          period_binary = ifelse(period == "After", 1, 0))  # Convert period to binary (0=Before, 1=After)
 
 # Run the regression model
 model <- lm(purchased ~ period, data = book6)
 
 # Display summary of the model
 summary(model)
 
