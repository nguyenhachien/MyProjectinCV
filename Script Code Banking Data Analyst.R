# Import the important library
library(readr)
library(tidyverse)    
library(dplyr)
library(scales)  
library(lubridate)
library(reshape2)

# Read banking csv file
banking_data <- read.csv("C://MyProjectinCV/banking_100k_dataset.csv")

# PART 1. CUSTOMER OVERVIEW
# 1. Age Distribution - Histogram

banking_data %>%               # Connect data in order to visualize
  ggplot(mapping=aes(x=age)) + 
  geom_histogram(
    binwidth=2,
    fill="pink",
    alpha=0.9,
    color="white") + # Create the histogram box
    scale_y_continuous(labels = comma)+
    labs(
      title = "Age Distribution of Banking",
      subtitle = "Distribution of customer ages in banking dataset (N = 100,000)",
      x="Age",
      y="Number of Customers"
    )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# 2. Gender Distribution – Bar Chart

banking_data %>%
  count(gender) %>%       # Use count outside in order to count gender 
  mutate(percentage = round(n/sum(n)*100,1)) %>%
  ggplot(mapping=aes(x=gender,y=percentage,fill=gender))+
  geom_col(
    width=0.1,
  ) +
  geom_text(aes(label = paste0(percentage,"%")),
            vjust = -0.2,
            color = "black",
            size = 5,
            fontface='bold.italic'
            ) +
  labs(
    title="Gender Distribution of Banking Customers",
    subtitle="The distribution of Gender in banking dataset(N=100,000)",
    x="Gender",
    y="Percentage(%)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# (3) Account Type Distribution – Bar Chart

# Savings vs Current vs Credit 

banking_data %>%
  count(account_type) %>%
  ggplot(mapping=aes(x=account_type,y=n,fill=account_type))+
  geom_col(
    width=0.2,
  ) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label =comma(n)),
            vjust = -0.1,
            color = "black",
            size = 4,
            fontface="bold",
            ) +
  labs(
    title="Account Type Distribution of Banking Customers",
    subtitle="This chart shows the distribution of account type from 100,000 customers",
    x="Account Type",
    y="Number of Customers"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# PART 2. FINANCIAL PERFORMANCE

# 4. Balance Distribution - Histogram
banking_data %>%
  ggplot(mapping=aes(x=balance_amount)) + 
  geom_histogram(
    binwidth=5000,
    fill="pink",
    alpha=0.9,
    color="white") + # Create the histogram box
  scale_x_continuous(labels = label_number(big.mark = ".")) +
  scale_y_continuous(labels=label_number(big.mark = "."))+
  labs(
    title = "Balance Distribution",
    subtitle = "Distribution of balance customers in banking dataset (N = 100,000)",
    x="Balance Banking",
    y="Number of Customers"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# 5) Average Balance by Branch – Bar Chart
banking_data %>% 
  group_by(branch) %>% # Group branch into 1 categorize
  summarise(average_balance=mean(balance_amount,na.rm=TRUE)) %>%
  ggplot(mapping=aes(x=reorder(branch,average_balance),y=average_balance))+
  geom_col(aes(fill=branch),
    width=0.5
  )+
  geom_text(aes(label =comma(average_balance)),
            vjust = -0.1,
            color = "black",
            size = 5,
            fontface="bold",
  )+
  scale_y_continuous(labels=label_number(big.mark = "."))+
  labs(
    title="Average Balance by Branch",
    subtitle="Performance comparison across banking branches (N = 100,000)",
    x="Branch",
    y="Average Account Balance"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

# 6. Monthly Transactions vs Balance – Scatter Plot
banking_data %>%
  ggplot(mapping=aes(x=monthly_transactions,y=balance_amount))+
  geom_point(
    color="steelblue",
    size=0.6,
    alpha=0.4
  )+
  geom_smooth(
    method="lm",
    color="red",
    se=FALSE,
    linewidth=1
  )+
  scale_y_continuous(labels=scales::label_number(big.mark = "."))+
  labs(
    title="Monthly Transactions and Balance Amount",
    subtitle="Relationship between monthly transactions and customer account balance (N = 100,000)",
    x="Monthly Transactions",
    y="Balance Amount"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# PART 3. LOAN ANALYSIS(very important)
# (7) Loan Status Distribution – Pie/Bar

banking_data %>%
  count(loan_status)%>%
  ggplot(mapping=aes(x=reorder(loan_status,n),y=n,fill=loan_status))+
  geom_col(
    width=0.3
  )+
  geom_text(
    aes(label=scales::label_number(big.mark = '.')(n),
        vjust=-0.2,
        fontface = 'bold')
  )+
  scale_y_continuous(labels=label_number(big.mark='.'))+
  labs(
    title='Loan status by 100,000 customers in banking',
    x='Loan Status',
    y='Number of Customers'
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size=18,face=('bold.italic'),hjust=0.5),
    legend.position="None"
  )

# 8. Loan Amount by Age Group – Box plot

banking_data %>% 
  mutate(age_group = case_when(
    age <= 25 ~ "18-25",
    age <= 35 ~ "26-35",
    age <= 45 ~ "36-45",
    age <= 55 ~ "46-55",
    TRUE ~ "56+"
  )) %>%
  ggplot(mapping=aes(x=age_group,y=loan_amount,fill=age_group))+
  geom_boxplot(
    outlier.shape = NA,
    median.colour = 'pink',
    box.colour = 'blue',
    whisker.color = 'brown',
  )+
  scale_y_continuous(labels=comma)+
  labs(
    title="Loan Amount by Age Group",
    x="Age",
    y="Loan Amount"
  )+
  theme_minimal()+
  theme(
    axis.title = element_text(size=15,face='bold.italic'),
    plot.title = element_text(size = 18, face='bold', hjust=0.5),
    legend.position = "none"
  )

# (9) Credit Score vs Loan Amount – Scatter Plot
banking_data %>%
  ggplot(mapping=aes(x=credit_score,y=loan_amount))+
  geom_point(
    color='#1F77B4',
    alpha=0.2,
    size=1.5
  )+
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  scale_y_continuous(labels=comma)+
  labs(
    title='Relationship Between Credit Score and Loan Amount',
    subtitle = "Scatter plot with regression line (N = 100,000)",
    x='Credit score',
    y='Loan Amount'
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(face = 'bold',size=16,hjust=0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# (10) Credit Score Distribution – Histogram
banking_data %>%
  ggplot(mapping=aes(x=credit_score))+
  geom_histogram(
    bins=50,
    alpha=0.8,
    fill='lightblue',
    color='black'
  )+
    labs(
      title="Credit Score Distribution",
      subtitle="Distribution of customer credit scores in the banking dataset",
      x='Credit Score',
      y='Number of Customers'
    )+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(
    plot.title = element_text(size=18,face='bold',hjust=0.5),
    axis.title.x = element_text(size=10),
    axis.title.y=element_text(size=12),
    plot.subtitle = element_text(size=12,hjust=0.5)
  )
# PART 4. Risk & Behavior Analysis
# (11) Credit Score vs Monthly Transactions – Scatter
banking_data %>%
  ggplot(mapping=aes(x=credit_score,y=monthly_transactions))+
  geom_point(
    alpha=0.9,
    color='#ff99FF',
    size=1
  )+
  geom_smooth(
    method = "lm",
    color = "blue",
    se = FALSE
  )+
  labs(
    title='Relationship Between Credit Score and Monthly Transactions',
    subtitle='Scatter plot with credit score and monthly transactions(N=100,000)',
    x='Credit Score',
    y='Monthly Transaction'
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(face = 'bold',size=16,hjust=0.5),
    plot.subtitle = element_text(size=12,hjust = 0.5)
  )
  

# 12. Heatmap: Age,Balance,Loan amount,Credit score,Transactions

cor_data <- banking_data %>%
  select(age, balance_amount, loan_amount, credit_score, monthly_transactions)
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_melt <- melt(cor_matrix)
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2E86C1",
    mid = "white",
    high = "#E74C3C",
    midpoint = 0
  ) +
  geom_text(aes(label = round(value,2)), size = 4) +
  labs(
    title = "Correlation Heatmap of Key Banking Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

#5 Time-based Analysis
# (13) Account Open Trend by Year – Line Chart
banking_data %>%
  mutate(
    account_open_date=mdy(account_open_date),
    open_year=year(account_open_date))%>%
  count(open_year)%>%
  ggplot(aes(x = open_year, y = n)) +
  geom_line(color = "#1F4E79", linewidth = 1.2) +
  geom_point(size = 3, color = "#E74C3C") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2015, 2024, 1))+
  labs(
    title = "Account Opening Trend by Year",
    x = "Year",
    y = "Number of New Accounts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# (14) Customer Growth by Branch – Line Chart
banking_data %>%
  mutate(
    account_open_date=mdy(account_open_date),
    open_year=year(account_open_date)) %>%
  group_by(open_year,branch) %>%
  summarise(customers=n(),.groups = "drop")%>%
  ggplot(aes(x = open_year, y = customers, color = branch)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2015, 2024, 1))+
  labs(
    title = "Customer Growth by Branch Over Time",
    x = "Year",
    y = "Number of Customers",
    color = "Branch"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

