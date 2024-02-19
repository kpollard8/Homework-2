# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 2
## Author:        Kendall Pollard 
## Date Created:  2/14/2024


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

install.packages("Matching")

#read in data 
final.hcris.v1996=read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010=read_rds('data/output/HCRIS_Data_v2010.rds')

## create missing variables for columns introduced in v2010 of hcris forms
final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

## count of hospitals/provider_number by year
final.hcris %>% group_by(fyear) %>% count()



# Clean data --------------------------------------------------------------

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())

## identify hospitals with only one report per fiscal year 
## this will be the first set of hospitals in the final dataset
unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  select(-report, -total_reports, -report_number, -npi, -status) %>%
  mutate(source='unique reports')

## identify hospitals with multiple reports per fiscal year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

#Question 1: How many hospitals filed more than one report in the same year? Show your answer as a line graph of the number of hospitals over time.
# Group data by year and count the number of hospitals
hospital_counts <- duplicate.hcris %>%
  group_by(year = lubridate::year(fy_start)) %>%
  summarise(num_hospitals = n_distinct(provider_number))

  hospitals_multreports <- ggplot(hospital_counts, aes(x = year, y = num_hospitals)) +
  geom_line() +
  labs(title = "Number of Hospitals with More Than One Report in the Same Year",
       x = "Year",
       y = "Number of Hospitals") +
  theme_minimal()
  print(hospitals_multreports)

#Question 2: After removing/combining multiple reports - how many unique hospital IDs (Medicare provider numbers) exist in the data?

final.hcris.data=read_rds('data/output/HCRIS_Data.rds') 

## Calculate the number of unique hospital IDs
library(dplyr)
num_unique_hospital_ids <- final.hcris.data %>% ungroup() %>% distinct(provider_number) %>%
  nrow()

# Print the result
print(num_unique_hospital_ids)

# ungrouping 
final.hcris.data <- final.hcris.data %>% ungroup()
#Question 3: What is the distribution of total charges (tot_charges in the data) in each year? Show your results with a “violin” plot, with charges on the y-axis and years on the x-axis

library(ggplot2)
library(scales)

# Define the custom upper limit for the y-axis
custom_upper_limit <- 3000000000 

tot.charges <- ggplot(final.hcris.data, aes(x = year, y = tot_charges)) +
  geom_violin() +
  labs(title = "Distribution of Total Charges by Year",
       x = "Year",
       y = "Total Charges") +
  theme_minimal() + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), limits = c(0, custom_upper_limit))

print(tot.charges)


#Question 4: What is the distribution of estimated prices in each year? 
# Calculate estimated price using the provided formula
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  )

# Filter out negative prices and potential outliers
final.hcris.data <- final.hcris.data %>%
  filter(price >= 0)  # Filter out negative prices

# Create the violin plot
custom_upper_limit <- 65000
est.prices <- ggplot(final.hcris.data, aes(x = year, y = price)) +
  geom_violin(trim = TRUE) +  # Adjust the `trim` parameter as needed
  labs(title = "Distribution of Estimated Prices by Year",
       x = "Year",
       y = "Estimated Price") +
  theme_minimal() +
  ylim(0, custom_upper_limit)  # Set custom upper limit for y-axis
print(est.prices)

#Question 5: 


final.hcris.data <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris <- final.hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

#Calculate mean prices for penalized and not penalized groups
mean.pen <- round(mean(final.hcris$price[which(final.hcris$penalty == 1)]), 2)
mean.nopen <- round(mean(final.hcris$price[which(final.hcris$penalty == 0)]), 2)

# Print mean prices
cat("Mean Price for Penalized Group: ", mean.pen, "\n")
cat("Mean Price for Not Penalized Group: ", mean.nopen, "\n")

# Question 6: Split hospitals into quartiles based on bed size
final.hcris <- final.hcris %>%
  mutate(quartile = ntile(beds, 4),
         quartile_1 = as.numeric(quartile == 1),
         quartile_2 = as.numeric(quartile == 2),
         quartile_3 = as.numeric(quartile == 3),
         quartile_4 = as.numeric(quartile == 4))

# Table of average price among treated/control groups for each quartile
quartile_price_summary <- final.hcris %>%
  group_by(quartile) %>%
  summarise(avg_price_treated = mean(ifelse(penalty == 1, price, NA), na.rm = TRUE),
            avg_price_control = mean(ifelse(penalty == 0, price, NA), na.rm = TRUE))

print(quartile_price_summary)

#Question 7 Find the average treatment effect using each of the following estimators, and present your results in a single table

#nearest neighbor inverse variance
lp.vars <- final.hcris %>% ungroup() %>% 
  select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price)

lp.covs2 <- final.hcris %>% ungroup() %>% 
  select(quartile)


m.nn.var2 <- Matching::Match(Y=lp.vars$price,
                             Tr=lp.vars$penalty,
                             X=lp.covs2,
                             M=1,   #<<
                             Weight=1,
                             estimand="ATE")
                            
                        
# Extract the ATE value from the matching result
ate_nn_var2 <- m.nn.var2$estimates$ate

# Print the ATE
cat("Average Treatment Effect (ATE) using Nearest Neighbor Matching with inverse variance:", ate_nn_var2, "\n")

# nearest neighbor Mahalanobis distance
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs2,
                           M=1,
                           Weight=2,
                           estimand="ATE")     

ate_nn_md <- m.nn.md$estimates$ate

# Print the ATE
cat("Average Treatment Effect (ATE) using Nearest Neighbor Matching with Mahalanobis:", ate_nn_md, "\n")

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))




#Inverse propensity weighting
logit.model <- glm(penalty ~ beds, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

lp.vars <- lp.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))
mean.t1 <- lp.vars %>% filter(penalty==1) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- lp.vars %>% filter(penalty==0) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t1$mean_p - mean.t0$mean_p

#Simple linear regression
ipw.reg <- lm(price ~ penalty, data=lp.vars, weights=ipw)
summary(ipw.reg)

#single table 
# Define a function to calculate the ATE for each estimator
calculate_ate <- function(matching_result, weights = NULL) {
  if (is.null(weights)) {
    ate <- matching_result$estimates$ate
  } else {
    ate <- weighted.mean(lp.vars$price * lp.vars$penalty, w = weights) -
           weighted.mean(lp.vars$price * (1 - lp.vars$penalty), w = weights)
  }
  return(ate)
}

# Create a data frame to store the results
results_table <- data.frame(
  Estimator = character(),
  ATE = numeric()
)

# Nearest neighbor inverse variance
if (!is.null(m.nn.var2$estimates$ate)) {
  results_table <- rbind(results_table, 
                         data.frame(
                           Estimator = "Nearest Neighbor Inverse Variance",
                           ATE = calculate_ate(m.nn.var2)
                         ))
} else {
  print("Nearest Neighbor Inverse Variance: Matching did not converge.")
}

# Nearest neighbor Mahalanobis distance
if (!is.null(m.nn.md$estimates$ate)) {
  results_table <- rbind(results_table, 
                         data.frame(
                           Estimator = "Nearest Neighbor Mahalanobis Distance",
                           ATE = calculate_ate(m.nn.md)
                         ))
} else {
  print("Nearest Neighbor Mahalanobis Distance: Matching did not converge.")
}

# Inverse propensity weighting
if (!is.null(m.nn.ps$estimates$ate)) {
  results_table <- rbind(results_table, 
                         data.frame(
                           Estimator = "Inverse Propensity Weighting",
                           ATE = calculate_ate(m.nn.ps, weights = lp.vars$ipw)
                         ))
} else {
  print("Inverse Propensity Weighting: Matching did not converge.")
}

# Simple linear regression
results_table <- rbind(results_table, 
                       data.frame(
                         Estimator = "Simple Linear Regression",
                         ATE = coef(ipw.reg)[2]
                       ))

# Print the results table
print(results_table)


rm(list=c("final.hcris.data", "final.hcris.v1996","final.hcris.v2010","unique.hcris1",
"unique.hcris2","unique.hcris3", "unique.hcris4", "duplicate.hcris","duplicate.hcris1", "duplicate.hcris2", "duplicate.hcris3", "final.hcris"))
save.image("submission1/Hw2_workspace.Rdata")
