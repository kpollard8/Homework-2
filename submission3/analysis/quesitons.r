# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 2
## Author:        Kendall Pollard 
## Date Created:  2/14/2024


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

install.packages("Matching")
install.packages("cobalt")

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

#Answer Question 7:
lp.vars <- final.hcris %>% 
  dplyr::select(beds, quartile_1, quartile_2, penalty, quartile_3, 
         quartile_4, price) %>%
  dplyr::filter(complete.cases(.))
lp.covs <- lp.vars %>% dplyr::select(-c("penalty","price"))


v.name=data.frame(new=c("Beds","Quartile 1", "Penalty", "Quartile 2",
                   "Quartile 3", "Quartile 4", "Price"))

# Part 1: Nearest Neighbor Matching (Inverse Variance Distance)
m.nn.var2 <- Matching::Match(Y=lp.vars$price,
                             Tr=lp.vars$penalty,
                             X=lp.covs,
                             M=1,   #<<
                             Weight=1,
                             estimand="ATE")
                             

#Part 2: Nearest neighbor matching (1-to-1) with Mahalanobis distance
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")                           


#Part 3: Inverse propensity weighting
logit.model <- glm(penalty ~ beds + quartile_1 + quartile_2 + quartile_3 + 
         quartile_4 + price, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")


#Part 4: Simple linear regression 
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)))
reg <- lm(price ~ penalty + beds + quartile_1 + quartile_2 + quartile_3 + quartile_4 + 
            beds_diff,
          data=reg.dat)
summary(reg)

library(cobalt)
# Extract ATE estimates
ATE_nn_var <- bal.tab(m.nn.var2, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_md <- bal.tab(m.nn.md, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_ps <- bal.tab(m.nn.ps, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_reg <- coef(summary(reg))["penaltyTRUE", "Estimate"]

# Create a data frame for the results
results_table <- data.frame(
  Estimator = c("Nearest Neighbor (Inverse Variance Distance)", 
                "Nearest Neighbor (Mahalanobis Distance)", 
                "Inverse Propensity Weighting", 
                "Simple Linear Regression"),
  ATE = c(ATE_nn_var, ATE_nn_md, ATE_nn_ps, ATE_reg)
)
# Print the results table
print(results_table)



rm(list=c("final.hcris.data", "final.hcris.v1996","final.hcris.v2010","unique.hcris1",
"unique.hcris2","unique.hcris3", "unique.hcris4", "duplicate.hcris","duplicate.hcris1", "duplicate.hcris2", "duplicate.hcris3", "final.hcris"))
save.image("submission3/Hw2_workspace.Rdata")
