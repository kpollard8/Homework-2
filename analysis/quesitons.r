# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 2
## Author:        Kendall Pollard 
## Date Created:  2/14/2024


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

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

# Create a line graph
ggplot(hospital_counts, aes(x = year, y = num_hospitals)) +
  geom_line() +
  labs(title = "Number of Hospitals with More Than One Report in the Same Year",
       x = "Year",
       y = "Number of Hospitals") +
  theme_minimal()

#Question 2: After removing/combining multiple reports - think i still need to do this, how many unique hospital IDs (Medicare provider numbers) exist in the data?
  # Count the number of unique hospital IDs
unique_hospital_count <- duplicate.hcris %>%
  summarise(num_unique_hospitals = n_distinct(provider_number))

# Display the result
print(unique_hospital_count$num_unique_hospitals)
