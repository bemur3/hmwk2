if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

# QUESTION 1: Count the number of hospitals with multiple reports per year

multi_report_counts <- duplicate.hcris %>%
group_by(fyear) %>%
summarise(num_hospitals = n_distinct(provider_number))

ggplot(multi_report_counts, aes(x = fyear, y = num_hospitals)) +
geom_line(color = "blue", size = 1) +
geom_point(color = "red", size = 2) +
labs(
title = "Hospitals Filing Multiple Reports Per Year",
x = "Year",
y = "Number of Hospitals",
caption = "Source: HCRIS Data (1996 & 2010 Versions)"
) +
theme_classic()

# QUESTION 2:
# Count the number of unique hospital IDs

fig.unique <- final.hcris.data %>% group_by(year) %>%
  summarize(unique_hospital_count=n()) %>%
  ggplot(aes(x=as.factor(year), y=unique_hospital_count, group=1)) +
  geom_line() +
  labs(
    x="Year",
    y="Number of Hospitals",
    title=""
  ) + theme_bw() +
  scale_y_continuous(labels=scales::comma,limits=c(0,6500)) +
  theme(axis.text.x = element_text(angle=70, hjust=1))
print(fig.unique)

# QUESTION 3: 
final.hcris.data <- final.hcris.data %>%
  group_by(year) %>%
  mutate(
    tot_charges_low = quantile(tot_charges, probs = 0.05, na.rm = TRUE),
    tot_charges_high = quantile(tot_charges, probs = 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%  # Ungroup to avoid issues with mutate()
  filter(
    tot_charges > tot_charges_low,
    tot_charges < tot_charges_high,
    !is.na(tot_charges),
    year > 1997
  ) %>%
  mutate(log_charge = log(tot_charges))


ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
scale_y_log10() + 
labs(
title = "Distribution of Total Charges by Year",
x = "Year",
y = "Total Charges (log scale)",
caption = "Source: HCRIS Data (1996 & 2010 Versions)"
) +
theme_classic()

# Qustion 4:
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num/price_denom) %>%
    filter(price_denom>100, !is.na(price_denom),
    price_num>0, !is.na(price_num),
    price<100000,
    beds>30, !is.na(beds))

ggplot(final.hcris.data, aes(x = as.factor(year), y = price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Question 5

final.hcris.data <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris.2012 <- final.hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

mean.pen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==0)]),2)
print(mean.pen)
print(mean.nopen)



# 6)Split hospitals into quartiles based on bed size. Provide a table of the average price among treated/control groups for each quartile.
## Define penalty: HVBP + HRRP < 0
# Define penalty and include only the year 2012 (if focusing only on this year)
final.hcris.2012 <- final.hcris.data %>%
  filter(year == 2012) %>%  # Ensuring only 2012 data is considered
  mutate(
    hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
    hrrp_payment = ifelse(is.na(hrrp_payment), 0, hrrp_payment),
    penalty = (hvbp_payment + hrrp_payment) < 0
  )

# Calculate bed size quartiles
bed_quartiles <- quantile(final.hcris.2012$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Addressing outliers: Removing extreme values based on price
# Assume that extreme values are defined as those beyond the 99th percentile or below the 1st percentile
price_cutoffs <- quantile(final.hcris.2012$price, probs = c(0.05, 0.95), na.rm = TRUE)

final.hcris.2012 <- final.hcris.2012 %>%
  filter(price >= price_cutoffs[1] & price <= price_cutoffs[2])

## Assign each hospital to a bed size quartile
bed_lower <- quantile(final.hcris.2012$beds, 0.05, na.rm = TRUE)
bed_upper <- quantile(final.hcris.2012$beds, 0.95, na.rm = TRUE)

final.hcris.2012 <- final.hcris.2012 %>%
filter(beds > bed_lower & beds < bed_upper) %>%
mutate(
Q1 = ifelse(beds <= bed_quartiles[1] & beds > 0, 1, 0),
Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
)


# Calculate average prices by quartile and penalty status
quartile_summary <- final.hcris.2012 %>%
  mutate(bed_quartile = case_when(
    Q1 == 1 ~ "Q1",
    Q2 == 1 ~ "Q2",
    Q3 == 1 ~ "Q3",
    Q4 == 1 ~ "Q4"
  )) %>%
  group_by(bed_quartile, penalty) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

# Renaming columns for clarity
quartile_summary <- quartile_summary %>%
  rename(
    `No Penalty` = `penalty_FALSE`,
    `Penalty` = `penalty_TRUE`
  )

print(quartile_summary)



# 7) Find the average treatment effect using each of the following estimators, and present your results in a single table:
# Load necessary libraries
library(Matching)
library(cobalt)
library(dplyr)

# Ensure 'penalty' is a numeric variable
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(penalty = as.numeric(penalty == TRUE))

# Nearest neighbor matching with inverse variance distance based on quartiles of bed size
near_match <- Matching::Match(
  Y = final.hcris.2012$price,
  Tr = final.hcris.2012$penalty,
  X = final.hcris.2012 %>% dplyr::select(Q1, Q2, Q3),  # Assuming Q1, Q2, Q3 are correctly defined in your data
  M = 1, 
  Weight = 1,
  estimand = "ATE"
)
summary(near_match)

# Nearest neighbor matching with Mahalanobis distance
mahalanobis_match <- Matching::Match(
  Y = final.hcris.2012$price,
  Tr = final.hcris.2012$penalty,
  X = final.hcris.2012 %>% dplyr::select(Q1, Q2, Q3),
  M = 1,
  Weight = 2,
  estimand = "ATE"
)
summary(mahalanobis_match)

# Inverse propensity weighting
logit_model <- glm(penalty ~ Q1 + Q2 + Q3, family = binomial, data = final.hcris.2012)
ps <- predict(logit_model, type = "response")

# Calculate inverse propensity weights (IPW)
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(ipw = if_else(penalty == 1, 1 / ps, 1 / (1 - ps)))

# Compute weighted average prices for treated (penalty == 1) and control (penalty == 0) groups
weighted_price_penalized <- final.hcris.2012 %>%
  filter(penalty == 1) %>%
  summarise(weighted_price = weighted.mean(price, w = ipw))
weighted_price_non_penalized <- final.hcris.2012 %>%
  filter(penalty == 0) %>%
  summarise(weighted_price = weighted.mean(price, w = ipw))

# Calculate the difference in weighted prices between penalized and non-penalized groups
ipw_difference <- weighted_price_penalized$weighted_price - weighted_price_non_penalized$weighted_price

# Simple linear regression adjusting for quartiles
reg_data <- final.hcris.2012 %>%
  mutate_at(vars(Q1, Q2, Q3), ~ penalty * (. - mean(.)))

reg_model <- lm(price ~ penalty + Q1 + Q2 + Q3 + Q1:penalty + Q2:penalty + Q3:penalty, data = reg_data)
summary(reg_model)

ate_linear_regression <- coef(reg_model)["penalty"]

# Compile ATE estimates from different methods into a single table
ate_estimates <- data.frame(
  Method = c("Nearest Matching (Inverse Variance)", "Nearest Matching (Mahalanobis Distance)", "Inverse Propensity Weighting", "Linear Regression"),
  ATE_Estimate = c(summary(near_match)$estimate, summary(mahalanobis_match)$estimate, ipw_difference, ate_linear_regression)
)

# Print the table using kable
knitr::kable(ate_estimates, caption = "ATE Estimates from Different Methods", col.names = c("Method", "ATE Estimate"))





save.image("submission3/Hwk2_workspace3.RData")
