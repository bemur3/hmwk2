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
unique_hospital_count <- final.hcris.data %>%
distinct(provider_number) %>%
nrow()

cat("Number of unique hospital IDs (Medicare provider numbers):", unique_hospital_count, "\n")

# QUESTION 3: 

ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
scale_y_log10() + # Apply log scale to handle skewed distributions
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
    price = if_else(price_denom > 0, price_num / price_denom, NA_real_) # Avoid division by zero or negative denominator
  )
final.hcris.data_clean <- final.hcris.data %>%
  filter(!is.na(price) & price > 0)

ggplot(final.hcris.data_clean, aes(x = as.factor(year), y = price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save.image("submission1/Hwk2_workspace.RData")