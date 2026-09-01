#Data 9001 Assignment 2 
# Name : Aayushi Jha 
# Z id : z5576935

library(tidyverse)

# Import the three datasets
apartment_prices <- read.csv("Apartment_prices.csv")
historical_demo <- read.csv("Historical_demographic.csv")
projected_demo <- read.csv("Projected_demographic.csv")

# View the datasets
View(apartment_prices)
View(historical_demo)
View(projected_demo)

# Display the first six rows
head(apartment_prices)
head(historical_demo)
head(projected_demo)

# Check dataset dimensions
dim(apartment_prices)
dim(historical_demo)
dim(projected_demo)

# Check variable types
str(apartment_prices)
str(historical_demo)
str(projected_demo)

# Summary statistics
summary(apartment_prices)
summary(historical_demo)
summary(projected_demo)

# Count missing values
colSums(is.na(apartment_prices))
colSums(is.na(historical_demo))
colSums(is.na(projected_demo))

# Check for duplicate suburbs
sum(duplicated(apartment_prices$Suburb_name))
sum(duplicated(historical_demo$Suburb_name))
sum(duplicated(projected_demo$Suburb_name))

# Investigate data quality issues(suspicious things)

# 1. Find apartment prices containing non-numeric characters
apartment_prices %>%
  filter(str_detect(Median_price_2023, "[^0-9]"))

# 2. Find the suburb with missing historical median income (there is one)
historical_demo %>%
  filter(is.na(Historical_median_income))

# 3. Find all negative historical unemployment rates(employment rates??)
historical_demo %>%
  filter(Historical_unemployment_rate < 0) %>%
  arrange(Historical_unemployment_rate)

# 4. Find all negative projected unemployment rates
projected_demo %>%
  filter(Projected_unemployment_rate < 0) %>%
  arrange(Projected_unemployment_rate)

# 5. Confirm priority-growth-area values are only 0 and 1
table(historical_demo$Historical_priority_growth_area)
table(projected_demo$Projected_priority_growth_area)

#6. Confirm that suburb names match across datasets
setequal(apartment_prices$Suburb_name,
         historical_demo$Suburb_name)

setequal(apartment_prices$Suburb_name,
         projected_demo$Suburb_name)

#7. Create cleaned copies of the original datasets

apartment_clean <- apartment_prices
historical_clean <- historical_demo
projected_clean <- projected_demo

# Remove the incorrect "x" and convert the entire price column to numeric
apartment_clean <- apartment_clean %>%
  mutate(
    Median_price_2023 = parse_number(Median_price_2023)
  )

# Check that NORLANE is now correct
apartment_clean %>%
  filter(Suburb_name == "NORLANE")

# Confirm price is now numeric
str(apartment_clean)

# 8. Merge apartment prices with historical demographics
historical_merged <- apartment_clean %>%
  left_join(historical_clean, by = "Suburb_name")

dim(historical_merged)
head(historical_merged)
colSums(is.na(historical_merged))

#9. Investigate missing historical median income
historical_merged %>%
  filter(is.na(Historical_median_income))

mean_income <- mean(
  historical_merged$Historical_median_income,
  na.rm = TRUE
)

median_income <- median(
  historical_merged$Historical_median_income,
  na.rm = TRUE
)

mean_income
median_income
#plot
ggplot(
  historical_merged,
  aes(x = Historical_median_income)
) +
  geom_histogram(
    bins = 25,
    colour = "white"
  ) +
  geom_vline(
    xintercept = mean_income,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Historical Median Income",
    subtitle = "Dashed line represents the sample mean",
    x = "Historical median income ($)",
    y = "Number of suburbs"
  ) +
  theme_minimal()

mean_income_rounded <- as.integer(round(mean_income))

mean_income_rounded

historical_merged <- historical_merged %>%
  mutate(
    Historical_median_income =
      replace_na(Historical_median_income, mean_income_rounded)
  )

# Confirm that the missing value has been replaced
colSums(is.na(historical_merged))

historical_merged %>%
  filter(Suburb_name == "POINT LONSDALE")

#10. Investigate apartment price outliers
summary(historical_merged$Median_price_2023)

historical_merged %>%
  arrange(desc(Median_price_2023)) %>%
  select(Suburb_name, Median_price_2023) %>%
  slice_head(n = 10)

ggplot(historical_merged, aes(y = Median_price_2023)) +
  geom_boxplot() +
  labs(
    title = "Apartment Price Outlier Investigation",
    x = NULL,
    y = "Median apartment price ($)"
  ) +
  theme_minimal()

# Calculate the extreme outlier threshold
price_q1 <- quantile(
  historical_merged$Median_price_2023,
  0.25
)

price_q3 <- quantile(
  historical_merged$Median_price_2023,
  0.75
)

price_iqr <- IQR(
  historical_merged$Median_price_2023
)

extreme_upper_limit <- price_q3 + 3 * price_iqr

extreme_upper_limit

historical_merged %>%
  filter(Median_price_2023 > extreme_upper_limit) %>%
  select(Suburb_name, Median_price_2023)

historical_merged %>%
  arrange(desc(Median_price_2023)) %>%
  select(Suburb_name, Median_price_2023) %>%
  slice_head(n = 10)

# 11. Handle the extreme apartment-price outlier

# Flag extreme price observations
historical_merged <- historical_merged %>%
  mutate(
    Extreme_price_outlier =
      Median_price_2023 > extreme_upper_limit
  )

# Confirm which suburb is flagged
historical_merged %>%
  filter(Extreme_price_outlier) %>%
  select(Suburb_name, Median_price_2023)

# Create a separate dataset for modelling
# Keep the complete dataset unchanged
historical_model <- historical_merged %>%
  filter(!Extreme_price_outlier)

dim(historical_model)
summary(historical_model$Median_price_2023)

ggplot(historical_model, aes(y = Median_price_2023)) +
  geom_boxplot() +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Apartment Prices After Excluding the Extreme Outlier",
    subtitle = "MAFFRA ($4,202,660) was excluded from the modelling sample",
    x = NULL,
    y = "Median apartment price"
  ) +
  theme_minimal()

ggplot(historical_model, aes(x = Median_price_2023)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Apartment Prices After Excluding the Extreme Outlier",
    subtitle = "MAFFRA ($4,202,660) was excluded from the modelling sample",
    x = "Median apartment price",
    y = NULL
  ) +
  theme_minimal()
# 12. Investigate historical unemployment values

ggplot(
  historical_model,
  aes(x = Historical_unemployment_rate)
) +
  geom_histogram(
    bins = 30,
    colour = "white"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Historical Unemployment Rates",
    subtitle = "Values to the left of zero are invalid unemployment rates",
    x = "Historical unemployment rate (%)",
    y = "Number of suburbs"
  ) +
  theme_minimal()

# Join historical and projected unemployment values
unemployment_comparison <- historical_demo %>%
  select(
    Suburb_name,
    Historical_unemployment_rate
  ) %>%
  left_join(
    projected_demo %>%
      select(
        Suburb_name,
        Projected_unemployment_rate
      ),
    by = "Suburb_name"
  )

# Confirm whether the same suburbs are negative in both datasets
unemployment_comparison %>%
  filter(
    Historical_unemployment_rate < 0 |
      Projected_unemployment_rate < 0
  )
ggplot(
  unemployment_comparison,
  aes(
    x = Historical_unemployment_rate,
    y = Projected_unemployment_rate
  )
) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Historical and Projected Unemployment Rates",
    subtitle = "The dashed line represents identical historical and projected values",
    x = "Historical unemployment rate (%)",
    y = "Projected unemployment rate (%)"
  ) +
  theme_minimal()

# Number and percentage of invalid historical values
negative_count <- sum(
  historical_model$Historical_unemployment_rate < 0
)

negative_percentage <-
  negative_count / nrow(historical_model) * 100

negative_count
negative_percentage

# 13. Correct invalid unemployment rates
historical_model <- historical_model %>%
  mutate(
    Historical_unemployment_rate_original =
      Historical_unemployment_rate,
    
    Historical_unemployment_rate =
      pmax(Historical_unemployment_rate, 0)
  )

projected_clean <- projected_demo %>%
  mutate(
    Projected_unemployment_rate_original =
      Projected_unemployment_rate,
    
    Projected_unemployment_rate =
      pmax(Projected_unemployment_rate, 0)
  )

# No negative values should remain
sum(historical_model$Historical_unemployment_rate < 0)
sum(projected_clean$Projected_unemployment_rate < 0)

# Number of corrected observations
sum(
  historical_model$Historical_unemployment_rate_original < 0
)

sum(
  projected_clean$Projected_unemployment_rate_original < 0
)

unemployment_cleaning_plot <- historical_model %>%
  select(
    Historical_unemployment_rate_original,
    Historical_unemployment_rate
  ) %>%
  rename(
    "Before cleaning" =
      Historical_unemployment_rate_original,
    
    "After cleaning" =
      Historical_unemployment_rate
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Cleaning_stage",
    values_to = "Unemployment_rate"
  )

ggplot(
  unemployment_cleaning_plot,
  aes(x = Unemployment_rate)
) +
  geom_histogram(
    bins = 30,
    colour = "white"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~ Cleaning_stage, ncol = 1) +
  labs(
    title = "Unemployment Rates Before and After Cleaning",
    subtitle = "Negative rates were clipped to the logical lower bound of zero",
    x = "Historical unemployment rate (%)",
    y = "Number of suburbs"
  ) +
  theme_minimal()

unemployment_cleaning_plot <- unemployment_cleaning_plot %>%
  mutate(
    Cleaning_stage = factor(
      Cleaning_stage,
      levels = c("Before cleaning", "After cleaning")
    )
  )

ggplot(
  unemployment_cleaning_plot,
  aes(x = Unemployment_rate)
) +
  geom_histogram(
    bins = 30,
    colour = "white"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~ Cleaning_stage, ncol = 1) +
  labs(
    title = "Unemployment Rates Before and After Cleaning",
    subtitle = "Twenty-two negative rates were clipped to the logical lower bound of zero",
    x = "Historical unemployment rate (%)",
    y = "Number of suburbs"
  ) +
  theme_minimal()


# 14. Explore relationships with apartment prices

numeric_relationships <- historical_model %>%
  select(
    Median_price_2023,
    Historical_population_growth,
    Historical_median_income,
    Historical_unemployment_rate
  ) %>%
  pivot_longer(
    cols = -Median_price_2023,
    names_to = "Predictor",
    values_to = "Value"
  )

ggplot(
  numeric_relationships,
  aes(x = Value, y = Median_price_2023)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(
    ~ Predictor,
    scales = "free_x"
  ) +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Apartment Prices and Historical Demographic Features",
    subtitle = "Lines show the estimated simple linear relationship",
    x = NULL,
    y = "Median apartment price"
  ) +
  theme_minimal()

historical_model <- historical_model %>%
  mutate(
    Priority_growth_area = factor(
      Historical_priority_growth_area,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

ggplot(
  historical_model,
  aes(
    x = Priority_growth_area,
    y = Median_price_2023
  )
) +
  geom_boxplot() +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Apartment Prices by Priority Growth Area Status",
    x = "Priority growth area",
    y = "Median apartment price"
  ) +
  theme_minimal()

numeric_relationships <- historical_model %>%
  select(
    Median_price_2023,
    Historical_population_growth,
    Historical_median_income,
    Historical_unemployment_rate
  ) %>%
  pivot_longer(
    cols = -Median_price_2023,
    names_to = "Predictor",
    values_to = "Value"
  ) %>%
  mutate(
    Predictor = recode(
      Predictor,
      Historical_population_growth = "Population growth",
      Historical_median_income = "Median income",
      Historical_unemployment_rate = "Unemployment rate"
    )
  )

ggplot(
  numeric_relationships,
  aes(x = Value, y = Median_price_2023)
) +
  geom_point(alpha = 0.45) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(
    ~ Predictor,
    scales = "free_x",
    nrow = 1
  ) +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Apartment Prices and Demographic Features",
    subtitle = "Lines represent separate simple linear relationships",
    x = NULL,
    y = "Median apartment price"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10)
  )


# 16. Final datasets after data cleaning

historical_analysis <- historical_model %>%
  select(
    Suburb_name,
    Median_price_2023,
    Historical_population_growth,
    Historical_median_income,
    Historical_unemployment_rate,
    Historical_priority_growth_area
  )

projected_features <- projected_clean %>%
  select(
    Suburb_name,
    Projected_population_growth,
    Projected_median_income,
    Projected_unemployment_rate,
    Projected_priority_growth_area
  )

# Final checks
dim(historical_analysis)
dim(projected_features)

colSums(is.na(historical_analysis))
colSums(is.na(projected_features))

sum(duplicated(historical_analysis$Suburb_name))
sum(duplicated(projected_features$Suburb_name))

sum(historical_analysis$Historical_unemployment_rate < 0)
sum(projected_features$Projected_unemployment_rate < 0)

summary(historical_analysis)
summary(projected_features)

# --------------------------------------------------
# PART 2: MODEL ESTIMATION
# --------------------------------------------------

# Full multiple linear regression model
full_model <- lm(
  Median_price_2023 ~
    Historical_population_growth +
    Historical_median_income +
    Historical_unemployment_rate +
    Historical_priority_growth_area,
  data = historical_analysis
)

# View regression results
summary(full_model)
# 95% confidence intervals
confint(full_model)

# Reduced model excluding historical median income
reduced_model <- lm(
  Median_price_2023 ~
    Historical_population_growth +
    Historical_unemployment_rate +
    Historical_priority_growth_area,
  data = historical_analysis
)

summary(reduced_model)
confint(reduced_model)

# Compare model fit
data.frame(
  Model = c("Full model", "Reduced model"),
  R_squared = c(
    summary(full_model)$r.squared,
    summary(reduced_model)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(full_model)$adj.r.squared,
    summary(reduced_model)$adj.r.squared
  ),
  Residual_standard_error = c(
    sigma(full_model),
    sigma(reduced_model)
  )
)

# Nested model comparison
anova(reduced_model, full_model)


# 2.1 Regression diagnostic plots

par(mfrow = c(2, 2))
plot(reduced_model)
par(mfrow = c(1, 1))

# Identify potentially influential observations
cooks_values <- cooks.distance(reduced_model)

influential_points <- historical_analysis %>%
  mutate(
    Observation = row_number(),
    Cooks_distance = cooks_values
  ) %>%
  filter(Cooks_distance > 4 / nrow(historical_analysis)) %>%
  arrange(desc(Cooks_distance))

influential_points %>%
  select(
    Suburb_name,
    Median_price_2023,
    Cooks_distance
  )

cor(
  historical_analysis %>%
    select(
      Historical_population_growth,
      Historical_unemployment_rate,
      Historical_priority_growth_area
    )
)

# 2.2 Sensitivity check for the most influential suburb
model_without_seaford <- lm(
  Median_price_2023 ~
    Historical_population_growth +
    Historical_unemployment_rate +
    Historical_priority_growth_area,
  data = historical_analysis %>%
    filter(Suburb_name != "SEAFORD")
)

# Compare coefficients
coef(reduced_model)
coef(model_without_seaford)

# Compare model fit
data.frame(
  Model = c("Reduced model", "Without SEAFORD"),
  R_squared = c(
    summary(reduced_model)$r.squared,
    summary(model_without_seaford)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(reduced_model)$adj.r.squared,
    summary(model_without_seaford)$adj.r.squared
  )
)

# 2.3 Final model selection

# Select the reduced model as the final model
final_model <- reduced_model

# Display final model results
summary(final_model)
confint(final_model)

# Final estimated coefficients
coef(final_model)

# PART 3: MODEL INTERPRETATION AND PREDICTION

# 3.1 Prepare projected features for prediction

prediction_data <- projected_features %>%
  transmute(
    Suburb_name,
    
    Historical_population_growth =
      Projected_population_growth,
    
    Historical_unemployment_rate =
      Projected_unemployment_rate,
    
    Historical_priority_growth_area =
      Projected_priority_growth_area
  )

# Check the prediction data
head(prediction_data)
dim(prediction_data)
colSums(is.na(prediction_data))

# Generate predicted prices and 95% prediction intervals

price_predictions <- predict(
  final_model,
  newdata = prediction_data,
  interval = "prediction",
  level = 0.95
)

head(price_predictions)

prediction_results <- prediction_data %>%
  select(Suburb_name) %>%
  bind_cols(as.data.frame(price_predictions)) %>%
  rename(
    Predicted_price_next_year = fit,
    Prediction_lower = lwr,
    Prediction_upper = upr
  )

head(prediction_results)
summary(prediction_results$Predicted_price_next_year)

# 3.2 Calculate expected investment returns

investment_results <- apartment_clean %>%
  select(
    Suburb_name,
    Median_price_2023
  ) %>%
  left_join(
    prediction_results,
    by = "Suburb_name"
  ) %>%
  mutate(
    # Expected dollar increase
    Expected_price_change =
      Predicted_price_next_year - Median_price_2023,
    
    # Expected percentage return
    Expected_return_percent =
      100 * Expected_price_change / Median_price_2023,
    
    # Conservative return using the lower prediction bound
    Lower_bound_return_percent =
      100 * (Prediction_lower - Median_price_2023) /
      Median_price_2023
  )

dim(investment_results)
colSums(is.na(investment_results))
summary(investment_results)

top_return_suburbs <- investment_results %>%
  filter(Suburb_name != "MAFFRA") %>%
  arrange(desc(Expected_return_percent)) %>%
  select(
    Suburb_name,
    Median_price_2023,
    Predicted_price_next_year,
    Expected_price_change,
    Expected_return_percent,
    Lower_bound_return_percent
  ) %>%
  slice_head(n = 10)

top_return_suburbs

top_dollar_gain_suburbs <- investment_results %>%
  filter(Suburb_name != "MAFFRA") %>%
  arrange(desc(Expected_price_change)) %>%
  select(
    Suburb_name,
    Median_price_2023,
    Predicted_price_next_year,
    Expected_price_change,
    Expected_return_percent
  ) %>%
  slice_head(n = 10)

top_dollar_gain_suburbs

top_predicted_price_suburbs <- investment_results %>%
  filter(Suburb_name != "MAFFRA") %>%
  arrange(desc(Predicted_price_next_year)) %>%
  select(
    Suburb_name,
    Median_price_2023,
    Predicted_price_next_year,
    Expected_price_change,
    Expected_return_percent
  ) %>%
  slice_head(n = 10)

top_predicted_price_suburbs

# Check that Montrose's values are not obviously incorrect
historical_analysis %>%
  filter(Suburb_name == "MONTROSE")

projected_features %>%
  filter(Suburb_name == "MONTROSE")

investment_results %>%
  filter(Suburb_name == "MONTROSE")

# Calculate upper-bound returns
investment_results <- investment_results %>%
  mutate(
    Upper_bound_return_percent =
      100 * (Prediction_upper - Median_price_2023) /
      Median_price_2023
  )

# Select the ten highest predicted percentage returns
top_10_return_plot <- investment_results %>%
  filter(Suburb_name != "MAFFRA") %>%
  arrange(desc(Expected_return_percent)) %>%
  slice_head(n = 10) %>%
  mutate(
    Suburb_name = forcats::fct_reorder(
      Suburb_name,
      Expected_return_percent
    ),
    Highlight = if_else(
      Suburb_name == "MONTROSE",
      "Recommended suburb",
      "Other suburbs"
    )
  )

ggplot(
  top_10_return_plot,
  aes(
    x = Suburb_name,
    y = Expected_return_percent,
    fill = Highlight
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(Expected_return_percent, 1),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Recommended suburb" = "red",
      "Other suburbs" = "grey70"
    )
  ) +
  expand_limits(
    y = max(top_10_return_plot$Expected_return_percent) * 1.12
  ) +
  labs(
    title = "Montrose Offers the Highest Predicted Investment Return",
    subtitle = "Expected next-year return relative to the 2023 median apartment price",
    x = NULL,
    y = "Expected return (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# Check whether Montrose's low current price is formally an outlier
lower_outlier_limit <- price_q1 - 1.5 * price_iqr
lower_extreme_limit <- price_q1 - 3 * price_iqr

lower_outlier_limit
lower_extreme_limit

historical_merged %>%
  filter(Median_price_2023 < lower_outlier_limit) %>%
  select(Suburb_name, Median_price_2023)

investment_results %>%
  filter(Suburb_name != "MAFFRA") %>%
  arrange(desc(Lower_bound_return_percent)) %>%
  select(
    Suburb_name,
    Median_price_2023,
    Predicted_price_next_year,
    Expected_return_percent,
    Lower_bound_return_percent
  ) %>%
  slice_head(n = 10)

ggsave(
  "montrose_investment_recommendation.png",
  width = 9,
  height = 6,
  dpi = 300
)

