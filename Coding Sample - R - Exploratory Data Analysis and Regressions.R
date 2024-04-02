# Installing Packages
#install.packages("reshape2")

#Loading the Libraries
library(tidyverse)
library(kableExtra)
library(readxl)
library(ggplot2)
library(knitr)
library(dplyr)
library(stargazer)
library(modelsummary)
library(fixest)
library(reshape2)

#Loading the dataframes
load("Dataframes/quality_review.RData")
load("Dataframes/ELA_math_grades.RData")
load("Dataframes/renewal_schools.RData")
load("Dataframes/demographic_data.RData")

################################################################################
#                           I. EXPLORATORY DATA ANALYSIS
################################################################################

#-------------------------------------------------------------------------------
# 1. Difference in Weighted Means Tables Functions
#-------------------------------------------------------------------------------

# Function to calculate the difference in means table, with weighted means
diff_t <- function(df, dummy_treatment, treat_group, control_group, var_name, weight) {
  # Prepare data: filter by group and calculate weighted mean
  treat_data <- df %>% 
    filter(!!sym(dummy_treatment) == treat_group) %>%
    summarize(mean = weighted.mean(!!sym(var_name), !!sym(weight), na.rm = TRUE))
  
  control_data <- df %>% 
    filter(!!sym(dummy_treatment) == control_group) %>%
    summarize(mean = weighted.mean(!!sym(var_name), !!sym(weight), na.rm = TRUE))
  
  # Calculate the difference and perform the t-test
  mean_treat <- round(treat_data$mean,2)
  mean_control <- round(control_data$mean,2)
  diff <- round(mean_treat - mean_control, 2)
  
  # Perform t-test using filter and pull to get the appropriate subsets
  treat_vals <- df %>% filter(!!sym(dummy_treatment) == treat_group) %>% pull(!!sym(var_name))
  control_vals <- df %>% filter(!!sym(dummy_treatment) == control_group) %>% pull(!!sym(var_name))
  p_value <- as.numeric(t.test(treat_vals, control_vals, var.equal = FALSE)$p.value)
  signif <- case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.1 ~ "*",
    TRUE ~ " "
  )
  
  # Return a dataframe
  tibble(
    variable = var_name,
    mean_treat = mean_treat,
    mean_control = mean_control,
    diff = diff,
    p_value = round(p_value, 3),
    signif = signif
  )
}

# Function to add difference in the number of schools and enrollment to the diff in means table
# Has to be done since those stats are not weighted averages!
add_school_enrollment_stats <- function(df, group_var, treat_group, control_group, code, enrollment_var) {
  # Calculating number of distinct schools
 num_distinct_dbn_treat <- n_distinct(df[[code]][df[[group_var]] == treat_group], na.rm = TRUE)
 num_distinct_dbn_control <- n_distinct(df[[code]][df[[group_var]] == control_group], na.rm = TRUE)
  
  # Calculating average enrollment
  avg_enrollment_treat <- round(mean(df[[enrollment_var]][df[[group_var]] == treat_group], na.rm = TRUE),2)
  avg_enrollment_control <- round(mean(df[[enrollment_var]][df[[group_var]] == control_group], na.rm = TRUE),2)
  
  # T-test for difference in enrollment
  treat_enrollment_vals <- df[[enrollment_var]][df[[group_var]] == treat_group]
  control_enrollment_vals <- df[[enrollment_var]][df[[group_var]] == control_group]
  p_value_enroll <- t.test(treat_enrollment_vals, control_enrollment_vals, var.equal = FALSE)$p.value
  
  # Significance levels
  signif_enroll <- case_when(
    p_value_enroll < 0.01 ~ "***",
    p_value_enroll < 0.05 ~ "**",
    p_value_enroll < 0.1 ~ "*",
    TRUE ~ " "
  )
  
  # Creating data frames for number of schools and average enrollment
  num_schools <- data.frame(
    variable = "Num. Schools",
    mean_treat = num_distinct_dbn_treat,
    mean_control = num_distinct_dbn_control,
    diff = num_distinct_dbn_treat - num_distinct_dbn_control,
    p_value = NA,
    signif = ""
  )
  
  avg_enroll <- data.frame(
    variable = "Avg. Enrollment",
    mean_treat = avg_enrollment_treat,
    mean_control = avg_enrollment_control,
    diff = avg_enrollment_treat - avg_enrollment_control,
    p_value = round(p_value_enroll, 3),
    signif = signif_enroll
  )
  
  # Combine and return the results
  return(rbind(num_schools, avg_enroll))
}


#-------------------------------------------------------------------------------
# 2. Grades Density Curves
#-------------------------------------------------------------------------------

# Density curve of the grades
ggplot(quality_review, aes(x = avg_math_score)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot - Math Scores", x = "Avg Math Scores", y = "Density")

ggplot(quality_review, aes(x = avg_english_score)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot - English Scores", x = "Avg Math Scores", y = "Density")



#-------------------------------------------------------------------------------
# 3. Number of Renewal Schools Per Year
#-------------------------------------------------------------------------------

rs <- quality_review %>% 
  filter(ever_renewal == "Renewal Schools")

uniquers <- rs %>% 
  group_by(year, active_renewal) %>% 
  filter(active_renewal == "Active Renewal Schools") %>% 
  summarise(num_schools = n_distinct(DBN))

table(rs$year, rs$active_renewal)


#-------------------------------------------------------------------------------
# 4. Renewal Schools that had more than one type
#-------------------------------------------------------------------------------
renewal_types <- quality_review %>%
  filter(ever_renewal == "Renewal Schools") %>%
  group_by(year, DBN, ever_renewal) %>%
  summarize(
    types = toString(unique(type)), # creates a comma-separated list of types
    number_of_types = n_distinct(type) # counts unique types per group
  )%>% 
  filter(number_of_types>1) %>%
  pivot_wider(
    names_from = year, 
    values_from = types,
    names_sep = "_" # This will create column names like 2020_types, 2020_number_of_types, etc.
  ) %>% 
  select(-number_of_types, -ever_renewal)
  

#-------------------------------------------------------------------------------
# 5. Renewal vs. Other Schools in NYC: Difference in Means Table - 2014
#-------------------------------------------------------------------------------

# OBs.: The decision on which schools would integrate the Renewal Program was 
# taken on Fall 2014, based on the data from 2013-2014 (classified as 2014 in 
# the quality report data)

# Filtering the data for 2014
quality_review_2014 <- quality_review %>% 
  filter(year==2014)


# CREATING A DIFFERENCE IN MEANS TABLE FOR RELEVANT VARIABLES

# Applying the function to each variable weighted by enrollment
variable_names <- c("avg_math_score",
                    "avg_english_score",
                    "pct_ELL",
                    "pct_disabilities",
                    "econ_need_index",
                    "pct_temp_housing",
                    "pct_HRA_eligib",
                    "pct_hispanic",
                    "pct_black",
                    "graduation_rate")


diff_renewal_2014 <- bind_rows(lapply(variable_names,
                                      function(v) diff_t(quality_review_2014,
                                                         "ever_renewal",
                                                         "Renewal Schools",
                                                         "Never Renewal School",
                                                         v,
                                                         "enrollment"
                                      )))

# Calculating the difference in means for the number of schools and enrollment (unwheighted)
num_enroll <- add_school_enrollment_stats(quality_review_2014, "ever_renewal", "Renewal Schools", "Never Renewal School", "DBN", "enrollment")

# Merging the tables and rename the columns
diff_renewal_2014 <- rbind(num_enroll, diff_renewal_2014)
names(diff_renewal_2014) <- c("Variable", "Renewal Schools", "Never Renewal School", "Difference", "P_value", "Signif")

# View the final table
diff_renewal_2014 %>% 
  kable()

# Removing unnecessary objects
rm(num_distinct_dbn_renewal, num_distinct_dbn_never, avg_enrollment_renewal,
   avg_enrollment_never, diff_dbn, diff_enrollment, num_schools, avg_enroll, p_value_enroll, signif_enrol)

# TABLE FOR TYPE OF SCHOOLS
table(quality_review_2014$type, quality_review_2014$ever_renewal) %>% 
  kable()

#-------------------------------------------------------------------------------
# 6. Renewal vs. Other Schools in NYC: Plot Scores vs. Econ. Need Index - 2014
#-------------------------------------------------------------------------------

# RENEWAL VS. OTHER NYC SCHOOLS

# Math grades
math_econ_plots14 <- ggplot(quality_review_2014, aes(x = econ_need_index,
                                                     y = avg_math_score,
                                                     color = ever_renewal)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(0.1, 6)) +
  scale_color_manual(values = c("Renewal Schools" = "#008080", "Never Renewal School" = "#CFCFC4")) +
  theme_minimal()+
  xlab("Economic Need Index")+
  ylab("Average Math Scores")+
  labs(title = "Average Math Scores by Economic Need Index",
       color = str_wrap("Participation in the Program",15))

math_econ_plots14

# English grades

engl_econ_plots14 <- ggplot(quality_review_2014, aes(x = econ_need_index,
                                                     y = avg_english_score,
                                                     color = ever_renewal)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(0.1, 6)) +
  scale_color_manual(values = c("Renewal Schools" = "#008080", "Never Renewal School" = "#CFCFC4")) +
  theme_minimal()+
  xlab("Economic Need Index")+
  ylab("Average English Scores")+
  labs(title = "Average English Scores by Economic Need Index",
       color = str_wrap("Participation in the Program",15))

engl_econ_plots14


#-------------------------------------------------------------------------------
# 7. Renewal vs. Comparison: Difference in Means Table - 2014
#-------------------------------------------------------------------------------

# CHECKING IF THE COMPARISON GROUP IS BALANCED
# BALANCE TEST: DIFFERENCE IN MEANS TABLE FOR COMPARISON GROUP

table(quality_review$ever_renewal, quality_review$groups)
table(quality_review_2014$ever_renewal, quality_review_2014$groups)

# Apply the function to each variable and bind the results
balance_test <- bind_rows(lapply(variable_names,
                                 function(v) diff_t(quality_review_2014,
                                                    "groups",
                                                    "Renewal",
                                                    "Comparison",
                                                    v,
                                                    "enrollment"
                                 )))

# Calculating the difference in means for the number of schools and enrollment (unwheighted)
num_enroll <- add_school_enrollment_stats(quality_review_2014,
                                          "groups",
                                          "Renewal",
                                          "Comparison",
                                          "DBN",
                                          "enrollment")

# Merging the tables and rename the columns
balance_test <- rbind(num_enroll, balance_test)
names(balance_test) <- c("Variable",
                         "Renewal",
                         "Comparison",
                         "Difference",
                         "P-value",
                         "Signif")

# Renaming the variables
balance_test$Variable[balance_test$Variable == "avg_math_score"] <- "Average Math Score"
balance_test$Variable[balance_test$Variable ==  "avg_english_score"] <- "Average English Score"
balance_test$Variable[balance_test$Variable ==  "pct_ELL"] <- "% English Language Learners"
balance_test$Variable[balance_test$Variable ==  "pct_disabilities"] <- "% Disabilities"
balance_test$Variable[balance_test$Variable ==  "econ_need_index"] <- "Economic Need Index"
balance_test$Variable[balance_test$Variable ==  "pct_temp_housing"] <- "% Temp. Housing"
balance_test$Variable[balance_test$Variable ==  "pct_HRA_eligib"] <- "% Eligible HRA"
balance_test$Variable[balance_test$Variable ==  "pct_hispanic"] <- "% Hispanic"
balance_test$Variable[balance_test$Variable ==  "pct_black"] <- "% Black"

# View the final table

balance_test %>% kable(align = "l",
                       caption = "Comparing Renewal Schools vs. Comparison Group in 2014 (Pre-Selection to Treatment)",
                       booktabs = TRUE) %>% 
  kable_styling(latex_options=c("scale_down",
                                "HOLD_position",
                                "striped"))
# Since most differences pre treatment are not statistically significant they are balanced

# Check the type of the schools
table(quality_review_2014$type, quality_review_2014$groups) %>% kable(align = "l",
                                                                      caption = "Number of Renewal vs. Comparison Group per Type",
                                                                      booktabs = TRUE) %>% 
  kable_styling(latex_options=c("HOLD_position",
                                "striped"))


# They are distributed in all the categories. Over-representing elementary and
# middle schools, and under-representing High schools and K-8.


#-------------------------------------------------------------------------------
# 8. Renewal vs. Comparison: Plot Scores vs. Econ. Need Index - 2014
#-------------------------------------------------------------------------------

# RENEWAL VS. COMPARISON

quality_review_groups <- quality_review %>% 
  filter(!is.na(groups))

# Math grades
math_econ_plots14_rc <- ggplot(quality_review_groups, aes(x = econ_need_index,
                                                     y = avg_math_score,
                                                     color = groups)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(0.1, 6)) +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#CFCFC4")) +
  theme_minimal()+
  xlab("Economic Need Index")+
  ylab("Average Math Scores")+
  labs(title = "Average Math Scores by Economic Need Index",
       color = str_wrap("Participation in the Program",15))

math_econ_plots14_rc

# English grades
engl_econ_plots14_rc <- ggplot(quality_review_groups, aes(x = econ_need_index,
                                                     y = avg_english_score,
                                                     color = groups)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(0.1, 6)) +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#CFCFC4")) +
  theme_minimal()+
  xlab("Economic Need Index")+
  ylab("Average English Scores")+
  labs(title = "Average English Scores by Economic Need Index",
       color = str_wrap("Participation in the Program",15))

engl_econ_plots14_rc

#-------------------------------------------------------------------------------
# 9. Quality Data: Time Series on Math and English Scores
#-------------------------------------------------------------------------------

# Time Series Math Scores
quality_review_plot <- quality_review %>% 
  mutate(groups_plots = case_when(
    groups == "Renewal" ~ "Renewal",
    groups == "Comparison" ~ "Comparison",
    is.na(groups) ~ "Other NYC Schools"
  ))

math_plot <- quality_review_plot %>%
  select(avg_math_score, enrollment, groups_plots, year) %>%
  filter(!is.na(avg_math_score)) %>%
  group_by(groups_plots, year) %>%
  summarise(
    mean_value = weighted.mean(avg_math_score, w = enrollment, na.rm = TRUE),
  ) %>%
  ggplot(aes(x = year, y = mean_value, color = groups_plots)) +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#1E90FF", "Other NYC Schools" = "#CFCFC4")) +
  geom_line() +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "purple") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "purple") +
  labs(title = "Average Math Scores",
       x = "Year",
       y = "Average Math Score (Weighted by Enrollment)",
       color = "Groups of Schools:") +
  theme_minimal()

math_plot

# Time Series English Scores
english_plot <- quality_review_plot %>%
  select(avg_english_score, enrollment, groups_plots, year) %>%
  filter(!is.na(avg_english_score)) %>%
  group_by(groups_plots, year) %>%
  summarise(
    mean_value = weighted.mean(avg_english_score, w = enrollment, na.rm = TRUE),
  ) %>%
  ggplot(aes(x = year, y = mean_value, color = groups_plots)) +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#1E90FF", "Other NYC Schools" = "#CFCFC4")) +
  geom_line() +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "purple") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "purple") +
  labs(title = "Average English Scores",
       x = "Year",
       y = "Average English Score (Weighted by Enrollment)",
       color = "Groups of Schools:") +
  theme_minimal()

print(english_plot)

#-------------------------------------------------------------------------------
# 10. Time Series on Math and English Scores before 2014
#-------------------------------------------------------------------------------

#Getting the score data on School-year level
scores_school_year <- ELA_math_grades %>% 
  filter(aggregation == "All Students") %>% 
  select(DBN, year, subject, number_tested, mean_score) %>% 
  group_by(DBN, year, subject) %>% 
  summarise(w_avg_score = weighted.mean(mean_score, number_tested, na.rm = TRUE),
            school_num_tested = sum(number_tested))

#Adding information on renewal and control groups
groups <- quality_review %>% 
  select(DBN, year, ever_renewal, groups)

scores_school_year <- scores_school_year %>% 
  left_join(groups, by = "DBN") %>% 
  select(-year.y) %>% 
  rename("year" = "year.x")

table(groups$year, groups$groups, useNA = "always")
table(scores_school_year$year, scores_school_year$groups, useNA = "always")

scores_school_year <- scores_school_year %>% 
  mutate(groups_plot = case_when(
    groups == "Renewal" ~ "Renewal",
    groups == "Comparison" ~ "Comparison",
    is.na(groups) ~ "Other NYC Schools" 
  ))

table(scores_school_year$year, scores_school_year$groups_plot, useNA = "always")

#Standardizing the scores by subject and year
df_standardized <- scores_school_year %>%
  group_by(year) %>%
  mutate(
    mean_score = mean(w_avg_score, na.rm = TRUE),
    sd_score = sd(w_avg_score, na.rm = TRUE),
    standardized_score = (w_avg_score - mean_score) / sd_score
  )

# Saving the standardized dataframe
save(df_standardized, file = "Dataframes/df_standardized.RData")

# Time series plot Math:
math_pre14_plot <- df_standardized %>%
  filter(subject == "math") %>%
  select(standardized_score, school_num_tested, groups_plot, year) %>%
  filter(!is.na(standardized_score)) %>%
  group_by(groups_plot, year) %>%
  summarise(
    mean_value = weighted.mean(standardized_score, w = school_num_tested, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = year, y = mean_value, color = groups_plot, group = groups_plot)) +
  geom_line() +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#1E90FF", "Other NYC Schools" = "#CFCFC4")) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "purple") +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "purple") +
  labs(title = "Average Math Scores",
       x = "Year",
       y = "Average Math Score (Weighted by Enrollment)",
       color = "Groups of Schools:") +
  theme_minimal()

print(math_pre14_plot)


# Time series plot English:
engl_pre14_plot <- df_standardized %>%
  filter(subject == "ELA") %>%
  select(standardized_score, school_num_tested, groups_plot, year) %>%
  filter(!is.na(standardized_score)) %>%
  group_by(groups_plot, year) %>%
  summarise(
    mean_value = weighted.mean(standardized_score, w = school_num_tested, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = year, y = mean_value, color = groups_plot, group = groups_plot)) +
  geom_line() +
  scale_color_manual(values = c("Renewal" = "#008080", "Comparison" = "#1E90FF", "Other NYC Schools" = "#CFCFC4")) +
  geom_vline(xintercept = "2015", linetype = "dashed", color = "purple") +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "purple") +
  labs(title = "Average English Scores",
       x = "Year",
       y = "Average English Score (Weighted by Enrollment)",
       color = "Groups of Schools:") +
  theme_minimal()

print(engl_pre14_plot)

#-------------------------------------------------------------------------------
# 11. DiD Table: Renewal vs. Comparison, before vs. during the Program
#-------------------------------------------------------------------------------
# Creating a time dummy: 0 - before and 1: during
DiD_quality <- quality_review %>%
  filter(year<2020) %>% 
  mutate(time_program = case_when(
   year < 2016 ~ 0,
   year < 2020 ~ 1,
   TRUE ~ NA
  ))

did_math <- lm(avg_english_score ~ groups_dummy * time_program, data = DiD_quality,
               na.action = na.exclude)

perform_t_test_on_linear_combination <- function(lm_model, combination_vector) {
  # Ensure names are provided in the combination vector
  if (is.null(names(combination_vector))) {
    stop("combination_vector must have named elements corresponding to model predictors.")
  }
  
  # Extract the coefficients and their variance-covariance matrix
  coefficients <- coef(lm_model)[names(combination_vector)]
  vcov_matrix <- vcov(lm_model)[names(combination_vector), names(combination_vector)]
  
  # Compute the linear combination of coefficients
  linear_combination <- sum(coefficients * combination_vector)
  
  # Calculate the standard error of the linear combination
  combination_se <- sqrt(sum((combination_vector^2) * diag(vcov_matrix)))
  
  # Calculate the t statistic
  t_statistic <- linear_combination / combination_se
  
  # Get the degrees of freedom from the model
  df <- lm_model$df.residual
  
  # Calculate the p-value from the t distribution
  p_value <- 2 * pt(-abs(t_statistic), df)
  
  # Return a list containing the results
  return(list(
    linear_combination = linear_combination,
    SE = combination_se,
    t_statistic = t_statistic,
    df = df,
    p_value = p_value
  ))
}

# Apply the function to your linear model
# The linear combination for DiD is usually the coefficient of the interaction term
# which is the product of groups_dummy and time_program
# In R model syntax, this interaction is denoted by `groups_dummy:time_program`

# Create the combination vector for the interaction term
ac <- c("groups_dummy" = 0, "time_program" = 1, "groups_dummy:time_program" = 0)
at <- c("groups_dummy" = 1, "time_program" = 1, "groups_dummy:time_program" = 1)
bt <- c("groups_dummy" = 1, "time_program" = 0, "groups_dummy:time_program" = 0)

# Apply the function to the model
after_control <- perform_t_test_on_linear_combination(did_math, ac)
after_treatment <- perform_t_test_on_linear_combination(did_math, at)
before_treatment <- perform_t_test_on_linear_combination(did_math, at)

# Print the results
print(after_control)

################################################################################
#                           II. REGRESSIONS
################################################################################

#-------------------------------------------------------------------------------
# 1. Preparing the Data
#-------------------------------------------------------------------------------

#Loading the dataframes
load("Dataframes/quality_review.RData")

# Excluding data from 2020
quality_review_pre20 <- quality_review %>% 
  filter(year < 2020)

# As there are 25 districts in each group, we decided not to cluster the SE by districts, we're using the approach of robust SEs
quality_review_pre20 %>% 
  group_by(groups) %>% 
  summarise(n_districts = n_distinct(district))


#-------------------------------------------------------------------------------
# 2. Fixed Effects - Math Scores
#-------------------------------------------------------------------------------
#store models for display
models_math <- list(
  "FE 1" = feols(avg_math_score ~ groups_dummy 
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero"),
  "FE 2" = feols(avg_math_score ~ groups_dummy + econ_need_index + pct_black + pct_hispanic + pct_ELL
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero"),
  "FE 3" = feols(avg_math_score ~ groups_dummy + econ_need_index + pct_temp_housing + pct_HRA_eligib + pct_black + pct_hispanic + pct_ELL + pct_disabilities
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero")
)

#set up an object to customize the regression table goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,                 ~clean,            ~fmt,
  "nobs",               "N",                  0,
  "r.squared",          "R²",                 3,
  "adj.r.squared",      "Adj. R²",            3,
  "FE: year",           "Year FEs",           0,
  "FE: DBN",            "Schools FEs",        0
)

#display results with added formatting options    
modelsummary(models_math,
             coef_omit = 'Intercept',
             coef_rename = c("groups_dummy" = "Renewal Schools", 
                             "econ_need_index" = "Economic Need Index",
                             "pct_black" = "% Black",
                             "pct_hispanic" = "% Hispanic",
                             "pct_ELL" = "% ELL",
                             "pct_temp_housing" = "% in Temporary Housing",
                             "pct_HRA_eligib" = "% eligible for HRA",
                             "pct_disabilities" = "% with disabilities"),
             gof_map = gm,
             stars = c('*' = .1, '**' = .05, '***' = .01))


#-------------------------------------------------------------------------------
# 3. Fixed Effects - English Scores
#-------------------------------------------------------------------------------

#store models for display
models_engl <- list(
  "FE 1" = feols(avg_english_score ~ groups_dummy 
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero"),
  "FE 2" = feols(avg_english_score ~ groups_dummy + econ_need_index + pct_black + pct_hispanic + pct_ELL
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero"),
  "FE 3" = feols(avg_english_score ~ groups_dummy + econ_need_index + pct_temp_housing + pct_HRA_eligib + pct_black + pct_hispanic + pct_ELL + pct_disabilities
                 | DBN + year,
                 data = quality_review_pre20,
                 weight = quality_review_pre20$enrollment,
                 vcov = "hetero")
)

#display results with added formatting options    
modelsummary(models_engl,
             coef_omit = 'Intercept',
             coef_rename = c("groups_dummy" = "Renewal Schools", 
                             "econ_need_index" = "Economic Need Index",
                             "pct_black" = "% Black",
                             "pct_hispanic" = "% Hispanic",
                             "pct_ELL" = "% ELL",
                             "pct_temp_housing" = "% in Temporary Housing",
                             "pct_HRA_eligib" = "% eligible for HRA",
                             "pct_disabilities" = "% with disabilities"),
             gof_map = gm,
             stars = c('*' = .1, '**' = .05, '***' = .01))

#-------------------------------------------------------------------------------
# 4. Simple Difference-in-Differences Model
#-------------------------------------------------------------------------------

# Preparing the Data
DiD_quality <- quality_review %>%
  filter(year<2020) %>% 
  mutate(time_program = case_when(
    year < 2016 ~ 0,
    year < 2020 ~ 1,
    TRUE ~ NA
  ))

# DiD for Elem
models_DiD <- list(
  "DiD Math 1" = lm(avg_english_score ~ groups_dummy * time_program,
                    data = DiD_quality,
                    na.action = na.exclude),
  "DiD Math 2" = lm(avg_english_score ~ groups_dummy * time_program+ econ_need_index + pct_black + pct_hispanic + pct_ELL,
                    data = DiD_quality,
                    na.action = na.exclude),
  "DiD English 1" = lm(avg_english_score ~ groups_dummy * time_program,
                       data = DiD_quality,
                       na.action = na.exclude),
  "DiD English 2" = lm(avg_english_score ~ groups_dummy * time_program + econ_need_index + pct_black + pct_hispanic + pct_ELL, 
                       data = DiD_quality,
                       na.action = na.exclude)
)

#set up an object to customize the regression table goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,                 ~clean,    ~fmt,
  "nobs",               "N",       0,
  "r.squared",          "R²",      3,
  "adj.r.squared",      "Adj. R²", 3
)

#display results with added formatting options    
modelsummary(models_DiD,
             coef_rename = c("groups_dummy" = "Renewal", 
                             "time_program" = "After",
                             "groups_dummy:time_program" = "Renewal * After",
                             "econ_need_index" = "Economic Need Index",
                             "pct_black" = "% Black",
                             "pct_hispanic" = "% Hispanic",
                             "pct_ELL" = "% ELL"),
             gof_map = gm,
             stars = c('*' = .1, '**' = .05, '***' = .01))