```{r, echo=FALSE}
# Sample data
data <- c(4.8, 5.1, 5.3, 4.9, 5.2, 5.0, 4.7, 5.3, 5.1, 5.0)

# Perform a z-test
hypothesized_value <- 5
significance_level <- 0.05
population_sd <- 0.2 

# Calculate the sample mean and standard deviation
sample_mean <- mean(data)
# sample_sd <- sd(data)

# Calculate the standard error (assuming population standard deviation is known)
n <- length(data)
standard_error <- population_sd / sqrt(n)

# Calculate the z-statistic
z_statistic <- (sample_mean - hypothesized_value) / standard_error

# Determine the critical value (two-tailed test)
critical_value <- qnorm(1 - (significance_level / 2))

# Calculate the p-value (two-tailed test)
p_value <- 2 * (1 - pnorm(abs(z_statistic)))

# # Compare the z-statistic with the critical value
# if (abs(z_statistic) < critical_value) {
#   z_decision <- "Fail to reject the null hypothesis"
# } else {
#   z_decision <- "Reject the null hypothesis"
# }

# Compare the p-value with the significance level
if (p_value > significance_level) {
  p_decision <- "Fail to reject the null hypothesis"
} else {
  p_decision <- "Reject the null hypothesis"
}

# Create the comparison statement comparing the z-statistic and the critical value
comparison_statement <- paste(
  "The z-statistic (", z_statistic, ") is",
  if (abs(z_statistic) < critical_value) "smaller than" else "larger than or equal to",
  "the critical value (", critical_value, ").\n",
  "Therefore, we",
  if (abs(z_statistic) < critical_value)
    "fail to reject" else "reject","the null hypothesis."
)

# Create the comparison statement comparing the p-value and the significance level
comparison_statement <- paste(
  comparison_statement,
  "\n \n Additionally, the p-value (", p_value, ") is",
  if (p_value > significance_level) "larger than" else "smaller than or equal to",
  "the significance level (", significance_level, ").", "\n",
  "Therefore, we", if (p_value > significance_level) "fail to reject" else "reject",
  "the null hypothesis."
)

# Display the results and the comparison statement
cat("Z-Test Results:\n")
cat("Sample mean:", sample_mean, "\n")
cat("Population standard deviation:", population_sd, "\n")
cat("Hypothesized value:", hypothesized_value, "\n")
cat("Z-statistic:", z_statistic, "\n")
cat("Critical value:", critical_value, "\n")
# cat("Decision (based on z-statistic):", z_decision, "\n\n")
cat("P-value:", p_value, "\n")
cat("Significance level:", significance_level, "\n")
# cat("Decision (based on p-value):", p_decision, "\n\n")
cat("Comparison:\n")
cat(comparison_statement, "\n")
```

