confi_inter <- function(n, Sigma0Squared, SSquared, alpha)
{
  df <- n - 1
  v <- df*(SSquared)/Sigma0Squared
  upper.critical <- qchisq((1-alpha), df, lower.tail = FALSE) # right-sided test
  
  # lower.critical <- qchisq((alpha), df, lower.tail = FALSE) # left-sided test
  
  # upper.critical <- qchisq((1-alpha)/2, df, lower.tail = FALSE) # two-sided test
  # lower.critical <- qchisq(alpha/2, df, lower.tail = FALSE) # two-sided test
  
  print(paste("degrees of freedom = ", df), quote = FALSE)
  print(paste("population variance = ", Sigma0Squared), quote = FALSE)
  print(paste("sample variance = ", SSquared), quote = FALSE) 
  print(paste("significance level = ", alpha), quote = FALSE)
  print(paste("confidence level = ", 1-alpha), quote = FALSE)
  print("  ")
  print(paste((1-alpha)*100, "% confidence interval for variance"), quote=FALSE)
  print(paste("   upper critical limit = ", round(upper.critical, 2)), quote = FALSE)
  print(paste("   chisq statistic      = ", round(v, 2)), quote = FALSE)
  print("  ")
  print(paste("P-value =", round(pchisq(v, df, lower.tail=FALSE),4)), quote = FALSE)
}