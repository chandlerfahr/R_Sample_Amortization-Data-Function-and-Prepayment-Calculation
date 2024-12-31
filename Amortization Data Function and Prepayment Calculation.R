# The following code builds a random loan data set and defines a function that output a data set that contains an amortization schedule for each loans.
# The amortization schedule has a variety of utilities - it's potential use for prepayment calculation is developed in this script. 

# Required Libraries 
library(tidyverse)
library(FinancialMath)
library(capitalR)

# Generate and transform the data
##############################################################################################
# set.seed(123)  # Uncomment set seed for reproducibility

# Dataframe of 100 loans. Assume they are fully amortizing with P&I payments. 
loans_df = data.frame(
  account = 1:100,  # Sequential account numbers
  principal = runif(100, 100000, 1000000),  # Random amounts between $100M and $1MM
  interest_rate = runif(100, 5, 10),  # Random interest rates between 5% and 10%
  ammortization = sample(c(120, 180, 300, 360), 100, replace = TRUE),  # Random initial amortization terms in months (120, 180, 300)
  date_opened = as.Date(Sys.Date() - sample(0:365*6, 100, replace = TRUE)), # Random date opened, within 6 years ago from today  
  date_updated = Sys.Date() - months(12) # Theoretical date balances are accrued through. Setting this to 12 months from the current date. 
  )

# Calculate the maturity date and remaining term

loans_df = loans_df %>% 
  mutate(
    maturity_date = date_opened %m+% months(ammortization),  # Calculate maturity date
    remaining_term = as.numeric(
      gsub(" days", "", maturity_date - date_updated) # Calculate remaining term in months - remove default " days" string when subtracting date columns 
                                ) 
    / 30  # Convert numeric and convert days to months 
  )

# Define a function to amortize the loan data 
##############################################################################################

amortize_loans = function(loans_df) {
  # Create an empty data frame to store the results
  result_df = data.frame()
  
  # Loop through each row in the input data frame
  for (i in 1:nrow(loans_df)) {
    # Extract loan details from the data frame we defined earlier 
    account = loans_df[i, "account"]
    principal = loans_df[i, "principal"]
    interest_rate = loans_df[i, "interest_rate"] / 100 / 12  # convert interest rate to months
    remaining_term = loans_df[i, "remaining_term"]
    
    # Calculate the monthly payment using the formula for a fixed-rate loan
    monthly_payment = (principal * interest_rate) / (1 - (1 + interest_rate)^(-remaining_term))
    
    # Create an amortization schedule data frame for this loan
    schedule = data.frame(
      account = account,
      month = 1:remaining_term,
      beginning_balance = rep(0, remaining_term),
      monthly_payment = rep(0, remaining_term),
      interest_payment = rep(0, remaining_term),
      principal_payment = rep(0, remaining_term),
      ending_balance = rep(0, remaining_term)
    )
    
    # Initialize the first row of the schedule
    schedule[1, "beginning_balance"] = principal
    schedule[1, "monthly_payment"] = monthly_payment
    
    # Calculate the amortization schedule for this loan
    for (j in 1:remaining_term) {
      schedule[j, "account"] = account
      schedule[j, "monthly_payment"] = schedule[1,4]
      schedule[j, "interest_payment"] = schedule[j, "beginning_balance"] * interest_rate
      schedule[j, "principal_payment"] = schedule[j, "monthly_payment"] - schedule[j, "interest_payment"]
      schedule[j, "ending_balance"] = schedule[j, "beginning_balance"] - schedule[j, "principal_payment"]
      
      # Update the beginning balance for the next period
      if (j < remaining_term) {
        schedule[j + 1, "beginning_balance"] = schedule[j, "ending_balance"]
      }
    }
    
    # Append the schedule for this loan to the result data frame
    result_df = rbind(result_df, schedule)
  }
  
  return(result_df)
}

# Run the function on the loans_df we defined. 
amortization_tables = amortize_loans(loans_df)

# The result is an amortization schedule of the remaining term for each of our 100 loans. 
# Each row represents a payment period (in this case months).

# A potential use of this dataframe is to calculate prepayment rates. 
# Running the function on historic data, one can then compare the scheduled balance to the actual principal balance today.
# The difference in these values reveals what has been prepaid between the two dates at the loan level.
# Aggregating this data, one can find prepayments on aggregated loan types over a given period of time. 

# To show how this can be done, first extract today's principal as it was scheduled 12 months prior to today 
scheduled_balances = amortization_tables %>% 
  filter(month==12) %>%
  select(account, ending_balance) %>%
  rename(scheduled_principal = ending_balance)


# Now create a another theoretical dataframe containing our 100 loans and their balances a year later.
# For the sake of the example, let's reduce all principal balances by a random amount between 1% and 3% for the current balance. 
loans_df_2 = loans_df %>%
  mutate(principal = principal * (1 - runif(n(), 0.01, 0.03))) %>% 
  select(account, principal) %>%
  rename(current_principal = principal)

# Join the dataframes 
prepayment_df = left_join(loans_df_2, scheduled_balances, by = join_by(account))

# Calculate the amount of principal that has been prepaid
prepayment_df = prepayment_df %>% 
  mutate(amount_prepaid = current_principal - scheduled_principal)





