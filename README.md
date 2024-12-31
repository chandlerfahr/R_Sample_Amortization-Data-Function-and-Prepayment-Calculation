# R Sample: Amortization-Data-Function-and-Prepayment-Calculation
# This code builds a random loan data set and defines a function that output a data set that contains an amortization schedule for each loans.
# The amortization schedule has a variety of utilities - it's potential use for prepayment calculation is developed in this script. 

# The amortization schedule dataframe contains the remaining term for each of our 100 loans, with each row represents a payment period (in this case months).

# A potential use of this dataframe is to calculate prepayment rates. 
# Running the function on historic data, one can then compare the scheduled balance to the actual principal balance today.
# The difference in these values reveals what has been prepaid between the two dates at the loan level.
# Aggregating this data, one can find prepayments on aggregated loan types over a given period of time. 
