library(sqldf)


#package
Kiva_loans <- read_xlsx("~/Downloads/cleaned_forSharing1.xlsx")

#Query 1: How many loan amount's were funded
query1 <- sqldf("
  SELECT COUNT(*) AS count_same
  FROM Kiva_loans
  WHERE funded_amount = loan_amount
")

print(query1)



#Idea 2, Query 2: Top Funded Sectors
library(sqldf)

query2 <- sqldf("
  SELECT sector, AVG(funded_amount) AS avg_funded_amount
  FROM Kiva_loans
  GROUP BY sector
  ORDER BY avg_funded_amount DESC
  LIMIT 10
")
print(query2)


#Idea 3, Query 3: Repayment Intervals Distribution
query3 <- sqldf("
  SELECT repayment_interval, COUNT(*) AS count
  FROM Kiva_loans
  GROUP BY repayment_interval
  ORDER BY count DESC
")

print(query3)



#Query 4: Loans with Longest Terms

Kiva_loans$disbursed_time <- as.Date(Kiva_loans$disbursed_time)
Kiva_loans$funded_time <- as.Date(Kiva_loans$funded_time)

# Use sqldf to calculate the top 5 loans with the longest terms
query4 <- sqldf("
  SELECT *, 
         (julianday(disbursed_time) - julianday(funded_time)) AS loan_duration_days
  FROM Kiva_loans
  ORDER BY loan_duration_days DESC
  LIMIT 10
")



#Idea 5, Query 5: Most Frequent Loan Activities
query5 <- sqldf("
  SELECT activity, COUNT(*) AS frequency
  FROM Kiva_loans
  GROUP BY activity
  ORDER BY frequency DESC
  LIMIT 10
")

#Idea 6 for Query 6-8: Repayment Interval Impact on Funding)

# Query 6: Average funded amount by repayment interval
query6 <- sqldf("
  SELECT repayment_interval, AVG(funded_amount) AS avg_funded_amount
  FROM Kiva_loans
  GROUP BY repayment_interval
  ORDER BY avg_funded_amount DESC
")

# Query 7: Average lender count by repayment interval
query7 <- sqldf("
  SELECT repayment_interval, AVG(lender_count) AS avg_lender_count
  FROM Kiva_loans
  GROUP BY repayment_interval
  ORDER BY avg_lender_count DESC
")

# Combine results for comparison

query8 <- sqldf("
  SELECT q6.repayment_interval, 
         q6.avg_funded_amount, 
         q7.avg_lender_count
  FROM query6 q6
  INNER JOIN query7 q7
  ON q6.repayment_interval = q7.repayment_interval
  ORDER BY q6.avg_funded_amount DESC
")

print(query8)


#Idea 7 for Query 9-11: 

# Query 9: Total funded amount and loan amount by sector
query9 <- sqldf("
  SELECT sector, 
         SUM(funded_amount) AS total_funded_amount, 
         SUM(loan_amount) AS total_loan_amount
  FROM Kiva_loans
  GROUP BY sector
  ORDER BY total_funded_amount DESC
")

# Query 10: Total funding shortfall for loans not fully funded
query10 <- sqldf("
  SELECT sector, 
         SUM(loan_amount - funded_amount) AS total_shortfall
  FROM Kiva_loans
  WHERE loan_amount > funded_amount
  GROUP BY sector
  ORDER BY total_shortfall DESC
")


query11 <- sqldf("
  SELECT q9.sector, 
         q9.total_loan_amount, 
         q10.total_shortfall
  FROM query9 q9
  LEFT JOIN query10 q10
  ON q9.sector = q10.sector
  ORDER BY q9.total_funded_amount DESC
")

# View the final comparison
print(query11)


# Idea 8 for query 12-13
query12 <- sqldf("
    SELECT activity, COUNT(*) AS frequency
    FROM Kiva_loans
    WHERE male_borrowers >= 1
    GROUP BY activity
    ORDER BY frequency DESC
    LIMIT 10
")

query13 <- sqldf("
    SELECT activity, COUNT(*) AS frequency
    FROM Kiva_loans
    WHERE female_borrowers >= 1
    GROUP BY activity
    ORDER BY frequency DESC
    LIMIT 10
")
print(query12)
print(query13)