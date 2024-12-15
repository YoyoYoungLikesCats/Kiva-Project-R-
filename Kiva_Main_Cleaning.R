# Load Libraries

library(writexl) # for creating an excel file of the Final Tidy Data set
library(readxl)
library(ggthemes)
library(tidyverse)
library(sqldf)
library(scales)  # For label formatting in one of the plots (I think its in tidyverse already)

# Importing Raw Data (Put on Github and change all of the Read Links)
loans_df <- read.csv("./Kiva_Raw_csv/kiva_loans.csv")
mpi_region <- read.csv("./Kiva_Raw_csv/kiva_mpi_region_locations.csv")
loan_theme <- read.csv("./Kiva_Raw_csv/loan_theme_ids.csv") 
loan_theme_by_region <- read.csv("./Kiva_Raw_csv/loan_themes_by_region.csv")
lenders <- read.csv("./Kiva_Raw_csv/lenders.csv")
loans_lenders <- read.csv("./Kiva_Raw_csv/loans_lenders.csv")
    # separate excel file for detailed description of what each field represents
    # field_descriptions <- read_xlsx("./Kiva_Raw_csv/kiva_field_descriptions.xlsx") 
    # ^^ Didnt really end up using that for now
    
dim(loans_df)
dim(mpi_region)
dim(loan_theme)
dim(loan_theme_by_region)
dim(lenders)
dim(loans_lenders)

 
 
 

#-----------------------------------Cleaning----------------------------------------------------------
    
# Have to remove the "." first in the field names and making it lowercase for consistency (and so I can use JOIN functions)
loan_theme_by_region <- loan_theme_by_region %>% rename_with(~ gsub("\\.", "_", tolower(.)), everything())
colnames(loan_theme_by_region) # checking to see that it worked

# Adding the Partner name to the main df 
field_partner <- subset(loan_theme_by_region, select = c(partner_id, field_partner_name))
field_partner <- field_partner %>% distinct()
#View(field_partner)

sql_joining_field_partner <- "SELECT*
                              FROM loans_df
                              LEFT JOIN field_partner
                              USING(partner_id);"
loans_df_iteration_2 <- sqldf(sql_joining_field_partner)

# omitting the tags column (because it's unclear what it represents)
loans_df_iteration_3 <- loans_df_iteration_2 %>% select(-tags)
#View(loans_df_iteration_3)

# Seeing which fields have nulls
colSums(is.na(loans_df_iteration_3)) # only field partner name contains nulls

# Deleting all rows where there is no partner_name associated with the partner_id (we have a lot of data, so the data set is still robust)
sql_test <-  "SELECT partner_id, field_partner_name
              FROM loans_df_iteration_3
              WHERE partner_id IS NOT NULL AND field_partner_name IS NULL;"
null_partner_name <- sqldf(sql_test)
#View(head(null_partner_name))
nrow(null_partner_name) # how many partner_id's aren't associated with a name

df_clean <- na.omit(loans_df_iteration_3)

# Moving the partner_id to be next to the partner_name for clarity
df_clean <- df_clean %>% select(-partner_id, -field_partner_name, everything(), partner_id, field_partner_name)
#View(df_clean)

# breaking gender into different categories 
df_clean$borrower_genders <- as.factor(df_clean$borrower_genders) # factoring it so I can count the distinct
levels(df_clean$borrower_genders) 
df_clean$borrower_genders <- as.character(df_clean$borrower_genders) #turning it back to a char
#     OK so there are a lot of different combos of gender...
#     Maybe try splitting the gender category into two fields, male, and female, and count the # of each
df_clean <- df_clean %>%
  mutate(
    female_borrowers = sapply(strsplit(borrower_genders, ","), function(x) sum(trimws(x) == "female")),
    male_borrowers = sapply(strsplit(borrower_genders, ","), function(x) sum(trimws(x) == "male")),
    total_borrowers = female_borrowers + male_borrowers
  )
#View(df_clean) # nice it worked, now lets remove the original gender column
df_clean <- df_clean %>%
  select(-borrower_genders)

#View(df_clean)

# Initially assumed funded amount and ask amount was the same, and was going to delete one of the fields
# This shows that funded amount is not always equal to the ask amount
# Will use this info later in the analysis 
identical(df_clean$loan_amount, df_clean$funded_amount) # False apparently
non_matching_rows <- df_clean[df_clean$loan_amount != df_clean$funded_amount, ]
#View(head(non_matching_rows, 5)) # Seeing when they are different

# Joining the lender usernames to the main df (test)
# Checking out the lenders <-> loans
#View(loans_lenders) # This df seems pretty straight forward
nrow(loans_lenders)
nrow(df_clean)# way more lender ids than there are rows in our data set to match
sum(is.na(loans_lenders)) # Ok no nulls, good. 
sql_joining_lenders <- "SELECT *
                        FROM df_clean
                        LEFT JOIN loans_lenders
                        ON df_clean.id = loans_lenders.loan_id"

test_lenders <- sqldf(sql_joining_lenders)
test_lenders <- test_lenders %>% select(-loan_id)
#view(test_lenders)
sum(is.na(test_lenders$lenders) | test_lenders$lenders == "") # ok so, not every row found a matching list of lender(s)
dim(test_lenders) # But I still have a lot of observations to work with if I choose to delete rows with nulls
#View(test_lenders)# NICE IT WORKED!!! But now I need to figure out how to use this...

df_clean <- test_lenders # The test worked so I'll lock that in 

#View(df_clean) # Good

# Now lets change posted disbursed and funded time to something readable (char)
# We can always change it back when we need to use it to analyse stuff

test_datetime <- df_clean # Just in case I fk up
test_datetime$posted_time <- as.POSIXct(test_datetime$posted_time, tz = "UTC") # Convert the 'datetime' column to POSIXct
test_datetime$posted_time <- format(test_datetime$posted_time, "%A, %B %d, %Y %I:%M:%S %p %Z") # Format and replace the column
#View(test_datetime) # ok it worked, lets do it for the other 2 (disbursed and funded time)

test_datetime$disbursed_time <- as.POSIXct(test_datetime$disbursed_time, tz = "UTC") # Convert the 'datetime' column to POSIXct
test_datetime$disbursed_time <- format(test_datetime$disbursed_time, "%A, %B %d, %Y %I:%M:%S %p %Z") # Format and replace the column

#test_datetime$funded_time <- as.POSIXct(test_datetime$funded_time, tz = "UTC") # Convert the 'datetime' column to POSIXct
#test_datetime$funded_time <- format(test_datetime$funded_time, "%A, %B %d, %Y %I:%M:%S %p %Z") # Format and replace the column
# There is an error for converting funded time... Maybe bc there are nulls in that column
colSums(is.na(df_clean))
colSums(df_clean == "")

test_datetime$funded_time[test_datetime$funded_time == "" | is.na(test_datetime$funded_time)] <- NA # replacing missing entries with NA
test_datetime$funded_time <- as.POSIXct(test_datetime$funded_time, tz = "UTC") # Convert the 'datetime' column to POSIXct
test_datetime$funded_time <- format(test_datetime$funded_time, "%A, %B %d, %Y %I:%M:%S %p %Z") # Format and replace the column

#View(test_datetime) # ok yeah it didn't work because some entries are missing, Now it works
df_clean <- test_datetime # Cementing the change 


#View(df_clean)
colSums(is.na(df_clean) | df_clean == "")

# Last minor touch up (Moving lender total to the end of the df, so its next to the actual names)
df_cleaned_main <- df_clean %>%
  select(-lenders,-lender_count, everything(), lender_count,lenders)
#View(df_cleaned_main)
df_clean <- df_cleaned_main 

# Making date more readable to a human
df_clean$date <- as.Date(df_clean$date, format = "%Y-%m-%d") # formatting original char to date
df_clean$date_char <- format(df_clean$date, format = "%B %d, %Y") # adding a column for date in words
df_clean <- df_clean %>%
  select(-date)

# Taking a look at the other data frame we are working with 
#View(lenders)
colSums(is.na(lenders)) # well this doesn't work, bc some of the values are empty (" "), not Null (n/a)
colSums(lenders != "" & !is.na(lenders)) # ok, so there's a lot of non responses, but I'll leave it for now

# This is a data frame of complete cases
non_null_test <- lenders[complete.cases(lenders) & !apply(lenders == "", 1, any), ]
#View(non_null_test)
dim(non_null_test) # only 16k answers




#-----------------------------------------Final cleaned data sets------------------------------------------------------
View(df_clean) # This is the main data set 
colSums(is.na(df_clean) | df_clean == "")

# Making an excel file of cleaned data to share to other group members
# write_xlsx(df_clean, "cleaned_forSharing1")


View(lenders) # This is the info on each individual lender, might use in analysis
colSums(lenders != "" & !is.na(lenders)) # ok, so there's a lot of non responses

  # I think its good to go. There are some instances of nulls or missing, but they aren't in fields that are crucial





