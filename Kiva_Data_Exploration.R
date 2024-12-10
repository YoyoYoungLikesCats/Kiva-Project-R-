# Top sectors 
top_sectors <- df_clean %>%
  count(sector)%>%
  arrange(desc(n)) %>%
  head(10)

print(top_sectors)

# Top countries
top_countries <- df_clean %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10)

print(top_countries)

# Top field partners
top_field_partners <- df_clean %>%
  count(field_partner_name) %>%
  arrange(desc(n)) %>%
  head(10)

print(top_field_partners)

# Average loan amount by activity
average_loan_per_activity <- df_clean %>%
  group_by(activity) %>%
  summarise(avg_loan_amount = mean(loan_amount, na.rm = TRUE)) %>%
  arrange(desc(avg_loan_amount)) %>%
  head(10)

print(average_loan_per_activity)

# Average loan amount by sector
average_loan_per_sector <- df_clean %>%
  group_by(sector) %>%
  summarise(avg_loan_amount = mean(loan_amount, na.rm = TRUE)) %>%
  arrange(desc(avg_loan_amount)) 

print(average_loan_per_sector)

# Seeing if there is a correlation between a loan amount not being completely funded, and sector? Or maybe loan amount?
df_missmatch_test <- df_clean %>%
  mutate(amount_mismatch = ifelse(funded_amount != loan_amount, 1, 0))

sectors_with_mismatch_percentage <- df_missmatch_test %>%
  group_by(sector) %>%
  summarise(
    mismatched_count = sum(amount_mismatch == 1),
    total_count = n(),
    mismatched_percentage = (mismatched_count / total_count) * 100
  ) %>%
  arrange(desc(mismatched_percentage))

print(sectors_with_mismatch_percentage)
#       Seeing as housing and Transportation are the top two, lets see if that's because those sectors ask for more money (to explain wy they might not be fully funded)
housing_transportation <- average_loan_per_sector %>%
  filter(sector %in% c("Housing", "Transportation"))

print(housing_transportation) # hmm I guess not.

# plotting loan amount distribution with sector as color
ggplot(df_clean, aes(x = loan_amount, color = sector)) +
  geom_density() +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(
    title = "Distribution of Loan Amounts by Sector",
    x = "Loan Amount",
    y = "Density",
    color = "Sector"
  ) +
  theme_minimal()

# Top 10 largest loans (based on the graph, there are some big ones)
top_10_loans <- df_clean %>%
  arrange(desc(loan_amount)) %>%  
  select(loan_amount, sector, activity, country, date) %>%  
  slice_head(n = 10)  

top_10_loans$loan_amount <- scales::label_number()(top_10_loans$loan_amount) # fixing the scientific notation

print(top_10_loans)

# Date Range of our data
min(df_clean$date_char) # Earliest
max(df_clean$date_char) # Most Recent

# Average loan term (months), and loan amount, number of people borrowers, by sector
info_by_sector <- df_clean %>%
  group_by(sector) %>%
  summarize(average_loan_term_in_months = mean(term_in_months),
            average_loan_amount = mean(loan_amount),
            average_number_of_borrowers = mean(total_borrowers)) %>%
  arrange(desc(average_loan_term_in_months))
print(info_by_sector)

# List of Distinct Sectors (more broad)
df_distinct_sector <- df_clean$sector %>% unique()
df_distinct_sector

# List of Distinct Activities (more specific)
df_distinct_activity <- df_clean$activity %>% unique()
df_distinct_activity

# Lets see which actitivies are catagorized as which sectors
sector_activities <- df_clean %>%
  group_by(sector) %>%
  summarise(activities = paste(unique(activity), collapse = ", ")) %>%
  ungroup()  

print(sector_activities) # Using View() cuts them off
View(sector_activities)





