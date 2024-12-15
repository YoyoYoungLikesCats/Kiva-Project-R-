library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(sf)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
install.packages("rworldmap")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
main_df <- read_xlsx("~/Desktop/DATA SCIENCE/cleaned_forSharing1.xlsx")

View(main_df)
#-------------------------------------------PIE CHART----------------------------------------------------------
#DO A PIE CHART ON GENDER
gender_summary <- main_df %>%
  summarize(
    male = sum(male_borrowers),
    female = sum(female_borrowers)
  )

#MAKE THE GENDER COUNT AS PERCENTAGES
gender_summary_long <- gender_summary %>%
  pivot_longer(cols = everything(), names_to = "gender", values_to = "count") %>%
  mutate(percentage = count / sum(count) * 100)

print(gender_summary_long)

#MAKE IT INTO A PIE CHART 
ggplot(gender_summary_long, aes(x = "", y = percentage, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) + 
  labs(title = "GENDER DISTRIBUTION OF BORROWERS", x = NULL, y = NULL) +
  theme_void() + 
  theme(legend.title = element_blank())

#-------------------------------------------WORLD HEAT MAP----------------------------------------------------------
install.packages("rworldmap")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
library(sf)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)

#CREATE A WORLD HEAT MAP 
world_map <- ne_countries(scale = "medium", returnclass = "sf")

#AGREGATE LOANS 
aggregated_df <- main_df %>%
  group_by(country) %>%
  summarise(total_loan_amount = sum(loan_amount, na.rm = TRUE))

#MERGE WORLD_MAP AND MAIN_DF
map_data <- world_map %>%
  left_join(aggregated_df, by = c("admin" = "country"))

#CREATE A HEAT MAP OF THE LOAN AMOUNT PER COUNTRY 
ggplot(map_data) +
  geom_sf(aes(fill = total_loan_amount), color = "white", size = 0.1) +  
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", na.value = "grey80",
    labels = scales::label_comma()  
  ) +  # Define the color gradient
  labs(
    title = "World Heat Map: Total Loan Amount",
    fill = "Total Loan Amount"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
#TO SHOW THE TOP 10 COUNTRIES BY TOTAL LOAN AMOUNT 
aggregated_df <- main_df %>%
  group_by(country) %>%
  summarise(total_loan_amount = sum(loan_amount, na.rm = TRUE)) %>%
  arrange(desc(total_loan_amount)) %>%  
  slice_head(n = 10)

aggregated_df <- aggregated_df %>%
  mutate(total_loan_amount = scales::comma(total_loan_amount))

view(aggregated_df)
#-------------------------------------------TOP DONATORS----------------------------------------------------------
#SHOW THE TOP DONATORS
topdonators_df <- main_df %>%
  group_by(field_partner_name) %>%
  summarise(total_loan_amount = sum(loan_amount, na.rm = TRUE)) %>% 
  arrange(desc(total_loan_amount)) %>%  
  slice_head(n = 10)  

topdonators_df <- topdonators_df %>%
  mutate(total_loan_amount = scales::comma(total_loan_amount))

View(topdonators_df)

#-------------------------------------------SECTORS----------------------------------------------------------
#AVERAGE NUMBER OF LENDERS BY SECTOR
ggplot(main_df %>%
         group_by(sector) %>%
         summarise(avg_lender_count = mean(lender_count, na.rm = TRUE)) %>%
         arrange(desc(avg_lender_count)),
       aes(x = reorder(sector, avg_lender_count), y = avg_lender_count, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line connecting sectors
  geom_point(color = "red", size = 3) +  # Points for each sector
  labs(
    title = "Average Lender Count by Sector",
    x = "Sector",
    y = "Average Lender Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#AVERAGE TOTAL FUNDED AMOUNT BY SECTOR
ggplot(main_df %>%
         group_by(sector) %>%
         summarise(avg_funded_amount = mean(funded_amount, na.rm = TRUE)) %>%
         arrange(desc(avg_funded_amount)),
       aes(x = reorder(sector, avg_funded_amount), y = avg_funded_amount, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line connecting sectors
  geom_point(color = "red", size = 3) +  # Points for each sector
  labs(
    title = "Average Total Funded Amount by Sector",
    x = "Sector",
    y = "Average Funded Amount"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#LOOK AT THE TOP 10 COUNTRIES BY LOAN AMOUNT MOST POPULAR SECTORS
country_totals <- main_df %>%
  group_by(country) %>%
  summarise(total_loan_amount = sum(loan_amount, na.rm = TRUE)) %>%
  arrange(desc(total_loan_amount)) %>%
  slice_head(n = 10)

top_countries_sectors <- main_df %>%
  filter(country %in% country_totals$country) %>%  
  group_by(country, sector) %>%
  summarise(total_sector_loan = sum(loan_amount, na.rm = TRUE), .groups = "drop") %>%
  arrange(country, desc(total_sector_loan)) %>%  
  group_by(country) %>%
  slice_head(n = 1)

top_countries_sectors <- top_countries_sectors %>%
  mutate(total_sector_loan = scales::comma(total_sector_loan))

View(top_countries_sectors)

#MAKE IT A BAR GRAPH 
ggplot(top_countries_sectors, aes(x = reorder(country, -total_sector_loan), y = total_sector_loan, fill = sector)) +
  geom_bar(stat = "identity", color = "black") +  # Bar graph
  labs(
    title = "Top 10 Countries by Loan Amount and Their Most Popular Sector",
    x = "Country",
    y = "Total Loan Amount",
    fill = "Sector"
  ) +
  scale_y_continuous(labels = comma) +  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
