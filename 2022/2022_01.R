# Data Import -------------------------------------------------------------

reindeer_snack_data <- read_csv("2022/inputs/2022_01.txt",
                                 col_names = "calorie_value",
                                 skip_empty_rows = FALSE)

# Data Cleaning -----------------------------------------------------------
## Need a way to group the data using the empty rows.

reindeer_snacks <- reindeer_snack_data %>%
  mutate(reindeer_id = cumsum(is.na(calorie_value))+1) %>%
  drop_na() %>%
  group_by(reindeer_id) %>%
  summarise(total_calories = sum(calorie_value))


# Part 1 ------------------------------------------------------------------

## What's the max value?
max(reindeer_snacks$total_calories)


# Part 2 ------------------------------------------------------------------

## What's the sum of the top three values?
top_reindeer_snacks <- reindeer_snacks%>%
  arrange(desc(total_calories)) %>%
  slice_head(n=3) %>%
  ungroup() %>%
  summarise(top_three = sum(total_calories))
