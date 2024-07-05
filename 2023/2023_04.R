data <- read_fwf("2023/input/2023_04.txt",
                 fwf_widths(c(10,29,NA)))
## Clean up the data
clean_data <- data %>%
  # name the columns
  rename(card_id  = X1,
         win_num  = X2,
         your_num = X3) %>%
  # drop the | and shit from column 3
  mutate(your_num = str_sub(your_num, start=3))
################################################################################
# Problem 1

# Pivot it Longer to make it tidy
p2 <- clean_data %>%
  pivot_longer(cols = ends_with("num"),
               names_to = "num_type") %>%
  separate_longer_delim(value,
                       delim = " ",) %>%
  filter(value != "")
# Group by the Card
# Find when there is a match.
p3 <- p2 %>%
  group_by(card_id) %>%
  mutate(value = as.numeric(value)) %>%
  add_count(card_id,value) %>%
  filter(n > 1) %>%
  filter(num_type == "your_num")
# We have all of your numbers that are winning numbers
# find our how many winners per card.
p4 <- p3 %>%
  count(card_id) %>%
  # add score
  mutate(score = 2^(n-1))

sum(p4$score)
#############################################################
# Part 2
#########
part2_1 <- clean_data %>%
  pivot_longer(cols = ends_with("num"),
               names_to = "num_type") %>%
  separate_longer_delim(value,
                        delim = " ",) %>%
  filter(value != "") %>%
  group_by(card_id) %>%
  mutate(value = as.numeric(value)) %>%
  add_count(card_id,value) %>%
  ## Can't filter; need to know what cards have 0 winners.
  summarise(winners = sum(n == 2)) %>%
  # hahaha This counts them twice
  mutate(winners = winners/2)

# That seems good. But now to do the math.
# Seems like we might need a loop here...



