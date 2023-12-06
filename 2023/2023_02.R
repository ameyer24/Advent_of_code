# Data Import -----------------------------------------------------------------
data <- read_delim("2023/inputs/2023_02.txt",
                   col_names = c("id","subsets"))
# Tidy Data
# There are games and observations; divide them

tidy_data_1 <- data %>%
  separate_wider_delim(subsets,
                       delim = ";",
                       names = c("s1","s2","s3","s4","s5","s6","s7"),
                       too_few = "align_start")
tidy_data_2 <- tidy_data_1 %>%
  pivot_longer(!id,
               names_to = "subset",
               values_drop_na = TRUE)
## Now divide the observations into color counts

tidy_data_3 <- tidy_data_2 %>%
  separate_wider_delim(value,
                       delim = ",",
                       names = c("c1","c2","c3"),
                       too_few = "align_start")
tidy_data_4 <- tidy_data_3 %>%
  pivot_longer(!c("id","subset"),
               names_to = "observation",
               values_drop_na = TRUE) %>%
  separate(value,
           into = c("blank","number","color"),
           sep = " ") %>%
  select(-observation) %>%
  select(-blank) %>%
  mutate(number = as.numeric(number))

tidy_data_final <- tidy_data_4
## Part 1
## Find the unallowable values

red_too_high <- tidy_data_final %>%
  filter(color == "red") %>%
  filter(number > 12)

green_too_high <- tidy_data_final %>%
  filter(color == "green") %>%
  filter(number > 13)

blue_too_high <- tidy_data_final %>%
  filter(color == "blue") %>%
  filter(number > 14)
## Put them back together.
too_high <- bind_rows(red_too_high,green_too_high,blue_too_high)
## Solve the problem
problem1 <- too_high %>%
  # remove the "Game " string
  mutate(game_id = str_remove(id, "Game ")) %>%
  mutate(game_id = as.numeric(game_id)) %>%
  #deduplicate
  distinct(game_id)

sum(problem1$game_id)
### hahhaha. That's the sum of the impossible games.
# thats the wrong problem.
sum(1:100)-sum(problem1$game_id)


############################################################
# Part 2
############################################################
# start with tidy_data_final
tidy_data_final
# group by game and color
# find the max value
problem2_1 <- tidy_data_final %>%
  group_by(id, color) %>%
  summarize(max = max(number)) %>%
  mutate(max = as.numeric(max))
# I'm sure there is a better way.
# I'm going to pivot wider
problem2_2 <- problem2_1 %>%
  pivot_wider(names_from = color, values_from = max) %>%
  mutate(game_power = red * blue * green)

sum(problem2_2$game_power)
