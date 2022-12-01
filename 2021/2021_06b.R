fish <- read_csv("2021/inputs/2021_06.txt",
                 col_names = FALSE)


tidy_fish <- fish %>%
  pivot_longer(everything()) %>%
  group_by(value) %>%
  tally() %>%
  rename(fish_group = value,
         population = n) %>%
  add_row(fish_group = 0, population = 0) %>%
  add_row(fish_group = 6, population = 0) %>%
  add_row(fish_group = 7, population = 0) %>%
  add_row(fish_group = 8, population = 0) %>%
  arrange(fish_group) %>%
  mutate(fish_group = paste0("Group_", fish_group)) %>%
  pivot_wider(names_from=fish_group, values_from=population) %>%
  add_column(day = 0) %>%
  relocate(day, .before = Group_0)


tidy_fish_count <- tidy_fish %>%
  add_row(day = 01:80)

tidy_fish_count1 <- tidy_fish_count %>%
  
