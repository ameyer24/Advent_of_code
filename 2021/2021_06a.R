fish <- read_csv("2021/inputs/2021_06.txt",
                 col_names = FALSE)

tidy_fish <- fish %>%
  pivot_longer(everything()) %>%
  group_by(value) %>%
  tally() %>%
  rename(fish_group = value,
         pop_d0 = n) %>%
  add_row(fish_group = 0, pop_d0 = 0) %>%
  add_row(fish_group = 6, pop_d0 = 0) %>%
  add_row(fish_group = 7, pop_d0 = 0) %>%
  add_row(fish_group = 8, pop_d0 = 0) %>%
  arrange(fish_group) %>%
  mutate(fish_group = paste0("Group_", fish_group))

## figure out day1
tidy_fish <- tidy_fish %>%
  mutate(pop_d1 = lead(pop_d0)) %>%
  mutate(bab_d1 = )
  mutate(pop_d2 = case_when(fish_group == "Group_6" ~ lag(pop_d1, n=6),
                            fish_group == "Group_8" ~ lag(pop_d1, n=8),
                            TRUE                    ~ lead(pop_d1, n=1)))
