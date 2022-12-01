fish <- read_csv("2021/inputs/2021_06.txt",
                 col_names = FALSE)
fish_list <- as.list(fish)

fish_list[10]

## we have a fish list.
fish_vector <- as.vector(fish)
## we also have a fish vector

## how to start thinking about this?
## maybe adults and babies?

fish_long <- fish %>%
  pivot_longer(everything()) %>%
  group_by(value) %>%
  tally() %>%
  rename(day_of_cycle = value,
         num_of_fish = n)

fish_counter <- fish_long %>%
  add_row(day_of_cycle = 0, num_of_fish = 0) %>%
  add_row(day_of_cycle = 6, num_of_fish = 0) %>%
  add_row(day_of_cycle = 7, num_of_fish = 0) %>%
  add_row(day_of_cycle = 8, num_of_fish = 0) %>%
  rename(d0 = num_of_fish) %>%
  arrange(day_of_cycle)

## Create function to move fish around.
## Do it manually first
## I don't think this is that hard.
## The different groups (fish on different timers) don't really interact.
## After a few days (after the initial input starts reproducing) there will be a pattern.
## brute force a bit more

fish_counter_d1 <- fish_counter %>%
  mutate(d1 = lead(d0, default=0)) %>%
  mutate(d2 = lead(d1, default=0)) %>%
  mutate(d3 = lead(d2, default=0)) %>%
  mutate(d4 = lead(d3, default=0)) %>%
  mutate(d5 = lead(d4, default=0)) %>%
  mutate(d6 = lead(d5, default=0)) %>%
  mutate(d7 = lead(d6, default=0))

# what to do when the fish reach day_0 of the cycle?
# add that number to the day_6 group (the adults)
# add that number to the day_8 group (the babies)

## DO this via brute force
fish_counter_d3A <- fish_counter %>%
  mutate(d1 = case_when(day_of_cycle == 6 ~  lag (d0, n=6),
                        day_of_cycle == 8 ~  lag (d0, n=8),
                        day_of_cycle <= 9 ~  lead(d0, n=1))) %>%
  mutate(d2 = case_when(day_of_cycle == 6 ~  sum(lag (d1, n=6), d1),
                        day_of_cycle == 8 ~  lag (d1, n=8),
                        day_of_cycle <= 9 ~  lead(d1, n=1))) %>%
  mutate(d3 = case_when(day_of_cycle == 6 ~  lag (d2, n=6),
                        day_of_cycle == 8 ~  lag (d2, n=8),
                        day_of_cycle <= 9 ~  lead(d2, n=1)))




mutate(d1 = case_when(day_of_cycle == 6 ~  lag (d0, n=6),
                      day_of_cycle == 8 ~  lag (d0, n=8),
                      day_of_cycle <= 9 ~  lead(d0, n=1)))



fish_counter_d3 <- fish_counter %>%
  mutate(d1 = case_when(day_of_cycle == 6 ~ lag(d0, n=6),
                        day_of_cycle == 8 ~ lag(d0, n=8),
                        day_of_cycle !=6 ~  lead(d0, n=1)
                        )
         )

####################
# the start of a fish function
count_fish <-function(data) {
  mutate(data, d1 = case_when(day_of_cycle == 6 ~ lag(d0, n=6),
                        day_of_cycle == 8 ~ lag(d0, n=8),
                        day_of_cycle !=6 ~  lead(d0, n=1)
  )
  )
}

fish_counter_d2 <- count_fish(fish_counter)


## improved fish function 
count_fish <-function(df, col1, col2) {
  df %>%
    mutate(col1 = case_when(day_of_cycle == 6 ~  lag (col2, n=6),
                            day_of_cycle == 8 ~  lag (col2, n=8),
                            day_of_cycle <= 9  ~ lead(col2, n=1)
  )
  )
}
fish_counter_d2 <- count_fish(fish_counter, d1, d0)

count_fish1 <-function(df, col1) {
  df %>%
    mutate(col1 = "ANDY"
    )
}


fish_counter_d2 <- count_fish1(fish_counter, "d4")

###

###########
## TAKE A FRESH APPROACH
##########
fish <- read_csv("2021/inputs/2021_06.txt",
                 col_names = FALSE)
fish_list <- as.list(fish)

fish_list[10]

## we have a fish list.
fish_vector <- as.vector(fish)
## we also have a fish vector

## how to start thinking about this?
## maybe adults and babies?

fish_long <- fish %>%
  pivot_longer(everything()) %>%
  group_by(value) %>%
  tally() %>%
  rename(day_of_cycle = value,
         num_of_fish = n)

fish_counter <- fish_long %>%
  add_row(day_of_cycle = 0, num_of_fish = 0) %>%
  add_row(day_of_cycle = 6, num_of_fish = 0) %>%
  add_row(day_of_cycle = 7, num_of_fish = 0) %>%
  add_row(day_of_cycle = 8, num_of_fish = 0) %>%
  rename(d0 = num_of_fish) %>%
  arrange(day_of_cycle)

## Will try and actually tidy the data.
## Variables are
### group membership
### days since 0
### number in group
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
  mutate(days_since_0 = 0) %>%
  mutate(fish_group = paste0("Group_", fish_group)) %>%
  rename(population_d0 = population) %>%
  relocate(days_since_0, .after = fish_group)
  

sum(tidy_fish$population_d0)

tidy_fish_d2 <- tidy_fish %>%
  add_row(fish_group = "Group_0", days_since_0 = 1) %>%
  mutate(population_d0 = lag(population_d0, n=5))
















## make a new dataframe for d1
tidy_fish_d1 <- tidy_fish %>%
  mutate(days_since_0 = 1) %>%
  mutate(population_d1 = lead(population_d0, default = 0))
  
## make a new dataframe for d2
tidy_fish_d2 <- tidy_fish_d1 %>%
  mutate(babies = )
  mutate(population_d2 = case_when(fish_group == "Group_6" ~ lead(population_d1, n=1) + (lag(population_d1, n=8)) ,
                                   fish_group == "Group_8" ~ lag(population_d1, n=8) ,
                                   TRUE                    ~ lead(population_d1, n=1)
                                   )
         )

tidy_fish_d3 <- tidy_fish_d2 %>%
  mutate(population_d3 = case_when(fish_group == "Group_6" ~ lag(population_d2, n=6) + lead(population_d2, n=1)
                                   fish_group == "Group_8" ~ lag(population_d2, n=8) + lead(population_d2, n=1),
                                   TRUE                    ~ lead(population_d2, n=1)))

############## NEWEST APPROACH YET

tidy_fish2 <- fish %>%
  pivot_longer(everything()) %>%
  group_by(value) %>%
  tally() %>%
  rename(fish_group = value,
         population = n) %>%
  add_row(fish_group = 0, population = 0) %>%
  add_row(fish_group = 6, population = 0) %>%
  add_row(fish_group = 7, population = 0) %>%
  add_row(fish_group = 8, population = 0) %>%
  arrange(desc(fish_group)) %>%
  mutate(days_since_0 = 0) %>%
  mutate(fish_group = paste0("Group_", fish_group)) %>%
  relocate(days_since_0, .after = fish_group) %>%
  add_row(days_since_0 = 1:80) %>%
  mutate(new_babies = 0)

## Determine the number of new babies.
tidy_fish3 <- tidy_fish2 %>%
  mutate(new_babies = lag(population, n=6)) %>%
  mutate(population = case_when(days_since_0 >=1 ~ new_babies,
                                TRUE ~ 6942069))
