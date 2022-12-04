input <- read_csv("2022/inputs/2022_03.txt",
                    col_names = "items")

# Part 1 ------------------------------------------------------------------
# Need to split the string in half.

compartments <- input %>%
  # subset the string in half into new columns
  mutate(com1 = str_sub(items, 0, (str_length(items)/2))) %>%
  mutate(com2 = str_sub(items, -(str_length(items)/2), -1)) %>%
  # split each string into list of characters.
  mutate(com1 = str_split(com1, "")) %>%
  mutate(com2 = str_split(com2, "")) %>%
  # look at each row and determine where the lists intersect
  rowwise() %>%
  mutate(pair = intersect(com1,com2)) %>%
  # calculate the score (using some constants build into R)
  mutate(score = match(pair, c(letters, LETTERS)))

# Find the sum of the score
sum(compartments$score)

# Part 2 ------------------------------------------------------------------
# Start with the input again.
# Need to look at groups of three; but how to create groups of three?
# I don't know. But I can...
# get the data I need and then filter out what I don't need.
group_input <- input %>%
  rename(items_1 = items) %>%
  mutate(items_2 = lead(items_1,1)) %>%
  mutate(items_3 = lead(items_1,2)) %>%
  filter(row_number() %% 3 == 1)

# Each elf group is in a row.
group_input2 <- group_input %>%
  mutate(items_1 = str_split(items_1, "")) %>%
  mutate(items_2 = str_split(items_2, "")) %>%
  mutate(items_3 = str_split(items_3, "")) %>%
  rowwise() %>%
  # Not clear why I can't find the intersection of three lists...
  # But I can do this little work around.
  mutate(pair12 = list(intersect(items_1,items_2))) %>%
  mutate(pair23 = list(intersect(items_2,items_3))) %>%
  mutate(match123 = list(intersect(pair12, pair23))) %>%
  select(-pair12, -pair23) %>% # Delete this garbage
  mutate(score = match(match123, c(letters, LETTERS)))

sum(group_input2$score)
