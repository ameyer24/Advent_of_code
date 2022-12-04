input <- read_csv("2022/inputs/2022_03.txt",
                    col_names = "items")

# Part 1 ------------------------------------------------------------------
# Need to split the string in half.

compartments <- input %>%
  mutate(string_len = str_length(items)) %>%
  mutate(com1 = str_sub(items, 0, (string_len/2))) %>%
  mutate(com2 = str_sub(items, -(string_len/2), -1))

# The strings are split.
# Find the matching character in each string.

compartments1 <- compartments %>%
  mutate(com1_split = str_split(com1, "")) %>%
  mutate(com2_split = str_split(com2, "")) %>%
  rowwise() %>%
  mutate(pair = intersect(com1_split,com2_split)) %>%
  select(-com1_split,-com2_split) # just delete this garbage.

# Got the match!
# Now need to calculate the score
# Found out that R has a list of all the letters!
# combine those lists into one big list
all_letters <- c(letters, LETTERS)
# then the score is the position of the letter on that list.
score <- compartments1 %>%
  mutate(score = match(pair, all_letters))

sum(score$score)

# Part 2 ------------------------------------------------------------------
# Start with the input again.
# Need to look at groups of three; but how to create groups of three?
group_input <- input %>%
  mutate(items_2 = lead(items,1)) %>%
  mutate(items_3 = lead(items,2)) %>%
  rename(items_1 = items) %>%
  mutate(row = row_number()) %>%
  filter(row %% 3 == 1) %>%
  select(-row) %>%
  mutate(elf_group = row_number())

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
  mutate(score = match(match123, all_letters))

sum(group_input2$score)
