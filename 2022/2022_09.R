input <- read_delim("2022/inputs/2022_09.txt",
                    delim = " ",
                    col_names = c("direction","steps"))

# Part 1 ------------------------------------------------------------------

# Put the H on the coordinate plan
# Plot the movement of the head

head_start <- input %>%
  mutate(head_x = 0) %>%
  mutate(head_y = 0)


