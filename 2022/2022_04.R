input <- read_csv("2022/inputs/2022_04.txt",
                  col_names = c("assignment1", "assignment2"))

# Part 1 ------------------------------------------------------------------

# Break apart the numbers
# get when assignment 1 contains assignment 2
input <- input %>%
  separate(assignment1, into = c("start1","end1"), sep = "-") %>%
  separate(assignment2, into = c("start2","end2"), sep = "-") %>%
  type_convert() # Need this! Forgot it and it botched me up.

fully_contains <- input %>%
  filter(start1 <= start2 & end1 >= end2 | start2 <= start1 & end2 >= end1)

nrow(fully_contains)

# Part 2 ------------------------------------------------------------------
# I approach this indirectly
# Find where there is absolutely no overlap
no_overlap <- input %>%
  filter(end1 < start2 | end2 < start1)
# and then subtract from the total
any_overlap <- nrow(input) - nrow(no_overlap)



