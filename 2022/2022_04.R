input <- read_csv("2022/inputs/2022_04.txt",
                  col_names = c("assignment1", "assignment2"))

# Part 1 ------------------------------------------------------------------

# Break apart the numbers
# get when assignment 1 contains assignment 2
input2 <- input %>%
  separate(assignment1, into = c("start1","end1"), sep = "-") %>%
  separate(assignment2, into = c("start2","end2"), sep = "-") %>%
  mutate(id = row_number()) %>%
  type_convert() # DAMN IT


# case 1, when the first set includes all the second set
# start 1 is <= start 2 / end1 is >=
case1 <- input2 %>%
  filter(start1 < start2) %>%
  filter(end1 > end2)

case1_1 <- input2 %>%
  filter(start1 == start2) %>%
  filter(end1 > end2)

case1_2 <- input2 %>%
  filter(start1 < start2) %>%
  filter(end1 == end2)
# case 2, when the second set includes all the first set
# start 1 is >= start2 / end1 is <= end 2
case2 <- input2 %>%
  filter(start2 < start1) %>%
  filter(end2 > end1)

case2_1 <- input2 %>%
  filter(start2 == start1) %>%
  filter(end2 > end1)

case2_2 <- input2 %>%
  filter(start2 < start1) %>%
  filter(end2 == end1)

# case 3, when they are identical.
# these are counted twice (in both cases above)
case3 <- input2 %>%
  filter(start1 == start2) %>%
  filter(end1 == end2)


# Part 2 ------------------------------------------------------------------

# find where there is no overlap at all

case90 <- input2 %>%
  filter(end1 < start2)
case91 <- input2 %>%
  filter(end2 < start1)
