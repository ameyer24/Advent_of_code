input <- read_lines("2021/inputs/2021_12.txt")
cave_map <- enframe(input, name = "line", value = "value")
rm(input)

# Clean Data --------------------------------------------------------------

cave_map1 <- cave_map %>%
  separate(value, into = c("left","right"), sep = "-")

## Cannot wrap my mind around this problem...
## how many rooms are there?

cave_map2 <- cave_map1
