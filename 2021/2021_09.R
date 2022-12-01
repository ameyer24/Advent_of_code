heightmap <- read_lines("2021/inputs/2021_09.txt",
                        n_max = 101)

height_map <- enframe(heightmap, name = "name", value = "value")
rm(heightmap)
## I've got the data in a dataframe now.

col_names <- 0:100
col_names <- as.character(col_names)
height_map1 <- height_map %>%
  separate(value, into = col_names, sep="") %>%
  rename_with(~ paste0("digit", .x)) %>%
  select(-digit0) %>%
  rename(row_num = digitname)

check_neighbors <- function(x) {
  up <- lag(x)
  down <- lead(x)
  print(paste("digit is", x, "digit above is:",up,"Digit below is:",down))
}

check_neighbors(height_map1[1,1])

height_map1[[3]][[4]] ## this is row 3, column 4 (actually line 2, column 4)

check_neighbors(height_map1$digit1)



### Try and make the data tidy?!??!
tidy_height_map <- height_map1 %>%
  pivot_longer(cols = starts_with("digit")) %>%
  mutate(col_num = str_remove(name,"digit")) %>%
  select(-name) %>%
  relocate(col_num, .after=row_num) %>%
  type_convert()

### Okay! Use group and lag to figure this out.
tidy_height_map1 <- tidy_height_map %>%
  group_by(row_num) %>%
  mutate(left_neighbor = lag(value)) %>%
  mutate(right_neighbor = lead(value)) %>%
  ungroup() %>%
  group_by(col_num) %>%
  mutate(top_neighbor = lag(value)) %>%
  mutate(bottom_neighbor = lead(value)) %>%
  ungroup()

### Looks great. Showing a lot of work but whatever.
## Do the comparisons and mark with an X
tidy_height_map2 <- tidy_height_map1 %>%
  mutate(compare_left = case_when(value < left_neighbor ~ "less than",
                                  value >= left_neighbor ~ "greater than",
                                  is.na(left_neighbor) ~ "no neighbor")) %>%
  mutate(compare_right = case_when(value < right_neighbor ~ "less than",
                                  value >= right_neighbor ~ "greater than",
                                  is.na(right_neighbor) ~ "no neighbor")) %>%
  mutate(compare_top = case_when(value < top_neighbor ~ "less than",
                                  value >= top_neighbor ~ "greater than",
                                  is.na(top_neighbor) ~ "no neighbor")) %>%
  mutate(compare_bottom = case_when(value < bottom_neighbor ~ "less than",
                                  value >= bottom_neighbor ~ "greater than",
                                  is.na(bottom_neighbor) ~ "no neighbor"))


## Now just filter - get rid of all "greater thans"
## less thans are fine. And so are no neighbors?
tidy_height_map3 <- tidy_height_map2 %>%
  filter(compare_left != "greater than") %>%
  filter(compare_right != "greater than") %>%
  filter(compare_top != "greater than") %>%
  filter(compare_bottom != "greater than") %>%
  rowid_to_column()

## Puzzle asks for the risk level which is the height +1

tidy_height_map4 <- tidy_height_map3 %>%
  mutate(risk_level = value + 1) %>%
  summarize(sum(risk_level))

#####################################################################
## PART TWO
## Now I need to determine the areas of "basins"
## Basically every low point is a basin.
## The borders of basins are points with a height of 9.
rm(height_map, height_map1, col_names, check_neighbors, tidy_height_map4, tidy_height_map, tidy_height_map1)
## remove stuff we don't need.

tidy_basins <- left_join(tidy_height_map2, tidy_height_map3)
## mutate row_id to basin_id
tidy_basins1 <- tidy_basins %>%
  rename(basin_id = rowid) %>%
  mutate(in_basin = case_when(value == 9  ~ "No, this is a high point",
                              basin_id >0 ~ paste0("This is low point ", basin_id),
                              TRUE        ~ paste0("Yes, but unknown")))

## expand basins horizontally.
## group by row
## if it's in a basin but we don't know which. Determine if the space is empty.
tidy_basins2$basin_id[1] <- 1
tidy_basins2 <- tidy_basins1 %>%
  group_by(row_num) %>%
  mutate(basin_id = case_when(value == 9  ~ 999999))


## I don't think this is working.. at least it's not working well.
## Definitely need to loop it.
## and the nested case_when is pretty nasty looking from a logical POV
tidy_basins2 <- tidy_basins1 %>%
  group_by(row_num) %>%
  mutate(basin_id = case_when(value ==9 ~ basin_id,
                              value !=9 & is.na(basin_id) ~ lag(basin_id),
                              value !=9 ~ basin_id))


## if the value isn't 9, get the basin_id from the previous row.























































###################################################################
## TRASH I MIGHT NEED


## Just iterate the stupid way?
## just do this over and over again..
tidy_basins2 <- tidy_basins2 %>%
  mutate(basin_id = case_when(in_basin == "Yes, but unknown" ~ lag(basin_id),
                              TRUE ~ basin_id)) %>%
  mutate(in_basin = case_when(in_basin =="Yes, but unknown" ~ case_when(basin_id >=0 ~ "Determinded from the left point",
                                                                        TRUE ~ in_basin),
                              TRUE ~ in_basin))

tidy_basins_WIP <- tidy_basins2

tidy_basins_WIP <- tidy_basins_WIP %>%
  mutate(basin_id = case_when(in_basin == "Yes, but unknown" ~ lead(basin_id),
                              TRUE ~ basin_id)) %>%
  mutate(in_basin = case_when(in_basin =="Yes, but unknown" ~ case_when(basin_id >=0 ~ "Determinded from the right point",
                                                                        TRUE ~ in_basin),
                              TRUE ~ in_basin))