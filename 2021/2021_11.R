input <- read_lines("2021/inputs/2021_11.txt")
energy_levels <- enframe(input, name = "row_num", value = "value")
rm(input)
## PART 1
## I think it makes sense to make this data tidy?
## Or at least separate values?
col_names <- as.character(0:10)

energy_levels1 <- energy_levels %>%
  separate(value, into =  col_names, sep="") %>%
  select(-c(2))
  
tidy_energy_levels <- energy_levels1 %>%
  pivot_longer(cols = -row_num, names_to = "column_num") %>%
  rowid_to_column(var= "location_id") %>%
  type_convert()

## How can we determine who are the neighbors?
## It seems that tidy data loses neighbors...
## but maybe we can define it somewhere and just use that again and again?

energy_neighbors <- tidy_energy_levels %>%
  select(-value) %>%
  mutate(N_neighbor = location_id - 10) %>%
  mutate(S_neighbor = location_id + 10) %>%
  mutate(E_neighbor = location_id + 1) %>%
  mutate(W_neighbor = location_id - 1) %>%
  mutate(NE_neighbor = location_id - 9) %>%
  mutate(SE_neighbor = location_id + 11) %>%
  mutate(SW_neighbor = location_id + 9) %>%
  mutate(NW_neighbor = location_id - 11)

## This is a comprehensive list of neighbors
## need to remove neighbors for the top, left, right, and bottom rows.
## This fixes the top and bottom rows
energy_neighbors1 <- energy_neighbors %>%
  mutate(N_neighbor = replace(N_neighbor, which(N_neighbor<1), NA)) %>%
  mutate(NE_neighbor = replace(NE_neighbor, which(NE_neighbor<1), NA)) %>%
  mutate(NW_neighbor = replace(NW_neighbor, which(NW_neighbor<1), NA)) %>%
  mutate(S_neighbor = replace(S_neighbor, which(S_neighbor>100), NA)) %>%
  mutate(SE_neighbor = replace(SE_neighbor, which(SE_neighbor>100), NA)) %>%
  mutate(SW_neighbor = replace(SW_neighbor, which(SW_neighbor>100), NA))


## Need to fix the values on the edges (1,11,21,..)
## and ()
energy_neighbors2 <- energy_neighbors1 %>%
  mutate(W_neighbor = replace(W_neighbor, which(column_num == 1), NA)) %>%
  mutate(NW_neighbor = replace(NW_neighbor, which(column_num == 1), NA)) %>%
  mutate(SW_neighbor = replace(SW_neighbor, which(column_num == 1), NA)) %>%
  mutate(E_neighbor = replace(E_neighbor, which(column_num == 10), NA)) %>%
  mutate(NE_neighbor = replace(NE_neighbor, which(column_num == 10), NA)) %>%
  mutate(SE_neighbor = replace(SE_neighbor, which(column_num == 10), NA))

energy_neighbors3 <- energy_neighbors2 %>%
  group_by(location_id, row_num, column_num) %>%
  nest()

## Our data is tidy.
## We have determined the neighbors and stored that information.
## We need functions and iteration but let's fudge our way

round_0 <- tidy_energy_levels %>%
  rename(value0 =  value)

round_1 <- round_0 %>%
  ## increment the values
  mutate(value1_1 = value0 +1) %>%
  ## check to see if it flashes
  mutate(flash_check1 = case_when(value1_1 >=10 ~ "YES",
                                   value1_1 < 10 ~ "NO")) %>%
  ## CHECK NEIHGBORS?!?
  ## RESET VALUES
  mutate(value1 = value1_1)

round_2 <- round_1 %>%
  ## increment the values
  mutate(value2_1 = value1 +1) %>%
  ## check to see if it flashes
  mutate(flash_check2 = case_when(value2_1 >=10 ~ "YES",
                                  value2_1 < 10 ~ "NO"))















## Will write functions to do things
flash_counter <- 0 # number of octopuses that have flashes (total)
round_counter <- 0 # number of rounds (need to get to 100)

do_one_flash<- function(energy_info, neighbor_info) {
  ##
  
  round_counter <- round_counter +1 
  
  
  print(round_counter)
  
  
}

do_one_flash()  

## This function increase the energy level for all octopuses by 1.
increment_energy <- function(energy_level) {
  energy_level <- energy_level+1
}

## Need to count flashes
## start variable for counting

count_flashes <- function(energy_level) {
  flash_counter <- case_when(energy_level >=10 ~ flash_counter +1,
                             TRUE  ~ flash_counter)
  return(flash_counter)
}
count_flashes(AJM)
## this function resets the energy for those octopuses that are greater than 10.
reset_energy <- function(energy_level) {
  energy_level <- case_when(energy_level >=10 ~ 0,
                            TRUE  ~ energy_level)
}
## This function determines if neighbors have flashed and increments accordingly.
## I don't know how to do this yet...
check_neighbors <-function(x) {
  
}

single_step <- function(x) {
  x <- increment_energy(x)
  y <- count_flashes(x)
  x <- reset_energy(x)
  print(x, y)
}

AJM <- single_step(tidy_energy_levels$value)
AJM2<- single_step(AJM)
