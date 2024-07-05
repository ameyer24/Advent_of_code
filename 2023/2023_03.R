#########################################################
# Import that data

import <- read_csv("2023/input/2023_03.txt",
                   col_names = "values")

tidy_import <- import %>%
  mutate(row = row_number()) %>%
  separate_longer_position(values, width = 1) %>%
  group_by(row) %>%
  mutate(col = row_number()) %>%
  mutate(values = na_if(values,"."))


  
#######################################################
# Problem 1
#######################################################

test_1 <- tidy_import %>%
  group_by(row) %>%
  mutate(west = lag(values,1)) %>%
  mutate(east = lead(values,1)) %>%
  ungroup() %>%
  group_by(col) %>%
  mutate(north = lag(values,1)) %>%
  mutate(south = lead(values,1)) %>%
  ## now to the diagonals
  ungroup() %>%
  group_by(row) %>%
  mutate(NW = lag (north,1)) %>%
  mutate(NE = lead(north,1)) %>%
  mutate(SW = lag (south,1)) %>%
  mutate(SE = lead(south,1))


symbol_list <- c("*","@","/","+","$","=","&","-","#","%")
num_sym_list <- c("0","1","2","3","4","5","6","7","8","9")
number_list <- c(0,1,2,3,4,5,6,7,8,9)

## need to test the neighborhood.
## does the cell touch a symbol?
test_2 <- test_1 %>%
  mutate(touch_symbol = ifelse(
    north %in% symbol_list |
    south %in% symbol_list |
    east  %in% symbol_list |
    west  %in% symbol_list |
    NE    %in% symbol_list |
    NW    %in% symbol_list |
    SE    %in% symbol_list |
    SW    %in% symbol_list ,
    TRUE,
    FALSE
  )) %>%
  mutate(touch_num = ifelse(
    north %in% num_sym_list |
      south %in% num_sym_list |
      NE    %in% num_sym_list |
      NW    %in% num_sym_list |
      SE    %in% num_sym_list |
      SW    %in% num_sym_list ,
    TRUE,
    FALSE
  ))
## Need to make the number
make_number <- function (x) {
  case_when(x %in% number_list & lead(x,1)%in% number_list & lead(x,2)%in% number_list ~ paste0(x,lead(x,1),lead(x,2)),
            x %in% number_list & lead(x,1)%in% number_list                             ~ paste0(x,lead(x,1)),
            x %in% number_list                                                         ~ paste0(x),
            .default = NA
  ) 
}

test_3 <- test_2 %>%
  # What's a number
  mutate(number = make_number(values)) %>%
  mutate(number = as.numeric(number))

## That makes a number by putting together all the digits AFTER a number.
## But it should be the whole number for all each position.

make_number_gooder <- function (x) {
  case_when(is.na(lag(x,1)) & !is.na(x) ~ x,
            is.na(lag(x,2)) & !is.na(x) ~ lag(x,1),
            is.na(lag(x,3)) & !is.na(x) ~ lag(x,2),
            .default = NA
  )
}

test_4 <- test_3 %>%
  mutate(good_num = make_number_gooder(number))

# This contains a lot of duplicate values
# If the column offset is 1, then it's a duplicate value.
# calculate that and remove it.

test_5 <- test_4 %>%
  filter(touch_symbol == TRUE) %>%
  filter(good_num >0) %>%
  mutate(col_offset = col-lag(col,1)) %>%
  mutate(col_offset = replace_na(col_offset, 0)) %>%
  filter(col_offset !=1)

sum(test_5$good_num)


























#######################################################
# Problem 2
#######################################################

symbol_list <- c("*","@","/","+","$","=","&","-","#","%")
num_sym_list <- c("0","1","2","3","4","5","6","7","8","9")
number_list <- c(0,1,2,3,4,5,6,7,8,9)



###
# trying a new approach
# change the single values into bigger numbers
# This is, don't think about the values as individual digits.
# Think about them as a numbers in a particular spot. 

make_number <- function (x) {
  case_when(x %in% number_list & lead(x,1)%in% number_list & lead(x,2)%in% number_list ~ paste0(x,lead(x,1),lead(x,2)),
            x %in% number_list & lead(x,1)%in% number_list                             ~ paste0(x,lead(x,1)),
            x %in% number_list                                                         ~ paste0(x),
            .default = NA
  ) 
}
make_number_gooder <- function (x) {
  case_when(is.na(lag(x,1)) & !is.na(x) ~ x,
            is.na(lag(x,2)) & !is.na(x) ~ lag(x,1),
            is.na(lag(x,3)) & !is.na(x) ~ lag(x,2),
            .default = NA
  )
}

a1 <- tidy_import %>%
  mutate(number = make_number(values)) %>%
  mutate(number = make_number_gooder(number)) %>%
  ungroup() %>%
  mutate(num_id = row_number())
# okay. this looks good!

a2 <- a1 %>%
  group_by(row) %>%
  mutate(east_num = case_when(
    is.na(lead(number,1)) ~ NA,
    lead(number,1) == number ~ NA,
    .default = lead(number,1)
  )) %>%
  mutate(west_num = case_when(
    is.na(lag(number,1)) ~ NA,
    lag(number,1) == number ~ NA,
    .default = lag(number,1)
  )) %>%
  ungroup() %>%
  group_by(col) %>%
  mutate(south_num = case_when(
    is.na(lead(number,1)) ~ NA,
    lead(number,1) == number ~ NA,
    .default = lead(number,1)
  )) %>%
  mutate(north_num = case_when(
    is.na(lag(number,1)) ~ NA,
    lag(number,1) == number ~ NA,
    .default = lag(number,1)
  )) %>%
  ungroup()

# now to do the combinations.
a3 <- a2 %>%
  group_by(row) %>%
  mutate(NE_num = case_when(
    is.na(lead(north_num,1)) ~ NA,
    lead(north_num,1) == north_num ~ NA,
    .default = lead(north_num,1)
  )) %>%
  mutate(NW_num = case_when(
    is.na(lag(north_num,1)) ~ NA,
    lag(north_num,1) == north_num ~ NA,
    .default = lag(north_num,1)
  )) %>%
  mutate(SE_num = case_when(
    is.na(lead(south_num,1)) ~ NA,
    lead(south_num,1) == south_num ~ NA,
    .default = lead(south_num,1)
  )) %>%
  mutate(SW_num = case_when(
    is.na(lag(south_num,1)) ~ NA,
    lag(south_num,1) == south_num ~ NA,
    .default = lag(south_num,1)
  ))
## Need to make those all numeric (and not characters)

a4 <- a3 %>%
  mutate(east_num  = as.numeric(east_num)) %>%
  mutate(west_num  = as.numeric(west_num)) %>%
  mutate(north_num = as.numeric(north_num)) %>%
  mutate(south_num = as.numeric(south_num)) %>%
  mutate(NE_num    = as.numeric(NE_num)) %>%
  mutate(NW_num    = as.numeric(NW_num)) %>%
  mutate(SE_num    = as.numeric(SE_num)) %>%
  mutate(SW_num    = as.numeric(SW_num))

## Looks good!
a5 <- a4 %>%
  ungroup() %>%
  mutate(num_of_neighbors = rowSums(!is.na(select(., ends_with("_num"))))) %>%
  filter(values == "*") %>%
  filter(num_of_neighbors == 2) %>%
  mutate(east_num  = as.numeric(east_num)) %>%
  mutate(west_num  = as.numeric(west_num)) %>%
  mutate(north_num = as.numeric(north_num)) %>%
  mutate(south_num = as.numeric(south_num)) %>%
  mutate(NE_num    = as.numeric(NE_num)) %>%
  mutate(NW_num    = as.numeric(NW_num)) %>%
  mutate(SE_num    = as.numeric(SE_num)) %>%
  mutate(SW_num    = as.numeric(SW_num))

a6 <- a5 %>%
  rowwise() %>%
  mutate(prod = prod(c_across(east_num:SW_num),na.rm = TRUE))

part_2 <- sum(a6$prod)

part_2






###################################################
# SCRAPS
# Need to find the "*" symbols...
# Inspired by the game battle ship...
find_number_left <- function (x) {
  case_when(is.na(x) ~ NA,
            x %in% number_list & lead(x,1)%in% number_list & lead(x,2)%in% number_list ~ paste0(x,lead(x,1),lead(x,2)),
            x %in% number_list & lead(x,1)%in% number_list                             ~ paste0(x,lead(x,1)),
            x %in% number_list                                                         ~ paste0(x)
  ) 
}

find_number_right <- function (x) {
  case_when(is.na(x) ~ NA,
            x %in% number_list & lag(x,1)%in% number_list & lag(x,2)%in% number_list  ~ paste0(lag(x,2),lag(x,1),x),
            x %in% number_list & lag(x,1)%in% number_list                             ~ paste0(lag(x,1),x),
            x %in% number_list                                                        ~ paste0(x)
  ) 
}



col_len <- max(tidy_import$col)
row_len <- max(tidy_import$row)

# This gets the east value and the west value.
p2 <- tidy_import %>%
  group_by(row) %>%
  mutate(east_ship = find_number_left(lead(values,1))) %>%
  mutate(west_ship = find_number_right (lag(values,1)))





# This (should) get the north and south value.
find_number_middle <- function (x,n) {
  case_when(is.na(x) ~ NA,
            # case X##
            x %in% number_list & lead(x,n) %in% number_list & lead(x,2*n)%in% number_list ~ paste0(x,lead(x,n),lead(x,2*n)),
            # case ##X
            x %in% number_list & lag(x,n)  %in% number_list & lag(x,2*n)%in% number_list  ~ paste0(lag(x,2*n),lag(x,n),x),
            # case #x#
            x %in% number_list & lead(x,n) %in% number_list & lag(x,n) %in% number_list  ~ paste0(lag(x,n),x,lead(x,n)),
            # case X#
            x %in% number_list & lead(x,n) %in% number_list ~ paste0(x,lead(x,n)),
            # case #x
            x %in% number_list & lag(x,n)  %in% number_list  ~ paste0(lag(x,n),x),
            # case x
            x %in% number_list                             ~ paste0(x)
  )
}

p3 <- p2 %>%
  ungroup() %>%
  group_by(col) %>%
  mutate(north_ship = find_number_middle(lag(values,1),3))
