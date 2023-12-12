#########################################################
# Import that data

import <- read_csv("2023/inputs/2023_03.txt",
                   col_names = "lines")

tidy_import <- import %>%
  mutate(row = row_number()) %>%
  separate_longer_position(lines, width = 1) %>%
  group_by(row) %>%
  mutate(col = row_number()) %>%
  ungroup() %>%
  mutate(num_id = row_number())


  
#######################################################
# Problem 1
#######################################################

test_1 <- tidy_import %>%
  group_by(row) %>%
  mutate(west = lag(lines,1)) %>%
  mutate(east = lead(lines,1)) %>%
  ungroup() %>%
  group_by(col) %>%
  mutate(north = lag(lines,1)) %>%
  mutate(south = lead(lines,1)) %>%
  ## now to the diagonals
  ungroup() %>%
  group_by(row) %>%
  mutate(NW = lag(north,1)) %>%
  mutate(NE = lead(north,1)) %>%
  mutate(SW = lag(south,1)) %>%
  mutate(SE = lead(south,1))


symbol_list <- c("*","@","/","+","$","=","&","-","#","%")
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
  ))
## Need to make the number
make_number <- function (x) {
  case_when(x %in% number_list & lead(x,1)%in% number_list & lead(x,2)%in% number_list & lead(x,3)%in% number_list ~ paste0(x,lead(x,1),lead(x,2),lead(x,3)),
            x %in% number_list & lead(x,1)%in% number_list & lead(x,2)%in% number_list                             ~ paste0(x,lead(x,1),lead(x,2)),
            x %in% number_list & lead(x,1)%in% number_list                                                         ~ paste0(x,lead(x,1)),
            x %in% number_list                                                                                     ~ paste0(x),
            .default = NA
  ) 
}

test_3 <- test_2 %>%
  # What's a number
  mutate(number = make_number(lines)) %>%
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

test_5 <- test_4 %>%
  filter(touch_symbol == TRUE) %>%
  filter(!is.na(good_num)) %>%
  distinct(row,good_num)







sum(test_5$good_num)

## dang
# 533156 is too low
# 551707 is too high
# 541704 is too high
# 532905 is also wrong.



make_number <- function(x) {
  ifelse(x %in% number_list,
         ifelse(lead(x,1) %in% number_list,
                ifelse(lead(x,2) %in% number_list,
                       ifelse(lead(x,3) %in% number_list,
                              paste0(x,lead(x,1),lead(x,2),lead(x,3)),
                              paste0(x,lead(x,1),lead(x,2))),
                       paste0(x,lead(x,1))
                ),
                x),
         FALSE)
}
         