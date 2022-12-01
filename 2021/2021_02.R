## Read in the input data
data <- read_csv("2021/inputs/2021_02.txt",
                 col_names = "command")
## Separate the information
data <- data %>%
  separate(command, into=c("direction", "amount"))

## make the amount actual numbers
data <- type_convert(data)
transmute(data,
          amount <- as.numeric(data$amount))
## Calculate Horizontal Position
## Filter for "forward" and sum
hor_data <- data %>%
  filter(direction == "forward")

hor_movement <- sum(hor_data$amount)

## Calculate Vertical Position
## Filter for "down" and sum
down_data <- data %>%
  filter(direction == "down")
down_movement <- sum(down_data$amount)
## filter for "up" and sum
up_data <- data %>%
  filter(direction == "up")
up_movement <- sum(up_data$amount)

## Vertical Position is the down movement - the up movement
ver_movement = down_movement - up_movement

## Puzzle wants these multiplied together
hor_movement * ver_movement

















###################### PART TWO
## New approach!
## re-read the data
data2 <- read_csv("inputs/2021_02.txt",
                  col_names = "command")

## still break the data up
data2 <- data2 %>%
  separate(command, into=c("direction", "amount"))

## make the amount actual numbers
data2 <- type_convert(data2)

## add column to show what "step" we are on
data3 <- data2 %>%
  mutate(step = 1:n()) %>%
  select(step, everything())
## Show my stupid work
## how much should each step change the aim

data4 <- data3 %>%
  mutate(
    aim_change = case_when(
      direction == "forward" ~ 0,
      direction == "down"    ~ amount*(-1),
      direction == "up"      ~ amount
))
data5 <- data4 %>%
  mutate(current_aim = cumsum(aim_change))
## Do the same thing for direction.
## Show all my stupid work.
data6 <- data5 %>%
  mutate(
    hor_change = case_when(
      direction == "forward" ~ amount,
      direction == "down"    ~ 0,
      direction == "up"      ~ 0
    )) %>%
  mutate(
    ver_change = case_when(
      direction == "forward" ~ amount * current_aim,
      direction == "down"    ~ 0,
      direction == "up"      ~ 0
    ))
## We need to sum those changes.
data7 <- data6 %>%
  mutate(current_hor_pos = cumsum(hor_change)) %>%
  mutate(current_ver_pos = cumsum(ver_change))
## now just multiple those last positions together.
  
