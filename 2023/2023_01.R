# Data Import -----------------------------------------------------------------
data <- read_csv("2023/inputs/2023_01.txt",
                                col_names = "value",
                                skip_empty_rows = FALSE)

# Problem 1 -------------------------------------------------------------------
numbers_only <- data %>%
  mutate(num1 = str_extract(value,"[:digit:]")) %>%
  mutate(num2 = str_extract(stri_reverse(value),"[:digit:]")) %>%
  mutate(num = str_c(num1, num2)) %>%
  mutate(num = as.numeric(num))

## Sum that column
sum(numbers_only$num)
                   
# Problem 2
numbers_only_2 <- data %>%
  ##replace the word with the number
  mutate(original_value = value) %>%
  ## Changing all the odd ones manually. Not a great start!
  mutate(value = str_replace(value,"oneight",   "18")) %>%
  mutate(value = str_replace(value,"threeight",   "38")) %>%
  mutate(value = str_replace(value,"fiveight", "58")) %>%
  mutate(value = str_replace(value,"nineight",  "98")) %>%
  mutate(value = str_replace(value,"twone",  "21")) %>%
  mutate(value = str_replace(value,"eightwo",   "82")) %>%
  mutate(value = str_replace(value,"eighthree", "83")) %>%
  ## Normal Values
  mutate(value = str_replace_all(value,"one",   "1")) %>%
  mutate(value = str_replace_all(value,"two",   "2")) %>%
  mutate(value = str_replace_all(value,"three", "3")) %>%
  mutate(value = str_replace_all(value,"four",  "4")) %>%
  mutate(value = str_replace_all(value,"five",  "5")) %>%
  mutate(value = str_replace_all(value,"six",   "6")) %>%
  mutate(value = str_replace_all(value,"seven", "7")) %>%
  mutate(value = str_replace_all(value,"eight", "8")) %>%
  mutate(value = str_replace_all(value,"nine",  "9")) %>%
  mutate(num1 = str_extract(value,"[:digit:]")) %>%
  mutate(num2 = str_extract(stri_reverse(value),"[:digit:]")) %>%
  mutate(num = str_c(num1, num2)) %>%
  mutate(num = as.numeric(num))

## Sum that column
sum(numbers_only_2$num)

## This causes problems. I think with things like nineight.
## should this be 9? or 8? or 98?

str_sub(fruit, 1, 3) <- "str" 
