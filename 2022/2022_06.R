
# Import Data -------------------------------------------------------------

input <- read_csv("2022/inputs/2022_06.txt",
                  col_names = "string")

input_string <- input$string

# Part 1 ------------------------------------------------------------------

# use regex to find four unique characters

str_length(input_string)
start <- 1:4091

test_case <- data.frame(
  start = 1:4092,
  end = 4:4095
)

test_case1 <- test_case %>%
  mutate(test_string = str_sub(input_string, start, end)) %>%
  mutate(unique = map_chr(str_extract_all(test_string, "."), ~ str_c(unique(sort(.x)), collapse=""))) %>%
  filter(str_length(unique) == 4)


# Part 2 ------------------------------------------------------------------

test_case2 <- data.frame(
  start = 1:4082,
  end = 14:4095
)

test_case2_1 <- test_case2 %>%
  mutate(test_string = str_sub(input_string, start, end)) %>%
  mutate(test = str_length(test_string)) %>%
  mutate(unique = map_chr(str_extract_all(test_string, "."), ~ str_c(unique(sort(.x)), collapse=""))) %>%
  filter(str_length(unique) == 14)
