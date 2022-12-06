
# Import Data -------------------------------------------------------------

input <- read_csv("2022/inputs/2022_06.txt",
                  col_names = "string")

input_string <- input$string
rm(input)

# Part 1 ------------------------------------------------------------------

# Need to look at four character strings from the input
sample_size_1 <- 4
string_size_1 <- str_length(input_string)

test_grid_1 <- data.frame(
  start = 1:(string_size_1-sample_size_1+1),
  end = sample_size_1:string_size_1
)

test_1 <- test_grid_1 %>%
  mutate(test_string = str_sub(input_string, start, end)) %>%
  mutate(unique = map_chr(str_extract_all(test_string, "."), ~ str_c(unique(sort(.x)), collapse=""))) %>%
  filter(str_length(unique) == sample_size_1)

min(test_1$end)

# Part 2 ------------------------------------------------------------------
# Now need to look at 14 characters
sample_size_2 <- 14
string_size_2 <- str_length(input_string)

test_grid_2 <- data.frame(
  start = 1:(string_size_2-sample_size_2+1),
  end = sample_size_2:string_size_2
)

test_2 <- test_grid_2 %>%
  mutate(test_string = str_sub(input_string, start, end)) %>%
  mutate(unique = map_chr(str_extract_all(test_string, "."), ~ str_c(unique(sort(.x)), collapse=""))) %>%
  filter(str_length(unique) == sample_size_2)

min(test_2$end)
