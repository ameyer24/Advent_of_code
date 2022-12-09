
input <- read_file("2022/inputs/2022_08.txt")

AJM <- tibble(input) %>%
  separate_rows(sep = "\\\r\\\n")

?separate_rows
