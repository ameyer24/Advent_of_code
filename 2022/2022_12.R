input <- read_csv("2022/inputs/2022_12.txt",
                  col_names="lines")
input[1]

tidy_input <- input %>%
  separate(lines,into=paste("col",0:143,sep=""),sep="") %>%
  rownames_to_column() %>%
  select(-col0)


# Part 1 ------------------------------------------------------------------


