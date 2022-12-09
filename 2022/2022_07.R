input <- read_csv("2022/inputs/2022_07.txt",
                  col_names = "readout")

# Part 1 ------------------------------------------------------------------

# to help me understand - add a description of each line
data <- input %>%
  mutate(desc = case_when(str_detect(readout, "\\$ ls") ~ "list_instruction",
                          str_detect(readout, "\\$ cd") ~ "cd_instruction",
                          str_detect(readout, "dir ") ~ "directory",
                          TRUE ~ "file"))

# separate the data into groups?
files <- data %>%
  filter(desc == "file") %>%
  separate(readout, into = c("file_size","file_name"),sep=" ") %>%
  select(-desc)

directories <- data %>%
  filter(desc == "directory") %>%
  separate(readout, into = c("command","folder_name"),sep=" ") %>%
  select(-command)

## looks like some directory names are repeated and nested...
cd_instructions <- data %>%
  filter(desc == "cd_instruction") %>%
  separate(readout, into = c("p1","p2","folder"),sep = " ", remove = FALSE) %>%
  select(-p1,-p2)


## Improve the data
improve_data <- function(x) {
  mutate(x, desc = case_when(str_detect(readout, "\\$ ls") ~ "list_instruction",
                          str_detect(readout, "\\$ cd") ~ "cd_instruction",
                          str_detect(readout, "dir ") ~ "directory",
                          TRUE ~ "file")) %>%
  mutate(file_size = case_when(desc == "file" ~ as.numeric(str_extract(readout, pattern = "(\\d)+")))) %>%
  mutate(folder_name = case_when(
      desc == "cd_instruction" ~ str_remove(readout,"\\$ cd " )
    ))
}

data1 <- improve_data(input)

file_path_paste = function(x, .sep = "-") {
  Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
}

data2 <- data1 %>%
  mutate(file_path = file_path_paste(folder_name)) %>%
  mutate(file_path = str_remove_all(file_path, "-NA"))


# This creates this mess.
# I need to create a function that looks for the -.. pattern and removes the chunk before it.
test_string <- "/-ctd-bblsqnwl-rng-jncpmzcs-..-pttvmghm-..-sgm-djcbdbgr-..-gqrr-..-..-..-..-gzcjrs-..-pgqmwn-zpvthlgp-..-..-qzgjp-pgqmwn"

regex_pattern <- "([:lower:])+-\\.\\.-"

data3 <- data2 %>%
  mutate(clean_file_path = str_remove_all(file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(clean_file_path = str_remove_all(clean_file_path, regex_pattern)) %>%
  mutate(final_file_path = str_remove_all(clean_file_path, "-\\.\\."))

## Boy that's ugly!

final_data <- data3

## Let's see what happens when I summarize the data
## I'm going to need to "unest" this data
## determine the max depth

depth <- max(str_count(final_data$final_file_path, "-"))

summary <- final_data %>%
  mutate(depth = str_count(final_file_path, "-")) %>%
  separate(final_file_path, into = paste("level", 1:10, sep="_"), sep="-", remove=FALSE)
## This summarizes the folders as they are

summary10 <- summary %>%
  unite(level, level_1:level_10, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary9 <- summary %>%
  unite(level, level_1:level_9, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))
  
summary8 <- summary %>%
  unite(level, level_1:level_8, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary7 <- summary %>%
  unite(level, level_1:level_7, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary6 <- summary %>%
  unite(level, level_1:level_6, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary5 <- summary %>%
  unite(level, level_1:level_5, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary4 <- summary %>%
  unite(level, level_1:level_4, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary3 <- summary %>%
  unite(level, level_1:level_3, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary2 <- summary %>%
  unite(level, level_1:level_2, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

summary1 <- summary %>%
  unite(level, level_1:level_1, na.rm = TRUE) %>%
  group_by(level) %>%
  summarize(sum_file_size = sum(file_size, na.rm = TRUE))

  
final_summary <- rbind(summary1,summary2,summary3,summary4,summary5,summary6,summary7,summary8,summary9,summary10)

final_summary1 <- final_summary %>%
  group_by(level) %>%
  summarise(max_size = max(sum_file_size)) %>%
  filter(max_size <= 100000)
  

final_summary <- distinct(final_summary)
answer <- sum(final_summary1$max_size)
answer

# 417075660 is not right. it's too high.  
# 8291480 is not right. it's too high.
# 1427048 is right!

# Part 2 ------------------------------------------------------------------

total_disk_space_available <- 70000000
disk_space_needed_for_update <- 30000000
disk_space_used <- sum(final_data$file_size, na.rm = TRUE) 
disk_space_empty <- disk_space_available - disk_space_used
disk_space_empty
disk_space_need_to_free <- disk_space_needed_for_update - disk_space_empty
disk_space_need_to_free


part_2 <- final_summary %>%
  group_by(level) %>%
  summarise(max_size = max(sum_file_size)) %>%
  filter(max_size > disk_space_need_to_free)

answer2 <- min(part_2$max_size)
answer2  

