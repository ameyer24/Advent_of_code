## import the data
raw_data <- read_csv("2021/inputs/2021_01.txt")

## find the difference in values
raw_data <- mutate(raw_data,
                   DIFF = VALUE - lag(VALUE))
# Count the number of times the value increases
# Count when DIFF is above 0
increases_only <- filter (raw_data, DIFF >0)

# Make a new column with the triple sum
raw_data <- mutate(raw_data,
                    SUM3 = VALUE + lag(VALUE) +lag(lag(VALUE)))

raw_data <-mutate(raw_data,
                   SUM3DIFF = SUM3 - lag(SUM3))

raw_data_filtered <- filter(raw_data, SUM3DIFF >0)

