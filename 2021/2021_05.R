line_data <- read_csv("2021/inputs/2021_05.txt",
                          col_names = FALSE)

line_data <- line_data %>%
  separate(X2,
           into = c("y1","x2"),
           sep= "->") %>%
  mutate(y1 = parse_number(y1)) %>%
  mutate(x2 = parse_number(x2)) %>%
  rename(x1 = X1,
         y2 = X3)
## line segment data looks good!

## I really care about points, so let's make a list of all the points.

left_points <- line_data %>%
  select (x1,y1) %>%
  rename(x = x1,
         y = y1)
right_points <- line_data %>%
  select(x2,y2) %>%
  rename(x = x2,
         y = y2)

point_data <- bind_rows(left_points,right_points)

## we now need to calculate all the points between two points.
## start with line_data again.
line_segment_data <- line_data

## thinking aloud:
## if y1=y2, add all x values between x1 and x2
## start small, add a column that checks if y1=y2
line_segment_data <- line_data %>%
  mutate(y_match = if_else(y1 == y2,
                           "MATCH","NOPE")) %>%
  mutate(x_match = if_else(x1 == x2,
                           "MATCH","NOPE"))



## okay. basically determined which lines are vertical and horizontal.
## now calculate the points between
## just do the y matches first

v_line_segment_data <- line_segment_data %>%
  filter(y_match == "MATCH") %>%
  group_by(x1,y1,x2,y2) %>%
  mutate(line_len = abs(x1-x2) + 1) %>%
  mutate(x_smaller = min(x1,x2)) %>%
  mutate(x_bigger  = max(x1,x2)) %>%
  uncount(line_len, .remove = FALSE) %>%
  mutate(line_pos = row_number()-1) %>%
  mutate(final_x = x_smaller + line_pos) %>%
  mutate(final_y = y1) %>%
  ungroup() %>%
  select(final_x,final_y)

h_line_segment_data <- line_segment_data %>%
  filter(x_match == "MATCH") %>%
  group_by(x1,y1,x2,y2) %>%
  mutate(line_len = abs(y1-y2) +1) %>%
  mutate(y_smaller = min(y1,y2)) %>%
  mutate(y_bigger  = max(y1,y2)) %>%
  uncount(line_len, .remove = FALSE) %>%
  mutate(line_pos = row_number()-1) %>%
  mutate(final_x = x1) %>%
  mutate(final_y = y_smaller + line_pos) %>%
  ungroup() %>%
  select(final_x,final_y)

point_data <- bind_rows(v_line_segment_data,h_line_segment_data)
### NOT SURE WHY THIS WORKED BUT WHATEVER!!!!!!!!!!
point_data_n <- point_data %>%
  group_by(final_x, final_y) %>%
  add_count() %>%
  filter(n>1) %>%
  n_distinct()


## I now need to find the diagonal lines.
d_line_segment_data <- line_segment_data %>%
  mutate(d_match = if_else(abs(x1-x2) == abs(y1-y2),
                           "MATCH",
                           "NOPE")) %>%
  filter(d_match == "MATCH") %>%
  mutate(diag_dir = if_else((y1-y2)/(x1-x2) == 1,
                            "UP",
                            "DOWN"))
  
du_line_segment_data <- d_line_segment_data %>%
  filter(diag_dir == "UP") %>%
  group_by(x1,y1,x2,y2) %>%
  mutate(line_lenx = abs(x1-x2) +1) %>%
  mutate(line_leny = abs(y1-y2) +1) %>% ## confirming they are the same.
  uncount(line_lenx, .remove = FALSE) %>%
  mutate(line_pos = row_number()-1) %>%
  mutate(final_x = min(x1,x2) + line_pos) %>%
  mutate(final_y = min(y1,y2) + line_pos) %>%
  ungroup() %>%
  select(final_x,final_y)


dd_line_segment_data <- d_line_segment_data %>%
  filter(diag_dir == "DOWN") %>%
  group_by(x1,y1,x2,y2) %>%
  mutate(line_lenx = abs(x1-x2) +1) %>%
  mutate(line_leny = abs(y1-y2) +1) %>% ## confirming they are the same.
  uncount(line_lenx, .remove = FALSE) %>%
  mutate(line_pos = row_number()-1) %>%
  mutate(final_x = min(x1,x2) + line_pos) %>%
  mutate(final_y = max(y1,y2) - line_pos) %>%
  ungroup()%>%
  select(final_x,final_y)

d_point_data <- bind_rows(du_line_segment_data,dd_line_segment_data)

all_points <- bind_rows(point_data,d_point_data)

## Do my messy count process
all_points_n <- all_points %>%
  group_by(final_x, final_y) %>%
  add_count() %>%
  filter(n >1) %>%
  n_distinct()
  
