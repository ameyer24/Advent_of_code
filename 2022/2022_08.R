
input <- read_csv("2022/inputs/2022_08.txt",
                  col_names = "value")


# Part 1 ------------------------------------------------------------------

# Make data tidy; split each character into it's own column

row_len <- str_length(input[[1,1]])

tidy_input <- input %>%
  separate(value,into=paste(00:99),sep="") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, names_to = "colname") %>%
  filter(colname != 0) %>%
  type_convert()

# write function to determine if the trees are visible.
# just think by row (left to right) ((aka top to bottom))

left <- tidy_input %>%
  group_by(rowname) %>%
  mutate(left_tree_height = lag(value)) %>%
  mutate(left_vis1 = case_when(is.na(left_tree_height) ~ TRUE,
                              value > left_tree_height ~ TRUE,
                              value <=left_tree_height ~ FALSE)) %>%
  mutate(hidden_tree_count = cumsum(left_vis1 == FALSE)) %>%
  mutate(visible = case_when(left_vis1 == TRUE & hidden_tree_count == 0 ~ TRUE,
                              TRUE ~ FALSE)) %>%
  select(-left_tree_height,-left_vis1,-hidden_tree_count) %>%
  filter(visible == TRUE)

right <-  tidy_input %>%
  group_by(rowname) %>%
  arrange(desc(colname), .by_group = TRUE) %>%
  mutate(right_tree_height = lag(value)) %>%
  mutate(right_vis1 = case_when(is.na(right_tree_height) ~ TRUE,
                               value > right_tree_height ~ TRUE,
                               value <=right_tree_height ~ FALSE)) %>%
  mutate(hidden_tree_count = cumsum(right_vis1 == FALSE)) %>%
  mutate(visible = case_when(right_vis1 == TRUE & hidden_tree_count == 0 ~ TRUE,
                              TRUE ~ FALSE)) %>%
  select(-right_tree_height,-right_vis1,-hidden_tree_count) %>%
  filter(visible == TRUE)

top <- tidy_input %>%
  group_by(colname) %>%
  arrange(rowname, .by_group = TRUE) %>%
  mutate(top_tree_height = lag(value)) %>%
  mutate(top_vis1 = case_when(is.na(top_tree_height) ~ TRUE,
                                value > top_tree_height ~ TRUE,
                                value <=top_tree_height ~ FALSE)) %>%
  mutate(hidden_tree_count = cumsum(top_vis1 == FALSE)) %>%
  mutate(visible = case_when(top_vis1 == TRUE & hidden_tree_count == 0 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  select(-top_tree_height,-top_vis1,-hidden_tree_count) %>%
  filter(visible == TRUE)
  
bottom <- tidy_input %>%
  group_by(colname) %>%
  arrange(desc(rowname), .by_group = TRUE) %>%
  mutate(bot_tree_height = lag(value)) %>%
  mutate(bot_vis1 = case_when(is.na(bot_tree_height) ~ TRUE,
                              value > bot_tree_height ~ TRUE,
                              value <=bot_tree_height ~ FALSE)) %>%
  mutate(hidden_tree_count = cumsum(bot_vis1 == FALSE)) %>%
  mutate(visible = case_when(bot_vis1 == TRUE & hidden_tree_count == 0 ~ TRUE,
                             TRUE ~ FALSE)) %>%
  select(-bot_tree_height,-bot_vis1,-hidden_tree_count) %>%
  filter(visible == TRUE)


combined <- bind_rows(left,right,top,bottom)

combined1 <- combined %>%
  distinct(rowname,colname)

# 606 is too low.

# Part 1 (Again) ----------------------------------------------------------
# This approach does not work because we cannot stop once we tree isn't visible.
# for example, 1415 should be TRUE TRUE FALSE TRUE
# (because tree 5 is taller than all the trees to the left)
# The approach above marks that as FALSE

# can we tweak the approach above?


left1 <- tidy_input %>%
  group_by(rowname) %>%
  arrange(colname, .by_group = TRUE) %>%
  mutate(tallest_tree_to_edge = cummax(value)) %>%
  mutate(visible = case_when(is.na(lag(tallest_tree_to_edge)) ~ TRUE,
                             value > lag(tallest_tree_to_edge) ~ TRUE,
                             value <= lag(tallest_tree_to_edge)~ FALSE)) %>%
  select(-tallest_tree_to_edge) %>%
  filter(visible == TRUE)

right1 <- tidy_input %>%
  group_by(rowname) %>%
  arrange(desc(colname), .by_group = TRUE) %>%
  mutate(tallest_tree_to_edge = cummax(value)) %>%
  mutate(visible = case_when(is.na(lag(tallest_tree_to_edge)) ~ TRUE,
                             value > lag(tallest_tree_to_edge) ~ TRUE,
                             value <= lag(tallest_tree_to_edge)~ FALSE)) %>%
  select(-tallest_tree_to_edge) %>%
  filter(visible == TRUE)

top1 <- tidy_input %>%
  group_by(colname) %>%
  arrange(rowname, .by_group = TRUE) %>%
  mutate(tallest_tree_to_edge = cummax(value)) %>%
  mutate(visible = case_when(is.na(lag(tallest_tree_to_edge)) ~ TRUE,
                             value > lag(tallest_tree_to_edge) ~ TRUE,
                             value <= lag(tallest_tree_to_edge)~ FALSE)) %>%
  select(-tallest_tree_to_edge) %>%
  filter(visible == TRUE)

bot1 <- tidy_input %>%
  group_by(colname) %>%
  arrange(desc(rowname), .by_group = TRUE) %>%
  mutate(tallest_tree_to_edge = cummax(value)) %>%
  mutate(visible = case_when(is.na(lag(tallest_tree_to_edge)) ~ TRUE,
                             value > lag(tallest_tree_to_edge) ~ TRUE,
                             value <= lag(tallest_tree_to_edge)~ FALSE)) %>%
  select(-tallest_tree_to_edge) %>%
  filter(visible == TRUE)

combined <- bind_rows(left1,right1,top1,bot1)

combined1 <- combined %>%
  distinct(rowname,colname)

answer1 <- nrow(combined1)     
answer1


# Part 2 ------------------------------------------------------------------


input <- read_csv("2022/inputs/2022_08.txt",
                  col_names = "value")

tidy_input <- input %>%
  separate(value,into=paste(00:99),sep="") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, names_to = "colname") %>%
  filter(colname != 0) %>%
  type_convert()

left1 <- tidy_input %>%
  group_by(rowname) %>%
  arrange(colname, .by_group = TRUE)




# Part 2 - STOLEN ANSWER ------------------------------------------------------------------


input <- readLines("2022/inputs/2022_08.txt") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(nrow=99, byrow=T)

scenic_score <- function(x,y) {
  
  this_tree <- input[x,y]
  
  if (any(x == 1, x == 99, y == 1, y == 99))   return(0)
  
  scores <- c(min(which(input[(x-1):1,y] >= this_tree)[1],length(input[(x-1):1]),na.rm = T),    #up 
              min(which(input[(x+1):99,y] >= this_tree)[1],length(input[(x+1):99]),na.rm = T),   #down
              min(which(input[x,(y-1):1] >= this_tree)[1],length(input[(y-1):1]),na.rm = T),    #left
              min(which(input[x,(y+1):99] >= this_tree)[1],length(input[(y+1):99]),na.rm = T))
  
  score <- scores[1]*scores[2]*scores[3]*scores[4]
  
  return(score)
  
}

scenic_trees <- matrix(nrow=99, ncol=99)

for (x in 1:99) {
  for (y in 1:99)
    scenic_trees[x,y] <- scenic_score(x,y)
}

max(scenic_trees)
