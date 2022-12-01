bingo_numbers <- read_csv("2021/inputs/2021_04.txt",
                          col_names = FALSE,
                          n_max = 1)

bingo_boards <- read_fwf("2021/inputs/2021_04.txt",
                         skip = 1,
                         fwf_widths(c(3,3,3,3,2),
                                    col_names = c("c1","c2","c3","c4","c5"))
)

## how should I do this?
## try making the bingo boards tidy?
## add column with bingo board number?

tidy_bb1 <- bingo_boards %>%
  drop_na()
## Add column with board number.
## need a way to say 1 five times and then 2 five times...
tidy_bb2 <- tidy_bb1 %>%
  mutate(board_num = ceiling(seq(0.1,99.9,0.2)))
## God what a crazy way to do that.

## Add column with row number (of the game)
tidy_bb3 <- tidy_bb2 %>%
  group_by(board_num) %>%
  mutate(row_num = row_number())

## okay - now I just need to pivot the data
tidy_bb <-tidy_bb3 %>%
  pivot_longer(starts_with("c"),
               names_to = "col_num")
rm(tidy_bb1,tidy_bb2,tidy_bb3)
## Okay! The bingo board data is now tidy!
## The other numbers are just a list so convert to list.
tidy_bn1 <- as.list(bingo_numbers)
## this function "calls" the game.

AJM <- tidy_bb %>%
  group_split(board_num)


AJM[1]








## It was hard to write functions.
## Just going for it.
tidy_bb0 <- tidy_bb %>%
  rename(value_0 = value)

tidy_bb1 <- tidy_bb0 %>%
  mutate(value_1 = case_when(value_0 == tidy_bn1[1]~ 999,
                             TRUE ~ value_0)) %>%
  check_for_hor_bingo()


check_for_hor_bingo <- function(df){
 df %>%
    group_by(board_num, row_num) %>%
    summarize(sum(value_1))
}










play_bingo <- function(boards, numbers) {
  for(i in numbers) {
    print(i)
  }
}

AJM <- play_bingo(tidy_bb,tidy_bn1)












pluck(tidy_bn1, 1)
pluck(tidy_bn1, 3)
pluck(tidy_bn1, 99)

## Let's figure out what a "bingo" is now!
# Horizontal bingo:
# Board Number the same, row number the same, all columns are called.
# Vertical bingo
# Board number the same, row numbers all different, column numbers are the same.

## create a dataframe for bingo sequences.
bingo_hor_lines <- tidy_bb %>%
  group_by(board_num,row_num) %>%
  summarize(seq = list(value))

bingo_ver_lines <- tidy_bb %>%
  group_by(board_num,col_num) %>%
  summarize(seq = list(value))

bingo_lines <- rbind(bingo_hor_lines,bingo_ver_lines)
bingo_lines <- bingo_lines %>%
  select(board_num, seq)

## These are the bingo lines that could win.
## Definitely want to make a loop for this.
i <- 0
while (i <= length(tidy_bn1)) {
  bingo_number <- pluck(tidy_bn1, i)
  print(bingo_number)
  tidy_bb$value[tidy_bb$value == bingo_number] <- "X"
  paste("bb", i) <- tidy_bb
  i <- i+1
}

check_for_ver_bingo <- function(){
  
}

