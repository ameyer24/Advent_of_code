input <- read_delim("2022/inputs/2022_02.txt",
                          col_names = c("elf_move","your_move"))

# Work with data ----------------------------------------------------------
# add column to determine if you won the game
# A is Rock, B is Paper, C is Scissors
# X is Rock, Y is Paper, Z is Scissors

rps <- input %>%
  mutate("win_lose" = case_when((elf_move == "A" & your_move == "Y") ~ "WIN",
                                (elf_move == "B" & your_move == "Z") ~ "WIN",
                                (elf_move == "C" & your_move == "X") ~ "WIN",
                                (elf_move == "A" & your_move == "X") ~ "DRAW",
                                (elf_move == "B" & your_move == "Y") ~ "DRAW",
                                (elf_move == "C" & your_move == "Z") ~ "DRAW",
                                TRUE ~ "LOSE")
  )
# add column to calculate the score of each round
rps_score <- rps %>%
  mutate("outcome_score" = case_when(win_lose == "WIN" ~ 6,
                                      win_lose == "DRAW" ~ 3,
                                      win_lose == "LOSE" ~ 0)) %>%
  mutate("shape_score" = case_when(your_move == "X" ~ 1,
                                   your_move == "Y" ~ 2,
                                   your_move == "Z" ~ 3)) %>%
  mutate("score" = outcome_score + shape_score)

# calculate the score of the whole strategy
sum(rps_score$score)

# Part 2 ------------------------------------------------------------------
input2 <- read_delim("2022/inputs/2022_02.txt",
                    col_names = c("elf_move","outcome"))
# The meaning of the inputs have changed so I'm re-importing
# X means lose, Y is Draw, Z is Win
# Need to determine the move to play

rps2 <- input2 %>%
  mutate("your_move" = case_when((outcome == "X" & elf_move == "A") ~ "Z",
                                 (outcome == "Y" & elf_move == "A") ~ "X",
                                 (outcome == "Z" & elf_move == "A") ~ "Y",
                                 (outcome == "X" & elf_move == "B") ~ "X",
                                 (outcome == "Y" & elf_move == "B") ~ "Y",
                                 (outcome == "Z" & elf_move == "B") ~ "Z",
                                 (outcome == "X" & elf_move == "C") ~ "Y",
                                 (outcome == "Y" & elf_move == "C") ~ "Z",
                                 (outcome == "Z" & elf_move == "C") ~ "X",
                                 TRUE ~ "?"))
# must be a better way to do that instead of doing it manually...
# now to calculate the scores
rps2_score <- rps2 %>%
  mutate("outcome_score" = case_when(outcome == "Z" ~ 6,
                                     outcome == "Y" ~ 3,
                                     outcome == "X" ~ 0)) %>%
  mutate("shape_score" = case_when(your_move == "X" ~ 1,
                                   your_move == "Y" ~ 2,
                                   your_move == "Z" ~ 3)) %>%
  mutate("score" = outcome_score + shape_score)
  
# calculate the score of the whole strategy
sum(rps2_score$score)
