input <- read_lines("2021/inputs/2021_10.txt")
nav_lines <- enframe(input, name = "name", value = "value")
rm(input)
nav_lines$value[1]

## For some reasons letters make more sense to me.
## And are easier to use in regular expressions.
## So I'm changing them.
nav_lines1 <- nav_lines %>%
  mutate(value = str_replace_all(value, "\\(", "A")) %>%
  mutate(value = str_replace_all(value, "\\[", "B")) %>%
  mutate(value = str_replace_all(value, "\\{", "C")) %>%
  mutate(value = str_replace_all(value, "\\<", "D")) %>%
  mutate(value = str_replace_all(value, "\\)", "a")) %>%
  mutate(value = str_replace_all(value, "\\]", "b")) %>%
  mutate(value = str_replace_all(value, "\\}", "c")) %>%
  mutate(value = str_replace_all(value, "\\>", "d"))

nav_lines1 <- nav_lines1 %>%
  mutate(orig_line_length = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length1 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length2 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length3 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length4 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length5 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length6 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length7 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length8 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length9 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length10 = str_length(value)) %>%
  mutate(value = str_remove_all(value,"Aa|Bb|Cc|Dd")) %>%
  mutate(line_length11 = str_length(value))

## doing some crude iteration there. hahaha
## string with ALL CAPS are fine just incomplete.
## but the first lower case letter is the problem
## so find a way to get that.
nav_lines2 <- nav_lines1 %>%
  mutate(illegal_char = str_extract(value, "[a-z]")) %>%
  drop_na(illegal_char)

## Assign the values and sum?
nav_lines3 <- nav_lines2 %>%
  mutate(answer_value = case_when(illegal_char == "a" ~ 3,
                                  illegal_char == "b" ~ 57,
                                  illegal_char == "c" ~ 1197,
                                  illegal_char == "d" ~ 25137)) %>%
  summarise(sum(answer_value))

######################################################################
## Part Two
######################################################################

## This looks at the incomplete stings so let's get those.
incomplete_nav_lines <- nav_lines1 %>%
  mutate(illegal_char = str_extract(value, "[a-z]")) %>%
  filter(is.na(illegal_char)) %>%
  select(name, value)

## First, determine the string needed to close it.
## Flip string and make it lower case.

incomplete_nav_lines1 <- incomplete_nav_lines %>%
  mutate(string_end = stri_reverse(value)) %>%
  mutate(string_end = str_to_lower(string_end))

## okay. I now have the closing sequence
## just need to figure out how to score it
## seems like a good time to write a function
## Writing a function is hard. Just mangle it.

convert_char_to_points <- function(x) {
  case_when(
    x=="a" ~ 1,
    x=="b" ~ 2,
    x=="c" ~ 3,
    x=="d" ~ 4,
    TRUE ~ 0
  )
}

incomplete_nav_lines2 <- incomplete_nav_lines1 %>%
  mutate(char1 = convert_char_to_points(str_sub(string_end, 1,1))) %>%
  mutate(char2 = convert_char_to_points(str_sub(string_end, 2,2))) %>%
  mutate(char3 = convert_char_to_points(str_sub(string_end, 3,3))) %>%
  mutate(char4 = convert_char_to_points(str_sub(string_end, 4,4))) %>%
  mutate(char5 = convert_char_to_points(str_sub(string_end, 5,5))) %>%
  mutate(char6 = convert_char_to_points(str_sub(string_end, 6,6))) %>%
  mutate(char7 = convert_char_to_points(str_sub(string_end, 7,7))) %>%
  mutate(char8 = convert_char_to_points(str_sub(string_end, 8,8))) %>%
  mutate(char9 = convert_char_to_points(str_sub(string_end, 9,9))) %>%
  mutate(char10 = convert_char_to_points(str_sub(string_end, 10,10))) %>%
  mutate(char11 = convert_char_to_points(str_sub(string_end, 11,11))) %>%
  mutate(char12 = convert_char_to_points(str_sub(string_end, 12,12))) %>%
  mutate(char13 = convert_char_to_points(str_sub(string_end, 13,13))) %>%
  mutate(char14 = convert_char_to_points(str_sub(string_end, 14,14))) %>%
  mutate(char15 = convert_char_to_points(str_sub(string_end, 15,15)))
 
score_the_score <- function(last_score,current_char) {
  score <- case_when(
    current_char == 0 ~ last_score,
    TRUE ~ (last_score *5 ) + current_char
  )
}

incomplete_nav_lines3 <- incomplete_nav_lines2 %>%
  mutate(score1 = char1) %>%
  mutate(score2 = score_the_score(score1,char2)) %>%
  mutate(score3 = score_the_score(score2,char3)) %>%
  mutate(score4 = score_the_score(score3,char4)) %>%
  mutate(score5 = score_the_score(score4,char5)) %>%
  mutate(score6 = score_the_score(score5,char6)) %>%
  mutate(score7 = score_the_score(score6,char7)) %>%
  mutate(score8 = score_the_score(score7,char8)) %>%
  mutate(score9 = score_the_score(score8,char9)) %>%
  mutate(score10 = score_the_score(score9,char10)) %>%
  mutate(score11 = score_the_score(score10,char11)) %>%
  mutate(score12 = score_the_score(score11,char12)) %>%
  mutate(score13 = score_the_score(score12,char13)) %>%
  mutate(score14 = score_the_score(score13,char14)) %>%
  mutate(score15 = score_the_score(score14,char15))

median(incomplete_nav_lines3$score15)

## I really need to find a better way to iterate!!!