signal_data <- read_delim("2021/inputs/2021_08.txt",
                      col_names = FALSE)

## Divide the data into "signal patterns" and "output values"
signal_patterns <- signal_data %>%
  select(1:10) %>%
  rename(signal1  = X1) %>%
  rename(signal2  = X2) %>%
  rename(signal3  = X3) %>%
  rename(signal4  = X4) %>%
  rename(signal5  = X5) %>%
  rename(signal6  = X6) %>%
  rename(signal7  = X7) %>%
  rename(signal8  = X8) %>%
  rename(signal9  = X9) %>%
  rename(signal10 = X10)
output_values   <- signal_data %>%
  select(12:15) %>%
  rename(output1  = X12) %>%
  rename(output2  = X13) %>%
  rename(output3  = X14) %>%
  rename(output4  = X15)
rm(signal_data)

## Looking at the output values, find the number of times the digits 1,4,7,8 appear.
## Digit 1 has a length of 2
## Digit 4 has a length of 4
## Digit 7 has a length of 3
## Digit 8 has a length of 7

## Make that data tidy!
tidy_output_values <- output_values %>%
  pivot_longer(everything()) %>%
  mutate(signal_length = nchar(value)) %>%
  filter(signal_length %in% c(2,3,4,7))

## okay. That was pretty easy.

###############################################################################
##PART TWO
###############################################################################
## Let's look at row one of the signal patterns.
## 1 and 7 tell you the top bar (it must be c)
## 1 and 4 tell you the middle bar and the top left
## the frequency of the display tells you something too.

### New approach
### Combine all the signal patterns.
### Look at the frequency of each signal.
signal_patterns_segments <- signal_patterns %>%
  rowid_to_column("entry_num") %>%
  unite("all_signals", signal1:signal10,
        remove = FALSE,
        sep = "")
## look at the frequency of each signal
## some of them are unique


## look just at the frequency of the signals to get some information.
uncrossed_wires1 <- signal_patterns_segments %>%
  mutate(code_for_A = NA) %>%
  mutate(code_for_B = case_when(str_count(all_signals,"a") == 6 ~ "a",
                                str_count(all_signals,"b") == 6 ~ "b",
                                str_count(all_signals,"c") == 6 ~ "c",
                                str_count(all_signals,"d") == 6 ~ "d",
                                str_count(all_signals,"e") == 6 ~ "e",
                                str_count(all_signals,"f") == 6 ~ "f",
                                str_count(all_signals,"g") == 6 ~ "g",
                                TRUE ~ "WTF")) %>%
  mutate(code_for_C = NA) %>%
  mutate(code_for_D = NA) %>%
  mutate(code_for_E = case_when(str_count(all_signals,"a") == 4 ~ "a",
                                str_count(all_signals,"b") == 4 ~ "b",
                                str_count(all_signals,"c") == 4 ~ "c",
                                str_count(all_signals,"d") == 4 ~ "d",
                                str_count(all_signals,"e") == 4 ~ "e",
                                str_count(all_signals,"f") == 4 ~ "f",
                                str_count(all_signals,"g") == 4 ~ "g",
                                TRUE ~ "WTF")) %>%
  mutate(code_for_F = case_when(str_count(all_signals,"a") == 9 ~ "a",
                                str_count(all_signals,"b") == 9 ~ "b",
                                str_count(all_signals,"c") == 9 ~ "c",
                                str_count(all_signals,"d") == 9 ~ "d",
                                str_count(all_signals,"e") == 9 ~ "e",
                                str_count(all_signals,"f") == 9 ~ "f",
                                str_count(all_signals,"g") == 9 ~ "g",
                                TRUE ~ "WTF")) %>%
  mutate(code_for_G = NA)



## use that information to get more information
uncrossed_wires2 <- uncrossed_wires1 %>%
  mutate(code_for_C = case_when(str_length(signal1) == 2 ~ str_remove(signal1, code_for_F),
                                str_length(signal2) == 2 ~ str_remove(signal2, code_for_F),
                                str_length(signal3) == 2 ~ str_remove(signal3, code_for_F),
                                str_length(signal4) == 2 ~ str_remove(signal4, code_for_F),
                                str_length(signal5) == 2 ~ str_remove(signal5, code_for_F),
                                str_length(signal6) == 2 ~ str_remove(signal6, code_for_F),
                                str_length(signal7) == 2 ~ str_remove(signal7, code_for_F),
                                str_length(signal8) == 2 ~ str_remove(signal8, code_for_F),
                                str_length(signal9) == 2 ~ str_remove(signal9, code_for_F),
                                str_length(signal10) == 2 ~ str_remove(signal10, code_for_F),)) %>%
  mutate(code_for_A = case_when(str_length(signal1) == 3 ~ str_remove(signal1, code_for_F),
                                str_length(signal2) == 3 ~ str_remove(signal2, code_for_F),
                                str_length(signal3) == 3 ~ str_remove(signal3, code_for_F),
                                str_length(signal4) == 3 ~ str_remove(signal4, code_for_F),
                                str_length(signal5) == 3 ~ str_remove(signal5, code_for_F),
                                str_length(signal6) == 3 ~ str_remove(signal6, code_for_F),
                                str_length(signal7) == 3 ~ str_remove(signal7, code_for_F),
                                str_length(signal8) == 3 ~ str_remove(signal8, code_for_F),
                                str_length(signal9) == 3 ~ str_remove(signal9, code_for_F),
                                str_length(signal10) == 3 ~ str_remove(signal10, code_for_F),)) %>%
  mutate(code_for_A = str_remove(code_for_A, code_for_C)) %>%
  mutate(code_for_D = case_when(str_length(signal1) == 4 ~ str_remove(signal1, code_for_F),
                                str_length(signal2) == 4 ~ str_remove(signal2, code_for_F),
                                str_length(signal3) == 4 ~ str_remove(signal3, code_for_F),
                                str_length(signal4) == 4 ~ str_remove(signal4, code_for_F),
                                str_length(signal5) == 4 ~ str_remove(signal5, code_for_F),
                                str_length(signal6) == 4 ~ str_remove(signal6, code_for_F),
                                str_length(signal7) == 4 ~ str_remove(signal7, code_for_F),
                                str_length(signal8) == 4 ~ str_remove(signal8, code_for_F),
                                str_length(signal9) == 4 ~ str_remove(signal9, code_for_F),
                                str_length(signal10) == 4 ~ str_remove(signal10, code_for_F),)) %>%
  mutate(code_for_D = str_remove(code_for_D, code_for_C)) %>%
  mutate(code_for_D = str_remove(code_for_D, code_for_B)) %>%
  mutate(code_for_G = str_remove("abcdefg", code_for_A)) %>%
  mutate(code_for_G = str_remove(code_for_G, code_for_B)) %>%
  mutate(code_for_G = str_remove(code_for_G, code_for_C)) %>%
  mutate(code_for_G = str_remove(code_for_G, code_for_D)) %>%
  mutate(code_for_G = str_remove(code_for_G, code_for_E)) %>%
  mutate(code_for_G = str_remove(code_for_G, code_for_F))

## Okay! I now know what each wire actually means.
## Just need to translate the output values now using that information.
## But I really don't... I just need to use that information to disambiguate some of them...

everything <- cbind(uncrossed_wires2,output_values)

everything1 <- everything %>%
  mutate(decoded1 = case_when(str_length(output1) == 2 ~ 1,
                              str_length(output1) == 3 ~ 7,
                              str_length(output1) == 4 ~ 4,
                              str_length(output1) == 7 ~ 8,
                              ## When the string is 5 of 6 characters...
                              ## We have more work todo!
                              str_length(output1) == 6 ~ case_when(str_remove(output1, code_for_C) == output1 ~ 6,
                                                                   str_remove(output1, code_for_E) == output1 ~ 9,
                                                                   str_remove(output1, code_for_D) == output1 ~ 0),
                              str_length(output1) == 5 ~ case_when(str_remove(output1, code_for_B) != output1 ~ 5,
                                                                   str_remove(output1, code_for_E) != output1 ~ 2,
                                                                   str_remove(output1, code_for_E) == output1 ~ 3)
                                
                              )
  ) %>%
  mutate(decoded2 = case_when(str_length(output2) == 2 ~ 1,
                              str_length(output2) == 3 ~ 7,
                              str_length(output2) == 4 ~ 4,
                              str_length(output2) == 7 ~ 8,
                              ## When the string is 5 of 6 characters...
                              ## We have more work todo!
                              str_length(output2) == 6 ~ case_when(str_remove(output2, code_for_C) == output2 ~ 6,
                                                                   str_remove(output2, code_for_E) == output2 ~ 9,
                                                                   str_remove(output2, code_for_D) == output2 ~ 0),
                              str_length(output2) == 5 ~ case_when(str_remove(output2, code_for_B) != output2 ~ 5,
                                                                   str_remove(output2, code_for_E) != output2 ~ 2,
                                                                   str_remove(output2, code_for_E) == output2 ~ 3)
                              
  )
  ) %>%
  mutate(decoded3 = case_when(str_length(output3) == 2 ~ 1,
                              str_length(output3) == 3 ~ 7,
                              str_length(output3) == 4 ~ 4,
                              str_length(output3) == 7 ~ 8,
                              ## When the string is 5 of 6 characters...
                              ## We have more work todo!
                              str_length(output3) == 6 ~ case_when(str_remove(output3, code_for_C) == output3 ~ 6,
                                                                   str_remove(output3, code_for_E) == output3 ~ 9,
                                                                   str_remove(output3, code_for_D) == output3 ~ 0),
                              str_length(output3) == 5 ~ case_when(str_remove(output3, code_for_B) != output3 ~ 5,
                                                                   str_remove(output3, code_for_E) != output3 ~ 2,
                                                                   str_remove(output3, code_for_E) == output3 ~ 3)
                              
  )
  ) %>%
  mutate(decoded4 = case_when(str_length(output4) == 2 ~ 1,
                              str_length(output4) == 3 ~ 7,
                              str_length(output4) == 4 ~ 4,
                              str_length(output4) == 7 ~ 8,
                              ## When the string is 5 of 6 characters...
                              ## We have more work todo!
                              str_length(output4) == 6 ~ case_when(str_remove(output4, code_for_C) == output4 ~ 6,
                                                                   str_remove(output4, code_for_E) == output4 ~ 9,
                                                                   str_remove(output4, code_for_D) == output4 ~ 0),
                              str_length(output4) == 5 ~ case_when(str_remove(output4, code_for_B) != output4 ~ 5,
                                                                   str_remove(output4, code_for_E) != output4 ~ 2,
                                                                   str_remove(output4, code_for_E) == output4 ~ 3)
                              
  )
  )
## Convert each digit to a number.
## Again, what a dumb way to do this!
final_answer <- everything1 %>%
  mutate(final_answer = decoded1 *1000 + decoded2 * 100 + decoded3 *10 + decoded4)
## This is the final answer.
sum(final_answer$final_answer)
         