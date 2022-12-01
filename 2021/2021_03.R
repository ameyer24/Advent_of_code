data <- read_csv("2021/inputs/2021_03.txt",
                 col_names = "numbers",
                 col_types = c(col_integer()))
## Find the gamma rate
## find the most common digit at each "spot"

## What's the most common first digit?
## Break it into each digit spot.
num_of_dig <- mean(str_length(data$numbers))

data1 <- data %>%
  separate(numbers,
           into = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12"),
           sep="") %>%
  select(-d0) %>%
  mutate_if(is.character,as.numeric)



## I bet I can just take the average of each column to get the answer
column_means <- data1 %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## find the gamma binary
gamma_bin <- data1 %>%
  summarise(
    across(
      where(is.numeric), ~ 
        case_when(mean(.x) >= 0.5 ~ 1,
                  mean(.x) <= 0.5 ~ 0)
        )
  ) %>%
  unite(col=gamma,
        sep = "")
## find the epsilon binary
epsilon_bin <- data1 %>%
  summarise(
    across(
      where(is.numeric), ~ 
        case_when(mean(.x) >= 0.5 ~ 0,
                  mean(.x) <= 0.5 ~ 1)
    )
  ) %>%
  unite(col=epsilon,
        sep = "")

gamma_bin$gamma
epsilon_bin$epsilon
## I just did that bit by hand. hahaha

strtoi(gamma_bin$gamma, base=2) * strtoi(epsilon_bin$epsilon, base=2)







####### PART 2
## start with data1
## find the most common value and filter for that.


most_common <- function(x) {
  case_when(mean(x) >= 0.5 ~ 1,
            mean(x) < 0.5 ~ 0)
}

## least common
least_common <- function(x) {
  case_when(mean(x) == 0.5 ~ 0,
            mean(x) > 0.5 ~ 0,
            mean(x) < 0.5 ~ 1)
}

most_common(data1$d2)
least_common(data1$d2)

oxy_test <- data1 %>%
  filter(d1==most_common(d1)) %>%
  filter(d2==most_common(d2)) %>%
  filter(d3==most_common(d3)) %>%
  filter(d4==most_common(d4)) %>%
  filter(d5==most_common(d5)) %>%
  filter(d6==most_common(d6)) %>%
  filter(d7==most_common(d7)) %>%
  filter(d8==most_common(d8)) %>%
  filter(d9==most_common(d9)) %>%
  filter(d10==most_common(d10)) %>%
  filter(d11==most_common(d11)) %>%
  filter(d12==most_common(d12))

oxy_bin <- unite(oxy_test,
                 col=number,
                 sep="")
oxy_gen_rating <- oxy_bin$number

### co2 test
co2_test <- data1 %>%
  filter(d1==least_common(d1)) %>%
  filter(d2==least_common(d2)) %>%
  filter(d3==least_common(d3)) %>%
  filter(d4==least_common(d4)) %>%
  filter(d5==least_common(d5)) %>%
  filter(d6==least_common(d6)) %>%
  filter(d7==least_common(d7)) %>%
  filter(d8==least_common(d8)) %>%
  filter(d9==least_common(d9))


co2_bin <- unite(co2_test,
                 col=number,
                 sep="")
co2_scrub_rating <- co2_bin$number

strtoi(oxy_gen_rating, base=2) * strtoi(co2_scrub_rating, base=2)




## what an inefficient way to do this! But it's going to work!!!
## update this does not work. the most common digit changes based on the sample.

## Invent stupider way to do this.
oxy_test1 <- data2 %>%
  filter(d1==1)

oxy_testd2 <- oxy_test1 %>%
  select(d2) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## Do it again
## d2
oxy_test2 <- oxy_test1 %>%
  filter(d2==1)

oxy_testd3 <- oxy_test2 %>%
  select(d3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## Do it again
## d3
oxy_test3 <- oxy_test2 %>%
  filter(d3==0)

oxy_testd4 <- oxy_test3 %>%
  select(d4) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## Do it again
## d4
oxy_test4 <- oxy_test3 %>%
  filter(d4==1)

oxy_testd5 <- oxy_test4 %>%
  select(d5) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## Do it again
## d5
oxy_test5 <- oxy_test4 %>%
  filter(d5==1)

oxy_testd6 <- oxy_test5 %>%
  select(d6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## Do it again
## d6
oxy_test6 <- oxy_test5 %>%
  filter(d6==1)

oxy_testd7 <- oxy_test6 %>%
  select(d7) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## Do it again
## d7
oxy_test7 <- oxy_test6 %>%
  filter(d7==1)

oxy_testd8 <- oxy_test7 %>%
  select(d8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## Do it again
## d8
oxy_test8 <- oxy_test7 %>%
  filter(d8==1)

oxy_testd9 <- oxy_test8 %>%
  select(d9) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## Do it again
## d9
oxy_test9 <- oxy_test8 %>%
  filter(d9==1)

oxy_testd10 <- oxy_test9 %>%
  select(d10) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## Do it again
## d10
oxy_test10 <- oxy_test9 %>%
  filter(d10==1)

oxy_testd11 <- oxy_test10 %>%
  select(d11) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## MAKE THIS A LITTLE BETTER
## Improve the test function.
## YOLO!!!

## CO2 d1
CO2_test1 <- data2 %>%
  filter(d1==0)

CO2_testd2 <- CO2_test1 %>%
  select(d2) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## CO2 d2
CO2_test2 <- CO2_test1 %>%
  filter(d2==0)

CO2_testd3 <- CO2_test2 %>%
  select(d3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## CO2 d3
CO2_test3 <- CO2_test2 %>%
  filter(d3==1)

CO2_testd4 <- CO2_test3 %>%
  select(d4) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## CO2 d4
CO2_test4 <- CO2_test3 %>%
  filter(d4==0)

CO2_testd5 <- CO2_test4 %>%
  select(d5) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## CO2 d5
CO2_test5 <- CO2_test4 %>%
  filter(d5==1)

CO2_testd6 <- CO2_test5 %>%
  select(d6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## CO2 d6
CO2_test6 <- CO2_test5 %>%
  filter(d6==0)

CO2_testd7 <- CO2_test6 %>%
  select(d7) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## CO2 d7
CO2_test7 <- CO2_test6 %>%
  filter(d7==0)

CO2_testd8 <- CO2_test7 %>%
  select(d8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## CO2 d8
CO2_test8 <- CO2_test7 %>%
  filter(d8==0)

CO2_testd9 <- CO2_test8 %>%
  select(d9) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
## CO2 d9
CO2_test9 <- CO2_test8 %>%
  filter(d9==1)

CO2_testd10 <- CO2_test9 %>%
  select(d10) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


strtoi("110111111111", base=2) * strtoi("001010001000", base=2)










