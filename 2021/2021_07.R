## read in the data
crabs  <- read_csv("2021/inputs/2021_07.txt",
                   col_names = FALSE)
## Make the data more tidy
tidy_crabs <- crabs %>%
  pivot_longer(everything()) %>%
  rename(hor_pos = value) %>%
  mutate(across(name, str_replace, 'X', 'crab'))
rm(crabs)
## right now we have the horizontal position of all the crabs
## get the range and average of that data
min_hor_pos <- min(tidy_crabs$hor_pos)
max_hor_pos <- max(tidy_crabs$hor_pos)
avg_hor_pos <- mean(tidy_crabs$hor_pos)

## Write a for loop to do this for us.
## Create a vector to hold the values.
crab_fuel <- vector(mode="double", length = max_hor_pos)

for (i in 1:max_hor_pos) {
  fuel <- sum(abs((i)-tidy_crabs$hor_pos))
  crab_fuel[[i]] <- fuel
  print(fuel)
}

crab_fuel
min(crab_fuel)

## Part Two
## Need to find a better function to calculate how much fuel each crab needs
## Start with tidy_crabs data
tidy_crabs2 <- tidy_crabs

# start with arbitrary point
tidy_crabs2 <- tidy_crabs2 %>%
  mutate(dist_to500 = abs(500-hor_pos)) %>%
  rowwise() %>%
  mutate(fuel_to500 = sum(0:dist_to500))

# I think accurately calculates both the distance and the fuel.

### Part Two, Attempt 2


## Returning to the basic loop

crab_fuel2 <- vector(mode="double", length = max_hor_pos)

for (i in 1:max_hor_pos) {
  dist <- abs((i)-tidy_crabs$hor_pos)
  fuel_per_crab <- (dist/2)*(dist+1)
  #print(paste("Crab moved",dist, "and used",fuel_per_crab,"fuel."))
  total_fuel <- sum(fuel_per_crab)
  crab_fuel2[[i]] <- total_fuel
}
min(crab_fuel2)
