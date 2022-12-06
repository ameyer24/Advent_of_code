
# Import Data -------------------------------------------------------------

cargo <- read.fwf("2022/inputs/2022_05.txt",
                  widths = c(4,4,4,4,4,4,4,4,4),
                  n = 8) # the column numbers are the headings so do 8 and not 9

instructions <- read_csv("2022/inputs/2022_05.txt",
                         col_names = "step",
                         skip = 9)

# Clean the Data -------------------------------------------------------
clean_cargo <- cargo %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("["))) %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("]")))
  
clear_instruction <- instructions %>%
  separate(step,
           into = c("w1","amount","w2","source","w3","destination"),
           sep = " ") %>%
  select(-starts_with("w")) %>% # We don't need the words
  type_convert() #convert numeric values to numbers

# Part 1 ------------------------------------------------------------------

# This creates a list of lists?
stacks = as.list(clean_cargo)
# this deletes all the empty spaces from each list
stacks <- lapply(stacks, function(z){ z[z != ""]})
# This reverses each list (we need the 'bottom' on the left)
stacks <- lapply(stacks, function(z){rev(z)})

# This function moves the boxes.
move_box <- function(starting_stacks,instructions) {
  for (i in 1:nrow(instructions)) {
    x <- instructions$source[i]
    y <- instructions$destination[i]
    amt <- instructions$amount[i]
    for (j in 1:amt) {
      box_to_move <- tail(stacks[[x]],1) # gets the box
      stacks[[x]] <- head(stacks[[x]],-1) # removes it from the source stack
      stacks[[y]] <- c(stacks[[y]], box_to_move)
    }
  }
  return(stacks)
}

final_stacks <- move_box(stacks,clear_instruction)
final_tops <- lapply(final_stacks, function(z){tail(z,1)})

# Part 2 ------------------------------------------------------------------
# Just need to update the move_boxes function
move_box_2 <- function(starting_stacks,instructions) {
  for (i in 1:nrow(instructions)) {
    x <- instructions$source[i]
    y <- instructions$destination[i]
    amt <- instructions$amount[i]
    box_to_move <- tail(stacks[[x]],amt) # gets the boxes
    stacks[[x]] <- head(stacks[[x]],-amt) # removes it from the source stack
    stacks[[y]] <- c(stacks[[y]], box_to_move)
  }
  return(stacks)
}

final_stacks_2 <- move_box_2(stacks,clear_instruction)
final_tops_2 <- lapply(final_stacks_2, function(z){tail(z,1)})
