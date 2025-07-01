
# For implementation, jump to line 133. 
# To see all helper functions, read below.

###############################################################################
### HELPER FUNCTIONS ###
###############################################################################

# Forms burner dataframe with columns verifying value suppression
checker_df <- function(df) {
  checker <- df |>
    # If a value has been suppressed, replace it with 1. 
    # If a value has not been suppressed, replace it with 0.
    mutate_at(supp_cols, ~ as.numeric(str_detect(., regex_suppchar))) 
  
  return (checker)
}

check_rows <- function(checker) {
  row_checker <- checker |>
    # How many cells in a given row have been suppressed?
    mutate(num_supp = rowSums(checker[,suppcol_idx])) #All rows, only suppression columns
  # What are the indices of the rows that need to be fixed (have only 1 value suppressed)?
  fix_rows = which(row_checker$num_supp == 1)
  return (fix_rows) 
}

check_cols <- function(checker) {
  # How many cells in a given column have been suppressed?
  colSums(checker[,which(names(checker) %in% supp_cols)])
  
  # What are the indices of the columns that need to be fixed (have only 1 value suppressed)?
  fix_cols = which(colSums(checker[which(names(checker) %in% supp_cols)]) == 1)
  return (fix_cols)
}

fix_row <- function(df, i) {
  # Save row to be fixed as vector, remove suppressed values, convert to numeric
  row <- df[i, suppcol_idx] 
  num_row <- as.numeric(row[!(row %in% c(supp_char, "0"))])
  
  # Randomly choose the index of one cell equal to the minimum row value to suppress
  # Subtract 1 to switch from 1-based indexing to 0-based indexing for data subsetting
  poss_idx <- which(row == min(num_row))
  if (length(poss_idx) != 1) {
    idx_to_supp <- sample(poss_idx, size = 1) - 1
  } else {
    idx_to_supp <- poss_idx - 1 }

  # Suppress selected cell with suppression character
  # Second argument enables proper indexing into larger dataframe: 
      #idx_to_supp = index of column within relevant suppression columns
      #suppcol_idx[1] = starter index of first relevant suppression column
  df[i, idx_to_supp + suppcol_idx[1]] <- supp_char
  return(df)
}

fix_col <- function(df, i) {
  # Save column to be fixed as vector, remove suppressed values, convert to numeric
  col <- df[,i]
  num_col <- as.numeric(col[!(col %in% c(supp_char, "0"))])
  
  # Randomly choose the index of one cell equal to the minimum column value to suppress
  poss_idx <- which(col == min(num_col))
  if (length(poss_idx) != 1) {
    idx_to_supp <- sample(poss_idx, size = 1)
  } else {
    idx_to_supp <- poss_idx}
  
  # Suppress selected cell with suppression character 
  # Second argument enables proper indexing into larger dataframe: 
      #i = column being fixed
      #idx_to_supp = row of column to be suppressed
  df[idx_to_supp, i] <- supp_char
  return(df)
}

# Iterate through all rows to be fixed
fix_all_rows <- function(df, rows_to_fix) {
  # Could be consolidated with map() functions, but left as-is for readability
  for (i in rows_to_fix) {
      df <- fix_row(df, i)
    }
  return(df)
}

# Iterate through all columns to be fixed
fix_all_cols <- function(df, cols_to_fix) { 
  # Could be consolidated with map() functions, but left as-is for readability
  for (i in cols_to_fix) {
    df <- fix_col(df, i)
  }
  return(df)
}

complementary <- function(df) {
  # Checker dataframe needs to be remade every recursive call
  checker <- checker_df(df)

  # Base case: check_rows() returns NULL AND check_cols() returns NULL
  rows_to_fix <- check_rows(checker)
  cols_to_fix <- check_cols(checker)
  if (length(rows_to_fix) == 0) {
    if (length(cols_to_fix) == 0) {
      return(df) # Base case
    }
    df <- fix_all_cols(df, cols_to_fix)
  } else {
    df <- fix_all_rows(df, rows_to_fix)
    if (length(cols_to_fix != 0)) {
      df <- fix_all_cols(df, cols_to_fix)
    }
  }
  
  # Recurse with edited dataframe
  complementary(df)
}

suppress <- function(df, supp_val, supp_char, supp_cols) {
  # First pass: 
     # For all suppression columns (supp_cols), replace any cell value equal to 
     # or less than suppression value (supp_val) with suppression character (supp_char)
  df <- df |>
    mutate(across(supp_cols, ~ if_else(.x <= supp_val & .x != 0, supp_char, as.character(.x))))
  
  # Call recursive function to perform complementary suppression
  df <- complementary(df)
  return(df)
}

###############################################################################
### MAIN ### 
###############################################################################

# Load relevant library
library(tidyverse)

# Loading example data
tabdata <- read_csv('data/burner_tabdata.csv')

# Suppression values of choice
cell_bound <- 5
numerator_bound <- 5
denominator_bound <- 20

# Suppression character to be used
supp_char <- '*'
# Define regex version of supp_char in case it is a special character
regex_suppchar <- if_else(supp_char %in% c('*', '.'), paste0('\\', supp_char), supp_char)

# Is your data in long format? If so, PIVOT_WIDER here (uncomment and use line below):
# tabdata <- pivot_wider(tabdata, names_from = name_column, values_from = value_column)
# Columns (COUNTS) to apply complementary suppresion to 
supp_cols <- c('white_count', 'black_count', 'hispanic_count') 
# Indices of suppression columns
suppcol_idx <- which(names(df) %in% supp_cols)

# Do suppression of percentages and aggregates before individual numbers
tabdata <- tabdata |>
  mutate(
    total = hispanic_count + white_count + black_count, # Total counts
    hispanic_perc = if_else(
      (hispanic_count < numerator_bound) | (total < denominator_bound), # If criterion not met,
      supp_char,                                                        # Suppress value.
      as.character(hispanic_count / total)),                            # Else, store as string.
    black_perc = if_else(
      (black_count < numerator_bound) | (total < denominator_bound), 
      supp_char, 
      as.character(black_count / total)),
    white_perc = if_else(
      (white_count < numerator_bound) | (total < denominator_bound),
      supp_char, 
      as.character(white_count / total)))

# Call function stack, using previously defined cell_bound, supp_char, and suppresion_columns
suppressed_tabdata <- suppress(tabdata, cell_bound, supp_char, supp_cols)

# Print suppressed data frame
print(suppressed_tabdata)

