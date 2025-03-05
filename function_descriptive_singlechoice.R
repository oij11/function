# Full sample description ####


# Loop summary over each variable


process_all_with_full_sample <- function(vars, var_labels) {
  # Ensure that every variable in vars has a corresponding label in var_labels
  missing_labels <- setdiff(vars, names(var_labels))
  if (length(missing_labels) > 0) {
    stop("The following variables are missing labels: ", 
         paste(missing_labels, collapse = ", "))
  }
  
  summary_list <- list()
  
  # Loop over each variable name in vars
  for (var in vars) {
    # Lookup the label using the variable name
    label <- var_labels[[var]]
    
    # Exclude NA values
    non_na_data <- df[[var]][!is.na(df[[var]])]
    
    # Frequency counts and percentages
    counts <- summary(non_na_data)
    percentages <- round(100 * counts / sum(counts), 1)
    percentages <- paste0(percentages, "%")
    
    # Build the summary data frame
    summary_df <- data.frame(
      Category   = names(counts),
      Count      = as.numeric(counts),
      Percentage = percentages,
      stringsAsFactors = FALSE
    )
    
    # Append a row showing the total sample size
    summary_df <- rbind(
      summary_df,
      data.frame(
        Category   = "",
        Count      = "",
        Percentage = paste0("Total N = ", sum(counts)),
        stringsAsFactors = FALSE
      )
    )
    
    rownames(summary_df) <- NULL
    
    # Print the header and the summary table for display
    cat(paste0("<br><h4>Full sample: ", label, " summary</h4><br>"))
    print(knitr::kable(summary_df, format = "html"))
    
    # Store the summary data frame in the list with the variable name as the key
    summary_list[[var]] <- summary_df
  }
  
  return(summary_list)
}



# another version

# process_all_with_full_sample <- function(vars, var_labels) {
#   # Check that the lengths match.
#   if (length(vars) != length(var_labels)) {
#     stop("The number of variables and variable labels must be the same.")
#   }
#   
#   # Initialize an empty list.
#   summary_list <- list()
#   
#   # Loop over each variable.
#   for (i in seq_along(vars)) {
#     var <- vars[i]
#     label <- var_labels[i]
#     
#     # Check if the variable exists in df.
#     if (!var %in% names(df)) {
#       warning("Variable ", var, " not found in df. Skipping.")
#       next
#     }
#     
#     # Exclude NA values.
#     non_na_data <- df[[var]][!is.na(df[[var]])]
#     
#     # Compute frequency counts (using summary() as an example).
#     counts <- summary(non_na_data)
#     
#     # Calculate percentages.
#     percentages <- round(100 * counts / sum(counts), 1)
#     percentages <- paste0(percentages, "%")
#     
#     # Build the summary data frame.
#     summary_df <- data.frame(
#       Category   = names(counts),
#       Count      = as.numeric(counts),
#       Percentage = percentages,
#       stringsAsFactors = FALSE
#     )
#     
#     # Append a row with the total count.
#     summary_df <- rbind(
#       summary_df,
#       data.frame(
#         Category   = "",
#         Count      = paste0("Total N = ", sum(counts)),
#         Percentage = "",
#         stringsAsFactors = FALSE
#       )
#     )
#     
#     # Remove row names.
#     rownames(summary_df) <- NULL
#     
#     # Print header and table.
#     cat(paste0("<br><h4>Full sample: ", label, " summary</h4><br>"))
#     print(knitr::kable(summary_df, format = "html"))
#     
#     # Store the summary table in the list.
#     summary_list[[length(summary_list) + 1]] <- summary_df
#   }
#   
#   # Only set names if summary_list has elements.
#   if (length(summary_list) > 0) {
#     names(summary_list) <- vars[seq_along(summary_list)]
#   }
#   
#   return(summary_list)
# }

# Alternative: using lapply instead of for loop 
# process_all_with_full_sample <- function(vars, var_labels) {
#   # Check that the lengths of vars and var_labels are the same
#   if (length(vars) != length(var_labels)) {
#     stop("The number of variables and variable labels must be the same.")
#   }
#   
#   # Loop over the variables and process each summary
#   summary_list <- lapply(seq_along(vars), function(i) {
#     # Get the variable name and corresponding label
#     var <- vars[i]
#     label <- var_labels[i]
#     
#     # Exclude NAs from the variable's data
#     non_na_data <- df[[var]][!is.na(df[[var]])]
#     
#     # Compute frequency counts (using summary() here; adjust if needed)
#     counts <- summary(non_na_data)
#     
#     # Calculate percentages relative to non-missing data
#     percentages <- round(100 * counts / sum(counts), 1)
#     percentages <- paste0(percentages, "%")
#     
#     # Build a data frame of the summary
#     summary_df <- data.frame(
#       Category   = names(counts),
#       Count      = as.numeric(counts),
#       Percentage = percentages,
#       stringsAsFactors = FALSE
#     )
#     
#     # Append a row showing the total sample size
#     summary_df <- rbind(
#       summary_df,
#       data.frame(
#         Category   = "",
#         Count      = paste0("Total N = ", sum(counts)),
#         Percentage = "",
#         stringsAsFactors = FALSE
#       )
#     )
#     
#     # Remove row names (to avoid printing them as a column)
#     rownames(summary_df) <- NULL
#     
#     # Print the header and the summary table for display
#     cat(paste0("<br><h4>Full sample: ", label, " summary</h4><br>"))
#     print(knitr::kable(summary_df, format = "html"))
#     
#     # Return the summary table for later use
#     return(summary_df)
#   })
#   
#   # Name the list elements with the variable names for easy lookup later
#   names(summary_list) <- vars
#   
#   # Return the complete list of summaries
#   return(summary_list)
# }






## Use Example ####


# # List of variables to analyze
# vars <- c(
#   "s1_q4", "s1_q5", "s1_q6", "race_7", "gender_5", "sexual_orientation",
#   "age_2group", "county_group1", "s1_q2", "s2_q10", "s2_q13",
#   "s4_q20", "s4_q21", "s4_q22", "s4_q23",
#   "s5_q29", "s5_q30", "s3_q14", "s3_q14a",
#   "s3_q15a", "s3_q15b", "s3_q15c", "s3_q15d", "s3_q15e", "s3_q15f",
#   "s3_q15g", "s3_q17", "s4_q27", "s5_q32", "s5_q33"
# )

# 
# # Corresponding variable labels
# var_labels <- c(
#   "Housing, s1_q4", "Food insecurity, s1_q5", "Mental health concerns, s1_q6",
#   "Race, race_7", "Gender", "Sexual orientation",
#   "Age", "Region", "Education, s1_q2", "Fentanyl awareness, s2_q10", "Risk perception,
#    s2_q13",
#   "Heard of anyone overdosing under 26, s4_q20", "Any family overdosed, s4_q21",
#   "Seen anyone overdosing, s4_q22", "Would you call 911, s4_q23",
#   "Received treatment/counseling for drug use, s5_q29", "Wanted treatment/counseling for drug use, s5_q30",
#   "Ever taken not prescribed pills, s3_q14", "If offered would you take, s3_q14a",
#   "Not prescribed pain meds, s3_q15a", "Not prescribed benzo, s3_q15b",
#   "Not prescribed ADHD meds, s3_q15c", "Not prescribed antidepressants, s3_q15d",
#   "Not prescribed fentanyl, s3_q15e", "Not prescribed MDMA, s3_q15f",
#   "Other, s3_q15g", "Would you take knowing fentanyl, s3_q17",
#   "Drug check willingness, s4_q27", "Gone through withdrawal, s5_q32",
#   "Used non-prescribed bup/subxone for withdrawal, s5_q33"
# )
# 
# 
# # 
# summary_list <- list()
# # 
# summary_list <- process_all_with_full_sample(vars, var_labels)




# Two group description per group #####
  process_variable_with_labels <- function(variable, label, group_var) {
        # Distinguish the ones that take strings vs. object for elements 
        
        # group_sym<- sym(group_var)
        
  processed_df <- df %>%
    drop_na(any_of(c(variable, group_var))) %>%  # Remove rows with missing data
    group_by(.data[[variable]], !!sym(group_var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(!!sym(group_var)) %>%
    mutate(
      perc = round(n / sum(n)* 100, 2)  # Calculate percentage as a proportion
    ) %>%
    pivot_wider(
      names_from = !!sym(group_var),
      values_from = c(n, perc),
      values_fill = list(n = 0, perc = 0),
      names_prefix = paste0(group_var , "_"),  # Add a prefix to prevent NA names
      names_sep = "_"
    )
  
  
  # Prevent NA to be a level
  df_filtered <- df %>%
    drop_na(any_of(c(variable, group_var))) 
  
  # Extract the unique (trimmed) levels from the original df for group_var.
  group_levels <- unique(str_trim(as.character(df_filtered[[group_var]])))
  
  # Build a mapping: for each level, map old names to new names.
  # new_name (left-hand side) = old_name (right-hand side) for later rename
  rename_mapping <- list()
  
  for (lvl in group_levels) {
    trimmed_lvl <- str_trim(lvl)
    display_name <- str_to_title(trimmed_lvl)
    
    # Old column names created by pivot_wider
    old_n    <- paste0("n_", group_var, "_", trimmed_lvl)
    old_perc <- paste0("perc_", group_var, "_", trimmed_lvl)
    
    # New names for display
    new_n    <- paste0(display_name, " (n)")
    new_perc <- paste0(display_name, " %")
    
    rename_mapping[[new_n]]    <- old_n
    rename_mapping[[new_perc]] <- old_perc
  }
  
  # Rename columns dynamically to avoid NA issues
  
  
  processed_df <- processed_df %>%
    rename(!!!rename_mapping)
  
  
  #Enforce column order after verifying there are two levels
  if(length(group_levels) >= 2) {
    level1 <- str_to_title(str_trim(group_levels[1]))
    level2 <- str_to_title(str_trim(group_levels[2]))
    
    level1_n    <- paste0(level1, " (n)")
    level1_perc <- paste0(level1, " %")
    level2_n    <- paste0(level2, " (n)")
    level2_perc <- paste0(level2, " %")
    
    processed_df <- processed_df %>%
      select(.data[[variable]], level1_n, level1_perc, level2_n, level2_perc)
  } 
  # Print label as a Markdown header
  # cat("\n\n\n### **", label, "**\n\n\n")
  cat("&nbsp;\n\n&nbsp;\n\n####", label, "\n\n")
  
  # Return the table formatted for Markdown
  
  print(knitr::kable(
    processed_df,
    format = "markdown"))
}


# Use example

# List of variables to analyze
# variables <- c(
#   "s1_q4", "s1_q5", "s1_q6", "race_7", "gender_5", "sexual_orientation",
#   "age_2group", "county_group1", "s1_q2", "s2_q10",
#   "s4_q20", "s4_q21", "s4_q22", "s4_q23"
# )
# 
# # Corresponding variable labels
# var_labels <- c(
#   "Housing, s1_q4", "Food insecurity, s1_q5", "Mental health concerns, s1_q6",
#   "Race, race_7", "Gender", "Sexual orientation",
#   "Age", "Region", "Education, s1_q2", "Fentanyl awareness, s2_q10",
#   "Heard of anyone overdosing under 26, s4_q20", "Any family overdosed, s4_q21",
#   "Seen anyone overdosing, s4_q22", "Would you call 911, s4_q23"
# )
# 
# group_var<- "queer"
# 
# # Process each variable and display results
# for (i in seq_along(variables)) {
#   process_variable_with_labels(variables[i], var_labels[i], group_var)
# }

