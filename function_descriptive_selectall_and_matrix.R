
# Package install #

if (!require("pacman")) {
  install.packages("pacman")
}


library(pacman)
pacman::p_load(tidyverse, here, readxl, readr,  knitr, haven, broom, openxlsx)







# Matrix responses (same questions multiple choices for multiple items) and get the percentage ####
## Denominator for % : # of respondents for each items ####





my_pivot_count_v1 <- function(df, items, choice_values) {
  df %>%
    select(record_id, all_of(items)) %>%
    pivot_longer(
      cols = all_of(items),
      names_to = "item", 
      values_to = "choice"
    ) %>%
    filter(!is.na(choice)) %>%
    group_by(item, choice) %>%
    summarise(n = n(), .groups = "drop") %>%
    # Add missing combinations of `item` and `choice`
    ungroup() %>%  # Ungroup before calling complete
    complete(item, choice = choice_values, fill = list(n = 0)) %>%
    # Calculate total responses per item and percentages
    group_by(item) %>%
    mutate(
      sum_n   = sum(n, na.rm = TRUE),              # The sum of n for this item
      total_n = ifelse(sum_n == 0, 0, sum_n),       # If sum_n=0 => total_n=0
      perc    = ifelse(
        total_n > 0,
        paste0(round(n / total_n * 100, 1), "%"),  # normal item-based %
        "0%"                                       # if total_n=0 => all 0%
      )
    ) %>%
    ungroup() %>%
    select(-sum_n)  # remove the helper column if not needed
}





pivot_and_reorder_v1 <- function(processed_df, choice_values) {
  
  # Pivot wider with custom column names
  final_table <- processed_df %>%
    pivot_wider(
      names_from = choice,
      values_from = c(n, perc),
      names_glue = "{choice}_{.value}"
    )
  
  # Add "N per item" column
  final_table <- final_table %>%
    mutate(`N per item` = processed_df %>% group_by(item) %>% summarise(total_n = first(total_n)) %>% 
             pull(total_n))
  
  # Create column order dynamically
  col_order <- c("item",
                 as.vector(rbind(
                   paste0(choice_values, "_n"),
                   paste0(choice_values, "_perc")
                 )),
                 "N per item"
  )
  
  
  # Reorder columns and return final result
  final_df <- final_table %>%
    select(any_of(col_order))
  
  return(final_df)
}


#### Example use ####


# items <- c("Pain_meds", "Benzodiazepines", "ADHD_meds", 
#            "Antidepressants", "Fentanyl", "MDMA", "Other")
# choice_values <- c("0 times", "1 or more times")
# processed_df <- my_pivot_count_v2(df, items, choice_values)
# q15_summary  <- pivot_and_reorder_v2 (processed_df, choice_values)
# 
# 
# 
# 
# cat("Experience with non-prescribed pills, per medtype (q15).\n\n")
# knitr::kable(
#   q15_summary,
#   format = "pipe",
#   col.names = c("Drug", "0 times (n)", "0 times (%)",
#                 "1 or more times (n)", "1 or more times (%)")
# )



## Denominator for % : # of respondents across the items (respond to at least one item)  ####

my_pivot_count_v2 <- function(df, items, choice_values) {
  # 1) Number of respondents who answered at least one item
  total_respondents <- df %>%
    filter(if_any(all_of(items), ~ !is.na(.))) %>%
    summarise(n = n_distinct(record_id)) %>%
    pull(n)
  
  # 2) Avoid zero division if no respondents
  if (is.na(total_respondents) || total_respondents == 0) {
    total_respondents <- 1
  }
  
  # 3) Pivot longer, count (item, choice)
  processed_df <- df %>%
    select(record_id, all_of(items)) %>%
    pivot_longer(
      cols = all_of(items),
      names_to = "item",
      values_to = "choice"
    ) %>%
    # Keep only non-NA 'choice' rows
    filter(!is.na(choice)) %>%
    group_by(item, choice) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    ungroup()
  
  # 4) Ensure all (item, choice) combos appear (fill missing combos with n=0)
  processed_df <- processed_df %>%
    complete(item, choice = choice_values, fill = list(n = 0))
  
  # 5) Group again by item 
  #    then compute overall percentages (n / total_respondents)
  processed_df <- processed_df %>%
    # After grouping and ungrouping
    group_by(item) %>%
    mutate(
      perc = paste0(round(n / total_respondents * 100, 1), "%")
    ) %>%
    ungroup() %>%
    # Convert n and perc to character
    mutate(
      n    = as.character(n),
      perc = as.character(perc)
    ) %>%
    # Replace any remaining NA with blanks
    mutate(
      n = if_else(is.na(n), "", as.character(n)),
      perc = if_else(is.na(perc), "", as.character(perc))
    )
      
  
  
  # 6) Add a final row that says "Total N = X"
  total_row <- tibble(
    item   = paste0("Total N = ", total_respondents),
    choice = "",
    n      = "",
    perc   = ""
  )
  
  # 7) Combine the main data with the total row
  final_df <- bind_rows(processed_df, total_row)
  return(final_df)
}



pivot_and_reorder_v2 <- function(processed_df, choice_values) {
  
  # Pivot wider with custom column names
  final_table <- processed_df %>%
    pivot_wider(
      names_from = choice,
      values_from = c(n, perc),
      names_glue = "{choice}_{.value}"
    )
  
  # Create column order dynamically
  col_order <- c("item",
                 as.vector(rbind(
                   paste0(choice_values, "_n"),
                   paste0(choice_values, "_perc")
                 ))
  )
  
  # Reorder columns and return final result
  final_df <- final_table %>%
    select(any_of(col_order))
  
  return(final_df)
}


#### Example use ####


# # Old variable names
# old_vars <- c(
#   "s4_q25a",
#   "s4_q25b",
#   "s4_q25c",
#   "s4_q25d",
#   "s4_q25e",
#   "s4_q25f",
#   "s4_q25g"
# )
# 
# # Corresponding new variable names
# new_vars <- c(
#   "Fentanyl test strips",
#   "Having Naloxone/Narcan",
#   "Not using alone",
#   "Trying small first",
#   "Good Samaritan Law",
#   "How to know if overdosing",
#   "Measure 110"
# )
# 
# familiarity <- c("Totally unfamiliar", "Unfamiliar", "Familiar", "Very familiar")
# 
# df[new_vars] <- df[old_vars]
# 
# processed_df<- my_pivot_count_v2 (df, new_vars, familiarity)
# q25_summary<- pivot_and_reorder_v2(processed_df, familiarity)
# 
# 
# cat("Familiarity with HR items (Overall Sample).\n\n")
# knitr::kable(q25_summary)




#Select all type questions ####


### When Columns exist for each choice (full sample & per group)  #####



library(dplyr)
library(tidyr)
library(stringr)
library(knitr)

# ------------------------------------------------------------------
# Function 1: Renaming the "Select All" Checkbox Columns
# ------------------------------------------------------------------
add_checkbox_columns <- function(df, 
                                 old_prefix = "s2_q11", 
                                 new_names_vector,
                                 remove_pattern = "_other$|gov$",
                                 keep_vars = NULL) {
  # If additional variables need to be kept (like a grouping variable),
  # include them along with record_id.
  if (!is.null(keep_vars)) {
    df_new <- df %>%
      select(record_id, all_of(keep_vars), starts_with(old_prefix))
  } else {
    df_new <- df %>%
      select(record_id, starts_with(old_prefix))
  }
  
  df_new <- df_new %>%
    # Remove columns that match the remove_pattern.
    select(-matches(remove_pattern)) %>%
    # Rename the checkbox columns using the provided new_names_vector.
    rename_with(~ new_names_vector, starts_with(old_prefix)) %>%
    # Optionally remove an unwanted prefix (here "q11_") from the names.
    rename_with(~ str_remove(., "q11_"))
  
  return(df_new)
}
# ------------------------------------------------------------------
# Function 2: Create a Summary Table for Select All Responses (Full Sample)
# ------------------------------------------------------------------
create_checkbox_summary <- function(df, 
                                    vars, 
                                    target_values = "Checked", 
                                    var_label = NULL,
                                    id = "record_id") {
  # 1) Pivot to long format using the provided id and vars
  df_long <- df %>%
    select(all_of(c(id, vars))) %>%
    tidyr::pivot_longer(
      cols = all_of(vars),
      names_to = "Source_Type",
      values_to = "value"
    )
  
  # 2) Summarize counts of the target value(s)
  summary_df <- df_long %>%
    dplyr::filter(value %in% target_values) %>%
    dplyr::count(Source_Type, name = "n") %>%
    dplyr::arrange(dplyr::desc(n))
  
  # 3) Calculate the total distinct respondents having the target value(s)
  total_records <- df_long %>%
    dplyr::filter(value %in% target_values) %>%
    dplyr::summarise(total = dplyr::n_distinct(!!sym(id))) %>%
    dplyr::pull(total)
  
  # 4) Compute percentage and format as string (e.g., "45%")
  summary_df <- summary_df %>%
    dplyr::mutate(
      perc = paste0(round(n / total_records * 100, 1), "%")
    ) %>%
    dplyr::arrange(dplyr::desc(n))
  
  # Convert numeric n and perc to character
  summary_df <- summary_df %>%
    dplyr::mutate(
      n = as.character(n),
      perc = as.character(perc)
    )
  
  # 5) Add "Total N = X" row (with empty string for perc)
  total_n_row <- tibble::tibble(
    Source_Type = "",
    n           = paste0("Total N = ", total_records),
    perc        = ""
  )
  summary_df <- dplyr::bind_rows(summary_df, total_n_row)
  
  # Attach the var_label if provided, and print the header.
  if (!is.null(var_label)) {
    attr(summary_df, "var_label") <- var_label
    cat("<br><h4>", var_label, " full sample summary </h4><br>")
  }
  
  # Print the final table using knitr::kable (HTML format).
  print(knitr::kable(summary_df, format = "html"))
  
  return(invisible(summary_df))
}


# ------------------------------------------------------------------
# Function 3: Create a Summary Table per "Group" for Select All Responses
# ------------------------------------------------------------------
create_checkbox_summary_by_group <- function(df, 
                                             vars, 
                                             group_var, 
                                             var_label = NULL,
                                             id = "record_id",
                                             target_value = "Checked") {
  
  # Error-check: Ensure grouping variable exists in the data frame.
  if (!group_var %in% names(df)) {
    stop(paste("Grouping variable", group_var, "not found in the data frame."))
  }
  
  # Error-check: Ensure all "select all" variables exist in the data frame.
  if (!all(vars %in% names(df))) {
    missing_vars <- vars[!vars %in% names(df)]
    stop(paste("The following variables are not found in the data frame:", 
               paste(missing_vars, collapse = ", ")))
  }
  
  # Error-check: The grouping variable must have exactly 2 non-missing levels.
  group_levels <- unique(na.omit(df[[group_var]]))
  if (length(group_levels) != 2) {
    stop("The grouping variable must have exactly two non-missing levels.")
  }
  group_levels <- as.character(group_levels)
  
  # Create display names for group levels (e.g., Title Case)
  group_display <- str_to_title(group_levels)
  
  # Pivot the data to long format for the specified "select all" variables,
  # using the provided id and group_var.
  df_long <- df %>%
    select(all_of(c(id, group_var, vars))) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "Response",
      values_to = "value"
    )
  
  # Filter for rows where the response equals the target value.
  df_long <- df_long %>% 
    filter(value == target_value)
  
  # Summarize counts of the target responses per group and per response option.
  summary_df <- df_long %>%
    count(!!sym(group_var), Response, name = "n") %>%
    arrange(Response)
  
  # Calculate the total distinct respondent count per group (for rows with the target value).
  total_by_group <- df_long %>%
    group_by(!!sym(group_var)) %>%
    summarise(total = n_distinct(!!sym(id)), .groups = "drop")
  
  # Compute the percentage for each response (as a formatted string).
  summary_df <- summary_df %>%
    left_join(total_by_group, by = group_var) %>%
    mutate(perc = ifelse(total > 0, paste0(round(n / total * 100, 0), "%"), "0%")) %>%
    select(!!sym(group_var), Response, n, perc)
  
  # Pivot the summary so that each group has its own columns (for n and percentage).
  summary_wide <- summary_df %>%
    pivot_wider(
      names_from = !!sym(group_var),
      values_from = c(n, perc),
      values_fill = list(n = 0, perc = "0%")
    )
  
  # Dynamically rename the columns based on the group levels.
  # For example, if group levels are "queer" and "non_queer", we want:
  #   "Queer (n)" from n_queer, "Queer %" from perc_queer, etc.
  rename_mapping <- list()
  for (i in seq_along(group_levels)) {
    old_n    <- paste0("n_", group_levels[i])
    old_perc <- paste0("perc_", group_levels[i])
    new_n    <- paste0(group_display[i], " (n)")
    new_perc <- paste0(group_display[i], " %")
    rename_mapping[[new_n]]    <- old_n
    rename_mapping[[new_perc]] <- old_perc
  }
  summary_wide <- summary_wide %>%
    rename(!!!rename_mapping)
  
  # Reorder columns: first Response, then the two groups (n and %).
  new_order <- c("Response", 
                 paste0(group_display[1], " (n)"), paste0(group_display[1], " %"),
                 paste0(group_display[2], " (n)"), paste0(group_display[2], " %"))
  summary_wide <- summary_wide %>%
    select(all_of(new_order))
  
  # Calculate totals for each group.
  tot1 <- total_by_group %>% filter(!!sym(group_var) == group_levels[1]) %>% pull(total)
  tot2 <- total_by_group %>% filter(!!sym(group_var) == group_levels[2]) %>% pull(total)
  tot1 <- ifelse(length(tot1) == 0, 0, tot1)
  tot2 <- ifelse(length(tot2) == 0, 0, tot2)
  
  totals_row <- tibble::tibble( #tibble doesn’t convert strings to factors by default, which avoids common pitfalls
    Response = "Total",
    !!paste0(group_display[1], " (n)") := tot1,
    !!paste0(group_display[1], " %") := "100%",
    !!paste0(group_display[2], " (n)") := tot2,
    !!paste0(group_display[2], " %") := "100%"
  )
  
  final_df <- bind_rows(summary_wide, totals_row)
  
  # Attach the var_label if provided, and print the header.
  if (!is.null(var_label)) {
    attr(final_df, "var_label") <- var_label
    cat("<br><h4>", var_label, " summary per group</h4><br>")
  }
  
  # Print the final table using knitr::kable (HTML format).
  print(knitr::kable(final_df, format = "html"))
  
  return(invisible(final_df))
}

#### Example use ######
##### Full sample #######
# option 1. using list and looping ##
## Define parameters for each question as a named list
# question_params <- list(
#   q12 = list(
#     old_prefix = "s2_q12",
#     new_names_vector = c("Acquaintance", "Teacher", "Friends", "Siblings",
#                          "Parents", "Healthcare", "Police", "TikTok", 
#                          "Snapchat", "Instagram", "YouTube", "Reddit",
#                          "TVnews", "Internet", "OHA", "Gvnt", "Other", "No one/No where"),
#     remove_pattern = "_other$|gov$",
#     var_label = "Fentanyl Information Trusted Source (q12)"
#   ),
#   q31 = list(
#     old_prefix = "s5_q31",
#     new_names_vector = c(
#       "I have never wanted/needed treatment", 
#       "I do not know where to go for treatment",
#       "It would be too expensive", 
#       "My insurance wouldnt cover it", 
#       "It would take too long", 
#       "I would have to be on a waiting list", 
#       "I am concerned what my peers would think", 
#       "It would interfere with my school/work", 
#       "I would be afraid to reach out for help", 
#       "Other"
#     ),
#     remove_pattern = "_other$",
#     var_label = "Reasons for Not Wanting Treatment (q31)"
#   )
# )
# 
# # Loop over the questions, creating a list of summary tables.
# tables_list <- list()
# titles_list <- list()
# for (q in names(question_params)) {
#   params <- question_params[[q]]
#   
#   # Rename checkbox columns (and include the grouping variable via keep_vars)
#   df_renamed <- add_checkbox_columns(
#     df = df,
#     old_prefix = params$old_prefix,
#     new_names_vector = params$new_names_vector,
#     remove_pattern = params$remove_pattern,
#   )
#   
#   
#   # Create summary table per group with percentage formatted as "45%" etc.
#   summary_table <- create_checkbox_summary(
#     df = df_renamed,
#     vars = params$new_names_vector,
#     var_label = params$var_label
#   )
#   
#   # Save the table and title in lists.
#   tables_list[[q]] <- summary_table
#   titles_list[[q]] <- params$var_label
# }
#
#
# option 2. Run function for each variable one at a time and later combine as a list. ##
# q12_vars <- c("Acquaintance", "Teacher", "Friends", "Siblings",
#               "Parents", "Healthcare", "Police", "TikTok", 
#               "Snapchat", "Instagram", "YouTube", "Reddit",
#               "TVnews", "Internet", "OHA", "Gvnt", "Other","No one/No where")
# 
# 
# 
# df_new <- add_checkbox_columns(
#   df               = df,
#   old_prefix       = "s2_q12",
#   new_names_vector = q12_vars,
#   remove_pattern   = "_other$|gov$"
# )
# 
# 
# 
# 
# q12_summary <- create_checkbox_summary(df_new, q12_vars)
# 
## We can make it to list with other summary tables later for excel export. For example, 
## table_list <- list("q12_summary" = q12_summary, .....)
#
# cat("Fentanyl information trusted source (q12).\n\n")
# knitr::kable(q12_summary, col.names = c("Value", "n", "perc"))
# 
#
#
##### Per group (2 groups) #######
# group_var <- "age_2group"
# 
# # Define parameters for each question as a named list
# question_params <- list(
#   q12 = list(
#     old_prefix = "s2_q12",
#     new_names_vector = c("Acquaintance", "Teacher", "Friends", "Siblings",
#                          "Parents", "Healthcare", "Police", "TikTok", 
#                          "Snapchat", "Instagram", "YouTube", "Reddit",
#                          "TVnews", "Internet", "OHA", "Gvnt", "Other", "No one/No where"),
#     remove_pattern = "_other$|gov$",
#     var_label = "Fentanyl Information Trusted Source (q12)"
#   ),
#   q31 = list(
#     old_prefix = "s5_q31",
#     new_names_vector = c(
#       "I have never wanted/needed treatment", 
#       "I do not know where to go for treatment",
#       "It would be too expensive", 
#       "My insurance wouldnt cover it", 
#       "It would take too long", 
#       "I would have to be on a waiting list", 
#       "I am concerned what my peers would think", 
#       "It would interfere with my school/work", 
#       "I would be afraid to reach out for help", 
#       "Other"
#     ),
#     remove_pattern = "_other$",
#     var_label = "Reasons for Not Wanting Treatment (q31)"
#   )
# )
# 
# # Loop over the questions, creating a list of summary tables.
# tables_list <- list()
# titles_list <- list()
# for (q in names(question_params)) {
#   params <- question_params[[q]]
#   
#   # Rename checkbox columns (and include the grouping variable via keep_vars)
#   df_renamed <- add_checkbox_columns(
#     df = df,
#     old_prefix = params$old_prefix,
#     new_names_vector = params$new_names_vector,
#     remove_pattern = params$remove_pattern,
#     keep_vars = group_var
#   )
#   
#   
#   # Create summary table per group with percentage formatted as "45%" etc.
#   summary_table <- create_checkbox_summary_by_group(
#     df = df_renamed,
#     vars = params$new_names_vector,
#     group_var = group_var,
#     var_label = params$var_label
#   )
#   
#   # Save the table and title in lists.
#   tables_list[[q]] <- summary_table
#   titles_list[[q]] <- params$var_label
# }


 
### When One column has multiple choices as characters ####

    create_multi_summary_from_onecolumn<-  function(
        data,
        id_col    = "record_id",
        var_col   = "select_all_var",
        var_label = "My Multi Variable Label",
        sep_char  = "/"
    ) {
      library(dplyr)
      library(tidyr)
      library(stringr)
      
      # 1) Compute correct denominator: distinct IDs who have a non-missing response for this column
      total_n <- data %>%
        filter(!is.na(.data[[var_col]])) %>%
        distinct(.data[[id_col]]) %>%
        nrow()
      
      # 2) Split slash-delimited responses into separate rows
      df_separated <- data %>%
        filter(!is.na(.data[[var_col]])) %>%
        separate_rows(.data[[var_col]], sep = sep_char) %>%
        mutate(!!var_col := str_trim(.data[[var_col]]))
      
      # 3) Count distinct IDs for each category
      summary_df <- df_separated %>%
        group_by(.data[[var_col]]) %>%
        dplyr::summarize(
          n = n_distinct(.data[[id_col]]),
          .groups = "drop"
        ) %>%
        mutate(
          perc = round(n / total_n * 100, 1)  # Use correct denominator
        )
      
      # 4) Rename the response column
      summary_df <- summary_df %>%
        rename(Response = !!var_col)
      
      # 5) Convert numeric to character before adding the total row
      summary_df <- summary_df %>%
        mutate(
          n    = as.character(n),
          perc = as.character(perc)
        )
      
      # 6) Add total row
      total_row <- tibble::tibble(
        Response = "",
        n        = paste0("Total N = ", total_n),
        perc     = ""
      )
      summary_df <- bind_rows(summary_df, total_row)
      
      # 7) Rename columns to "Intake n" / "Intake %"
      summary_df <- summary_df %>%
        rename(
          "Intake n" = n,
          "Intake %" = perc
        )
      
      # 8) Attach label (optional)
      attr(summary_df, "var_label") <- var_label
    
      cat("<br><h4>Full sample: ", var_label, " summary</h4><br>")
      print(knitr::kable(summary_df, format = "html"))
      
      
      return(summary_df)
    }
    



#### Example use #####


# all_tables_list <- list()
# 
# vars_multiselect<- c("med_oud","money_source" )
# var_multi_labels<-  c( med_oud= "MOUD",
#                        money_source = "money_source")
# 
# 
# for (v in vars_multiselect) {
#   table <- create_multi_summary_from_onecolumn(
#     data      = gpra_full_long_intakeonly,  # Already filtered to "Intake"
#     id_col    = "ClientID",
#     var_col   = v,
#     var_label = var_multi_labels[[v]],
#     sep_char  = "/"   # Adjust separator if needed
#   )
#   
#   all_tables_list[[v]] <- table
# }
# 
# 
# 
# title_list <- var_multi_labels
# 
# export_list_to_excel(
#   table_list = all_tables_list,
#   title_list = title_list,
#   file_name  = "descriptive_select_all_intakefullsample.xlsx",
#   sheet_name = "AllTables"
# )



