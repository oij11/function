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
  
  # 5) Group again by item (just like the code that 'worked'),
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
       # <--- THIS line ensures no "NA"
  
  
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


  
  add_checkbox_columns <- function(df, 
                                   old_prefix = "s2_q11", 
                                   new_names_vector,
                                   remove_pattern = "_other$|gov$") {
    
    df_new <- df %>%
      select(record_id, starts_with(old_prefix)) %>%
      select(-matches(remove_pattern)) %>%
      rename_with(~ new_names_vector, starts_with(old_prefix)) %>%
      rename_with(~ str_remove(., "q11_"))
    

  
  # # 2) Join back to original df
  # df_out <- df %>%
  #   left_join(df_new, by = "record_id")
  
  
  return(df_new)
    
    
}





create_checkbox_summary <- function(df, vars) {
  
  

  # 2) Pivot to long format
  #    Here we pivot ONLY the newly added columns,
  #    but if you'd like to include others, you can adjust accordingly.
  df_long <- df %>%
    select(record_id, all_of(vars)) %>%
    tidyr::pivot_longer(
      cols      = all_of(vars),
      names_to  = "Source_Type",
      values_to = "value"
    )
  
  # 3) Summarize counts of "Checked"
  summary_df <- df_long %>%
    dplyr::filter(value == "Checked") %>%
    dplyr::count(Source_Type, name = "n") %>%
    dplyr::arrange(dplyr::desc(n))
  
  # 4) Calculate how many distinct respondents were "Checked"
  total_records <- df_long %>%
    dplyr::filter(value == "Checked") %>%
    dplyr::summarise(total = dplyr::n_distinct(record_id)) %>%
    dplyr::pull(total)
  
  # 5) Compute percentage and append "Total" row
  summary_df <- summary_df %>%
    dplyr::mutate(
      perc = paste0(round(n / total_records * 100, 1), "%")
    ) %>% arrange(desc(n)) 
  
  # Convert numeric n -> character
  summary_df <- summary_df %>%
    mutate(
      n = as.character(n),
      perc = as.character(perc)
    )
  
  # Now n and perc are character columns. 
  # Totals can also be character easily:
  
  # totals <- tibble::tibble(
  #   Source_Type = "Total",
  #   n           = as.character(total_records),  # "123" for example
  #   perc        = "100%"
  # )
  # summary_df <- dplyr::bind_rows(summary_df, totals)
  
  # 6) Add "Total N = X" row (with empty string for n and perc)
  total_n_row <- tibble::tibble(
    Source_Type = "",
    n           = paste0("Total N = ", total_records),
    perc        = ""
  )
  summary_df <- dplyr::bind_rows(summary_df, total_n_row)
  
  return(summary_df)
  
}


