


if (!require("pacman")) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(tidyverse, here, readxl, lubridate, readr, purrr, knitr, broom,  
               car, ggplot2, webshot2,stringr)


# gpra_full_long <- readRDS("gpra_full_long_prepped.rds")



# Single select #####


# This works when even there is no observation for any timepoint. 

create_two_timepoint_table <- function(data,
                                       id_col = "record_id",
                                       time_col = "time_point",    # e.g. "Intake", "6 month"
                                       var_col  = "some_variable", # the question/variable of interest
                                       var_label = "My Variable Label",
                                       intake_val = "Intake",
                                       followup_val = "6 month") {
  # 1) Identify IDs with matched sample for this variable.
  valid_ids <- data %>%
    filter(.data[[time_col]] %in% c(intake_val, followup_val),
           !is.na(.data[[var_col]])) %>%
    group_by(.data[[id_col]]) %>%
    filter(n_distinct(.data[[time_col]]) == 2) %>%
    distinct(.data[[id_col]]) %>%
    pull(!!sym(id_col))
  # 2) Subset to those IDs, keep only relevant time points, and remove missing var_col.
  df_sub <- data %>%
    filter(.data[[id_col]] %in% valid_ids,
           .data[[time_col]] %in% c(intake_val, followup_val),
           !is.na(.data[[var_col]]))
  
  # 3) Summarize counts per response category x time point.
  summary_df <- df_sub %>%
    group_by(.data[[time_col]], .data[[var_col]]) %>%
    dplyr::summarize(n = n_distinct(.data[[id_col]]), .groups = "drop") %>%
    group_by(.data[[time_col]]) %>%
    mutate(perc = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # 4) Pivot wider to get columns like "n_Intake", "perc_Intake", etc.
  summary_wide <- summary_df %>%
    tidyr::pivot_wider(
      names_from  = .data[[time_col]],
      values_from = c(n, perc),
      names_sep   = "_"
    )
  
  # 4.5) Ensure that the expected columns exist; if missing, add them with 0.
  expected_cols <- c(paste0("n_", intake_val), paste0("perc_", intake_val),
                     paste0("n_", followup_val), paste0("perc_", followup_val))
  for (col in expected_cols) {
    if (!(col %in% names(summary_wide))) {
      summary_wide[[col]] <- 0
    }
  }
  
  # 5) Rename columns to user-friendly names.
  summary_wide <- summary_wide %>%
    rename(
      Response      = !!sym(var_col),
      "Intake n"    = paste0("n_", intake_val),
      "Intake %"    = paste0("perc_", intake_val),
      "6 month n"   = paste0("n_", followup_val),
      "6 month %"   = paste0("perc_", followup_val)
    ) %>%
    dplyr::mutate(
      `Intake n`   = as.character(`Intake n`),
      `Intake %`   = as.character(`Intake %`),
      `6 month n`  = as.character(`6 month n`),
      `6 month %`  = as.character(`6 month %`)
    )
  
  # 6) Add a "Total N" row at the bottom.
  total_n <- length(unique(df_sub[[id_col]]))
  total_row <- tibble::tibble(
    Response    = "" ,
    `Intake n`  = "",
    `Intake %`  = "",
    `6 month n` = "",
    `6 month %` = paste0("Total N = ", total_n),
  )
  summary_wide <- dplyr::bind_rows(summary_wide, total_row)
  
  # Optionally attach the variable label as an attribute.
  attr(summary_wide, "var_label") <- var_label
  
  # Print the table in HTML format.
  cat("<br><h4>Matched sample: ", var_label, " summary</h4><br>")
  print(knitr::kable(summary_wide, format = "html"))
  
  return(summary_wide)
}



## This does not work when there is no observation
# create_two_timepoint_table <- function(data,
#                                        id_col = "record_id",
#                                        time_col = "time_point",    # e.g. "Intake", "6 month"
#                                        var_col  = "some_variable", # the question/variable of interest
#                                        var_label = "My Variable Label",
#                                        intake_val = "Intake",
#                                        followup_val = "6 month") {
#   # 1) Identify IDs that have a non-missing follow-up for this variable.
#   #    This ensures we only count individuals who truly have follow-up data.
#   valid_ids <- data %>%
#     filter(.data[[time_col]] == followup_val, !is.na(.data[[var_col]])) %>%
#     distinct(.data[[id_col]]) %>%
#     pull(!!sym(id_col))
#   
#   # 2) Subset to those IDs, keep only relevant time points, and remove missing var_col.
#   df_sub <- data %>%
#     filter(.data[[id_col]] %in% valid_ids,
#            .data[[time_col]] %in% c(intake_val, followup_val),
#            !is.na(.data[[var_col]]))
#   
#   # 3) Summarize counts per response category x time point
#   summary_df <- df_sub %>%
#     group_by(.data[[time_col]], .data[[var_col]]) %>%
#     dplyr::summarize(n = n_distinct(.data[[id_col]]), .groups = "drop") %>%
#     group_by(.data[[time_col]]) %>%
#     mutate(perc = round(n / sum(n) * 100, 1)) %>%
#     ungroup()
#   
#   # 4) Pivot wider to get columns for "Intake n", "Intake %", "6 month n", "6 month %", etc.
#   # 4) Pivot wider — remove names_glue, use names_sep
#   summary_wide <- summary_df %>%
#     tidyr::pivot_wider(
#       names_from  = .data[[time_col]],
#       values_from = c(n, perc),
#       names_sep   = "_"
#     )
# 
#   # 5) Rename columns to something user-friendly
#   #    e.g. "Response" for var_col, "Intake n", "Intake %", ...
# 
#   
#   
#     summary_wide <- summary_wide %>%
#     rename(Response = !!sym(var_col))
#   
#   # Then conditionally rename the Intake columns if they exist
#   if (paste0("n_", intake_val) %in% names(summary_wide)) {
#     summary_wide <- summary_wide %>%
#       rename(
#         "Intake n" = paste0("n_", intake_val),
#         "Intake %" = paste0("perc_", intake_val)
#       )
#   }
#   
#   # And conditionally rename the follow-up columns if they exist
#   if (paste0("n_", followup_val) %in% names(summary_wide)) {
#     summary_wide <- summary_wide %>%
#       rename(
#         "6 month n" = paste0("n_", followup_val),
#         "6 month %" = paste0("perc_", followup_val)
#       )
#   }
#   
#   
#     summary_wide <- summary_wide %>%
#     dplyr::mutate(
#       `Intake n`   = as.character(`Intake n`),
#       `Intake %`   = as.character(`Intake %`),
#       `6 month n`  = as.character(`6 month n`),
#       `6 month %`  = as.character(`6 month %`)
#     )
#   
#   # 6) Add a "Total N" row at the bottom
#   total_n <- length(unique(df_sub[[id_col]]))
#   total_row <- tibble::tibble(
#     Response   = paste0("Total N = ", total_n),
#     `Intake n` = "",
#     `Intake %` = "",
#     `6 month n` = "",
#     `6 month %` = ""
#   )
#   summary_wide <- dplyr::bind_rows(summary_wide, total_row)
#   
#   # Optionally, attach the label as an attribute or store it in a column for referencing later
#   attr(summary_wide, "var_label") <- var_label
#   
#   # --  Print the table in HTML format for R Markdown
#   cat("<br><h4>Full sample: ", var_label, " summary</h4><br>")
#   print(knitr::kable(summary_wide, format = "html"))
#   
#   
#   return(summary_wide)
# }



## Example use ####

# vars_single <- c(
#   "demo_gender_fct", "demo_children_fct", "demo_race_cleaned_fct",                     
#   "demo_ethnicity_cleaned", "LivingWhere_fct", "Housed_fct","Employed_fct","insurance_type", 
#   "education", "AttendVoluntary", "InteractFamilyFriends", "overdose", 
#   "arrest30", "depression30", "anxiety30",
#   "has_naloxone", "injever", "inj30", "ed_visit_count", "ed_visit_last_time",
#   "TxSUD", "TxSUDWhen", "Injected_days_used_syringe_fct"
# )
# 
# 
# var_labels<- c(
#   demo_gender_fct = "gender", 
#   demo_children_fct = "having children", 
#   demo_race_cleaned_fct = "race",
#   demo_ethnicity_cleaned = "ethnicity",
#   LivingWhere_fct = "living condition", 
#   Housed_fct = "housed or not",
#   Employed_fct = "employed or not",
#   insurance_type = "insurance type",
#   education = "education",
#   AttendVoluntary = "attending support group", 
#   InteractFamilyFriends = "interaction with family/friends", 
#   overdose = "overdose past 6m",
#   arrest30 = "arrested past 30days",
#   depression30 = "depression past 30days", 
#   anxiety30 = "anxiety past 30days",
#   has_naloxone = "having naloxone",
#   injever = "ever injected",
#   inj30 = "injection in the past 30days",
#   ed_visit_count = "lifetime ED visit",
#   ed_visit_last_time = "last time ED visit",
#   TxSUD = "life time SUD treatment",
#   TxSUDWhen = "last time treated for SUD",
#   Injected_days_used_syringe_fct = "Injected with used syringe in the past 30days"
# )
# 
# 
# 
# 
# all_tables_list <- list()
# 
# # 1) Loop over single-response variables
# for (v in vars_single) {
#   my_table <- create_two_timepoint_table(
#     data = gpra_full_long,
#     id_col = "ClientID",
#     time_col = "time_point",
#     var_col = v,
#     var_label = var_labels[[v]], # label from the named vector
#     intake_val = "Intake",
#     followup_val = "6 month"
#   )
#   
#   # Put into the list, naming it after the variable
#   all_tables_list[[v]] <- my_table
# }
# 
# 
# title_list <- var_labels
# 
# # Then use another function to export to excel:
# export_list_to_excel(
#   table_list = all_tables_list, 
#   title_list = title_list,
#   file_name  = "descriptive_tables_in_one_sheet.xlsx",
#   sheet_name = "AllTables"
# )



# Select_multiple ####

create_two_timepoint_table_select_multiple <- function(
    data,
    id_col = "record_id",
    time_col = "time_point",
    var_col = "select_all_var",
    var_label = "My Multi-Select Label",
    sep_char = "/",
    intake_val = "Intake",
    followup_val = "6 month"
) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)  # Required for HTML output
  
  # -- 1) Identify participants who have data at *both* time points
  matched_ids <- data %>%
    dplyr::filter(.data[[time_col]] %in% c(intake_val, followup_val)) %>%
    dplyr::filter(!is.na(.data[[var_col]])) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarize(num_timepoints = dplyr::n_distinct(.data[[time_col]]), .groups = "drop") %>%
    dplyr::filter(num_timepoints == 2) %>%
    dplyr::pull(!!sym(id_col))
  
  # -- 2) Subset to matched IDs only
  df_sub <- data %>%
    dplyr::filter(
      .data[[id_col]] %in% matched_ids,
      .data[[time_col]] %in% c(intake_val, followup_val),
      !is.na(.data[[var_col]])
    )
  
  # -- 3) Separate slash-delimited values into individual rows
  df_separated <- df_sub %>%
    tidyr::separate_rows(.data[[var_col]], sep = sep_char) %>%
    dplyr::mutate(!!var_col := stringr::str_trim(.data[[var_col]]))
  
  # -- 4) Count distinct responses per category per time point
  summary_df <- df_separated %>%
    dplyr::group_by(.data[[time_col]], .data[[var_col]]) %>%
    dplyr::summarize(n = dplyr::n_distinct(.data[[id_col]]), .groups = "drop")
  
  # -- 5) Compute the number of unique participants per time point
  distinct_counts <- df_separated %>%
    dplyr::group_by(.data[[time_col]]) %>%
    dplyr::summarize(denominator = dplyr::n_distinct(.data[[id_col]]), .groups = "drop")
  
  # -- 6) Merge to calculate percentages
  summary_df <- summary_df %>%
    dplyr::left_join(distinct_counts, by = time_col) %>%
    dplyr::mutate(perc = round(n / denominator * 100, 1)) %>%
    dplyr::select(-denominator)  # Remove denominator column
  
  # -- 7) Pivot wide (convert time points into columns)
  summary_wide <- summary_df %>%
    tidyr::pivot_wider(
      names_from  = .data[[time_col]],
      values_from = c(n, perc),
      names_sep   = "_"
    )
  

  
  # -- 8) Rename columns for clarity
  summary_wide <- summary_wide %>%
    dplyr::rename(
      Response      = !!var_col,
      "Intake n"    = paste0("n_", intake_val),
      "Intake %"    = paste0("perc_", intake_val),
      "6 month n"   = paste0("n_", followup_val),
      "6 month %"   = paste0("perc_", followup_val)
    )
  
  
  
  
  # Convert numeric → character for proper formatting
  summary_wide <- summary_wide %>%
    dplyr::mutate(
      `Intake n`   = as.character(`Intake n`),
      `Intake %`   = as.character(`Intake %`),
      `6 month n`  = as.character(`6 month n`),
      `6 month %`  = as.character(`6 month %`)
    )
  
  # -- 9) Add "Total N" row
  total_n <- length(matched_ids)
  total_row <- tibble::tibble(
    Response     = "",
    `Intake n`   = "",
    `Intake %`   = "",
    `6 month n`  = "",
    `6 month %`  = paste0("Total N = ", total_n)
  )
  
  summary_wide <- dplyr::bind_rows(summary_wide, total_row)
  
  # -- 10) Attach variable label (optional)
  attr(summary_wide, "var_label") <- var_label
  
  # -- 11) Print the table in HTML format for R Markdown
  cat("<br><h4>Matched sample: ", var_label, " summary</h4><br>")
  print(knitr::kable(summary_wide, format = "html"))
  
  return(summary_wide)
}


## Example use ####

# 
# all_tables_list <- list()
# vars_multiselect<- c("med_oud","money_source" )
# var_multi_labels<-  c( med_oud= "MOUD",
#                        money_source = "money_source")
# 
# for (v in vars_multiselect) {
#   # Run your multi-response summary function
#   this_table <- create_two_timepoint_table_select_multiple(
#     data        = gpra_full_long,
#     id_col      = "ClientID",
#     time_col    = "time_point",
#     var_col     = v,
#     var_label   = var_multi_labels[[v]],
#     sep_char    = "/",       # or whatever delimiter your data uses
#     intake_val  = "Intake",
#     followup_val= "6 month"
#   )
#   
#   all_tables_list[[v]] <- this_table
# }
# 
# 
# title_list <- var_multi_labels
# 
# export_list_to_excel(
#   table_list = all_tables_list, 
#   title_list = title_list,
#   file_name  = "descriptive_select_all_in_one_sheet.xlsx",
#   sheet_name = "AllTables"
# )


