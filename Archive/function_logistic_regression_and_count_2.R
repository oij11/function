# Function to generate the table for each variable



analyze_variable <- function(var_name, var_label, data, group_var, reference_value = NULL) {
  # Remove NA values from the variable column
  data <- data %>% filter(!is.na(!!sym(var_name)))
  
  # Identify the reference value if not provided
  if (is.null(reference_value)) {
    reference_value <- levels(data[[var_name]])[2] # Default to the second factor level
  }
  
  # Calculate total counts per group excluding NA
  total_counts <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(total_n = n(), .groups = 'drop')
  
  # Calculate frequencies and percentages within each group
  freq_table <- data %>%
    group_by(!!sym(group_var), !!sym(var_name)) %>%
    summarise(n = n(), .groups = 'drop') %>%
    left_join(total_counts, by = group_var) %>%
    mutate(percent = round(n / total_n * 100, 1),
           formatted = paste0(n, " (", percent, "%)")) %>%
    select(!!sym(group_var), !!sym(var_name), formatted) %>%
    pivot_wider(names_from = !!sym(group_var), values_from = formatted)
  
  # Fit logistic regression model, ensuring proper handling of factor levels
  data <- data %>% mutate(!!sym(group_var) := as.factor(!!sym(group_var)))
  model <- glm(as.formula(paste(var_name, "~", group_var)), data = data, family = binomial)
  model_tidy <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      OR = round(exp(estimate), 2),
      `Conf Low` = round(exp(conf.low), 2),
      `Conf High`= round(exp(conf.high), 2),
      `p-value`= round((p.value), 3),
      OR_CI = paste0(OR, " (", `Conf Low`, " - ", `Conf High`, ")")
    ) %>%
    select(Predictor = term, `OR (Conf. Low - Conf. High)` = OR_CI, `p-value`)
  
  # Ensure the term names match correctly with the variable name
  model_tidy <- model_tidy %>% filter(Predictor == paste0(group_var, levels(data[[group_var]])[2]))
  
  # Merge logistic regression results only for the reference row, but retain other rows for counts
  result_table <- freq_table %>%
    left_join(model_tidy %>% mutate(!!sym(var_name) := reference_value), by = var_name)
  
  # Rename columns appropriately
  col_names <- setNames(names(result_table)[-1], paste0(names(result_table)[-1], ": n (%)"))
  result_table <- result_table %>% rename(!!!col_names)
  
  # Display results
  ##### option: adding Cat as a title  ####
  #cat("\n\n####", var_label, "by", group_var, "\n\n")
  print(knitr::kable(result_table, caption = paste0(var_label," by ", group_var, " comparison")))
  cat("\n\n&nbsp;\n\n")
}


#### Example Input ####
# # List of variables and their labels
# variables <- list(
#   "s2_q10binary" = "Fentanyl Awareness (binary) s2_q10",
#   "s3_q17binary" = "Would you take knowing fentanyl (binary) s3_q17"
# )
# 
# group_var <- "age_2group"
# 
# # Run the function for each variable
# for (var in names(variables)) {
#   analyze_variable(var, variables[[var]], df, group_var)
# }

## Function to generate the table with adjusted p value (multiple testing) ####

library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(stats)

library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(stats)

analyze_multiple_variables <- function(var_list, data, group_var) {
  results_list <- list()  # will store a table for each variable
  
  for (var_name in names(var_list)) {
    var_label <- var_list[[var_name]]
    
    # 1) Filter out NA from the current variable
    data_filtered <- data %>%
      filter(!is.na(!!sym(var_name)))
    
    # 2) Calculate group-wise counts & percentages
    #    so that each group's total is the denominator
    freq_table <- data_filtered %>%
      group_by(!!sym(group_var)) %>%
      mutate(group_total = n()) %>%               # total per group
      ungroup() %>%
      group_by(!!sym(group_var), !!sym(var_name)) %>%
      summarise(
        n = n(),
        group_total = first(group_total),         # same group total for all rows in that group
        percent = round(n / group_total * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(formatted = paste0(n, " (", percent, "%)")) %>%
      select(!!sym(group_var), !!sym(var_name), formatted) %>%
      # Convert to wide format: one row per level of the var_name, columns = groups
      pivot_wider(
        names_from   = !!sym(group_var), 
        values_from  = formatted,
        names_prefix = "Group_"
      )
    
    # 3) Run the logistic regression
    #    We want the row for the group_var effect (typically the second level)
    data_model <- data_filtered %>%
      mutate(!!sym(group_var) := as.factor(!!sym(group_var)))
    model <- glm(
      formula = as.formula(paste(var_name, "~", group_var)),
      data = data_model,
      family = binomial
    )
    
    # Tidy the results; keep only the row with the group_var effect
    model_tidy <- broom::tidy(model, conf.int = TRUE) %>%
      filter(grepl(group_var, term)) %>%      # select the row with the group_var effect
      mutate(
        OR        = round(exp(estimate), 2),
        Conf.Low  = round(exp(conf.low), 2),
        Conf.High = round(exp(conf.high), 2),
        `p-value` = round(p.value, 4), 
        `OR (Conf. Low - Conf. High)` = paste0(OR, " (", Conf.Low, " - ", Conf.High, ")")
      ) %>%
      # We join on the second factor level of the var_name so it appears on that row only
      transmute(
        !!sym(var_name) := levels(data_model[[var_name]])[2],
        `OR (Conf. Low - Conf. High)`,
        `p-value`
      )
    
    # 4) Merge logistic regression results so only the second level row gets them
    result_table <- freq_table %>%
      left_join(model_tidy, by = var_name)
    
    # Store in the list for later p-value adjustment
    results_list[[var_name]] <- result_table
  }
  
  # 5) Adjust p-values across all models
  #    Collect all non-NA p-values, adjust them, and then assign back.
  all_p_values <- unlist(lapply(results_list, function(tbl) {
    tbl$`p-value`[!is.na(tbl$`p-value`)]
  }))
  
  if (length(all_p_values) > 0) {
    adjusted <- p.adjust(all_p_values, method = "fdr")
    
    idx <- 1
    for (nm in names(results_list)) {
      tbl <- results_list[[nm]]
      # Identify row(s) with non-NA p-values
      has_p <- which(!is.na(tbl$`p-value`))
      if (length(has_p) == 1) {
        # Create the column if it doesn't exist and assign the adjusted p-value
        tbl$`Adjusted p-value` <- NA
        tbl$`Adjusted p-value`[has_p] <- round(adjusted[idx], 3)
        idx <- idx + 1
      } else {
        # If there is no p-value or more than one (unexpected), fill with NA
        tbl$`Adjusted p-value` <- NA
      }
      results_list[[nm]] <- tbl
    }
  } else {
    # If no p-values were found at all, add the column with NAs
    for (nm in names(results_list)) {
      results_list[[nm]]$`Adjusted p-value` <- NA
    }
  }
  
  # 6) Print the results using knitr::kable
  for (nm in names(results_list)) {
    print(
      knitr::kable(
        results_list[[nm]],
        caption = paste(var_list[[nm]], "Comparison Table")
      )
    )
    cat("\n\n&nbsp;\n\n")
  }
  
  # 7) Return the complete list of results
  return(results_list)
}



#### Example input ######

# 
# # List of variables and their labels
# variables <- list(
#   "s2_q10binary" = "Fentanyl Awareness (binary) s2_q10",
#   "s3_q17binary" = "Would you take knowing fentanyl (binary) s3_q17",
#   "s2_q11___2" = "Info Source: Teacher/School",
#   "s2_q11___3" = "Info Source: Friend",
#   "s2_q11___5" = "Info Source: Parents",
#   "s2_q11___14"= "Info Source: Internet",
#   "s2_q11___6" = "Info Source: Healthcare provider",
#   "s2_q11___15" = "Info Source: OHA",
#   "s2_q12___2" = "Trusted info Source: Teacher/School",
#   "s2_q12___3" = "Trusted info Source: Friend",
#   "s2_q12___5" = "Trusted info Source: Parents",
#   "s2_q12___14"= "Trusted info Source: Internet",
#   "s2_q12___6" = "Trusted info Source: Healthcare provider",
#   "s2_q12___15" = "Trusted info Source: OHA"
# )
# 
# group_var <- "age_2group"
# 
# results_list<- analyze_multiple_variables(variables, df, group_var)


## Export to excel Functions ##### 
#1.After doing the above, Export to excel (in one sheet) 

# Use example #
# results_list <- analyze_multiple_variables(variables, df, "age_2group")

export_tables_to_excel <- function(results_list, var_list, file_name = "results.xlsx") {
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "All_Results")
  
  # Write main title
  writeData(wb, "All_Results", "My Combined Results", startRow = 1, startCol = 1)
  
  rowIndex <- 3  # Start writing from this row
  for (nm in names(results_list)) {
    # Write table label
    writeData(wb, "All_Results", var_list[[nm]], startRow = rowIndex, startCol = 1)
    rowIndex <- rowIndex + 1
    
    # Write table
    writeData(wb, "All_Results", results_list[[nm]], startRow = rowIndex, startCol = 1)
    rowIndex <- rowIndex + nrow(results_list[[nm]]) + 2  # Move down for next table
  }
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
}


#### Example Input####
# # Export results
# export_tables_to_excel(results_list, variables, file_name = "all_results_age.xlsx")

