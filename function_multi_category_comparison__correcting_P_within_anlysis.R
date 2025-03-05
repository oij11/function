
###########################################

# Omnibus testing #######

# Load necessary libraries (if not already loaded)
library(dplyr)
library(nnet)
library(knitr)
library(openxlsx)

# Function to run omnibus tests
omnibus_tests <- function(df, outcomes, predictors, var_labels) {
  library(dplyr)
  library(nnet)
  library(knitr)
  
  results_list <- list()
  all_p_values <- c()  # To collect all raw p-values
  
  # Loop over predictors and outcomes to compute tests
  for (predictor in predictors) {
    df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
    
    for (outcome in outcomes) {
      df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
      if (nrow(df_filtered) == 0) next
      
      # Create contingency table
      cont_table <- table(df_filtered[[outcome]], df_filtered[[predictor]])
      if (all(cont_table == 0)) next
      
      # Run tests with error handling
      chisq_test <- tryCatch(
        chisq.test(cont_table),
        error = function(e) list(statistic = NA, p.value = NA)
      )
      fisher_test <- tryCatch(
        fisher.test(cont_table),
        error = function(e) list(p.value = NA)
      )
      
      formula_str <- paste0(outcome, " ~ ", predictor)
      try_model <- tryCatch({
        multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
        multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
        lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
        list(
          lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
          lrt_p_value   = lrt_test$`Pr(Chi)`[2]
        )
      }, error = function(e) list(lrt_statistic = NA, lrt_p_value = NA))
      
      # Append the raw p-values for this set of tests (3 tests per table)
      all_p_values <- c(all_p_values,
                        chisq_test$p.value,
                        fisher_test$p.value,
                        try_model$lrt_p_value)
      
      # Build a results table as a tibble
      results_table <- tibble(
        Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
        `Test Statistic` = c(round(chisq_test$statistic, 3), NA, try_model$lrt_statistic),
        `p-value` = c(round(chisq_test$p.value, 3),
                      round(fisher_test$p.value, 3),
                      round(try_model$lrt_p_value, 3))
      )
      
      # Store the table in the list using a unique key (e.g., "s2_q10__age_2group")
      key <- paste0(outcome, "__", predictor)
      results_list[[key]] <- results_table
      
    }
  }
  
  # --- Add the Adjusted p-value column to each results table --- #
  
  # Compute global adjusted p-values from all collected raw p-values
  adjusted_p_values <- p.adjust(all_p_values, method = "fdr")
  
  # Loop over each table and add the new column
  adj_index <- 1
  for (key in names(results_list)) {
    tab <- results_list[[key]]
    n_tests <- nrow(tab)  # Typically, this is 3 (one row per test)
    # Add the column using the appropriate slice from the global vector
    tab$`Adjusted p-value` <- round(adjusted_p_values[adj_index:(adj_index + n_tests - 1)], 3)
    results_list[[key]] <- tab
    adj_index <- adj_index + n_tests
    
  }
  
  
  for (key in names(results_list)) {
    # Extract outcome and predictor from the key (assumes key is in the format "outcome__predictor")
    parts <- unlist(strsplit(key, "__"))
    outcome <- parts[1]
    predictor <- parts[2]
    
    title_text <- paste0(
      "Omnibus Test for differences between ",
      var_labels[[predictor]], " for ", var_labels[[outcome]]
    )
    
    cat("\n\n", title_text, "\n", strrep("-", nchar(title_text)), "\n")
    print(knitr::kable(results_list[[key]], format = "pipe"))
  }
  
  return(results_list)
}


### Export Omnibus test ####
# Function to export the omnibus test results to a single-sheet Excel file
export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx", var_labels) {
  wb <- createWorkbook()
  addWorksheet(wb, "Omnibus Tests")
  current_row <- 1
  
  for (name in names(results_list)) {
    tbl <- results_list[[name]]
    # Split the key into outcome and predictor
    name_components <- strsplit(name, "__")[[1]]
    outcome <- name_components[1]
    predictor <- name_components[2]
    
    predictor_label <- ifelse(!is.null(var_labels[[predictor]]), var_labels[[predictor]], predictor)
    outcome_label   <- ifelse(!is.null(var_labels[[outcome]]), var_labels[[outcome]], outcome)
    title <- paste0("Omnibus Test for differences between ", predictor_label, " for ", outcome_label)
    
    writeData(wb, "Omnibus Tests", title, startRow = current_row, colNames = FALSE)
    current_row <- current_row + 1
    writeData(wb, "Omnibus Tests", tbl, startRow = current_row, colNames = TRUE)
    current_row <- current_row + nrow(tbl) + 2
  }
  
  saveWorkbook(wb, file_path, overwrite = TRUE)
  cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
}





export_omnibus_results_separate_sheet <- function(results_list, file_path = "Omnibus_Tests.xlsx") {
  library(openxlsx)  # Needed for writing Excel files
  
  wb <- createWorkbook()  # Create an empty workbook
  
  # Loop through each result and add to a new sheet
  for (name in names(results_list)) {
    addWorksheet(wb, name)  # Create a sheet with the variable combination name
    writeData(wb, name, results_list[[name]])  # Write the data to the sheet
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  cat("\nOmnibus test results saved to:", file_path, "\n")
}

# Post-hoc pairwise testing #######

## Pairwise Fisher Test ####

pairwise_fisher_multiple <- function(df, outcomes, predictors, var_labels) {
  library(dplyr)
  library(gt)
  library(rstatix)  # Needed for p.adjust with method = "fdr"
  
  results_list <- list()
  
  # Loop through each predictor
  for (predictor in predictors) {
    
    # Remove missing values for the predictor
    df_filtered <- df %>% filter(!is.na(.data[[predictor]]))
    
    # Loop through each outcome
    for (outcome in outcomes) {
      
      # Remove missing values for the outcome
      df_filtered <- df_filtered %>% filter(!is.na(.data[[outcome]]))
      
      # Create contingency table
      tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
      
      # Function for Fisher's test for each response
      pairwise_fisher_corrected <- function(response) {
        # Extract counts for this response
        response_counts <- tab[response, ]  # Select row
        
        # Compute total counts in each group
        total_counts <- colSums(tab)
        
        # Compute percentages
        percent_Group_1 <- round((response_counts[1] / total_counts[1]) * 100, 1)
        percent_Group_2 <- round((response_counts[2] / total_counts[2]) * 100, 1)
        
        # Format as "N (X%)"
        formatted_Group_1 <- paste0(response_counts[1], " (", percent_Group_1, "%)")
        formatted_Group_2 <- paste0(response_counts[2], " (", percent_Group_2, "%)")
        
        # Compute "Not this response"
        not_response_counts <- total_counts - response_counts
        
        # Create a valid 2x2 contingency table
        valid_2x2 <- matrix(c(response_counts, not_response_counts), nrow = 2, byrow = TRUE)
        rownames(valid_2x2) <- c(response, "Not this response")
        colnames(valid_2x2) <- levels(df_filtered[[predictor]])
        
        # Perform Fisher’s Exact Test
        test_result <- fisher.test(valid_2x2)
        
        return(data.frame(
          Response = response,
          Group_1 = levels(df_filtered[[predictor]])[1],
          Group_2 = levels(df_filtered[[predictor]])[2],
          N_Group_1 = formatted_Group_1,
          N_Group_2 = formatted_Group_2,
          P_value = test_result$p.value
        ))
      }
      
      # Run pairwise Fisher’s test on each response level
      response_levels <- levels(df_filtered[[outcome]])
      pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))
      
      # Apply FDR correction
      pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "fdr")
      
      # Add significance labels
      pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")
      
      # Generate dynamic table title
      title_text <- paste0("Pairwise Comparisons between ", var_labels[[predictor]], " for ", var_labels[[outcome]])
      
      # Store results in list
      results_list[[paste0(outcome, "_", predictor)]] <- pairwise_tests_corrected %>%
        gt() %>%
        tab_header(title = title_text) %>%
        cols_label(
          Response = "Response",
          Group_1 = "Group 1",
          Group_2 = "Group 2",
          N_Group_1 = "N (Group 1, %)",
          N_Group_2 = "N (Group 2, %)",
          P_value = "Raw P-value",
          Adjusted_P = "Adjusted P-value",
          p.adj.signif = "Significance"
        ) %>%
        fmt_number(columns = c(P_value, Adjusted_P), decimals = 3) %>%
        tab_options(
          table.font.size = px(14),
          column_labels.font.weight = "bold"
        ) %>%
        data_color(
          columns = p.adj.signif,
          colors = scales::col_factor(
            palette = c("black", "red"),
            domain = c("ns", "*")
          )
        )
    }
  }
  
  # for (name in names(results_list)) {
  #   print(results_list[[name]])  # This displays the `gt` table interactively
  #   
  #   # # Convert gt table to a data frame
  #   # df <- as.data.frame(results_list[[name]]$`_data`)
  #   # 
  #   # # Print the table in Markdown format using kable
  #   # cat("\n\n###", results_list[[name]]$`_heading`$title, "\n\n")  # Print title in Markdown
  #   # cat(knitr::kable(df, format = "markdown"), "\n\n")  # Print table in Markdown format
  # }
  # 
  return(results_list)
}


### Example use #######

# var_labels <- list(
#   s2_q10 = "Fentanyl Awareness",
#   s4_q23 = "Calling 911",
#   age_2group = "Age Groups",
#   s3_q14 = "Have vs. Haven't"
# )
# 
# 
# pairwise_fisher_multiple(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)
# 



### function: Export to Excel ####

library(openxlsx)

export_fisher_results_to_excel <- function(results_list, file_name = "fisher_results.xlsx") {
  # Create a new workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Results")
  
  row_counter <- 1  # Track row position
  
  # Loop through the list of tables
  for (name in names(results_list)) {
    
    # Extract title from gt table
    title <- results_list[[name]]$`_heading`$title
    
    print("===")
    print(title)
    
    # Convert gt table to data frame
    df <- as.data.frame(results_list[[name]]$`_data`)
    

    # Write title row
    writeData(wb, "Results", title, startRow = row_counter, startCol = 1)
    row_counter <- row_counter + 1  # Move to next row
    
    # Write data to worksheet
    writeData(wb, "Results", df, startRow = row_counter, startCol = 1, rowNames = FALSE)
    row_counter <- row_counter + nrow(df) + 2  # Leave a blank row after each table
  }
  
  # Save workbook
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

# # Example usage:
#  results_list <- pairwise_fisher_multiple(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)
#  export_fisher_results_to_excel(results_list, "fisher_results.xlsx")


###########################################


