
###########################################

# Omnibus testing #######



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






# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)   # For multinom()
#   library(knitr)  # For rendering
#   
#   results_list <- list()
#   all_p_values <- c()  # Collect all p-values for adjustment
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
#       
#       if (nrow(df_filtered) == 0) next
#       
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       if (all(tab == 0)) next
#       
#       # Perform tests
#       chisq_test <- tryCatch(
#         chisq.test(tab),
#         error = function(e) list(statistic = NA, p.value = NA)
#       )
#       fisher_test <- tryCatch(
#         fisher.test(tab),
#         error = function(e) list(p.value = NA)
#       )
#       
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       try_model <- tryCatch(
#         {
#           multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#           multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#           lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
#           list(
#             lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
#             lrt_p_value = lrt_test$`Pr(Chi)`[2]
#           )
#         },
#         error = function(e) list(lrt_statistic = NA, lrt_p_value = NA)
#       )
#       
#       # Collect raw p-values for adjustment
#       all_p_values <- c(all_p_values, chisq_test$p.value, fisher_test$p.value, try_model$lrt_p_value)
#       
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,  # Fisher's test does not have a test statistic
#           try_model$lrt_statistic
#         ),
#         `p-value` = c(
#           chisq_test$p.value,
#           fisher_test$p.value,
#           try_model$lrt_p_value
#         )
#       )
#       
#       # Store results for each outcome-predictor combination
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
#   
#   # Adjust p-values across all tests
#   adjusted_p_values <- p.adjust(all_p_values, method = "BH")
#   
#   # Add adjusted p-values to each table
#   adj_index <- 1
#   for (key in names(results_list)) {
#     # Extract the table
#     table <- results_list[[key]]
#     
#     # Add the adjusted p-value column
#     table <- table %>%
#       mutate(`Adjusted p-value` = adjusted_p_values[adj_index:(adj_index + nrow(table) - 1)])
#     
#     # Update the table in the results list
#     results_list[[key]] <- table
#     
#     # Update the index
#     adj_index <- adj_index + nrow(table)
#     
#     # Render the table for R Markdown
#     title_text <- paste0("Omnibus Test for differences between ", var_labels[[key]], "\n")
#     cat("\n\n", title_text, strrep("-", nchar(title_text)), "\n")
#     print(knitr::kable(table, format = "pipe"))
#   }
#   
#   return(results_list)
# }



# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)
#   library(knitr)
#   
#   results_list <- list()  # Keep the original list structure
#   all_p_values <- c()     # Collect p-values for adjustment
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
#       
#       if (nrow(df_filtered) == 0) next
#       
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       if (all(tab == 0)) next
#       
#       # Perform tests
#       chisq_test <- tryCatch(
#         chisq.test(tab),
#         error = function(e) list(statistic = NaN, p.value = NA)
#       )
#       fisher_test <- tryCatch(
#         fisher.test(tab),
#         error = function(e) list(p.value = NA)
#       )
#       
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       try_model <- tryCatch(
#         {
#           multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#           multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#           lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
#           list(
#             lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
#             lrt_p_value = lrt_test$`Pr(Chi)`[2]
#           )
#         },
#         error = function(e) list(lrt_statistic = NA, lrt_p_value = NA)
#       )
#       
#       # Collect raw p-values for adjustment
#       all_p_values <- c(all_p_values, chisq_test$p.value, fisher_test$p.value, try_model$lrt_p_value)
#       
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,
#           try_model$lrt_statistic
#         ),
#         `p-value` = c(
#           chisq_test$p.value,
#           fisher_test$p.value,
#           try_model$lrt_p_value
#         )
#       )
#       
#       # Store results for each outcome-predictor combination
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
#   
#   # Adjust p-values across all tests (Benjamini-Hochberg by default)
#   adjusted_p_values <- p.adjust(all_p_values, method = "BH")
#   
#   # Add adjusted p-values to each table
#   p_value_index <- 1
#   for (key in names(results_list)) {
#     # Extract the table
#     table <- results_list[[key]]
#     
#     # Add the adjusted p-value column
#     table <- table %>%
#       mutate(`Adjusted p-value` = adjusted_p_values[p_value_index:(p_value_index + nrow(table) - 1)])
#     
#     # Update the table in the results list
#     results_list[[key]] <- table
#     
#     # Update the index
#     p_value_index <- p_value_index + nrow(table)
#   }
#   
#   return(results_list)
# }
# 


# omnibus_tests <- function(df, outcomes, predictors, var_labels, output_file = "Omnibus_Tests_OneSheet.xlsx") {
#   library(dplyr)
#   library(nnet)
#   library(knitr)
#   library(openxlsx)  # For Excel output
#   
#   results_list <- list()
#   all_results <- tibble()  # Collect all results for a single table
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
#       
#       if (nrow(df_filtered) == 0) next
#       
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       if (all(tab == 0)) next
#       
#       chisq_test <- tryCatch(
#         chisq.test(tab),
#         error = function(e) list(statistic = NA, p.value = NA)
#       )
#       
#       fisher_test <- tryCatch(
#         fisher.test(tab),
#         error = function(e) list(p.value = NA)
#       )
#       
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       try_model <- tryCatch(
#         {
#           multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#           multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#           lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
#           list(
#             lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
#             lrt_p_value = lrt_test$`Pr(Chi)`[2]
#           )
#         },
#         error = function(e) list(lrt_statistic = NA, lrt_p_value = NA)
#       )
#       
#       results_table <- tibble(
#         Outcome = outcome,
#         Predictor = predictor,
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,
#           try_model$lrt_statistic
#         ),
#         `p-value` = c(
#           chisq_test$p.value,
#           fisher_test$p.value,
#           try_model$lrt_p_value
#         )
#       )
#       
#       # Adjust p-values (Benjamini-Hochberg)
#       results_table <- results_table %>%
#         mutate(`Adjusted p-value` = p.adjust(`p-value`, method = "BH", n = length(outcomes) * length(predictors) * 3))
#       
#       # Append to all_results
#       all_results <- bind_rows(all_results, results_table)
#     }
#   }
#   
#   # Save to Excel
#   write.xlsx(all_results, output_file, overwrite = TRUE)
#   
#   # Return results
#   return(all_results)
# }



# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)   # For multinom()
#   library(knitr)  # For kable()
#   
#   results_list <- list()
#   all_p_values <- c()  # Store all p-values for correction
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     
#     # Remove missing values for the predictor only once
#     df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       
#       # Remove missing values for the outcome only once
#       df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
#       
#       # Ensure that the dataset is not empty
#       if (nrow(df_filtered) == 0) next
#       
#       # Create contingency table
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       
#       # Check if contingency table is valid
#       if (all(tab == 0)) next
#       
#       # Chi-square test
#       chisq_test <- tryCatch(
#         chisq.test(tab),
#         error = function(e) list(statistic = NaN, p.value = NA)
#       )
#       
#       # Fisher’s Exact Test
#       fisher_test <- tryCatch(
#         fisher.test(tab),
#         error = function(e) list(p.value = NA)
#       )
#       
#       # Multinomial Logistic Regression (Fixing model failures)
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       try_model <- tryCatch(
#         {
#           multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#           multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#           lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
#           list(
#             lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
#             lrt_p_value = lrt_test$`Pr(Chi)`[2]
#           )
#         },
#         error = function(e) list(lrt_statistic = NA, lrt_p_value = NA)
#       )
#       
#       # Collect p-values for correction
#       all_p_values <- c(all_p_values, chisq_test$p.value, fisher_test$p.value, try_model$lrt_p_value)
#       
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,  # Fisher's test does not have a test statistic
#           try_model$lrt_statistic
#         ),
#         `p-value` = c(
#           chisq_test$p.value,
#           fisher_test$p.value,
#           try_model$lrt_p_value
#         )
#       )
#       
#       # Store results for Excel export
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
#   
#   # Apply Multiple Testing Correction (Benjamini-Hochberg by default)
#   adjusted_p_values <- p.adjust(all_p_values, method = "BH")
#   
#   # Update results with adjusted p-values
#   adj_index <- 1
#   for (key in names(results_list)) {
#     results_list[[key]]$`Adjusted p-value` <- adjusted_p_values[adj_index:(adj_index + 2)]
#     adj_index <- adj_index + 3
#   }
#   
#   return(results_list)
# }

# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)   # For multinom()
#   library(knitr)  # For kable()
#   
#   results_list <- list()
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     
#     # Remove missing values for the predictor only once
#     df_filtered_pred <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       
#       # Remove missing values for the outcome only once
#       df_filtered <- df_filtered_pred %>% filter(!is.na(.data[[outcome]]))
#       
#       # Ensure that the dataset is not empty
#       if (nrow(df_filtered) == 0) next
#       
#       # Create contingency table
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       
#       # Check if contingency table is valid
#       if (all(tab == 0)) next
#       
#       # Chi-square test
#       chisq_test <- tryCatch(
#         chisq.test(tab),
#         error = function(e) list(statistic = NaN, p.value = NA)
#       )
#       
#       # Fisher’s Exact Test
#       fisher_test <- tryCatch(
#         fisher.test(tab),
#         error = function(e) list(p.value = NA)
#       )
#       
#       # Multinomial Logistic Regression (Fixing model failures)
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       try_model <- tryCatch(
#         {
#           multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#           multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#           lrt_test <- anova(multi_null, multi_outcome, test = "Chisq")
#           list(
#             lrt_statistic = round(lrt_test$`LR stat.`[2], 2),
#             lrt_p_value = format.pval(lrt_test$`Pr(Chi)`[2], digits = 3, eps = 0.001)
#           )
#         },
#         error = function(e) list(lrt_statistic = NA, lrt_p_value = NA)
#       )
#       
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,  # Fisher's test does not have a test statistic
#           try_model$lrt_statistic
#         ),
#         `p-value` = c(
#           format.pval(chisq_test$p.value, digits = 3, eps = 0.001),
#           format.pval(fisher_test$p.value, digits = 3, eps = 0.001),
#           try_model$lrt_p_value
#         )
#       )
#       
#       # Generate dynamic table title
#       title_text <- paste0("Omnibus Test for differences between ", var_labels[[predictor]], " for ", var_labels[[outcome]])
#       
#       # Print title first
#       cat("\n\n", title_text, "\n", strrep("-", nchar(title_text)), "\n")
#       
#       # Print table
#       print(knitr::kable(results_table))
#       
#       # Store results for Excel export
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
#   
#   return(results_list)
# }


# WRONG FORMAT 
# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)   # For multinom()
#   library(knitr)  # For kable()
#   
#   results_list <- list()
#   
#   # Loop through each predictor
#   for (predictor in predictors) {
#     
#     # Remove missing values for the predictor
#     df_filtered <- df %>% filter(!is.na(.data[[predictor]]))
#     
#     # Loop through each outcome
#     for (outcome in outcomes) {
#       
#       # Remove missing values for the outcome
#       df_filtered <- df_filtered %>% filter(!is.na(.data[[outcome]]))
#       
#       # Create contingency table
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
#       
#       # Chi-square test
#       chisq_test <- chisq.test(tab)
#       
#       # Fisher’s Exact Test (in case of small counts)
#       fisher_test <- fisher.test(tab)
#       
#       # Multinomial logistic regression for Likelihood Ratio Test
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#       multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
#       
#       # Likelihood Ratio Test
#       lrt_test <- tryCatch(
#         anova(multi_null, multi_outcome, test = "Chisq"),
#         error = function(e) NULL  # Handle errors gracefully
#       )
#       
#       # Extract Likelihood Ratio Test statistic and p-value safely
#       lrt_statistic <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
#         round(lrt_test$`LR stat.`[2], 2)
#       } else {
#         NA
#       }
#       
#       lrt_p_value <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
#         format.pval(lrt_test$`Pr(Chi)`[2], digits = 3, eps = 0.001)
#       } else {
#         NA
#       }
#       
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,  # Fisher's test does not have a test statistic
#           lrt_statistic
#         ),
#         `p-value` = c(
#           format.pval(chisq_test$p.value, digits = 3, eps = 0.001),
#           format.pval(fisher_test$p.value, digits = 3, eps = 0.001),
#           lrt_p_value
#         )
#       )
#       
#       # Generate dynamic table title
#       title_text <- paste0("Omnibus Test for differences between ", var_labels[[predictor]], " for ", var_labels[[outcome]])
#       
#       # Print title and table once
#       cat("\n\n", title_text, "\n", strrep("-", nchar(title_text)), "\n\n")
#       cat(knitr::kable(results_table, format = "markdown"), "\n\n")  # Proper Markdown format
#       
#       # Store results for Excel export
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
#   
#   # Return results silently (no automatic printing)
#   invisible(results_list)
# }


# omnibus_tests <- function(df, outcomes, predictors, var_labels) {
#   library(dplyr)
#   library(nnet)   # For multinom()
#   library(knitr)  # For kable()
# 
#   results_list <- list()
# 
#   # Loop through each predictor
#   for (predictor in predictors) {
# 
#     # Remove missing values for the predictor
#     df_filtered <- df %>% filter(!is.na(.data[[predictor]]))
# 
#     # Loop through each outcome
#     for (outcome in outcomes) {
# 
#       # Remove missing values for the outcome
#       df_filtered <- df_filtered %>% filter(!is.na(.data[[outcome]]))
# 
#       # Create contingency table
#       tab <- table(df_filtered[[outcome]], df_filtered[[predictor]])
# 
#       # Chi-square test
#       chisq_test <- chisq.test(tab)
# 
#       # Fisher’s Exact Test (in case of small counts)
#       fisher_test <- fisher.test(tab)
# 
#       # **Fix: Use `as.formula()` to avoid `.data` issue**
#       formula_str <- paste0(outcome, " ~ ", predictor)
#       multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
#       multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
# 
#       # Likelihood Ratio Test
#       lrt_test <- tryCatch(
#         anova(multi_null, multi_outcome, test = "Chisq"),
#         error = function(e) NULL  # Handle errors gracefully
#       )
# 
#       # Extract Likelihood Ratio Test statistic and p-value safely
#       lrt_statistic <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
#         round(lrt_test$`LR stat.`[2], 2)
#       } else {
#         NA
#       }
# 
#       lrt_p_value <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
#         format.pval(lrt_test$`Pr(Chi)`[2], digits = 3, eps = 0.001)
#       } else {
#         NA
#       }
# 
#       # Create summary table
#       results_table <- tibble(
#         Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
#         `Test Statistic` = c(
#           round(chisq_test$statistic, 2),
#           NA,  # Fisher's test does not have a test statistic
#           lrt_statistic
#         ),
#         `p-value` = c(
#           format.pval(chisq_test$p.value, digits = 3, eps = 0.001),
#           format.pval(fisher_test$p.value, digits = 3, eps = 0.001),
#           lrt_p_value
#         )
#       )
# 
#       # Generate dynamic table title
#       title_text <- paste0("Omnibus Test for differences between ", var_labels[[predictor]], " for ", var_labels[[outcome]])
# 
#       # **Print title first**
#       cat("\n\n", title_text, "\n", strrep("-", nchar(title_text)), "\n")
# 
#       # **Print table**
#       print(knitr::kable(results_table))
# 
#       # **Store results for Excel export**
#       results_list[[paste0(outcome, "_", predictor)]] <- results_table
#     }
#   }
# 
#   return(results_list)
# }



### Export Omnibus test ####


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


#ISSUE FLAG ####
# it does not fetch var labels. only print with vars. multiple attempts all failed. 

export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx", var_labels) {
  library(openxlsx)  # Needed for writing Excel files
  
  # Create a new workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Omnibus Tests")  # Add a single sheet
  
  current_row <- 1  # Track the current row to write data
  
  # Loop through each table in results_list
  for (name in names(results_list)) {
    # Extract the table
    table <- results_list[[name]]
    
    # Mutate the names to match the var_labels
    name_components <- strsplit(name, "__")[[1]]  # Split name into components
    predictor <- name_components[length(name_components)]  # Last part is predictor
    outcome <- name_components[1] # Rest is outcome
    
    # Replace variable names with labels
    predictor_label <- ifelse(!is.null(var_labels[[predictor]]), var_labels[[predictor]], predictor)
    outcome_label <- ifelse(!is.null(var_labels[[outcome]]), var_labels[[outcome]], outcome)
    
 
    # Generate title using the mutated names
    title <- paste0("Omnibus Test for differences between ", predictor_label, " for ", outcome_label)
    
    # Write the title to the sheet
    writeData(wb, "Omnibus Tests", title, startRow = current_row, colNames = FALSE)
    current_row <- current_row + 1  # Move to the next row
    
    # Write the table to the sheet
    writeData(wb, "Omnibus Tests", table, startRow = current_row, colNames = TRUE)
    current_row <- current_row + nrow(table) + 2  # Add space after the table
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
}



# export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx", var_labels) {
#   library(openxlsx)  # Needed for writing Excel files
#   
#   # Create a new workbook
#   wb <- createWorkbook()
#   addWorksheet(wb, "Omnibus Tests")  # Add a single sheet
#   
#   current_row <- 1  # Track the current row to write data
#   
#   # Loop through each table in results_list
#   for (name in names(results_list)) {
#     # Extract the table
#     table <- results_list[[name]]
#     
#     # Parse the predictor and outcome from the `name`
#     split_name <- strsplit(name, "_")[[1]]
#     predictor <- split_name[length(split_name)]  # Last element is the predictor
#     outcome <- paste(split_name[1:(length(split_name) - 1)], collapse = "_")  # Remaining part is the outcome
#     
#     # Map variable names to labels using var_labels
#     predictor_label <- var_labels[[predictor]]
#     outcome_label <- var_labels[[outcome]]
#     
#     # If labels are missing, fallback to raw variable names
#     if (is.null(predictor_label)) predictor_label <- predictor
#     if (is.null(outcome_label)) outcome_label <- outcome
#     
#     # Generate the title using the labels
#     title <- paste0("Omnibus Test for differences between ", predictor_label, " for ", outcome_label)
#     
#     # Write the title to the sheet
#     writeData(wb, "Omnibus Tests", title, startRow = current_row, colNames = FALSE)
#     current_row <- current_row + 1  # Move to the next row
#     
#     # Write the table to the sheet
#     writeData(wb, "Omnibus Tests", table, startRow = current_row, colNames = TRUE)
#     current_row <- current_row + nrow(table) + 2  # Add space after the table
#   }
#   
#   # Save the workbook
#   saveWorkbook(wb, file_path, overwrite = TRUE)
#   
#   cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
# }
# 


# export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx", var_labels) {
#   library(openxlsx)  # Needed for writing Excel files
#   
#   # Create a new workbook
#   wb <- createWorkbook()
#   addWorksheet(wb, "Omnibus Tests")  # Add a single sheet
#   
#   current_row <- 1  # Track the current row to write data
#   
#   # Loop through each table in results_list
#   for (name in names(results_list)) {
#     # Extract the table
#     table <- results_list[[name]]
#     
#     # Parse the predictor and outcome from the `name`
#     split_name <- strsplit(name, "_")[[1]]
#     predictor <- split_name[length(split_name)]  # Last element is the predictor
#     outcome <- paste(split_name[1:(length(split_name) - 1)], collapse = "_")  # Remaining part is the outcome
#     
#     # Generate the title using var_labels
#     predictor_label <- var_labels[[predictor]]
#     outcome_label <- var_labels[[outcome]]
#     
#     if (is.null(predictor_label) || is.null(outcome_label)) {
#       title <- paste0("Omnibus Test for differences between ", predictor, " for ", outcome)
#     } else {
#       title <- paste0("Omnibus Test for differences between ", predictor_label, " for ", outcome_label)
#     }
#     
#     # Write the title to the sheet
#     writeData(wb, "Omnibus Tests", title, startRow = current_row, colNames = FALSE)
#     current_row <- current_row + 1  # Move to the next row
#     
#     # Write the table to the sheet
#     writeData(wb, "Omnibus Tests", table, startRow = current_row, colNames = TRUE)
#     current_row <- current_row + nrow(table) + 2  # Add space after the table
#   }
#   
#   # Save the workbook
#   saveWorkbook(wb, file_path, overwrite = TRUE)
#   
#   cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
# }



# export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx", var_labels = NULL) {
#   library(openxlsx)  # Needed for writing Excel files
#   
#   # Create a new workbook
#   wb <- createWorkbook()
#   addWorksheet(wb, "Omnibus Tests")  # Add a single sheet
#   
#   current_row <- 1  # Track the current row to write data
#   
#   # Loop through each table in results_list
#   for (name in names(results_list)) {
#     # Extract the table
#     table <- results_list[[name]]
#     
#     # Generate a title for the table
#     if (!is.null(var_labels)) {
#       split_name <- strsplit(name, "_")[[1]]
#       predictor <- split_name[length(split_name)]
#       outcome <- paste(split_name[1:(length(split_name) - 1)], collapse = "_")
#       title <- paste0(
#         "Omnibus Test for differences between ", 
#         var_labels[[predictor]], " for ", var_labels[[outcome]]
#       )
#     } else {
#       title <- paste0("Omnibus Test for ", name)
#     }
#     
#     # Write the title to the sheet
#     writeData(wb, "Omnibus Tests", title, startRow = current_row, colNames = FALSE)
#     current_row <- current_row + 1  # Move to the next row
#     
#     # Write the table to the sheet
#     writeData(wb, "Omnibus Tests", table, startRow = current_row, colNames = TRUE)
#     current_row <- current_row + nrow(table) + 2  # Add space after the table
#   }
#   
#   # Save the workbook
#   saveWorkbook(wb, file_path, overwrite = TRUE)
#   
#   cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
# }



# export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx") {
#   library(openxlsx)  # Needed for writing Excel files
#   library(dplyr)     # For data manipulation
#   
#   # Convert list of tables into one large dataframe
#   combined_results <- bind_rows(
#     lapply(names(results_list), function(name) {
#       results_list[[name]] %>%
#         mutate(Comparison = name) %>%  # Add a column with predictor-outcome name
#         relocate(Comparison)  # Move the new column to the front
#     }),
#     .id = "ID"
#   ) %>% select(-ID)  # Remove unnecessary ID column
#   
#   # Create a workbook
#   wb <- createWorkbook()
#   addWorksheet(wb, "Omnibus Tests")  # Single sheet
#   writeData(wb, "Omnibus Tests", combined_results)  # Write all data to one sheet
#   
#   # Save the workbook
#   saveWorkbook(wb, file_path, overwrite = TRUE)
#   
#   cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
# }


# export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx") {
#   library(openxlsx)  # Needed for writing Excel files
#   library(dplyr)     # For data manipulation
#   
#   # Convert list of tables into one large dataframe
#   combined_results <- bind_rows(
#     lapply(names(results_list), function(name) {
#       results_list[[name]] %>%
#         mutate(Comparison = name) %>%  # Add a column with predictor-outcome name
#         relocate(Comparison)  # Move the new column to the front
#     }),
#     .id = "ID"
#   ) %>% select(-ID)  # Remove unnecessary ID column
#   
#   # Create a workbook
#   wb <- createWorkbook()
#   addWorksheet(wb, "Omnibus Tests")  # Single sheet
#   writeData(wb, "Omnibus Tests", combined_results)  # Write all data to one sheet
#   
#   # Save the workbook
#   saveWorkbook(wb, file_path, overwrite = TRUE)
#   
#   cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
# }
# 




# omnibus_tests(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)
# 
# omnibus_results<- omnibus_tests(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)
# 
# print(omnibus_results[["s2_q10_age_2group"]]) 



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


