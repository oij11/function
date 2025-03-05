
#alternative
# Create a contingency table

# Load necessary libraries
library(dplyr)
library(rstatix)
library(gt)

# Create contingency table
tab <- table(df$s2_q10, df$age_2group)

# Function to perform Fisher’s test for each response category
pairwise_fisher_corrected <- function(response) {
  sub_tab <- tab[response, , drop = FALSE]  # Select the row corresponding to the response category
  test_result <- fisher.test(sub_tab)  # Perform Fisher's Exact Test
  
  # Extract counts
  n_Age_1 <- sub_tab[1, 1]  # Age 15-17 count for this response
  n_Age_2 <- sub_tab[1, 2]  # Age 18-25 count for this response
  
  return(data.frame(
    Response = response,
    Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
    Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
    n_Age_Group_1 = n_Age_1,
    n_Age_Group_2 = n_Age_2,
    P_value = test_result$p.value
  ))
}

# Apply to each response category
response_levels <- levels(df$s2_q10)
pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))

# Apply Bonferroni correction
pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")

# Add significance labels
pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")

# Display results
pairwise_tests_corrected %>%
  gt() %>%
  tab_header(
    title = "Pairwise Comparisons Between Age Groups for Each Response Category",
    subtitle = "Bonferroni-adjusted p-values"
  ) %>%
  cols_label(
    Response = "Response Category",
    Age_Group_1 = "Age Group 1",
    Age_Group_2 = "Age Group 2",
    n_Age_Group_1 = "N (Age Group 1)",
    n_Age_Group_2 = "N (Age Group 2)",
    P_value = "Raw P-value",
    Adjusted_P = "Adjusted P-value",
    p.adj.signif = "Significance"
  ) %>%
  fmt_number(
    columns = c(P_value, Adjusted_P),
    decimals = 3
  ) %>%
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







# Load necessary libraries
library(dplyr)
library(rstatix)
library(gt)

# Create contingency table
tab <- table(df$s2_q10, df$age_2group)

# Function to perform Fisher’s test for each response category
pairwise_fisher_corrected <- function(response) {
  # Extract counts for this response category
  response_counts <- tab[response, ]  # Select the row for this response
  
  # Compute "Not this response" (total in each age group - selected response)
  total_counts <- colSums(tab)  # Total counts in each age group
  not_response_counts <- total_counts - response_counts
  
  # Create a valid 2x2 contingency table
  valid_2x2 <- matrix(c(response_counts, not_response_counts), nrow = 2, byrow = TRUE)
  rownames(valid_2x2) <- c(response, "Not this response")
  colnames(valid_2x2) <- levels(df$age_2group)
  
  # Perform Fisher’s Exact Test
  test_result <- fisher.test(valid_2x2)
  return(valid_2x2)
  return(test_result)
  
  # Extract counts
  n_Age_1 <- response_counts[1]  # People in Age Group 1 selecting this response
  n_Age_2 <- response_counts[2]  # People in Age Group 2 selecting this response
  
  return(data.frame(
    Response = response,
    Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
    Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
    n_Age_Group_1 = n_Age_1,
    n_Age_Group_2 = n_Age_2,
    P_value = test_result$p.value
  ))
}

# Apply to each response category
response_levels <- levels(df$s2_q10)
pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))

# Apply Bonferroni correction
pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")

# Add significance labels
pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")

# Display results
pairwise_tests_corrected %>%
  gt() %>%
  tab_header(
    title = "Pairwise Comparisons Between Age Groups for Each Response Category",
    subtitle = "Bonferroni-adjusted p-values"
  ) %>%
  cols_label(
    Response = "Response Category",
    Age_Group_1 = "Age Group 1",
    Age_Group_2 = "Age Group 2",
    n_Age_Group_1 = "N (Age Group 1)",
    n_Age_Group_2 = "N (Age Group 2)",
    P_value = "Raw P-value",
    Adjusted_P = "Adjusted P-value",
    p.adj.signif = "Significance"
  ) %>%
  fmt_number(
    columns = c(P_value, Adjusted_P),
    decimals = 3
  ) %>%
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




### CORRECT ONE #####

# Load necessary libraries
library(dplyr)
library(rstatix)
library(gt)

# Create contingency table
tab <- table(df$s2_q10, df$age_2group)

# Function to perform Fisher’s test for each response category
pairwise_fisher_corrected <- function(response) {
  # Extract counts for this response category
  response_counts <- tab[response, ]  # Select the row for this response
  
  # Compute "Not this response" (total in each age group - selected response)
  total_counts <- colSums(tab)  # Total counts in each age group
  not_response_counts <- total_counts - response_counts
  
  # Create a valid 2x2 contingency table
  valid_2x2 <- matrix(c(response_counts, not_response_counts), nrow = 2, byrow = TRUE)
  rownames(valid_2x2) <- c(response, "Not this response")
  colnames(valid_2x2) <- levels(df$age_2group)
  
  # Perform Fisher’s Exact Test
  test_result <- fisher.test(valid_2x2)
  
  # Extract counts
  n_Age_1 <- response_counts[1]  # People in Age Group 1 selecting this response
  n_Age_2 <- response_counts[2]  # People in Age Group 2 selecting this response
  
  return(data.frame(
    Response = response,
    Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
    Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
    n_Age_Group_1 = n_Age_1,
    n_Age_Group_2 = n_Age_2,
    P_value = test_result$p.value
  ))
}

# Apply to each response category
response_levels <- levels(df$s2_q10)
pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))

# Apply Bonferroni correction
pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")

# Add significance labels
pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")

# Display results
pairwise_tests_corrected %>%
  gt() %>%
  tab_header(
    title = "Pairwise Comparisons Between Age Groups for Each Response Category",
    subtitle = "Bonferroni-adjusted p-values"
  ) %>%
  cols_label(
    Response = "Response Category",
    Age_Group_1 = "Age Group 1",
    Age_Group_2 = "Age Group 2",
    n_Age_Group_1 = "N (Age Group 1)",
    n_Age_Group_2 = "N (Age Group 2)",
    P_value = "Raw P-value",
    Adjusted_P = "Adjusted P-value",
    p.adj.signif = "Significance"
  ) %>%
  fmt_number(
    columns = c(P_value, Adjusted_P),
    decimals = 3
  ) %>%
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

###########################

### CORRECT ONE with % #######

# Create contingency table
tab <- table(df$s2_q10, df$age_2group)

# Function to perform Fisher’s test for each response category
pairwise_fisher_corrected <- function(response) {
  # Extract counts for this response category
  response_counts <- tab[response, ]  # Select the row for this response
  
  # Compute total counts in each age group
  total_counts <- colSums(tab)  
  
  # Compute percentages
  percent_Age_1 <- round((response_counts[1] / total_counts[1]) * 100, 1)
  percent_Age_2 <- round((response_counts[2] / total_counts[2]) * 100, 1)
  
  # Format as "N (X%)"
  formatted_Age_1 <- paste0(response_counts[1], " (", percent_Age_1, "%)")
  formatted_Age_2 <- paste0(response_counts[2], " (", percent_Age_2, "%)")
  
  # Compute "Not this response" (total in each age group - selected response)
  not_response_counts <- total_counts - response_counts
  
  # Create a valid 2x2 contingency table
  valid_2x2 <- matrix(c(response_counts, not_response_counts), nrow = 2, byrow = TRUE)
  rownames(valid_2x2) <- c(response, "Not this response")
  colnames(valid_2x2) <- levels(df$age_2group)
  
  # Perform Fisher’s Exact Test
  test_result <- fisher.test(valid_2x2)
  
  return(data.frame(
    Response = response,
    Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
    Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
    N_Age_Group_1 = formatted_Age_1,
    N_Age_Group_2 = formatted_Age_2,
    P_value = test_result$p.value
  ))
}

# Apply to each response category
response_levels <- levels(df$s2_q10)
pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))

# Apply Bonferroni correction
pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "fdr")

# Add significance labels
pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")

# Display results
pairwise_tests_corrected %>%
  gt() %>%
  tab_header(
    title = "Pairwise Comparisons Between Age Groups for Each Response Category",
    subtitle = "fdr-adjusted p-values"
  ) %>%
  cols_label(
    Response = "Response Category",
    Age_Group_1 = "Age Group 1",
    Age_Group_2 = "Age Group 2",
    N_Age_Group_1 = "N (Age Group 1, %)",
    N_Age_Group_2 = "N (Age Group 2, %)",
    P_value = "Raw P-value",
    Adjusted_P = "Adjusted P-value",
    p.adj.signif = "Significance"
  ) %>%
  fmt_number(
    columns = c(P_value, Adjusted_P),
    decimals = 3
  ) %>%
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


####################################
### LOOPING #####

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
  
  return(results_list)
}


var_labels <- list(
  s2_q10 = "Fentanyl Awareness",
  s4_q23 = "Calling 911",
  age_2group = "Age Groups",
  s3_q14 = "Have vs. Haven't"
)


pairwise_fisher_multiple(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)



###########################################

####Obnibus testing #######

omnibus_tests <- function(df, outcomes, predictors, var_labels) {
  library(dplyr)
  library(nnet)   # For multinom()
  library(knitr)  # For kable()
  
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
      
      # Chi-square test
      chisq_test <- chisq.test(tab)
      
      # Fisher’s Exact Test (in case of small counts)
      fisher_test <- fisher.test(tab)
      
      # **Fix: Use `as.formula()` to avoid `.data` issue**
      formula_str <- paste0(outcome, " ~ ", predictor)
      multi_outcome <- multinom(as.formula(formula_str), data = df_filtered, trace = FALSE)
      multi_null <- multinom(as.formula(paste0(outcome, " ~ 1")), data = df_filtered, trace = FALSE)
      
      # Likelihood Ratio Test
      lrt_test <- tryCatch(
        anova(multi_null, multi_outcome, test = "Chisq"),
        error = function(e) NULL  # Handle errors gracefully
      )
      
      # Extract Likelihood Ratio Test statistic and p-value safely
      lrt_statistic <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
        round(lrt_test$`LR stat.`[2], 2)
      } else {
        NA
      }
      
      lrt_p_value <- if (!is.null(lrt_test) && nrow(lrt_test) > 1) {
        format.pval(lrt_test$`Pr(Chi)`[2], digits = 3, eps = 0.001)
      } else {
        NA
      }
      
      # Create summary table
      results_table <- tibble(
        Test = c("Chi-square Test", "Fisher's Exact Test", "Likelihood Ratio Test"),
        `Test Statistic` = c(
          round(chisq_test$statistic, 2),
          NA,  # Fisher's test does not have a test statistic
          lrt_statistic
        ),
        `p-value` = c(
          format.pval(chisq_test$p.value, digits = 3, eps = 0.001),
          format.pval(fisher_test$p.value, digits = 3, eps = 0.001),
          lrt_p_value
        )
      )
      
      # Generate dynamic table title
      title_text <- paste0("Omnibus Test for differences between ", var_labels[[predictor]], " for ", var_labels[[outcome]])
      
      # **Print title first**
      cat("\n\n", title_text, "\n", strrep("-", nchar(title_text)), "\n")
      
      # **Print table**
      print(knitr::kable(results_table))
      
      # **Store results for Excel export**
      results_list[[paste0(outcome, "_", predictor)]] <- results_table
    }
  }
  
  return(results_list)  
}



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


export_omnibus_results_one_sheet <- function(results_list, file_path = "Omnibus_Tests_OneSheet.xlsx") {
  library(openxlsx)  # Needed for writing Excel files
  library(dplyr)     # For data manipulation
  
  # Convert list of tables into one large dataframe
  combined_results <- bind_rows(
    lapply(names(results_list), function(name) {
      results_list[[name]] %>%
        mutate(Comparison = name) %>%  # Add a column with predictor-outcome name
        relocate(Comparison)  # Move the new column to the front
    }),
    .id = "ID"
  ) %>% select(-ID)  # Remove unnecessary ID column
  
  # Create a workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Omnibus Tests")  # Single sheet
  writeData(wb, "Omnibus Tests", combined_results)  # Write all data to one sheet
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  cat("\nOmnibus test results saved in one sheet to:", file_path, "\n")
}





var_labels <- list(
  s2_q10 = "Fentanyl Awareness",
  s4_q23 = "Calling 911",
  age_2group = "Age Groups",
  s3_q14 = "Have vs. Haven't"
)

omnibus_tests(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)

omnibus_results<- omnibus_tests(df, outcomes = c("s2_q10", "s4_q23"), predictors = c("age_2group", "s3_q14"), var_labels = var_labels)

print(omnibus_results[["s2_q10_age_2group"]]) 


##################


# Create contingency table
tab <- table(df$s2_q10, df$age_2group)
# Function to perform Fisher’s Exact Test for each response category
pairwise_fisher_corrected <- function(response) {
  # Extract counts for this response category
  response_counts <- as.numeric(tab[response, ])  # Convert to numeric vector
  
  # Ensure we have exactly two values (for a 2×2 test)
  if (length(response_counts) != 2) {
    stop("Error: Expected a 2-element vector, but got ", length(response_counts))
  }
  
  # Create a valid 2×2 table (ensuring correct dimensions)
  valid_2x2 <- matrix(response_counts, nrow = 2, byrow = FALSE)
  colnames(valid_2x2) <- levels(df$age_2group)
  
  # Perform Fisher’s Exact Test
  test_result <- fisher.test(valid_2x2)
  
  # Extract counts
  n_Age_1 <- response_counts[1]  # Age 15-17 count for this response
  n_Age_2 <- response_counts[2]  # Age 18-25 count for this response
  
  return(data.frame(
    Response = response,
    Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
    Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
    n_Age_Group_1 = n_Age_1,
    n_Age_Group_2 = n_Age_2,
    P_value = test_result$p.value
  ))
}

# Apply to each response category
response_levels <- levels(df$s2_q10)
pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))

# Apply Bonferroni correction
pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")

# Add significance labels
pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")

# Display results

# Display results
pairwise_tests_corrected %>%
  gt() %>%
  tab_header(
    title = "Pairwise Comparisons Between Age Groups for Each Response Category",
    subtitle = "Bonferroni-adjusted p-values"
  ) %>%
  cols_label(
    Response = "Response Category",
    Age_Group_1 = "Age Group 1",
    Age_Group_2 = "Age Group 2",
    n_Age_Group_1 = "N (Age Group 1)",
    n_Age_Group_2 = "N (Age Group 2)",
    P_value = "Raw P-value",
    Adjusted_P = "Adjusted P-value",
    p.adj.signif = "Significance"
  ) %>%
  fmt_number(
    columns = c(P_value, Adjusted_P),
    decimals = 3
  ) %>%
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





# Load necessary libraries
library(dplyr)
library(gt)

# Create contingency table (r × c)
tab <- table(df$s2_q10, df$age_2group)

# Step 1: Run omnibus Fisher’s Exact Test on full r × c table
omnibus_fisher <- fisher.test(tab)

# Display omnibus test result
print(omnibus_fisher)

# Step 2: If significant, run pairwise 2×2 tests
if (omnibus_fisher$p.value < 0.05) {
  pairwise_fisher_corrected <- function(response) {
    # Extract counts for this response category
    response_counts <- tab[response, , drop = FALSE]  # Force it to remain a matrix
    # Ensure we have a valid 2×2 table
    if (ncol(response_counts) != 2) {
      stop("Error: Extracted table does not have 2 columns as expected.")
    }
    
    # Transpose if needed (sometimes R returns as 1x2 instead of 2x1)
    if (nrow(response_counts) == 1) {
      response_counts <- t(response_counts)
    }
    
    # Perform Fisher’s Exact Test
    test_result <- fisher.test(response_counts)
    
    # Extract counts
    n_Age_1 <- response_counts[1, 1]  # Age 15-17 count for this response
    n_Age_2 <- response_counts[1, 2]  # Age 18-25 count for this response
    
    return(data.frame(
      Response = response,
      Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
      Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
      n_Age_Group_1 = n_Age_1,
      n_Age_Group_2 = n_Age_2,
      P_value = test_result$p.value
    ))
  }
  
  # Apply to each response category
  response_levels <- levels(df$s2_q10)
  pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))
  
  # Apply Bonferroni correction
  pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")
  
  # Add significance labels
  pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")
  
  # Display results
  pairwise_tests_corrected %>%
    gt() %>%
    tab_header(
      title = "Pairwise Comparisons Between Age Groups for Each Response Category",
      subtitle = "Bonferroni-adjusted p-values"
    ) %>%
    cols_label(
      Response = "Response Category",
      Age_Group_1 = "Age Group 1",
      Age_Group_2 = "Age Group 2",
      n_Age_Group_1 = "N (Age Group 1)",
      n_Age_Group_2 = "N (Age Group 2)",
      P_value = "Raw P-value",
      Adjusted_P = "Adjusted P-value",
      p.adj.signif = "Significance"
    ) %>%
    fmt_number(
      columns = c(P_value, Adjusted_P),
      decimals = 3
    ) %>%
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


















# Load necessary libraries
library(dplyr)
library(gt)

# Create contingency table (r × c)
tab <- table(df$s2_q10, df$age_2group)

# Step 1: Fisher’s Exact Test on full r × c table
omnibus_fisher <- fisher.test(tab)

# Display omnibus test result
print(omnibus_fisher)

# Step 2: If the omnibus test is significant, conduct pairwise 2×2 Fisher’s tests
if (omnibus_fisher$p.value < 0.05) {
  pairwise_fisher_corrected <- function(response) {
    # Extract counts for this response category
    response_counts <- tab[response, ]  # Extract row from the table
    
    # Create a valid 2×2 table (only two columns: one per age group)
    valid_2x2 <- matrix(c(response_counts), nrow = 2, byrow = FALSE)
    colnames(valid_2x2) <- levels(df$age_2group)
    
    # Perform Fisher’s Exact Test
    test_result <- fisher.test(valid_2x2)
    
    # Extract counts
    n_Age_1 <- response_counts[1]  # Age 15-17 count for this response
    n_Age_2 <- response_counts[2]  # Age 18-25 count for this response
    
    return(data.frame(
      Response = response,
      Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
      Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
      n_Age_Group_1 = n_Age_1,
      n_Age_Group_2 = n_Age_2,
      P_value = test_result$p.value
    ))
  }
  
  # Apply to each response category
  response_levels <- levels(df$s2_q10)
  pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))
  
  # Apply Bonferroni correction
  pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")
  
  # Add significance labels
  pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")
  
  # Display results
  pairwise_tests_corrected %>%
    gt() %>%
    tab_header(
      title = "Pairwise Comparisons Between Age Groups for Each Response Category",
      subtitle = "Bonferroni-adjusted p-values"
    ) %>%
    cols_label(
      Response = "Response Category",
      Age_Group_1 = "Age Group 1",
      Age_Group_2 = "Age Group 2",
      n_Age_Group_1 = "N (Age Group 1)",
      n_Age_Group_2 = "N (Age Group 2)",
      P_value = "Raw P-value",
      Adjusted_P = "Adjusted P-value",
      p.adj.signif = "Significance"
    ) %>%
    fmt_number(
      columns = c(P_value, Adjusted_P),
      decimals = 3
    ) %>%
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



# Load necessary libraries
library(dplyr)
library(gt)

# Create contingency table (r × c)
tab <- table(df$s2_q10, df$age_2group)

# Step 1: Omnibus Fisher’s Exact Test on full r × c table
omnibus_fisher <- fisher.test(tab)

# Display omnibus test result
print(omnibus_fisher)

# Step 2: If the omnibus test is significant, conduct pairwise 2×2 Fisher’s tests
if (omnibus_fisher$p.value < 0.05) {
  pairwise_fisher_corrected <- function(response) {
    # Extract counts for this response category
    response_counts <- as.numeric(tab[response, ])  # Convert row to numeric vector
    
    # Ensure we have exactly two values (for a 2×2 test)
    if (length(response_counts) != 2) {
      stop("Error: Expected a 2-element vector, but got ", length(response_counts))
    }
    
    # Create a valid 2×2 contingency table
    valid_2x2 <- matrix(response_counts, nrow = 2, byrow = FALSE)
    colnames(valid_2x2) <- levels(df$age_2group)
    rownames(valid_2x2) <- c("Selected", "Not Selected")  # Generic row names
    
    # Perform Fisher’s Exact Test
    test_result <- fisher.test(valid_2x2)
    
    # Extract counts
    n_Age_1 <- response_counts[1]  # Age 15-17 count for this response
    n_Age_2 <- response_counts[2]  # Age 18-25 count for this response
    
    return(data.frame(
      Response = response,
      Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
      Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
      n_Age_Group_1 = n_Age_1,
      n_Age_Group_2 = n_Age_2,
      P_value = test_result$p.value
    ))
  }
  
  # Apply to each response category
  response_levels <- levels(df$s2_q10)
  pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))
  
  # Apply Bonferroni correction
  pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")
  
  # Add significance labels
  pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")
  
  # Display results
  pairwise_tests_corrected %>%
    gt() %>%
    tab_header(
      title = "Pairwise Comparisons Between Age Groups for Each Response Category",
      subtitle = "Bonferroni-adjusted p-values"
    ) %>%
    cols_label(
      Response = "Response Category",
      Age_Group_1 = "Age Group 1",
      Age_Group_2 = "Age Group 2",
      n_Age_Group_1 = "N (Age Group 1)",
      n_Age_Group_2 = "N (Age Group 2)",
      P_value = "Raw P-value",
      Adjusted_P = "Adjusted P-value",
      p.adj.signif = "Significance"
    ) %>%
    fmt_number(
      columns = c(P_value, Adjusted_P),
      decimals = 3
    ) %>%
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



# Load necessary libraries
library(dplyr)
library(gt)

# Create contingency table (r × c)
tab <- table(df$s2_q10, df$age_2group)

# Step 1: Run omnibus Fisher’s Exact Test on full r × c table
omnibus_fisher <- fisher.test(tab)

# Display omnibus test result
print(omnibus_fisher)

# Step 2: If significant, run pairwise 2×2 tests
if (omnibus_fisher$p.value < 0.05) {
  pairwise_fisher_corrected <- function(response) {
    # Extract counts for this response category
    response_counts <- tab[response, , drop = FALSE]  # Keep as table
    
    # Convert to a proper 2×2 matrix
    response_counts <- as.matrix(response_counts)
    browser()
    # Ensure it has correct dimensions for Fisher’s Exact Test
    if (!all(dim(response_counts) == c(1, 2))) {
      stop("Error: Extracted table does not have 1 row and 2 columns as expected.")
    }
    
    # Perform Fisher’s Exact Test
    test_result <- fisher.test(response_counts)
    
    # Extract counts
    n_Age_1 <- response_counts[1, 1]  # Age 15-17 count for this response
    n_Age_2 <- response_counts[1, 2]  # Age 18-25 count for this response
    
    return(data.frame(
      Response = response,
      Age_Group_1 = levels(df$age_2group)[1],  # Age 15-17
      Age_Group_2 = levels(df$age_2group)[2],  # Age 18-25
      n_Age_Group_1 = n_Age_1,
      n_Age_Group_2 = n_Age_2,
      P_value = test_result$p.value
    ))
  }
  
  # Apply to each response category
  response_levels <- levels(df$s2_q10)
  pairwise_tests_corrected <- do.call(rbind, lapply(response_levels, pairwise_fisher_corrected))
  
  # Apply Bonferroni correction
  pairwise_tests_corrected$Adjusted_P <- p.adjust(pairwise_tests_corrected$P_value, method = "bonferroni")
  
  # Add significance labels
  pairwise_tests_corrected$p.adj.signif <- ifelse(pairwise_tests_corrected$Adjusted_P < 0.05, "*", "ns")
  
  # Display results
  pairwise_tests_corrected %>%
    gt() %>%
    tab_header(
      title = "Pairwise Comparisons Between Age Groups for Each Response Category",
      subtitle = "Bonferroni-adjusted p-values"
    ) %>%
    cols_label(
      Response = "Response Category",
      Age_Group_1 = "Age Group 1",
      Age_Group_2 = "Age Group 2",
      n_Age_Group_1 = "N (Age Group 1)",
      n_Age_Group_2 = "N (Age Group 2)",
      P_value = "Raw P-value",
      Adjusted_P = "Adjusted P-value",
      p.adj.signif = "Significance"
    ) %>%
    fmt_number(
      columns = c(P_value, Adjusted_P),
      decimals = 3
    ) %>%
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

