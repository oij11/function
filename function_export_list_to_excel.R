
# Export list (in general) to excel

export_list_to_excel <- function(table_list, title_list = NULL, 
                                 file_name = "descriptive_tables_in_one_sheet.xlsx",
                                 sheet_name = "AllTables"){
  # Error proof messages
  # Load necessary packages (if not already loaded)
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it.")
  }
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required. Please install it.")
  }
  
  # Check that table_list is a non-empty list.
  if (!is.list(table_list) || length(table_list) == 0) {
    stop("table_list must be a non-empty list.")
  }
  
  # Create a new workbook and add a worksheet.
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name)
  
  current_row<- 1
  
  # Looping over each list/table
  for (i in seq_along(table_list)){
    table_name <- names(table_list)[[i]]
    table_data <- table_list[[i]]
    
    if(!is.null(title_list) && table_name %in% names(title_list)){
      title_text<- title_list[[table_name]]
    }
    else {
      title_text <- table_name
    }
    
    # Write the title as a data.frame (one cell) at the current row.
    openxlsx::writeData(wb, 
                        sheet = sheet_name,
                        x = data.frame(Title = title_text),
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    current_row <- current_row + 2  # Leave one blank row after the title.
    
    # Write data (summary table)
    openxlsx::writeData(wb, 
                        sheet = sheet_name,
                        x = table_data,
                        startRow = current_row,
                        startCol = 1,
                        rowNames = FALSE)
    # For the next table, move current_row below the written table (plus an extra blank row).
    current_row <- current_row + nrow(table_data) + 2
  }
  # Build the full file path in the "excel_export" folder.
  file_path <- here::here("excel_export", file_name)
  
  # Create the "excel_export" directory if it doesn't exist.
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  # Save the workbook to the specified file path.
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
  
  
  
}



export_list_to_excel_multiplesheet <- function(table_list, title_list = NULL, 
                                 file_name = "descriptive_tables_in_seperate_sheet.xlsx")
  {
  # Error proof messages
  # Load necessary packages (if not already loaded)
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it.")
  }
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required. Please install it.")
  }
  
  # Check that table_list is a non-empty list.
  if (!is.list(table_list) || length(table_list) == 0) {
    stop("table_list must be a non-empty list.")
  }
  
  # Create a new workbook and add a worksheet.
  wb <- openxlsx::createWorkbook()
  
  
  # Looping over each list/table
  for (i in seq_along(table_list)){
    table_name <- names(table_list)[[i]]
    table_data <- table_list[[i]]
    
    if(!is.null(title_list) && table_name %in% names(title_list)){
      title_text<- title_list[[table_name]]
    }
    else {
      title_text <- table_name
    }
    
    
    # Create a worksheet name (shorten to 31 chars if needed)
    sheet_name  <- substr(table_name, 1, 31)
    
    addWorksheet(wb, sheetName = sheet_name)
    
    # (1) Write the title in row 1
    writeData(wb, 
              sheet = sheet_name, 
              x     = data.frame(Title = title_text),
              startRow = 1, startCol = 1,
              colNames = FALSE)
    
    # (2) Write the table below the title 
    writeData(wb, 
              sheet    = sheet_name,
              x        = table_data,
              startRow = 3, 
              startCol = 1,
              rowNames = FALSE)
  }
    

  # Build the full file path in the "excel_export" folder.
  file_path <- here::here("excel_export", file_name)
  
  # Create the "excel_export" directory if it doesn't exist.
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  # Save the workbook to the specified file path.
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
  
}


# Usage Example ####

# Case 1. 

# 1. Tables in ascending order
# table_list <- list(
#   "q11_summary"     = q11_summary,
#   "q12_summary"     = q12_summary,
#   "q14_summary"     = q14_summary,
#   "q15_summary"     = q15_summary)
# 
# 
# # 2. Parallel list (or named vector) of titles keyed by the same names
# title_list <- c(
# "q15_summary"   = "Experience with non-prescribed pills, per medtype (q15).",
# "q14_summary"     = "Depending on what? (q14a)",
# "q11_summary"   = "Fentanyl information source (q11).",
# "q12_summary"   = "Fentanyl information trusted source (q12).")
# 
# 
# export_list_to_excel(table_list = table_list, 
#                      title_list = title_list, 
#                      file_name = "Full_sample_descriptive_stats.xlsx")



# Case 2. var and var_labels need to be matched


# summary_list <- process_all_with_full_sample(vars, var_labels)
# 
# 
# # Create a list of tables by subsetting summary_list using vars (in case to ensure they have matched order and names...)
# table_list <- lapply(vars, function(v) summary_list[[v]])
# names(table_list) <- vars 
# 
# # Create a named list (or vector) of titles where the names match the table_list keys.
# title_list <- setNames(var_labels, vars)
# 
# 
# export_list_to_excel(table_list, title_list, file_name = "descriptive_single_choice.xlsx")
# 
