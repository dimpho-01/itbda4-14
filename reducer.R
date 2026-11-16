#!/usr/bin/env Rscript

con <- file("stdin", open = "r") 
current_keyword <- NULL 
match_count <- 0 
locations <- list()

cat(sprintf("%-12s | %-15s | %-10s | %-10s\n", "KEYWORD", "DOCUMENT", "ROW (LINE)", "COL (WORD)"))

while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) { 
  line <- trimws(line) 
  if (nchar(line) == 0) next 
  
  parts <- unlist(strsplit(line, "\t")) 
  if (length(parts) < 4) next 
  
  keyword <- parts[13] 
  doc_name <- parts[14] 
  row_num <- as.integer(parts[15]) 
  col_num <- as.integer(parts[16]) 
  
  match_count <- match_count + 1 
  cat(sprintf("%-12s | %-15s | Row %-6d | Col %-6d\n", keyword, doc_name, row_num, col_num)) 
} 

close(con)

cat(sprintf("Total occurrences found: %d\n", match_count))
