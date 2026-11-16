#!/usr/bin/env Rscript 

# Accept dynamic keyword from command-line arguments 
args <- commandArgs(trailingOnly = TRUE) 
if (length(args) >= 1 && !is.na(args[1]) && nchar(trimws(args[1])) > 0) { 
  target_keyword <- tolower(args[1])  
} else { 
  target_keyword <- "africa" 
} 

# Identify the input file name from Hadoop environment variable 
input_file <- Sys.getenv("mapreduce_map_input_file") 
if (input_file == "") { 
  input_file <- Sys.getenv("map_input_file") 
  if (input_file == "") { 
    input_file <- "doc.txt" 
  } 
} 

doc_name <- basename(input_file) 

# Open connection to standard input (stdin) 
con <- file("stdin", open = "r") 
line_num <- 0 

while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) { 
  line_num <- line_num + 1 
  
  clean_line <- tolower(line) 
  clean_line <- gsub("[^a-z0-9\\s]", " ", clean_line) 
  clean_line <- gsub("\\s+", " ", clean_line) 
  clean_line <- trimws(clean_line) 
  
  if (nchar(clean_line) == 0) next 
  
  # Split line into individual tokens (words) to determine column position 
  words <- unlist(strsplit(clean_line, " ")) 
  words <- words[!is.na(words) & words != ""]
  
  # Scan words for keyword matches 
  for (col_idx in seq_along(words)) { 
    token <- words[col_idx]
    if (!is.na(token) && token == target_keyword) { 
      # Emit tab-delimited Key-Value Pair: 
      # Key t Value (doc_name, row, col) 
      cat(sprintf("%s\t%s\t%d\t%d\n", target_keyword, doc_name, line_num, col_idx)) 
    } 
  }
} 

close(con)