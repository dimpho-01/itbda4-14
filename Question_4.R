library(rvest) 
library(httr) 
library(stringr) 

# 4.1.

# Ethical scraping: provide an identifying User-Agent 
custom_ua <- "EduvosDataAnalystETL/1.0 (academic_poc@eduvos.ac.za)" 

# Target: Wikipedia's Timeline of Artificial Intelligence 
url <- "https://en.wikipedia.org/wiki/Timeline_of_artificial_intelligence" 
response <- GET(url, user_agent(custom_ua), timeout(15)) 
web_page <- read_html(response) 

# Extract bullet-point entries from chronological history sections (<li> tags) 
raw_li_nodes <- web_page %>% 
  html_elements("div#mw-content-text ul li") %>% 
  html_text2() 

# Extract at least 20 records
raw_entries <- raw_li_nodes[nchar(raw_li_nodes) > 60] 
raw_entries <- head(raw_entries, 25) 

# Initialize vectors to build noisy, real-world representative data frame 
n_records <- length(raw_entries) 
entry_ids <- 1:n_records 
titles <- character(n_records) 
text_bodies <- character(n_records) 
dates <- character(n_records) 
engagement_raw <- character(n_records)

set.seed(42) 

for (i in seq_along(raw_entries)) { 
  full_entry <- raw_entries[i] 
  
  # Separate title and body using colon or dash, preserving raw web artifacts 
  parts <- unlist(strsplit(full_entry, ":|–|—")) 
  if (length(parts) > 1) { 
    titles[i] <- parts[1] 
    # Intentionally preserve citations [1], [edit], and tabs to represent messy data 
    text_bodies[i] <- paste(parts[-1], collapse = " : ") 
  } else { 
    titles[i] <- paste("Milestone Entry", i) 
    text_bodies[i] <- full_entry 
  } 
  
  # Extract or synthesize realistic messy date strings using regex 
  year_match <- stringr::str_extract(full_entry, "\\b(19|20)\\d{2}\\b") 
  if (!is.na(year_match)) { 
    dates[i] <- paste0(year_match, "-", sample(c("01", "06", "11", "XX"), 1), "-15") 
  } else { 
    dates[i] <- NA # Missing date
  } 
  
  # Generate an inferred numerical engagement metric with real-world flaws: 
  # Contains currency symbols, commas or completely missing entries (NA) 
  rand_val <- sample(1:100, 1) 
  if (rand_val > 85) { 
    engagement_raw[i] <- NA # Missing value 
  } else if (rand_val %% 3 == 0) { 
    engagement_raw[i] <- paste0(sample(100:999, 1), " pts") # Spurious unit text 
  } else { 
    engagement_raw[i] <- as.character(sample(100:999, 1)) 
  } 
} 

# Infer raw word count as a numerical attribute 
raw_word_count <- sapply(strsplit(text_bodies, "\\s+"), length) 

# Construct raw data frame containing spurious and inconsistent information 
raw_df <- data.frame( 
  entry_id = entry_ids, 
  raw_title = titles, 
  raw_text = text_bodies, 
  raw_date = dates, 
  word_count = raw_word_count, 
  engagement_score = engagement_raw, 
  stringsAsFactors = FALSE 
) 

cat("--- RAW EXTRACTED DATA FRAME (First 3 Rows) ---\n") 

print(head(raw_df, 3))


# 4.2. 
# Clean Data and Export CSV
cleaned_df <- raw_df 

# Text Denoising using Regular Expressions 
cleaned_df$clean_title <- cleaned_df$raw_title %>% 
  stringr::str_replace_all("\\[.*?\\]", "") %>% # Remove Wikipedia citation brackets 
  stringr::str_replace_all("[^a-zA-Z0-9\\s]", " ") %>% # Remove special symbols 
  stringr::str_replace_all("\\s+", " ") %>% # Collapse excess whitespace 
  trimws() 

cleaned_df$clean_text <- cleaned_df$raw_text %>% 
  stringr::str_replace_all("\\[.*?\\]", "") %>% # Remove [1], [citation needed] 
  stringr::str_replace_all("[\\r\\n\\t]+", " ") %>% # Remove control characters 
  stringr::str_replace_all("[^a-zA-Z0-9.,;?\\s]", "") %>%# Retain sentence punctuation, remove noise 
  stringr::str_replace_all("\\s+", " ") %>% 
  trimws() 

# Recalculate clean word count post-denoising 
cleaned_df$clean_word_count <- sapply(strsplit(cleaned_df$clean_text, "\\s+"), length) 

# Clean and Impute Inconsistent Dates 
cleaned_df$clean_date <- stringr::str_replace(cleaned_df$raw_date, "-XX-", "-01-") 
# Replace any lingering NAs with standard default anchor 
cleaned_df$clean_date[is.na(cleaned_df$clean_date)] <- "2000-01-01" 

# Clean and Impute the Numerical Attribute (engagement_score) 
cleaned_df$engagement_numeric <- as.numeric(stringr::str_extract(cleaned_df$engagement_score, "\\d+")) 

# Impute missing values (NA) with the median 
median_engagement <- median(cleaned_df$engagement_numeric, na.rm = TRUE) 
cleaned_df$engagement_imputed <- ifelse( 
  is.na(cleaned_df$engagement_numeric), 
  median_engagement, 
  cleaned_df$engagement_numeric 
) 

# Assemble Final Structured Schema 
final_dataset <- data.frame( 
  record_id = cleaned_df$entry_id, 
  title = cleaned_df$clean_title, 
  article_text = cleaned_df$clean_text, 
  publication_date = as.Date(cleaned_df$clean_date), 
  word_count = as.integer(cleaned_df$clean_word_count), 
  engagement_score = as.integer(cleaned_df$engagement_imputed), 
  stringsAsFactors = FALSE ) 

# Export to CSV 
output_csv_path <- "cleaned_web_data.csv" 
write.csv(final_dataset, file = output_csv_path, row.names = FALSE) 
cat("\nCleaned dataset exported successfully to:", output_csv_path, "\n") 

cat("--- CLEANED DATA PREVIEW ---\n") 
print(head(final_dataset, 3))