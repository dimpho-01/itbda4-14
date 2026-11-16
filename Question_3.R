 
# install.packages(c("rvest", "httr", "stringr", "tm", "stopwords", "topicmodels", "ggplot2", "wordcloud", "RColorBrewer")) 
library(rvest) 
library(httr) 
library(stringr) 
library(tm) 
library(stopwords) 
library(topicmodels) 
library(ggplot2) 
library(wordcloud) 
library(RColorBrewer)

# 3.1. 

# Define target URL 
target_url <- "https://en.wikipedia.org/wiki/Natural_language_processing" 

# Ethical Scraping with User-Agent Header
custom_user_agent <- "AcademicResearchBot/1.0 (student_nlp_assignment@eduvos.ac.za)" 

# Fetch the HTML document 
response <- httr::GET( 
  url = target_url, 
  user_agent(custom_user_agent), 
  timeout(15) 
) 

# Parse HTML content 
web_page <- read_html(response) 

# Extract Paragraph Text (<p> tags) 
# Extract all paragraph nodes within the main body content
paragraphs <- web_page %>% 
  html_elements("div#mw-content-text p") %>% 
  html_text2() 

# Filter out empty or whitespace-only paragraphs 
paragraphs <- paragraphs[nchar(trimws(paragraphs)) > 30] 

# Create R Data Structure
web_df <- data.frame( 
  paragraph_id = seq_along(paragraphs), 
  source_url = target_url, 
  raw_content = paragraphs, 
  stringsAsFactors = FALSE 
) 

# Inspect raw extracted structure 
cat("Successfully extracted", nrow(web_df), "paragraphs.\n") 
head(web_df, 3)


# 3.2. 

preprocess_text <- function(raw_text_vec) {
  
  # Remove Wikipedia-style citation tags like [9], [12], etc. 
  cleaned <- raw_text_vec
  
  # Convert to lowercase 
  cleaned <- tolower(cleaned) 
  
  # Remove non-alphabetical characters (punctuation, numbers, special symbols) 
  cleaned <- stringr::str_replace_all(cleaned, "[^a-zA-Z\\s]", " ") 
  
  # Collapse multiple spaces and trim 
  cleaned <- stringr::str_replace_all(cleaned, "[\\s]+", " ") 
  cleaned <- trimws(cleaned, which = "both") 
  
  # Tokenize 
  tokens <- unlist(stringr::str_split(cleaned, "\\s+")) 
  tokens <- tokens[tokens != ""] 
  
  # Stop word removal 
  base_stopwords <- stopwords::stopwords("en") 
  domain_stopwords <- c("also", "may", "can", "used", "use", "one", "two", "many", "often", "e", "g", "retrieved", "archived", "isbn", "doi", "et", "al", "page", "wikipedia") 
  all_stopwords <- unique(c(base_stopwords, domain_stopwords)) 
  
  filtered_tokens <- tokens[!tokens %in% all_stopwords & nchar(tokens) >1] 
  
  # Return denoised text as a single string 
  return(paste(filtered_tokens, collapse = " ")) 
  
} 


web_df$clean_content <- sapply(web_df$raw_content, preprocess_text) 
web_df <- web_df[nchar(web_df$clean_content) > 0, ] 
cat("Data successfully denoised. Total usable paragraphs:", nrow(web_df), "\n")


# 3.3. 

# ----------------------- Summarisation Technique 1 ----------------------------
# Frequency-Based Extractive Summarisation (TF Sentence Scoring)

# Compute Term Frequencies across the Corpus
corpus_all_text <- paste(web_df$clean_content, collapse = " ") 
words_vec <- unlist(strsplit(corpus_all_text, " ")) 
word_freq_table <- table(words_vec) 

# Sort words by frequency 
term_freq_df <- data.frame( 
  term = names(word_freq_table), 
  freq = as.numeric(word_freq_table), 
  stringsAsFactors = FALSE 
) 

term_freq_df <- term_freq_df[order(-term_freq_df$freq), ] 
top_terms <- head(term_freq_df, 15) 

# Sentence-Level Extractive Scoring 
# Split the raw content into individual sentences 
all_raw_text <- paste(web_df$raw_content, collapse = " ") 
raw_sentences <- unlist(stringr::str_split(all_raw_text, "(?<=[.?!])\\s+")) 
raw_sentences <- trimws(raw_sentences) 
raw_sentences <- raw_sentences[nchar(raw_sentences) > 40 & nchar(raw_sentences) < 250] 

# Score sentences based on the presence of top terms 
sentence_scores <- numeric(length(raw_sentences)) 
for (i in seq_along(raw_sentences)) { 
  s_lower <- tolower(raw_sentences[i])
  
  # Count occurrences of top 15 keywords 
  score <- 0 
  for (j in 1:nrow(top_terms)) { 
    if (grepl(paste0("\\b", top_terms$term[j], "\\b"), s_lower)) { 
      score <- score + top_terms$freq[j] 
    } 
  } 
  sentence_scores[i] <- score / (nchar(s_lower)^0.5) # Penalise excessive sentence length 
} 

# Extract Top 3 Key Sentences for the Extractive Summary 
ranked_sentences <- data.frame( 
  sentence = raw_sentences, 
  score = sentence_scores, 
  stringsAsFactors = FALSE 
) 

ranked_sentences <- ranked_sentences[order(-ranked_sentences$score), ] 

cat(" TECHNIQUE 1: EXTRACTIVE KEY-SENTENCE SUMMARY \n")
top_k = 3
top_summary_sentences <- head(ranked_sentences$sentence, top_k) 
for (k in seq_along(top_summary_sentences)) { 
  cat(paste0("[Key Finding ", k, "]: ", top_summary_sentences[k], "\n\n")) 
} 


# Visualisation 1: Top 15 Informative Keywords
plot_tf <- ggplot(top_terms, aes(x = reorder(term, freq), y = freq)) + 
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + 
  geom_text(aes(label = freq), hjust = -0.2, size = 3.5, color = "black") + 
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs( 
    title = "Extractive Summarisation: Top 15 Informative Keywords", 
    x = "Keywords / Tokens", 
    y = "Absolute Term Frequency" 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(face = "bold", size = 13), 
    axis.text = element_text(size = 10) 
  ) 

print(plot_tf)


# ----------------------- Summarisation Technique 2 ----------------------------
# Thematic Summarisation using Latent Dirichlet Allocation (LDA)

# Construct Document-Term Matrix
corpus_obj <- Corpus(VectorSource(web_df$clean_content))
dtm_obj <- DocumentTermMatrix(corpus_obj) 

# Remove zero-entry rows if any 
dtm_clean <- dtm_obj[apply(dtm_obj, 1, sum) > 0, ] 

# Fit LDA Model for n = 3 Topics 
num_topics <- 3 
lda_web <- LDA(dtm_clean, k = num_topics, control = list(seed = 42)) 

# Top k words per topic
top_k = 5
lda_top_terms <- terms(lda_web, top_k) 

cat(" TECHNIQUE 2: LDA THEMATIC TOPIC SUMMARY \n") 
for (t in 1:num_topics) { 
  cat(paste0("Topic ", t, " Theme: ", paste(lda_top_terms[, t], collapse = ", "), "\n")) 
} 


# Visualisation 2: Thematic Word Cloud

lda_posterior <- posterior(lda_web)$terms 
topic_summary_terms <- unique(as.vector(lda_top_terms)) 
cloud_weights <- colSums(lda_posterior[, topic_summary_terms, drop = FALSE]) * 1000 

par(mar = c(2, 2, 3, 2)) 
wordcloud( 
  words = names(cloud_weights), 
  freq = cloud_weights, 
  scale = c(2, 0.5), 
  min.freq = 1, 
  max.words = 25, 
  random.order = FALSE, 
  rot.per = 0.25, 
  colors = brewer.pal(8, "Dark2") 
) 
title( 
  main = "Thematic Word Cloud: Latent Topics from Scraped NLP Article", 
  cex.main = 1.1, 
  font.main = 2 
) 


