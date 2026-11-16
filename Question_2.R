
# install.packages(c("tm", "topicmodels", "stringr", "stopwords", "wordcloud", "RColorBrewer", "ggplot2")) 
library(tm) 
library(topicmodels) 
library(stringr) 
library(stopwords) 
library(wordcloud) 
library(RColorBrewer) 
library(ggplot2)

# 2.1.

# Preprocessing function
filetext_prep <- function(file_name){
  file <- file(file_name,"r")
  text <- readLines(file)
  close(file)
  text_rep <- c()
  n_line <- length(text)
  for (i in 1:n_line) {
    text_line <- tolower(text[i]) # Lower case
    
    text_line <- stringr::str_replace_all(text_line, "[^a-zA-Z\\s]"," ")
    text_line <- stringr::str_replace_all(text_line, "[\\s]+", " ")
    
    text_line <-trimws(text_line, which = c("both")) 
    if (nchar(text_line) > 0) {
      text_rep <- paste(text_rep, text_line, sep = " ")
    }
  }
  
  text_rep <- trimws(text_rep, which = c("both")) 
  word_list <- stringr::str_split(text_rep, " ")[[1]]
  stw <- stopwords("en")
  n_sw <- length(stw)
  
  # Remove Stop words
  words <- unlist(strsplit(word_list, "\\s+"))
  word_list <- words[!words %in% stw]
  
  bag_of_words <- unique(word_list)
  res <- list("bag" = bag_of_words, "words" = word_list)
}


# File handling
prefix <- "sa_news\\"
suffix <- c("news_1.txt", "news_2.txt", "news_3.txt", "news_4.txt", "news_5.txt",
            "news_6.txt", "news_7.txt", "news_8.txt", "news_9.txt", "news_10.txt")

NDoc <- length(suffix)
text_list <- c()
file_list <- c()


for (i in 1:NDoc){
  file_name <- paste(prefix, suffix[i], sep = "")
  DocWords <- filetext_prep(file_name)
  text_line <- paste(DocWords$words, collapse=" ")
  text_list <- append(text_list, text_line)
  file_list <- append(file_list, suffix[i])
  print(DocWords)
}


# Create a data frame 
doc_frame <- data.frame(file_name = file_list, content = text_list, stringsAsFactors = FALSE)


# 2.2.

# Generate Document Term Matrix
corpus <- Corpus(VectorSource(doc_frame$content))
dtm <- DocumentTermMatrix(corpus)


# Perform LDA
num_topics = 5
top_k = 10

lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
terms(lda_model,top_k)
topic_probabilities <- as.data.frame(posterior(lda_model)$terms)

topic_dfs <- list()
terms <- terms(lda_model, top_k)

for (i in 1:num_topics) {
  topic_df <- data.frame(term = terms[, i], probability = as.numeric(topic_probabilities[i, terms[, i]]))
  topic_df <- topic_df[order(topic_df$probability, decreasing = TRUE), ]  # Sort by probability in descending order
  topic_dfs[[paste("Topic", i)]] <- topic_df
}


# Display word distributions for each topic 
for (t_name in names(topic_dfs)) { 
  cat("\n=== Distribution for", t_name, "===\n") 
  print(topic_dfs[[t_name]]) 
}


# 2.3.

#Visualistaions
for(i in 1:num_topics){
  topic <- topic_dfs[[i]]
  
  # Word Cloud
  cat(paste("Word Cloud for Topic", i), ":\n")
  wordcloud(words = topic$term, 
            freq = topic$probability * 1000, #Scale frequencies
            scale = c(1, 0.5),
            min.freq = 1, 
            max.words = top_k, 
            random.order = FALSE,
            rot.per = 0.35, 
            colors = brewer.pal(8, "Dark2")
  )
  title(main = paste("Word Cloud - Topic", i))
  
  # Bar Plot
  cat(paste("Bar Plot for Topic", i), ":\n")
  
  p <- ggplot(topic, aes(x = reorder(term, -probability), y = probability)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(
      title = paste("Top Terms in Topic", i), 
      x = "Term", 
      y = "Probability") +
    theme_minimal()
  print(p)
}
