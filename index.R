#loading the required packages in R session
require(tm)
require(SnowballC)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)
require(ggplot2)
require(proxy)

setwd(getwd())

#setting up file paths
data_file <- "input/plot_summaries.txt"
vector_file <- "plot_summary.RData"
corpus_file <- "summary_corpus.RData"
datraframe_file <- "summary_df.RData"

#reading the file and saving the data into local to save time
getDataFromFile <- function(file_name) {
  if(!file.exists(vector_file)) {
    data <- read.delim(file_name, header = FALSE, sep = "\t", quote = "") 
    data$V1 <- as.integer(data$V1)
    data$V2 <- as.character(data$V2)
    saveRDS(data, vector_file)
    print("Saved.")
  }
  else {
    data <- readRDS(vector_file)
  }
  return(data)
}

#getting clean corpus from the documents given (summary plots)
getCorpusFromDocument <- function(documents) {
  if(!file.exists(corpus_file)) {
    corpus <- VCorpus(VectorSource(documents)) %>%
      tm_map(stemDocument) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace)
    saveRDS(corpus, corpus_file)
    print("Saved.")
  }
  else {
    corpus <- readRDS(corpus_file)
  }
  return(corpus)
}

#getting the Dataframe from the DTM generated from the corpus
getDFfromCorpus <- function(corpus) {
  if(!file.exists(dataframe_file)) {
    document_dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) 
      weightTfIdf(x),
      stopwords = TcorRUE,
      wordLengths = c(1, Inf)
    )
    )
    document_df <- tidy(document_dtm) %>%
      document_df <- bind_tf_idf(term = term, document = document, n = count)
    document_df$term <- gsub("[^[:alpha:] ]", "", document_df$term) %>%
      document_df$document <- as.integer(document_df$document)
    saveRDS(document_df, dataframe_file)
    print("Saved!")
  }
  else {
    document_df <- readRDS(dataframe_file)
  }
  return(document_df)
}

singleword_query <- function (word) {
  top_10_results <- plot_tdidf %>%
    filter(str_detect(plot_tdidf$term, word)) %>%
    arrange(desc(tf_idf)) %>%
    select(document, term, tf_idf) %>%
    top_n(10, tf_idf)
  
  temp <-
    left_join(top_10_results, plot_summary, by = c("document" = "V1")) %>%
    select (document, V2)
}

multiword_query <- function(words) {
  
}

#main program starts
print("Welcome to Recommendation Search Engine!")
query <- "q"
while (query != "q") {
  query <- readline("Enter your query : ")
  x <- unlist(strsplit(query, " "))
  if (length(x) == 1) {
    results <- singleword_query(x)
  }
  else{
    results <- multiword_query(paste(x, collapse = " "))
  }
}

temp <- getDataFromFile(data_file)
corpus <- getCorpusFromDocument(temp$V2)
dtm <- getDFfromCorpus(corpus)