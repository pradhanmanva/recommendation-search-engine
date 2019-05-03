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
vector_file <- "summary.RData"
corpus_file <- "summary_corpus.RData"
dtm_file <- "summary_dtm.RData"
dataframe_file <- "summary_df.RData"

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

#generating DTM from the given corpus with TD-IDF weighting
getDTMFromCorpus <- function(corpus) {
  if(!file.exists(dtm_file)) {
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) 
      weightTfIdf(x),
      stopwords = TRUE
    )
    )
    saveRDS(dtm, dtm_file)
    print("Saved!")
  }
  else {
    dtm <- readRDS(dtm_file)
  }
  return(dtm)
}

#getting the Dataframe from the DTM
getDFfromDTM <- function(dtm) {
  if(!file.exists(dataframe_file)) {
    df <- tidy(dtm)
    df <- bind_tf_idf(df, term = term, document = document, n = count)
    df$term <- gsub("[^[:alpha:] ]", "", df$term)
    df$document <- as.integer(df$document)
    saveRDS(df, dataframe_file)
    print("Saved!")
  }
  else {
    df <- readRDS(dataframe_file)
  }
  return(df)
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
dtm <- getDTMFromCorpus(corpus)
df <- getDFfromDTM(dtm)
