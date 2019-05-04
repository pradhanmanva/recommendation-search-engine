#loading the required packages in R session
require(tm)
require(SnowballC)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)
require(ggplot2)

setwd(getwd())

#setting up file paths
data_file <- "input/plot_summaries.txt"

#reading the file and saving the data into local to save time
getDataFromFile <- function(file_name) {
    data <- read.delim(file_name, header = FALSE, sep = "\t", quote = "") 
    data$V1 <- as.integer(data$V1)
    data$V2 <- as.character(data$V2)
  return(data)
}

#getting clean corpus from the documents given (summary plots)
getCorpusFromDocument <- function(documents) {
  
    corpus <- VCorpus(VectorSource(documents)) %>%
      tm_map(stemDocument) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace)
  return(corpus)
}

#generating DTM from the given corpus with TD-IDF weighting
getDTMFromCorpus <- function(corpus) {
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) 
      weightTfIdf(x),
      stopwords = TRUE
    )
    )
  return(dtm)
}

getTDMFromCorpus <- function(corpus) {
    tdm <- TermDocumentMatrix(corpus, control = list(weighting = function(x) 
      weightSMART(x,spec="ltc"),
      wordLengths=c(1,Inf)))
  return(tdm)
}

#getting the Dataframe from the DTM
getDFfromDTM <- function(dtm) {
  df <- tidy(dtm)
  df <- bind_tf_idf(df, term = term, document = document, n = count)
  df$term <- gsub("[^[:alpha:] ]", "", df$term)
  df$document <- as.integer(df$document)
  return(df)
}

singleword_query <- function (word) {
  word <- tolower(word)
  top_10_results <- df %>%
    filter(str_detect(df$term, word)) %>%
    arrange(desc(tf_idf)) %>%
    select(document, term, tf_idf) %>%
    top_n(10, tf_idf)
  
  temp <- left_join(top_10_results, data, by = c("document" = "V1")) %>%
    select (document)
  return(data[temp$document,])
}

multiword_query <- function(df) {
  docLen <- length(data$V2)
  df <- df %>% 
    group_by(document) %>% 
    mutate(docLen = sqrt(sum(count^2))) %>% 
    mutate(count = count/docLen) %>% 
    ungroup() %>% 
    select(term:count)
  
  documentMatrix <- df %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document < docLen + 1)
  queryMatrix <- df %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document >= docLen + 1)
  
  top_10_documents <- documentMatrix %>% 
    inner_join(queryMatrix, by=c("term"="term"), suffix=c(".doc",".query")) %>% 
    mutate(term_score = round(count.doc * count.query, 5)) %>% 
    group_by(document.query, document.doc) %>% 
    summarise(score = sum(term_score)) %>%
    arrange(desc(score)) %>%
    top_n(10, score)
  
  temp <- left_join(top_10_documents, data, by = c("document.doc" = "V1"))
  return(data[temp$document.doc,])
}

#main program starts
print("Welcome to Recommendation Search Engine!")
query <- ""
while (query != "q") {
  query <- readline("Enter your query : ")
  x <- unlist(strsplit(query, " "))
  if (query != "q") {
    if (length(x) == 1) {
      #single word query
      #calculating top 10 tdidf documents for the query word
      data <- getDataFromFile(data_file)
      corpus <- getCorpusFromDocument(data$V2)
      dtm <- getDTMFromCorpus(corpus)
      df <- getDFfromDTM(dtm)
      results <- singleword_query(x)
    }
    else{
      #multi-word query
      #calculating the cosine similiarity between the query and the documents
      words <- tolower(paste(gsub("[^[:alpha:] ]", "", x), collapse = " "))
      data <- getDataFromFile(data_file)
      corpus <- getCorpusFromDocument(c(data$V2, words))
      tdm <- getTDMFromCorpus(corpus)
      df <- getDFfromDTM(tdm) 
      results <- multiword_query(df)
    }
    print(results)
  }
  else {
    x <- ""
  }
}