#loading the required packages in R session
require(tm)
require(SnowballC)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)

setwd(getwd())

#setting up file paths
data_file <- "http://utdallas.edu/~rxk164330/MovieSummaries/plot_summaries.txt"

#reading the file and saving the data into local to save time
getDataFromFile <- function(file_name) {
  print("Reading Data.")
  #Reading the input data file
  plot_summary <- read.delim(file_name, header = FALSE, sep = "\t", quote = "")
  names(plot_summary)<-c("V1","V2")
  plot_summary$V1 <- as.integer(plot_summary$V1)
  plot_summary$V2 <- as.character(plot_summary$V2)
  print("Reading Data Done.")
  return (plot_summary)
}

#getting clean corpus from the documents given (summary plots)
getCorpusFromDocument <- function(documents) {
  print("Creating Corpus")
  corpus <- VCorpus(VectorSource(documents)) %>%
    tm_map(stemDocument) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  print("Creating Corpus Done.")
  return(corpus)
}

#generating DTM from the given corpus with TD-IDF weighting
getDTMFromCorpus <- function(corpus) {
  print("Creating DTM")
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) 
    weightTfIdf(x),
    stopwords = TRUE
  )
  )
  print("Creating DTM done")
  return(dtm)
}

#generating the TDM from the corpus+query
getTDMFromCorpus <- function(corpus) {
  print("Creating TDM")
  tdm <- TermDocumentMatrix(corpus, control = list(weighting = function(x) 
    weightSMART(x,spec="ltc"),
    wordLengths=c(1, Inf)))
  print("Creating TDM done")
  return(tdm)
}

#getting the Dataframe from the DTM
getDFfromDTM <- function(dtm) {
  print("Creating Dataframe")
  df <- tidy(dtm)
  df <- bind_tf_idf(df, term = term, document = document, n = count)
  df$term <- gsub("[^[:alpha:] ]", "", df$term)
  df$document <- as.integer(df$document)
  print("Creating Dataframe done")
  return(df)
}

#for single word query
singleword_query <- function (word) {
  print("Querying single word")
  word <- tolower(word)
  top_10_results <- df %>%
    filter(str_detect(df$term, word)) %>%
    arrange(desc(tf_idf)) %>%
    select(document, term, tf_idf) %>%
    top_n(10, tf_idf)
  
  temp <- left_join(top_10_results, data, by = c("document" = "V1")) %>%
     select (document)
  print("Querying single word done")
  return(data[temp$document,])
}

#for multi word query
multiword_query <- function(df) {
  print("Querying multi word")
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
  
  print("Querying multi word")
  return(data[temp$document.doc,])
}

#main program starts
print("Welcome to Recommendation Search Engine!")
query <- ""
while (query != "q") {
  query <- readline("Enter your query : ")
  
  if(query == "") {
    print("Empty String!")
    next
  }
  
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
      print(results)
      View(results)
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
      print(results)
      View(results)
    }
  }
  else{
    x<-""
  }
}
