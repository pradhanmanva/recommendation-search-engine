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
readFile <- function(file_name) {
  if(!file.exists(vector_file)) {
    plot_summary <- read.delim(file_name, header=FALSE, sep = "\t", quote="")
    plot_summary$V1 <- as.integer(plot_summary$V1)
    plot_summary$V2 <- as.character(plot_summary$V2)
    saveRDS(plot_summary, vector_file)
    print("Saved.")
  }
  else {
    plot_summary <- readRDS(vector_file)
  }
  return(plot_summary)
}

#getting clean corpus from the documents given (summary plots)
getCorpusFromDocument <- function(plot_summary){
  if(!file.exists(corpus_file)) {
    corpus <- VCorpus(VectorSource(plot_summary)) %>%
      tm_map(stemDocument) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords,stopwords("en")) %>%
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
getDFfromCorpus <- function(corpus){
  if(!file.exists(dataframe_file)) {
    document_dtm <- DocumentTermMatrix(corpus, 
                                       control = list(
                                         weighting = function(x) weightTfIdf(x, normalize = TRUE), 
                                         stopwords = TRUE, wordLengths = c(1, Inf)))
    document_df <- tidy(document_dtm) %>%
      bind_tf_idf(term = term, document = document, n=count) %>%
      gsub("[^[:alpha:] ]","", term) %>%
      as.integer(document)
    saveRDS(document_df, dataframe_file)
    print("Saved!")
  }
  else {
    document_df <- readRDS(dataframe_file)
  }
  return(document_df)
}

single_word_query <- function (word) {
  top_10_results <- plot_tdidf %>%
    filter(str_detect(plot_tdidf$term, word)) %>%
    arrange(desc(tf_idf)) %>%
    select(document, term, tf_idf) %>%
    top_n(10, tf_idf)
  
  temp <- left_join(top_10_results, plot_summary, by = c("document" = "V1")) %>%
    select (document, V2)
}

docList <- plot_summary$V2
N.docs <- length(docList)

QrySearch <- function(query_term) {
  query_term <- "Action"
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    group_by(document) %>% 
    mutate(vtrLen=sqrt(sum(count^2))) %>% 
    mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    select(term:count)
  
  docMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document>=N.docs+1)
  
  # Calcualte top ten results by cosine similarity
  searchRes <- docMatrix %>% 
    inner_join(qryMatrix, by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    mutate(termScore=round(count.doc*count.query,4)) %>% 
    group_by(document.query,document.doc) %>% 
    summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    arrange(desc(Score)) %>% 
    left_join(plot_summary, by=c("document.doc"="V1")) %>% 
    ungroup() %>% 
    rename(Result=V2) %>% 
    select(Result, Score) %>% 
    data.frame()
  return(searchRes)
}


#main program starts
print("Welcome to Recommendation Search Engine!")
query <- ""
while (query != "q") {
  query <- readline("Enter your query : ")
  x <- unlist(strsplit(query, " "))
  
  if (length(x) == 1) {
    single_word_query(x)
  }
  else{
    multi_word_query(paste(x, collapse = " "))
  }
}