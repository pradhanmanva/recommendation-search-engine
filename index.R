#loading the required packages in R session
require(tm)
require(SnowballC)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)

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

#generating the TDM from the corpus+query
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

#for single word query
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

#for multi word query
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

#UI Logic
ui <- fluidPage(
  titlePanel("Recommendation Search Engine"),
  sidebarLayout(
    sidebarPanel(
      helpText("Type in query to see the movie plots that match best."),
      textInput("query", h3("Text input", align = "center"), value = "")),
    mainPanel(
      h2("Let's see how this engine works.", align = "left"),
      h4("Let's see how it works"),
      p("We have to put in the query (single or multiword) that results in creating a Vector of Documents. Then a Document-Term/ Term-Document Matrix is generated with respect to the query and normalized weighted Term Frequency - Inverse Document Frequency (TD-IDF)."),
      p("Then this DTM/TDM is converted to dataframe (tibble) for further processing. We strip the ", em("dataframe$terms"), " to lower alphabets. Then filter out the ", em("dataframe$terms"), " and get only relevant ones."),
      p("In case of single word query, only top ten documents (plot summaries) with highest TD-IDF are returned in the results, while in multi word query, the cosine similiarity between query and documents is calculated. Top ten similiar documents are returned in the results."),
      h3("Here are the results: "),
      textOutput("message"),
      dataTableOutput("results")
    )
  )
)


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
    View(results)
  }
  else {
    x <- ""
    
  }
}
