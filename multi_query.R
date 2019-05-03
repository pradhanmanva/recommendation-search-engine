documents_to_corpus <- function(plot_summary){
  corpus <- VCorpus(VectorSource(plot_summary)) %>%
    tm_map(stemDocument) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  return(corpus)
}
#summary_dtm <- DocumentTermMatrix(summary_corpus, 
#control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
dtm_to_tdidf_dataframe <- function(){
  document_dtm <- 
}

docList <- plot_summary$V2
N.docs <- length(docList)



QrySearch <- function(query_term) {
  
  query_term <- "Action comedy"
  # Store docs into a term document matrix where rows=terms and cols=docs
  # Normalize term counts by applying TDiDF weightings
  term.doc.matrix.stm <- DocumentTermMatrix(my.corpus,
                                            control=list(
                                              weighting=function(x) weightSMART(x,spec="ltc"),
                                               wordLengths=c(1,Inf)))
  
  
  
  # Transform term document matrix into a dataframe
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
  
  
  # Record when it stops and take the difference
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,4)
  print(paste("Used",time.taken,"seconds"))
  
  return(searchRes)
  
}
QrySearch("data science")