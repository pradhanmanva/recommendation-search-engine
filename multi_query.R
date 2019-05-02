docList <- plot_summary$V2
N.doc <- length(docList)

QrySearch <- function(query_term) {
  
  # Record starting time to measure your search engine performance
  start.time <- Sys.time()
  
  # store docs in Corpus class which is a fundamental data structure in text mining
  my.docs <- VectorSource(c(docList, query_term))
  
  
  # Transform/standaridze docs to get ready for analysis
  my.corpus <- VCorpus(my.docs) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  
  # Store docs into a term document matrix where rows=terms and cols=docs
  # Normalize term counts by applying TDiDF weightings
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
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
    inner_join(qryMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    mutate(termScore=round(count.doc*count.query,4)) %>% 
    group_by(document.query,document.doc) %>% 
    summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    arrange(desc(Score)) %>% 
    left_join(tweets,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    rename(Result=text) %>% 
    select(Result,Score,retweetCount) %>% 
    data.frame()
  
  
  # Record when it stops and take the difference
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,4)
  print(paste("Used",time.taken,"seconds"))
  
  return(searchRes)
  
}
QrySearch("data science")