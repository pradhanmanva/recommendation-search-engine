#loading the required packages in R session
require(tm)
require(SnowballC)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)
require(ggplot2)
require(proxy)

#setting up the Spark session
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#setting up file paths
data_file <- "input/plot_summaries.txt"
vector_file <- "plot_summary.RData"
corpus_file <- "summary_corpus.RData"
dtm_df_file <- "summary_tdidf_df.RData"

#read the data from "plot_summaries.txt" and save the data into a file
if (!file.exists(vector_file)){
  plot_summary <- read.delim(data_file, header=FALSE, sep = "\t", quote="")
  plot_summary$V2 <- as.character(plot_summary$V2)
  saveRDS(plot_summary, vector_file)
  print("Saved!")
}
#reading the saved data.
plot_summary <- readRDS(vector_file)

if(!file.exists(corpus_file)){
  #getting the plot summaries into another dataframe
  summary_data <- plot_summary$V2
  
  #creating VCorpus for stemming by removing punctuations, stopwords and numbers, and strip whitespaces.
  summary_corpus <- VCorpus(VectorSource(summary_data))
  summary_corpus <- tm_map(summary_corpus, removePunctuation)
  summary_corpus <- tm_map(summary_corpus, removeNumbers)
  summary_corpus <- tm_map(summary_corpus, content_transformer(tolower))
  summary_corpus <- tm_map(summary_corpus, removeWords, stopwords(kind="en"))
  summary_corpus <- tm_map(summary_corpus, stripWhitespace)
  
  #stemming the documents
  summary_corpus <- tm_map(summary_corpus, stemDocument)
  summary_dtm <- DocumentTermMatrix(summary_corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
  saveRDS(summary_dtm, corpus_file)
  print("Saved!")
}
summary_dtm <- readRDS(corpus_file)

if(!file.exists(dtm_df_file)){
  #converting the DTM to dataframe and manipulating it to get the TD_IDF values in it.
  summary_dtm_table <- tidy(summary_dtm)
  summary_dtm_table <- bind_tf_idf(summary_dtm_table, term = term, document = document, n=count)
  saveRDS(summary_dtm_table, dtm_df_file)
  print("Saved!")
}
plot_tdidf <- readRDS(dtm_df_file)

#removing punctuation from the plot_tdidf$terms
plot_tdidf$term <- gsub("[^[:alpha:] ]","", plot_tdidf$term)

#convert plot_tdidf$docs to integer
plot_tdidf$document <- as.integer(plot_tdidf$document)

#group by on plot_tdidf$document
plot_tdidf %>%
  group_by(document) %>%
  summarise(n=n()) %>%
  ggplot(aes(document,n)) +
  geom_line() 


#single word query
plot_tdidf <- plot_tdidf %>%
  filter(str_detect(plot_tdidf$term, "comedy")) %>%
  arrange(desc(tf_idf))

word<-"comedy"
top_10_results <- plot_tdidf %>%
  filter(str_detect(plot_tdidf$term, word)) %>%
  arrange(desc(tf_idf)) %>%
  select(document, term, tf_idf) %>%
  top_n(10, tf_idf)


temp <- plot_summary[top_10_results$document
                     ]