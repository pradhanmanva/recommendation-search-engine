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