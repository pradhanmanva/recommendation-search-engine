#setting up the Spark session
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#group by on plot_tdidf$document
plot_tdidf %>%
  group_by(document) %>%
  summarise(n=n()) %>%
  ggplot(aes(document,n)) +
  geom_line() 

