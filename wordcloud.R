library("tm")
library("wordcloud")
library("dplyr")

cameron <- Corpus (DirSource("~/Documents/coursera_metrics/wordcloud/"))

inspect(cameron)



camnew <- tm_map(cameron, stripWhitespace) %>% tm_map(tolower) %>%
  tm_map(removeWords, stopwords("russian")) %>% tm_map(stemDocument)

cam2 <- gsub("[[:punct:]]"," ",camnew)

wordcloud(camnew, scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(camnew)
