# library("tm")
library("wordcloud")
library("dplyr")
library("RColorBrewer")

#cameron <- Corpus (DirSource("~/Documents/coursera_metrics/wordcloud/"))

#inspect(cameron)



#camnew <- tm_map(cameron, stripWhitespace) %>% tm_map(tolower) %>%
#  tm_map(removeWords, stopwords("russian")) %>% tm_map(stemDocument)

#cam2 <- gsub("[[:punct:]]"," ",camnew)

#wordcloud(camnew, scale=c(5,0.5), max.words=100, 
#          random.order=FALSE, rot.per=0.35, 
#          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
#wordcloud(camnew)

d <- data_frame(
  word=c("Эконометрика","R","ВШЭ","гетероскедастичность","автокорреляция",
         "робастные ошибки","P-значение", "тест", "теорема Гаусса-Маркова")
  )

# выдумываем частоты для слов
d$freq <- sample(8:10,size = nrow(d),replace = TRUE) 
d$freq[1:3] <- c(20,18,16) # первые три слова крупно

pal <- brewer.pal(9, "BuGn")

wordcloud(d$word,d$freq, scale=c(3,.1),min.freq=2,max.words=100, 
          random.order=T, rot.per=.15, colors=pal) 

wordcloud(d$word,d$freq, random.order=T, rot.per=.15, colors=pal) 







