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

d <- data.frame(
  word=c("Эконометрика","R","ВШЭ","гетероскедастичность","автокорреляция",
         "робастные ошибки","P-значение", "тест", "Гаусс", "Марков",
         "мультиколлинеарность","гипотеза", "стохастический", "регрессор",
         "стандартное отклонение", "данные","МНК","логит",
         "коэффициент детерминации","регрессия","предельные эффекты","стационарность",
         "инструменты","остатки")
  )

# выдумываем частоты для слов
d$freq <- sample(5:10,size = nrow(d),replace = TRUE) 
d$freq[1:3] <- c(20,18,14) # первые три слова крупно

pal <- brewer.pal(9, "Set1")
pal <- c("#0000FF","#FF6600","#00CC33","#FFFF00","","","","")

pal <- c(brewer.pal(9, "Blues")[5:9], brewer.pal(9, "Oranges")[4:9], 
         brewer.pal(9, "Greens")[5:9],brewer.pal(9, "GnBu")[5:9], 
         brewer.pal(9, "Reds")[5:9], brewer.pal(9, "Purples")[5:9])

wordcloud(d$word,d$freq, scale=c(3,.1),min.freq=2,max.words=100, 
          random.order=T, rot.per=0.15, colors=pal) 

wordcloud(d$word,d$freq, random.order=T, rot.per=.15, colors=pal) 







