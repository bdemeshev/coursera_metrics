
for (i in 5:10) {
  k <- i^2
  cat("i=",i," i^2=",k,"\n")
}

all_data <- NULL
for (fname in c("file01.csv","file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(all_data,temp)
}
head(all_data)
