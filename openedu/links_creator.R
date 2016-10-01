links <- read.delim("~/Downloads/links.txt", 
                    header = FALSE, 
                    stringsAsFactors = FALSE)

glimpse(links)
links2 <- select(links, -V1, -V10)
glimpse(links2)
colnames(links2) <- c("status", "guid",
                      "name", "duration",
                      "screensize", "bitrate",
                      "filesize", "date")
links2 <- mutate_each(links2, funs(str_trim))
links2$loadid <- "38"
links2$loadid[1:45] <- "39"
links3 <- mutate(links2,
     or_link = paste0("http://93.175.29.153/video/original/00", loadid, "/", guid, ".mp4"),
     hd_link = paste0("http://93.175.29.153/video/hd/00", loadid, "/", guid, ".mp4"),
     sd_link = paste0("http://93.175.29.153/video/sd/00", loadid, "/", guid, ".mp4")
)

# 7.1.7 - уже 39
library(readr)
write_csv(links3, path = "video_links.csv")
