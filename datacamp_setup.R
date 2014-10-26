install.packages("devtools")
library("devtools")
install_github("datacampSCT", "data-camp")
install_github("datacamp", "data-camp")
install_github("slidify", "ramnathv", ref = "dev")
install_github("slidifyLibraries", "ramnathv")
library("datacamp")



author_course("metrics")

datacamp_login() # ДО этой команды нужно залогиниться в браузере в datacamp 

# отредактировать course.yml

upload_course()

# отредактировать chapter1.Rmd

upload_chapter("chapter1.Rmd")

upload_course()

upload_course(force=TRUE)
