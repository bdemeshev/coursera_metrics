x <- c(23,15,46,NA)
z <- c(5,6,NA,8)

mean(x)
mean(x,na.rm = TRUE)
mean(z,na.rm = TRUE)

sum(x)
sum(x,na.rm = TRUE)

d <- data.frame(rost=x,ves=z)
d

d[4,1]
d[3,1]

d[2,]
d[,2]

d$rost
d$ves

my_list <- list(a=7,b=10:20,table=d)

my_list$a
my_list$b
my_list$table
d$rost

my_list[[2]]

# Ура!


