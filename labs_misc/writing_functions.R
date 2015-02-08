
f <- function(x) {
  res <- x^2
  return(res)
}

f(3)
f(-1)

fs <- function(x, stepen=2) {
  res <- x^stepen
  return(res)
}

fs(4)
fs(2,stepen=5)


cars

d <- cars
d[1,2] <- NA
d[3,1] <- NA
d

na_perc <- function(d) {
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}

na_perc(d)

na_perc <- function(d) {
  if(!is.data.frame(d)) stop("d should be a data.frame")
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}

na_perc(d)
x <- c(5,6,7)
na_perc(x)



