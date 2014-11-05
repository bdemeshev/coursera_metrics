library("HSAUR")
library("dplyr")
library("psych")

# Multicollinearity



# PCA

h <- heptathlon
glimpse(h)
help(heptathlon)

h <- select(h,-score)
cor(h)
describe(h)

h.pca <- prcomp(h,scale=TRUE)
pc1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1]
v1

summary(h.pca)
cor(heptathlon$score,pc1)

plot(h.pca)


biplot(h.pca,xlim = c(-1,1))
