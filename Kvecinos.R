
install.packages("VIM", dependencies = TRUE)
library(VIM)

install.packages("palmerpenguins")
library(palmerpenguins)

BD<-penguins
anyNA(BD)
summary(BD)

BD1<-kNN(BD)
summary(BD1)
str(BD1)
dim(BD1)

BD2<-as.data.frame(BD1)
write.csv(BD2, "BD2.csv")



