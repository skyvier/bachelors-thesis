data.list <- read.table("degrees.csv")
data <- unlist(data.list, use.names=F)
data.tail <- Filter(function (x) { return(x > 2); }, data)
hist(data.tail)
