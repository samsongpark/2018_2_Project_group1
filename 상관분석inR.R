setwd("/Users/yejinpark/Documents/univ/경영빅데이터분석/팀프로젝트/여기서하기/raw data/raw_data")
getwd()
data <- read.csv("data_finals.csv", sep=",",header=T,fileEncoding='utf-8')

str(data) #데이터구조확인
ppl_old <- as.numeric(ppl_old)
is.numeric(data$main_count)
is.numeric(data$metro_count)
is.numeric(data$ppl_old)
ppl_old <- as.numeric(ppl_old)
is.numeric(ppl_old)
str(data$ppl_old)

#산점도(scatter plot)
attach(data)
par("mar")
par(mar=c(1,1,1,1))


plot(main_count~ppl_old,main="ppl_old~main_count", xlab="PPL_OLD", ylab="MAIN_COUNT", cex=1, pch=1, col='red')

cov(main_count, data$ppl_old)
main_count
data$ppl_old
