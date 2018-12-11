library(data.table)
happy <- fread("ALL_happy.csv",encoding='UTF-8')
sad <- fread("ALL_sad.csv",encoding = 'UTF-8')


delete <- function(data) {
  data <- data[ grep("카카오톡", data$V2, invert = TRUE) , ]
  data <- data[ grep("카톡", data$V2, invert = TRUE) , ]
  data <- data[ grep("상담", data$V2, invert = TRUE) , ]
  data <- data[ grep("주소", data$V2, invert = TRUE) , ]
  data <- data[ grep("예약", data$V2, invert = TRUE) , ]
  data <- data[ grep("문의", data$V2, invert = TRUE) , ]
  data <- data[ grep("환불", data$V2, invert = TRUE) , ]
  data <- data[ grep("링크", data$V2, invert = TRUE) , ]
  data <- data[ grep("추천", data$V2, invert = TRUE) , ]
  data <- data[ grep("노리터", data$V2, invert = TRUE) , ]
  data <- data[ grep("ㅂㅐ팅", data$V2, invert = TRUE) , ]
}


tmp <- delete(happy[,2])
df = as.data.frame(x = tmp)

tmp2 <- delete(sad[,2])
df2 = as.data.frame(x = tmp2)

############# 

rmCelb<-function(tweetData){  #tweetData <- type Vector 
  
  user_name<-regmatches(tweetData,regexpr("@[[:graph:]]*",tweetData))   #user_name 추출
  
  freq_name<-data.frame(table(user_name))   #빈도표 작성
  freq_name<-freq_name[c(order(-freq_name$Freq)),]   #빈도순으로 정렬
  
}

# rowNum : 빈도수가 100 이상인 user_name이 포함된 모든 행번호 출력하는 함수

freqName1 <- rmCelb(df$V2) # rowNum 함수의 input data
freqName2 <- rmCelb(df2$V2)



rowNum <- function(freqNameData,df){   #freqNameData : input 데이터는 rmCelb함수 사용한 결과 
  
  rowNumber <- list()  #list 생성
  rmName <- as.vector(freqNameData[freqNameData$Freq>100,]$user_name)   #빈도수가 100 이상인 username만 추출
  
  for(i in 1:length(rmName)){    # 해당 username이 포함된 모든 행번호 rowNumber 리스트에 저장
    rowNumber[[i]] <- grep(rmName[i],df$V2)
    print(i)
  }
  
  unlist(rowNumber)  # list to vector
  
}


newdf <- df$V2[-rowNum(freqName1,df)]
newdf2 <- df2$V2[-rowNum(freqName2,df2)]




##########



remove <- function(x) {
  x <- gsub("[<].+[>]", "", x)
  x<- gsub("@[[:graph:]]*", "", x)
  x<- gsub("#[[:graph:]]*","",x)
  x <- gsub("http://[[:graph:]]*", "", x)
  x<- gsub("https://[[:graph:]]*", "", x)
  x <- gsub("pic.twitter.com/[[:graph:]]*", "", x)
  x<- gsub("RT","",x)
  x<- gsub("\n"," ",x)
  x <- gsub("_"," ",x)
  x <- gsub("[[:punct:][:blank:]]+", " ",x)
  x <- gsub('[0-9]+', '', x)
  x <- gsub('[a-zA-Z]','',x)
  x <- gsub("\r"," ",x)
  x <- gsub("  "," ",x)
  x <- gsub("   "," ",x)
  x <- gsub("    "," ",x)
  x <- gsub("     "," ",x)
  x <- gsub("      "," ",x)
  x <- gsub("       "," ",x)
  x <- gsub("        "," ",x)
  x <- gsub("         "," ",x)
  x <- gsub("♥","",x)
  x <- gsub("♡","",x)
  x <- gsub("▶","",x)
}  
newnewdf <- remove(newdf) 
newnewdf2 <- remove(newdf2)

newnewdf <- unique(newnewdf)
newnewdf2 <- unique(newnewdf2)
