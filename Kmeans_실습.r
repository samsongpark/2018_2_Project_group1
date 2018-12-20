###### 0. 공지사항 ######

# 이해 안되면  첨부한 해설집을 참고해서 보세요

###### 1. Collecting data ######

teens <- read.csv("snsdata.csv")
str(teens)

###### 2. Exploring and preparing the data ######

# 위에서 확인했듯이 gender 변수에는 NA값이 포함되어 있다.
table(teens$gender)

# 요렇게 하면 NA값까지 볼 수 있다.
table(teens$gender, useNA = "ifany")

# 데이터를 잘 살펴보면 gender값 외에도 age 변수 역시 5086개의 NA의 값을 가지고 있다.
summary(teens$age)

# 또한 최소값과 최대값을 살펴보면 3살과 106살이 포함되어 있다.. 
# 우린 고등학생 즉 청소년을 상대로 분석하는 중이니 잘못 기입한 정보를 다 NA 값으로 만들어버리자
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

# 다시 확인해보니 불행하게도.. NA값이 더 늘었다.
summary(teens$age)

###### 2-1. Dummy coding missing values ######

teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)


table(teens$gender, useNA = "ifany")

table(teens$female, useNA = "ifany")

table(teens$no_gender, useNA = "ifany")

###### 2-2. Imputing missing values ######

mean(teens$age) # doesn't work

# 학생들의 평균나이가 17세 정도인 것을 알 수 있다.
mean(teens$age, na.rm = TRUE) 

# 하지만 gradyear별 평균 나이를 보고 싶기 때문에 aggregate함수를 사용한다.
# aggregate(계산될컬럼~기준될컬럼, 데이터, 함수)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# ave(적용할 벡터, 기준이 되는 벡터, 적용할 함수)
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

# NA값이 들어가 잇는 곳에 ave함수를 적용시켜서 졸업년도(gradyear)에 대응되는 나이 평균을 집어넣는다.
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

head(teens$age)

summary(teens$age)

###### 3. Training a model on the data ######

library(stats)

str(teens[5:40])

# kmeans()에는 오직 numeric data만이 parameter로 들어 갈 수 있기 때문에
# 36개 단어에 대한 변수만을 따로 담는다. (gradyear, gender, friends , age, female, no_gender 제외)
interests <- teens[5:40]

# nomarlize를 하기 위해 각 변수들에 scale을 먹인다.
interests_z <- as.data.frame(lapply(interests, scale))

# 맨 처음 말했듯이 5개의 군집으로 나눌 것이기에 k에 5를 넣는다.
teen_clusters <- kmeans(interests_z, 5)

###### 4. Evaluating model performance ######

teen_clusters$size

teen_clusters$centers

apply(teen_clusters$centers, 2, which.max)

###### 5. Improving model performance ######

teens$cluster <- teen_clusters$cluster

teens[1:5, c("cluster", "gender", "age", "friends")]

aggregate(data = teens, age ~ cluster, mean)

aggregate(data = teens, female ~ cluster, mean)

aggregate(data = teens, friends ~ cluster, mean)
