install.packages("tidyverse")
library("readr")
library("dplyr")

install.packages(c("twitteR"), dependencies =  TRUE)
library(twitteR)

CONSUMERKEY = "IKiLDD5JOuHp6cpHCS7OxvNdt"
CONSUMERSECRET = "dvzrP6Vh6g38uxVk098jxXr9L0l8gFcF3rXvEy9kUWiTHlOYhY"
ACCESSTOKEN = "2399567513-PQ7pkcfmr7cqEWp4588RNDLRoB3wFfLYiSv9xgy"
ACCESSSECRET = "9uwiQ5yP00VqKZrcEspfiy0nIAga5dLP5Mad7mXrxhnON"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(CONSUMERKEY, CONSUMERSECRET, ACCESSTOKEN, ACCESSSECRET)

# 検索ワード
kw <- "バチェラー 広告" 

# 検索
TwGetDF1　<-　twListToDF(searchTwitter(iconv(kw,"CP932","UTF-8"), n=1000, lang="ja", since=NULL, until=NULL))

# リツイートを除外
df1 <- TwGetDF1[unlist(lapply(TwGetDF1[, 'isRetweet'], function(i){if (isTRUE(i)) return (FALSE)
  else return (TRUE)}
)), ]

#エクスポート
write.table(df1, file = "Twitter.csv", sep= ",")

df1 %>%
  summarise(n())

TwGetDF <- twListToDF(searchTwitter(iconv(kw,"CP932","UTF-8"), n=1500, lang="ja", since= "2018-01-05", until= "2018-01-07"))
View(TwGetDF)

library(lubridate)
# 日付のベクトルを生成
days <- seq(ymd("2018-01-05"), ymd("2018-01-07"), by = "day")

# 日付でループ
nextday <- NULL
days_chr <- NULL
next_chr <- NULL
reslist <- list()
for (i in 1:length(days)) {
  # 1 日後を作成
  nextday[i] <- days[i] + 1
  
  # 文字列型に
  days_chr[i] <- as.character(days[i])
  next_chr[i] <- as.character(nextday[i])
  
  # 検索
  reslist[[i]] <- twListToDF(searchTwitter(iconv(kw,"CP932","UTF-8"), n=1500, lang="ja", since=days_chr[i], until=next_chr[i]))
  
  # 5 秒停止(API 制限回避(この秒数でできるかは不明))
  Sys.sleep(5)
}

View(reslist[[i]])


dates <- seq(date("2018-01-05"), date("2018-01-07"), by = "1 day")

reslist <- lapply(dates, function(date) {
  twListToDF(searchTwitter(kw, n = 1500, lang = "ja", since = as.character(date), until = as.character(date + 1)))
})