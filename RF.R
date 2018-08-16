setwd("C:/Users/taiga/Dropbox/R/RandomForest")
install.packages("tidyverse")
library("readr")
library("dplyr")
library("tidyr")
install.packages("randomForest")
require("randomForest")

# データ抽出
Jan <- read.csv("201701.csv",stringsAsFactors = FALSE)
Feb <- read.csv("201702.csv",stringsAsFactors = FALSE)
Mar <- read.csv("201703.csv",stringsAsFactors = FALSE)
Apr <- read.csv("201704.csv",stringsAsFactors = FALSE)
May <- read.csv("201705.csv",stringsAsFactors = FALSE)
Jun <- read.csv("201706.csv",stringsAsFactors = FALSE)
Jul <- read.csv("201707.csv",stringsAsFactors = FALSE)
Aug <- read.csv("201708.csv",stringsAsFactors = FALSE)
Sep <- read.csv("201709.csv",stringsAsFactors = FALSE)
Oct <- read.csv("201710.csv",stringsAsFactors = FALSE)

# 使うカラムだけに絞る
rename(Aug,status == `statusAug`)
Jan_col <- Jan %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Feb_col <- Feb %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Mar_col <- Mar %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Apr_col <- Apr %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
May_col <- May %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Jun_col <- Jun %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Jul_col <- Jul %>%
  select(id,New,sex,age,status,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Aug_col <- Aug %>%
  select(id,New,sex,age,statusAug,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
Sep_col <- Sep %>%
  select(id,New,sex,age,statusSep,count_payment,
         sum_payment,match_send,match_gets,send_goods,get_goods,photos,profile,appeal)
rename(Sep_col,status = `statusSep`)
rename(Aug_col,status = `statusAug`)

# RRユーザの抽出
Full <- Jan_col %>%
  full_join(Feb_col,by = "id") %>%
  full_join(Mar_col,by = "id") %>%
  full_join(Apr_col,by = "id") %>%
  full_join(May_col,by = "id") %>%
  full_join(Jun_col,by = "id") %>%
  full_join(Jul_col,by = "id") %>%
  full_join(Aug_col,by = "id") %>%
  full_join(Sep_col,by = "id")　#全デーブルをジョイン
full_RR <-Full %>% 
  select(id,sex.x,sex.y,sex.x.x,sex.y.y,sex.x.x.x,sex.y.y.y,sex.x.x.x.x,sex.y.y.y.y,sex)
class(full_RR[5 ,5])
Full_RR <- full_RR %>%
  replace_na(list(sex.x=0, sex.y= 0,sex.x.x = 0, sex.y.y = 0,sex.x.x.x = 0, sex.y.y.y = 0,
                  sex.x.x.x.x = 0,sex.y.y.y.y = 0, sex = 0 ))
RR <- dplyr::mutate_at(Full_RR, vars(2:10), funs(as.numeric))

# RRしたユーザを1,そうじゃないユーザを0として分類
RR_tag <- RR %>%
  mutate(JF=if_else(sex.x>0,if_else((sex.x-sex.y) == 0,1,0),0)) %>%
  mutate(FM=if_else(sex.y>0,if_else((sex.y-sex.x.x)== 0,1,0),0)) %>%
  mutate(MA=if_else(sex.x.x>0,if_else((sex.x.x-sex.y.y)== 0,1,0),0)) %>%
  mutate(AM=if_else(sex.y.y>0,if_else((sex.y.y-sex.x.x.x)== 0,1,0),0)) %>%
  mutate(MJ=if_else(sex.x.x.x>0,if_else((sex.x.x.x-sex.y.y.y)== 0,1,0),0)) %>%
  mutate(JJ=if_else(sex.y.y.y>0,if_else((sex.y.y.y-sex.x.x.x.x) == 0,1,0),0)) %>%
  mutate(JA=if_else(sex.x.x.x.x>0,if_else((sex.x.x.x.x-sex.y.y.y.y)== 0,1,0),0)) %>%
  mutate(AS=if_else(sex>0,if_else((sex.y.y.y.y-sex)== 0,1,0),0))

# 元のデータと結合させる
JAN <- Jan_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,JF)%>%
  rename(CV = `JF`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
FEB <- Feb_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,FM)%>%
  rename(CV = `FM`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
MAR <- Mar_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,MA)%>%
  rename(CV = `MA`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
APR <- Apr_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,AM)%>%
  rename(CV = `AM`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
MAY <- May_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,MJ)%>%
  rename(CV = `MJ`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
JUN <- Jun_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,JJ)%>%
  rename(CV = `JJ`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
JUL <- Jul_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,JA)%>%
  rename(CV = `JA`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))
AUG <- Aug_col %>%
  left_join(RR_tag, by = "id") %>%
  select(New,age,match_send,match_gets,
         send_goods,get_goods,photos,profile,appeal,AS)%>%
  rename(CV = `AS`)%>%
  dplyr::mutate_at(vars(1:10), funs(as.numeric))

dat <- dplyr::bind_rows(list(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG))
Dat <- dat %>%
  replace_na(list(New=0,age=0,match_send=0,match_gets=0,
                  send_goods=0,get_goods=0,photos=0,profile=0,appeal=0 ))
write.csv(Dat,"dat.csv")
d <- read_csv("dat.csv")

d1 <- d %>%
  select(4:11)
d2 <- as.data.frame(d1)

View(d2)
install.packages("randomForest")
require("randomForest")

# 個々の木の特徴量選択は何個ずつが最もいいかを
tuneRF(d2[,-10],d2[,10],doBest=T)
length(d1$appeal)

# mtry=2を引数にしてrandomForest()関数で分類する
d.rf<-randomForest(CV~.,d2,mtry=2)
print(d.rf)

# 決定木:特徴量を把握
importance(d.rf)

# 分類正当率
table(d$cv,predict(d.rf,d[,-8]))