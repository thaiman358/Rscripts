# ディレクトリの設定
setwd("C:/Users/taiga/Dropbox/R/prj_Omiai/MAU_original/")
library("readxl")
library("readr")
dat <- read_xlsx("dat.xlsx")
View(dat)
write_csv(dat,"dat.csv")


# ライブラリインストール
install.packages("tidyverse")
library("readr")
library("dplyr")
library("readxl")

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
WAU_1 <- read.csv("WAU_20171009.csv",stringsAsFactors = FALSE)
WAU_2 <- read.csv("WAU_20171016.csv",stringsAsFactors = FALSE)
WAU_3 <- read.csv("WAU_20171023.csv",stringsAsFactors = FALSE)
WAU_4 <- read.csv("WAU_20171030.csv",stringsAsFactors = FALSE)
WAU_5 <- read.csv("WAU_20171106.csv",stringsAsFactors = FALSE)
WAU_good1 <- read_excel("ID_merged_good1.xlsx")
WAU_good2 <- read_excel("ID_merged_good2.xlsx")
WAU_match1 <- read_excel("ID_merged_match1.xlsx")
WAU_match2 <- read_excel("ID_merged_match2.xlsx")
WAU_message <- read_excel("ID_merged_match2.xlsx")

View(Sep)

ID <- read.csv("ID_merged_201711071843.csv",stringsAsFactors = FALSE)

# 列名確認
colnames(WAU_5)
colnames(Jul)
colnames(Sep)

# 列名変更
RM_WAU_1 <- WAU_1 %>%
  rename(left_goods1 = `left_goods`)
RM_WAU_2 <- WAU_2 %>%
  rename(left_goods2 = `leftgoods`)
RM_WAU_3 <- WAU_3 %>%
  rename(left_goods3 = `left_goods`)
RM_WAU_4 <- WAU_4 %>%
  rename(left_goods4 = `left_goods`)
RM_WAU_5 <- WAU_5 %>%
  rename(left_goods5 = `left_goods`)

RM_Jan <- Jan %>%
  rename(statusJan = `status`)
RM_Feb <- Feb %>%
  rename(statusFeb = `status`)
RM_Mar <- Jan %>%
  rename(statusMar = `status`)
RM_Apr <- Feb %>%
  rename(statusApr = `status`)
RM_May <- May %>%
  rename(statusMay = `status`)
RM_Jun <- Jun %>%
  rename(statusJun = `status`)
RM_Jul <- Jul %>%
  rename(statusJul = `status`)

# 各月データをステータスのみにする
sep_male <- Sep %>%
  filter(sex == 1)%>%
  select(New,send_goods)
View(sep_male)

ID_countget2 <- ID %>%
  group_by(status2,Paid,get_good3) %>%
  summarise(UU = n())

male_1  <- RM_Jan %>% 
  filter(sex == 1) %>%
  select(id,statusJan)
male_2  <- RM_Feb %>% 
  filter(sex == 1) %>%
  select(id,statusFeb)
male_3  <- RM_Mar %>% 
  filter(sex == 1) %>%
  select(id,statusMar)
male_4  <- RM_Apr %>% 
  filter(sex == 1) %>%
  select(id,statusApr)
male_5  <- RM_May %>% 
  filter(sex == 1) %>%
  select(id,statusMay)
male_6  <- RM_Jun %>% 
  filter(sex == 1) %>%
  select(id,statusJun)
male_7  <- RM_Jul %>% 
  filter(sex == 1) %>%
  select(id,statusJul)

WAU_1_selected <- select(RM_WAU_1, 1:12)
WAU_2_selected <- select(WAU_2, 1:12)
WAU_3_selected <- select(WAU_3, 1:12)
WAU_4_selected <- select(WAU_4, 1:12)
WAU_5_selected <- select(WAU_5, 1:12)

WAU_1_male <- WAU_1_selected %>%
  filter(sex == 1)
WAU_2_male <- WAU_2_selected %>%
  filter(sex == 1)
WAU_3_male <- WAU_3_selected %>%
  filter(sex == 1)
WAU_4_male <- WAU_4_selected %>%
  filter(sex == 1)
WAU_5_male <- WAU_5_selected %>%
  filter(sex == 1)
colnames(May)

New_May <- May %>%
  filter(New == 1) %>%
  select(id,age,sum_payment)
New_Jun <- Jun %>%
  filter(New == 1) %>%
  select(id,age,sum_payment)
New_Jul <- Jul %>%
  filter(New == 1) %>%
  select(id,age,sum_payment)
New_Aug <- Aug %>%
  filter(New == 1) %>%
  select(id,age,sum_payment)
New_Sep <- Sep %>%
  filter(New == 1) %>%
  select(id,age,sum_payment)

# Mergeする
mergedWAU_5_4 <- WAU_5_male %>%
  full_join(WAU_4_male, by = "id")
mergedWAU_5_3 <- mergedWAU_5_4 %>%
  full_join(WAU_3_male, by = "id")
mergedWAU_5_2 <- mergedWAU_5_3 %>%
  full_join(WAU_2_male, by = "id")
mergedWAU_5_1 <- mergedWAU_5_2 %>%
  full_join(WAU_1_male, by = "id")
mergedWAU_5_Sep <- mergedWAU_5_1 %>%
  full_join(male_9, by = "id")
mergedWAU_5_Aug <- mergedWAU_5_Sep %>%
  full_join(male_8, by = "id")

colnames(mergedWAU_5_Aug)

# 列を指定
ID_merged_20171107<- mergedWAU_5_Aug %>%
  select(id,status1,status2,status3,status4,status5,status.x,status.y,left_goods1,left_goods2,left_goods3,left_goods4,left_goods5)
mergedWAU_5_Jul <- ID_merged_20171107 %>%
  left_join(male_7, by = "id")
mergedWAU_5_Jun <- mergedWAU_5_Jul %>%
  left_join(male_6, by = "id")
mergedWAU_5_May <- mergedWAU_5_Jun %>%
  left_join(male_5, by = "id")
mergedWAU_5_Apr <- mergedWAU_5_May %>%
  left_join(male_4, by = "id")
mergedWAU_5_Mar <- mergedWAU_5_Apr %>%
  left_join(male_3, by = "id")
mergedWAU_5_Feb <- mergedWAU_5_Mar %>%
  left_join(male_2, by = "id")
mergedWAU_5_Jan <- mergedWAU_5_Feb %>%
  left_join(male_1, by = "id")

write.csv(mergedWAU_5_Jan,"left_goods_201711211430.csv")

View(ID_merged_20171107)


# filter_sendgoodで
colnames(ID)
ID_countsend1 <- ID %>%
  group_by(status1,Paid,send_good2) %>%
  summarise(UU = n())
ID_countsend2 <- ID %>%
  group_by(status2,Paid,send_good3) %>%
  summarise(UU = n())
ID_countsend3 <- ID %>%
  group_by(status3,Paid,send_good4) %>%
  summarise(UU = n())
ID_countsend4 <- ID %>%
  group_by(status4,Paid,send_good5) %>%
  summarise(UU = n())

# filter_getgoodで
ID_countget1 <- ID %>%
  group_by(status1,Paid,get_good2) %>%
  summarise(UU = n())
ID_countget2 <- ID %>%
  group_by(status2,Paid,get_good3) %>%
  summarise(UU = n())
ID_countget3 <- ID %>%
  group_by(status3,Paid,get_good4) %>%
  summarise(UU = n())
ID_countget4 <- ID %>%
  group_by(status4,Paid,get_good5) %>%
  summarise(UU = n())

View(ID_countsend1)

# 行列化する
library(tidyr)
ID_countsend1_yoko <- spread(ID_countsend1, key = send_good2, value = UU)
ID_countsend2_yoko <- spread(ID_countsend2, key = send_good3, value = UU)
ID_countsend3_yoko <- spread(ID_countsend3, key = send_good4, value = UU)
ID_countsend4_yoko <- spread(ID_countsend4, key = send_good5, value = UU)

ID_countget1_yoko <- spread(ID_countget1, key = get_good2, value = UU)
ID_countget2_yoko <- spread(ID_countget2, key = get_good3, value = UU)
ID_countget3_yoko <- spread(ID_countget3, key = get_good4, value = UU)
ID_countget4_yoko <- spread(ID_countget4, key = get_good5, value = UU)


View(ID_countsend_yoko)

write.csv(ID_countsend1_yoko,"ID_countsend1_yoko.csv")
write.csv(ID_countsend2_yoko,"ID_countsend2_yoko.csv")
write.csv(ID_countsend3_yoko,"ID_countsend3_yoko.csv")
write.csv(ID_countsend4_yoko,"ID_countsend4_yoko.csv")
write.csv(ID_countget1_yoko,"ID_countget1_yoko.csv")
write.csv(ID_countget2_yoko,"ID_countget2_yoko.csv")
write.csv(ID_countget3_yoko,"ID_countget3_yoko.csv")
write.csv(ID_countget4_yoko,"ID_countget4_yoko.csv")

sendgood <- mergedWAU_3_Aug %>%
  select(id,status,status.x,status.y,status.x.x,status.y.y,send_good,send_good.x,send_good.y)
getgood <- mergedWAU_3_Aug %>%
  select(id,status,status.x,status.y,status.x.x,status.y.y,get_good,get_good.x,get_good.y)
match_send <- mergedWAU_3_Aug %>%
  select(id,status,status.x,status.y,status.x.x,status.y.y,match_send,match_send.x,match_send.y)
match_get <- mergedWAU_3_Aug %>%
  select(id,status,status.x,status.y,status.x.x,status.y.y,match_get,match_get.x,match_get.y)
messages <- mergedWAU_3_Aug %>%
  select(id,status,status.x,status.y,status.x.x,status.y.y,messages,messages.x,messages.y)

# 男女別のデータに変換
male_New_1  <- Jan %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_2  <- Feb %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_3  <- Mar %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_4  <- Apr %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_5  <- May %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_6  <- Jun %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_7  <- Jul %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_8  <- Aug %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)
male_New_9  <- Sep %>% 
  filter(sex == 1) %>%
  filter(New == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

merged1_2 <- male_New_1 %>%
  left_join(Feb, by = "id") 
merged1_3 <- merged1_2 %>%
  left_join(Mar, by = "id") 
merged1_4 <- merged1_3 %>%
  left_join(Apr, by = "id") 
merged1_5 <- merged1_4 %>%
  left_join(May, by = "id") 
merged1_6 <- merged1_5 %>%
  left_join(Jun, by = "id") 
merged1_7 <- merged1_6 %>%
  left_join(Jul, by = "id") 
merged1_8 <- merged1_7 %>%
  left_join(Aug, by = "id") 
merged1_9 <- merged1_8 %>%
  left_join(Sep, by = "id")

merged1_9_pastday <- merged1_8 %>%
  left_join(Sep, by = "id") %>%
  select(past_day.x, past_day.y, past_day.x.x, past_day.y.y,past_day.x.x.x, past_day.y.y.y,past_day.x.x.x.x,past_day.y.y.y.y,past_day)
merged1_9_sum_payments <- merged1_8 %>%
  left_join(Sep, by = "id") %>%
  select(sum_payment.x, sum_payment.y, sum_payment.x.x, sum_payment.y.y,sum_payment.x.x.x, sum_payment.y.y.y,sum_payment.x.x.x.x,sum_payment.y.y.y.y,sum_payment)
colnames(merged1_9)

# 男性抽出用
Feb_male  <- Feb %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Mar_male  <- Mar %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Apr_male  <- Apr %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

May_male  <- May %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Jun_male  <- Jun %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Jul_male  <- Jul %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Aug_male  <- Aug %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)

Sep_male  <- Jun %>% 
  filter(sex == 1) %>%
  select(id,age,New,status,send_goods,get_goods,match_send,match_gets,messeges,past_day,sum_payment)



# LTV算出
colnames(Jan_male)

Jan_LTV <- Jan_male %>%
  summarise(UU = n(),
            avg_pastday = mean(past_day),
            sum_payment = sum(sum_payment))

Jan_LTV <- Jan_male %>%
  summarise(UU = n(),
            pastday = mean(past_day),
            sum_payment = sum(sum_payment))
# 地域別
Jun_reg <- Jun %>%
  group_by(sex,New,reg) %>%
  summarise(UU = n(),
            get_good = mean(get_goods),
            send_good = mean(send_goods),
            match_send = mean(match_send),
            match_get = mean(match_gets),
            massages = mean(messeges))

Jul_reg <- Jul %>%
  group_by(sex,New,reg) %>%
  summarise(UU = n(),
            get_good = mean(get_goods),
            send_good = mean(send_goods),
            match_send = mean(match_send),
            match_get = mean(match_gets),
            massages = mean(messeges))
Aug_reg <- Aug %>%
  group_by(sex,New,reg) %>%
  summarise(UU = n(),
            get_good = mean(get_goods),
            send_good = mean(send_goods),
            match_send = mean(match_send),
            match_get = mean(match_gets),
            massages = mean(messeges))

Sep_reg <- Sep %>%
  group_by(sex,New,reg) %>%
  summarise(UU = n(),
            get_good = mean(get_goods),
            send_good = mean(send_goods),
            match_send = mean(match_send),
            match_get = mean(match_gets),
            massages = mean(messeges))

write_csv(Sep_reg,"_reg.csv")


May_new <- May_female %>%
  group_by(New) %>%
  summarise(avg_messeges = mean(messeges),
            sum_females = n()) #カテゴリーごとの平均、最大値、最小値
Jun_new <- Jun_female %>%
  group_by(New) %>%
  summarise(avg_messeges = mean(messeges),
            sum_females = n()) #カテゴリーごとの平均、最大値、最小値
Jul_new <- Jul_female %>%
  group_by(New) %>%
  summarise(avg_messeges = mean(messeges),
            sum_females = n()) #カテゴリーごとの平均、最大値、最小値
Aug_new <- Aug_female %>%
  group_by(New) %>%
  summarise(avg_messeges = mean(messeges),
            sum_females = n()) #カテゴリーごとの平均、最大値、最小値
Sep_new <- Sep_female %>%
  group_by(New) %>%
  summarise(avg_messeges = mean(messeges),
            sum_females = n()) #カテゴリーごとの平均、最大値、最小値

write.csv(Sep_new,"Sep_new.csv")

Aug_Sep_female <- Aug_female %>%
  inner_join(Sep_female, by = "id") #product内のProductIDを基準にくっつけた

View(Aug_Sep_female)

Aug_Sep_female %>%
  summarise(count =n())

colnames(Sep)

Sep_new <- Sep %>%
  group_by(New,sex) %>%
  summarise(count = n()) #カテゴリーごとの平均、最大値、最小値

Sep %>%
  group_by(sex,New) %>%
  summarise(count = n())