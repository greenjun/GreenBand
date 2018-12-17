#=====================================================================================================================
# 2018-12-13
# 
# topic : Online preference index
# 
# Capyright 2018 by Yun Junhyuck
#=====================================================================================================================
# Step 0 : 분석 환경 설정
#=====================================================================================================================

library(dplyr)

library(ggplot2)

library(ggrepel)

library(readr)

library(stringr)

setwd("C:/Users/YunJunHyuck/Desktop/lpoint/lpoint_5/") ; getwd()

load(file="Code/R/Digital_Trend_Analyzer.RData")

#=====================================================================================================================
# Step 1 : 분석 데이터 호출
#===================================================================================================================== 

## Search1 data

Search1_original <- read.csv("Data/Search1_original.csv", header = T, encoding = "UTF-8")

Search1_original[is.na(Search1_original$SEARCHCNT),5] <- Search1_original[!is.na(Search1_original$X),4]

Search1_original <- Search1_original[,-4]

colnames(Search1_original) <- c("CLNT_ID", "SESS_ID", "KWD_NM", "SEARCH_CNT")

Search1_original$KWD_NM <- gsub(pattern = ",", replacement = "", Search1_original$KWD_NM)

## Session data

Session_original <- read.csv("Data/Session_original.csv", stringsAsFactors = F, header = T, encoding = "UTF-8")

colnames(Session_original) <- c("CLNT_ID", "SESS_ID", "SESS_SEQ", "SESS_DT", "TOT_PAG_VIEW_CT", "TOT_SESS_HR_V", "DVC_CTG_NM", "ZON_NM", "CITY_NM")

sapply(Session_original, function(x){sum(is.na(x))})

which(Session_original$TOT_SESS_HR_V == "")

## Custom data

Custom_original <- read.csv("Data/Custom_original.csv", stringsAsFactors = F, header = T, encoding = "UTF-8")

colnames(Custom_original) <- c("CLNT_ID", "CLNT_GENDER", "CLNT_AGE" )

## Master data - due to special character, moved data to python

Master_original <- read.csv("Data/python_Master_original.csv", stringsAsFactors = F, header = T)

Master_original <- Master_original[c(1:847652),]

## Product data

Product_original <- read.csv("Data/python_Product_original.csv", stringsAsFactors = F, header = T)

## Search2

Search2_original <- read.csv("Data/python_Search2_original.csv", stringsAsFactors = F, header = T)

## save RData

# save.image("C:/Users/YunJunHyuck/Desktop/lpoint/code/R/online_preference_index_data.RData")

## load RData

## load(file="code/R/Online_preference_index_data.RData")

#=====================================================================================================================
# Step 2 : 데이터 전처리
#=====================================================================================================================











#=====================================================================================================================
# Step 3 : 탐색적 데이터 분석 
#=====================================================================================================================

# one by one data analysis

summary(Custom_original)

summary(Master_original)

summary(Product_original)

summary(Session_original) ; View(Session_original)

View(Session_original %>%
  arrange(SESS_SEQ, SESS_DT))






ggplot(Custom_original, aes(x = CLNT_AGE, fill = CLNT_GENDER)) +
  
  geom_bar(stat='count', position='stack') +
  
  geom_label_repel(stat='count', aes(label=..count..)) +
  
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80)) +
  
  ggtitle("사이트 접속 고객 나이대별 성별 분포") + 
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        axis.text.y = element_blank())

