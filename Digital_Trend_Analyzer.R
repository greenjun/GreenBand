#=====================================================================================================================
# 2018-12-13
# 
# topic : Digital Trend Analyzer
# 
# Capyright 2018 by GreenBand
#=====================================================================================================================
# Step 0 : 분석 환경 설정
#=====================================================================================================================

library(tidyverse) # Preprocessing, Visualizing

library(lubridate) # time data

library(knitr) # Tidy document

library(ggrepel) # ggplot tidy text

setwd("C:/Project/GreenBand_Lpoint5/Source/") ; getwd()

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

save.image("C:/Project/GreenBand_Lpoint5/Source/Code/R/online_preference_index_data.RData")

## load RData

load(file="Code/R/Digital_Trend_Analyzer.RData")

#=====================================================================================================================
# Step 2 : 데이터 전처리
#=====================================================================================================================

summary(Product_original) ; str(Product_original) ; View(Product_original) ; sapply(Product_original, function(x){sum(is.na(x))})

summary(Search1_original) ; str(Search1_original) ; View(Search1_original) ; sapply(Search1_original, function(x){sum(is.na(x))})

summary(Search2_original) ; str(Search1_original) ; View(Search2_original) ; sapply(Search1_original, function(x){sum(is.na(x))})

summary(Custom_original) ; str(Custom_original) ; View(Custom_original) ; sapply(Custom_original, function(x){sum(is.na(x))})

summary(Session_original) ; str(Session_original) ; View(Session_original) ; sapply(Session_original, function(x){sum(is.na(x))})

summary(Master_original) ; str(Master_original) ; View(Master_original) ; sapply(Master_original, function(x){sum(is.na(x))})

## Product data

tr_Product_original <- Product_original %>% 
  mutate(PD_BUY_AM = as.numeric(gsub(",","", PD_BUY_AM)),
         PD_BUY_CT = as.numeric(gsub(",","", PD_BUY_CT)))

# Product data$PD_ADD_NM -> 추후 처리

## Search1 data

tr_Search1_original <- Search1_original

## Search2 data

tr_Search2_original <- Search2_original %>%
  mutate(SESS_DT = ymd(SESS_DT),
         SEARCH_CNT = as.numeric(gsub(",","", SEARCH_CNT)))

## Custom data

tr_Custom_original <- Custom_original %>%
  mutate(CLNT_AGE = as.factor(CLNT_AGE))

## Session data

Session_original$TOT_SESS_HR_V[Session_original$TOT_SESS_HR_V == ""] <- NA

tr_Session_original <- Session_original %>%
  mutate(SESS_DT = ymd(SESS_DT),
         TOT_SESS_HR_V = as.numeric(gsub(",","", TOT_SESS_HR_V)),
         DVC_CTG_NM = as.factor(DVC_CTG_NM),
         ZON_NM = as.factor(ZON_NM),
         CITY_NM = as.factor(CITY_NM))

## Master data

tr_Master_original <- Master_original[1:847652,]


## Save tr_data

rm(list = c('Custom_original','Master_original','Product_original','Search1_original','Search2_original','Session_original'))

## save RData

save.image("C:/Project/GreenBand_Lpoint5/Source/Code/R/tr_Digital_Trend_Analyzer.RData")

## load RData

load(file="Code/R/tr_Digital_Trend_Analyzer.RData")

#=====================================================================================================================
# Step 3 : 탐색적 데이터 분석 
#=====================================================================================================================


































# HITS_SEQ는 상품 구매가 이루어지기 전 까지 Session 내 활동들이 얼마나 되는지 확인

qplot(y = Product_original$HITS_SEQ, geom=c('boxplot'))

# median 49번 정도의 세션 내 클릭하게 된다.


## Product_original data + Search1_original data

Product_Search1_original <- full_join(Product_original, Search1_original, c('CLNT_ID', 'SESS_ID')) ; View(Product_Search1_original)

summary(Product_Search1_original) ; str(Product_Search1_original) ; View(Product_Search1_original)

sapply(Product_Search1_original, function(x){sum(is.na(x))})

# Product의 NA는 구매한 방문자인데 검색을 했지만, 구매를 안헀다?? 모순.. 뭐지  ?? 

# Search1의  NA는 검색하지 않고 바로구입한 사람 

##############################

## Search2_original data

# 데이터 부피가 너무 커서 샘플로 확인



# 날짜 데이터 처리, 숫자로 전환 

## 총 검색량 흐름
View(
  Search2_original %>%
    group_by(SESS_DT) %>%
    mutate(count = sum(SEARCH_CNT))
  )

# 같은 날짜의 검색어 통합

# 검색어의 분류 - 상품군

# 검색량을 바탕으로 지수에 포함

# 주의할 것은 한 사람이 여러번 검색한 것은 어떻게 처리? 

##############################


summary(Custom_original) ; str(Search2_original) ; View(Custom_original)




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

