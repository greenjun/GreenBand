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

library(plotly) # interactive

library(robust) # robustness

library(ggforce) # zoom

library(leaflet) # map

setwd("C:/Project/GreenBand_Lpoint5/Source/") ; getwd()

source("Function/multiplot.R", encoding = "utf-8")

load(file="Code/R/tr_Digital_Trend_Analyzer.RData")

load(file="Code/R/Integrated_Product.RData")

load(file="Code/R/freq_index_material.RData")


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
# Step 2 : 데이터 전처리, 데이터 타입 처리, 이상치 처리, 데이터 통합
#=====================================================================================================================

summary(Product_original) ; str(Product_original) ; View(Product_original) ; sapply(Product_original, function(x){sum(is.na(x))})

summary(Search1_original) ; str(Search1_original) ; View(Search1_original) ; sapply(Search1_original, function(x){sum(is.na(x))})

summary(Search2_original) ; str(Search1_original) ; View(Search2_original) ; sapply(Search1_original, function(x){sum(is.na(x))})

summary(Custom_original) ; str(Custom_original) ; View(Custom_original) ; sapply(Custom_original, function(x){sum(is.na(x))})

summary(Session_original) ; str(Session_original) ; View(Session_original) ; sapply(Session_original, function(x){sum(is.na(x))})

summary(Master_original) ; str(Master_original) ; View(Master_original) ; sapply(Master_original, function(x){sum(is.na(x))})

## (공지) HITS_SEQ == 1 인 경우 분석에 고려하지 않는다

outlier <- Product_original[Product_original$HITS_SEQ==1,]

## Product data

tr_Product_original <- Product_original %>% 
  filter(HITS_SEQ != 1) %>%
  mutate(PD_BUY_AM = as.numeric(gsub(",","", PD_BUY_AM)),
         PD_BUY_CT = as.numeric(gsub(",","", PD_BUY_CT)))

## Search1 data

outlier_Search1_original <- left_join(outlier, Search1_original, by=c('CLNT_ID','SESS_ID')) ; sum(complete.cases(outlier_Search1_original))

outlier_detect_Search1_original <- data.frame(CLNT_ID = outlier_Search1_original[complete.cases(outlier_Search1_original),]$CLNT_ID,
                                              SESS_ID = outlier_Search1_original[complete.cases(outlier_Search1_original),]$SESS_ID,
                                              KWD_NM = outlier_Search1_original[complete.cases(outlier_Search1_original),]$KWD_NM,
                                              SEARCH_CNT = outlier_Search1_original[complete.cases(outlier_Search1_original),]$SEARCH_CNT)

outlier_detect_Search1_original$KWD_NM <- as.character(outlier_detect_Search1_original$KWD_NM)

outlier_detect_Search1_original_index <- c()

for(i in c(1:length(outlier_detect_Search1_original[,1]))){
  outlier_detect_Search1_original_index <- c(outlier_detect_Search1_original_index,
                                             which(Search1_original[,1] ==  outlier_detect_Search1_original[i,1] & 
                                                     Search1_original[,2] ==  outlier_detect_Search1_original[i,2] &
                                                     Search1_original[,3] ==  outlier_detect_Search1_original[i,3] &
                                                     Search1_original[,4] ==  outlier_detect_Search1_original[i,4]))
}

Search1_original <- Search1_original[-c(outlier_detect_Search1_original_index),] ; rownames(Search1_original) <- NULL

tr_Search1_original <- Search1_original

## Search2 data

tr_Search2_original <- Search2_original %>%
  mutate(SESS_DT = ymd(SESS_DT),
         SEARCH_CNT = as.numeric(gsub(",","", SEARCH_CNT)))

## Custom data

tr_Custom_original <- Custom_original %>%
  mutate(CLNT_AGE = as.factor(CLNT_AGE),
         CLNT_GENDER = as.factor(CLNT_GENDER))

## Session data

Session_original$TOT_SESS_HR_V[Session_original$TOT_SESS_HR_V == ""] <- NA

outlier_Session_original <- left_join(outlier, Session_original, by=c('CLNT_ID','SESS_ID')) ; sum(complete.cases(outlier_Session_original))

outlier_detect_Session_original <- NA
for(i in c(1,2,9:15)){
  outlier_detect_Session_original <- data.frame(outlier_detect_Session_original,
                                                outlier_Session_original[complete.cases(outlier_Session_original),][,i])
} ; outlier_detect_Session_original <- outlier_detect_Session_original[,-1] ; colnames(outlier_detect_Session_original) <- colnames(outlier_Session_original)[c(1,2,9:15)]

for(i in c(6,7,8,9)){
  outlier_detect_Session_original[,i] <- as.character(outlier_detect_Session_original[,i])
}

outlier_detect_Session_original_index <- c()
for(i in c(1:length(outlier_detect_Session_original[,1]))){
  outlier_detect_Session_original_index <- c(outlier_detect_Session_original_index,
                                             which(Session_original[,1] ==  outlier_detect_Session_original[i,1] & 
                                                     Session_original[,2] ==  outlier_detect_Session_original[i,2] &
                                                     Session_original[,3] ==  outlier_detect_Session_original[i,3] &
                                                     Session_original[,4] ==  outlier_detect_Session_original[i,4] &
                                                     Session_original[,5] ==  outlier_detect_Session_original[i,5] &
                                                     Session_original[,6] ==  outlier_detect_Session_original[i,6] &
                                                     Session_original[,7] ==  outlier_detect_Session_original[i,7] &
                                                     Session_original[,8] ==  outlier_detect_Session_original[i,8] &
                                                     Session_original[,9] ==  outlier_detect_Session_original[i,9]
                                             ))
}

Session_original <- Session_original[-c(outlier_detect_Session_original_index),] ; rownames(Session_original) <- NULL

tr_Session_original <- Session_original %>%
  mutate(SESS_DT = ymd(SESS_DT),
         TOT_SESS_HR_V = as.numeric(gsub(",","", TOT_SESS_HR_V)),
         DVC_CTG_NM = as.factor(DVC_CTG_NM),
         ZON_NM = as.factor(ZON_NM),
         CITY_NM = as.factor(CITY_NM))

## Master data

tr_Master_original <- Master_original[1:847652,]

## save RData

rm(list = c('Custom_original','Master_original','Product_original','Search1_original','Search2_original','Session_original',
            'outlier','outlier_Search1_original','outlier_detect_Search1_original_index','i','outlier_detect_Search1_original',
            'outlier_detect_Session_original','outlier_Session_original','outlier_detect_Session_original_index'))

save.image("C:/Project/GreenBand_Lpoint5/Source/Code/R/tr_Digital_Trend_Analyzer.RData")

## Integrated Product

Integrated_Product <-tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  left_join(tr_Search1_original, c('CLNT_ID','SESS_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
  
  select(CLNT_ID,
         SESS_ID, SESS_SEQ, SESS_DT, TOT_PAG_VIEW_CT, TOT_SESS_HR_V,
         KWD_NM, SEARCH_CNT,
         CLAC1_NM, CLAC2_NM, CLAC3_NM,
         HITS_SEQ, 
         PD_BUY_CT, PD_BUY_AM,
         CLNT_GENDER, CLNT_AGE, ZON_NM, CITY_NM, DVC_CTG_NM,
         PD_BRA_NM, PD_NM, PD_ADD_NM, PD_C) %>%
  
  mutate(CLAC1_NM = as.factor(CLAC1_NM),
         CLAC2_NM = as.factor(CLAC2_NM),
         CLAC3_NM = as.factor(CLAC3_NM)) %>%
  
  arrange(CLNT_ID, SESS_SEQ, SESS_DT)

## save RData

save(Integrated_Product, file = "C:/Project/GreenBand_Lpoint5/Source/Code/R/Integrated_Product.RData")

#=====================================================================================================================
# Step 3 : 탐색적 데이터 분석
#=====================================================================================================================

as.Date("2018-09-30") - as.Date("2018-04-01") + 1

############################# tr_Product_original #############################

# HITS_SEQ 의미 - 제품을 주문하기 전까지의 Session 내 히트수

summary(tr_Product_original) ; str(tr_Product_original) ; View(tr_Product_original) ; sapply(tr_Product_original, function(x){sum(is.na(x))})

View(
  tr_Product_original %>%
    
    arrange(desc(CLNT_ID))
)

View(
tr_Product_original %>%
  
  select(HITS_SEQ) %>%
  
  group_by(HITS_SEQ) %>%
  
  mutate(count = n()) %>% 
  
  distinct() %>%
  
  arrange(desc(count))
)

tr_Product_original %>%
  
  ggplot(aes(x=HITS_SEQ, fill = HITS_SEQ)) +
  
  geom_bar()

############################# tr_Master_original #############################

summary(tr_Master_original) ; str(tr_Master_original) ; View(tr_Master_original) ; sapply(tr_Master_original, function(x){sum(is.na(x))})

############################# tr_Product_original + tr_Master_original + tr_Search1_original #############################

tr_Product_Master_Search1_original <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID'))

summary(tr_Product_Master_Search1_original) ; str(tr_Product_Master_Search1_original) ; View(tr_Product_Master_Search1_original) ; sapply(tr_Product_Master_Search1_original, function(x){sum(is.na(x))})

View(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    arrange(CLNT_ID,SESS_ID)
)

# NA의 이유? 

# 0. 일반적으로 구매자의 경우 해당 세션에서 검색을 하고 구입을 했다면 NA가 없이 자료가 기록 된다.

# 1. 한 세션에서는 상품을 구매하고 다른 세션에서는 검색만 하고 구입하지 않은 경우이다.

# 2. 그런데 분석기간 내 상품을 구매한 방문자의 CLNT_ID인데도 불구하고 구매한 구매기록이 존재하지 않는경우가 있다. 

View(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    arrange(CLNT_ID,SESS_ID) %>%
    
    filter(CLNT_ID == 3392808)
)

View(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    arrange(CLNT_ID,SESS_ID) %>%
    
    filter(CLNT_ID == 14)
)

# 처리) 추후에 SEARCH_CNT 처리 할 때, 한 사람이 여러번 검색한 것은 1로 처리
# ans) 선호도를 파악 할 때, 한사람이 해당 상품에 가지고 있는 흥미도는 여러번 검색을 했다는 것으로 다른 상품군에 비해 흥미도가 높다고 
# 파악이 가능하지만, 수 많은 사람들을 대상으로 해당 상품에 대한 흥미도를 고려할때는 한 사람을 1로 처리하는 것이 타당해보인다. 

############################# tr_Product_original + tr_Master_original + tr_Search1_original + tr_Session_original #############################

View(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    left_join(tr_Session_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    arrange(CLNT_ID,SESS_ID) %>%
    
    filter(CLNT_ID == 3392808)
)

# 구매하지 않고 검색만 한 경우에 검색한 것에 대한 향후 구매 가능성이 있는가? 

# ans) CLNT_ID==56 꼭 그렇진 않다

View(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    left_join(tr_Session_original, c('CLNT_ID', 'SESS_ID')) %>%
    
    arrange(SESS_DT) %>%
    
    filter(CLNT_ID==56)
)

# ans) CLNT_ID==305 그런 경우도 있다

View(
tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  full_join(tr_Search1_original, c('CLNT_ID', 'SESS_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID', 'SESS_ID')) %>%
  
  arrange(SESS_DT) %>%
  
  filter(CLNT_ID==305)
)

############################## tr_Search2_original #############################

# 일반적으로 검색 패턴을 보면 브랜트 이름이나 제품의 포괄적인 이름으로 검색을함. 

summary(tr_Search2_original) ; str(tr_Search2_original) ; View(tr_Search2_original) ; sapply(tr_Search2_original, function(x){sum(is.na(x))})

# 과제) 일별로 검색어를 상품군으로 분류 후 해당 상품군의 검색량을 추적 ( 중요 )

# 과제) 구입된 상품과 검색된 상품군의 상관관계여부 파악 - count로 파악 - 당연히 있을것으로 파악 

boxplot(tr_Search1_original$SEARCH_CNT) ; median(tr_Search1_original$SEARCH_CNT)

# 하룻동안 많이 검색된 상품명 top 1000 -> 특정 기간에 폭발적인 관심을 보이는 상품 

View(
  tr_Search2_original %>%
    
    arrange(desc(SEARCH_CNT)) %>%
    
    head(1000)
)

# 분석 기간동안 많이 검색된 상품명 top 1000 -> 꾸준히 관심을 가지는 상품 

View(
  tr_Search2_original %>%
    
    group_by(KWD_NM) %>%
    
    count() %>%
    
    arrange(desc(n)) %>%
    
    head(1000)
)

############################# tr_Session_original  #############################

summary(tr_Session_original) ; str(tr_Session_original) ; View(tr_Session_original) ; sapply(tr_Session_original, function(x){sum(is.na(x))})

View(
  tr_Session_original %>%
    
    arrange(CLNT_ID, SESS_SEQ)
)

View(
  tr_Product_original %>%
    
    left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
    
    arrange(CLNT_ID, SESS_SEQ)
)

# TOT_PAG_VIEW_CT

tr_Session_original %>%
  
  group_by(TOT_PAG_VIEW_CT) %>%
  
  count() %>%
  
  ggplot(aes(x = TOT_PAG_VIEW_CT, y = n)) +
  
  geom_col(aes(fill = n)) +
  
  labs(x = "",
       y = "Count",
       title = "총 페이지 조회건수",
       subtitle = "",
       caption = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        axis.text.y = element_blank())

# TOT_SESS_HR_V

tr_Session_original %>%
  
  group_by(TOT_SESS_HR_V) %>%
  
  count() %>%
  
  ggplot(aes(x = TOT_SESS_HR_V, y = n)) +
  
  geom_col(aes(fill = n)) +

  labs(x = "",
       y = "Count",
       title = "총 세션 시간 값",
       subtitle = "",
       caption = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        axis.text.y = element_blank())

## TOT_PAG_VIEW_CT, TOT_SESS_HR_V 의 상관 관계

plot(tr_Session_original$TOT_PAG_VIEW_CT, tr_Session_original$TOT_SESS_HR_V)

cor.test(tr_Session_original$TOT_PAG_VIEW_CT, tr_Session_original$TOT_SESS_HR_V, method = 'pearson')

# the Orthogonalized Quadrant Correlation estimator

data.frame(tr_Session_original$TOT_PAG_VIEW_CT, tr_Session_original$TOT_SESS_HR_V) %>%
  
  covRob(corr = T) 

############################## tr_Custom_original #############################

summary(tr_Custom_original) ; str(tr_Custom_original) ; View(tr_Custom_original) ; sapply(tr_Custom_original, function(x){sum(is.na(x))})

# 상품을 구매하는 고객의 나이와 성별 분포 

p1 <- tr_Custom_original %>%
  
  group_by(CLNT_AGE,CLNT_GENDER) %>%
  
  count() %>%
  
  ggplot(aes(x = CLNT_AGE, y = n, fill = CLNT_GENDER)) +
  
  geom_col(position = 'dodge') +

  labs(x = "Age",
       y = "Count",
       title = "상품을 구매한 방문자의 나이대별 성별 분포",
       subtitle = "",
       caption = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

############################## tr_Product_original + tr_Master_original + tr_Custom_original #############################

tr_product_Master_Custom_original <-  tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID'))
  
summary(tr_product_Master_Custom_original) ; str(tr_product_Master_Custom_original) ; View(tr_product_Master_Custom_original) ; sapply(tr_product_Master_Custom_original, function(x){sum(is.na(x))})


# 어느 나잇대 성별이 가장 돈이 될까? (추후 이익률을 고려)
# ans) 30대 여성, 40대 여성, 20대 여성, 40대 남성, 50대 여성 순 ..

p2 <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER)) %>%
  
  group_by(CLNT_AGE, CLNT_GENDER) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = CLNT_AGE, y = money)) +
  
  geom_col(aes(fill = CLNT_GENDER), position = 'dodge') +

  labs(x = "Age",
       y = "Total sales amount",
       title = "판매액이 가장 많은 집단",
       subtitle = "",
       caption = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

# CUSTOM 정보 누락자 판매액
# ans) 5,859,143,869

tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(is.na(CLNT_GENDER)) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT))

# 방문자/판매액 비율

p3 <- tr_Custom_original %>%
  
  group_by(CLNT_AGE,CLNT_GENDER) %>%
  
  count() %>%
  
  full_join(tr_Product_original %>%
              
              left_join(tr_Master_original, c('PD_C')) %>%
              
              left_join(tr_Custom_original, c('CLNT_ID')) %>%
              
              filter(!is.na(CLNT_GENDER)) %>%
              
              group_by(CLNT_AGE, CLNT_GENDER) %>%
              
              summarise(money = sum(PD_BUY_AM * PD_BUY_CT)), c('CLNT_AGE','CLNT_GENDER')) %>%
  
  mutate(Sales_to_visitors = money/n) %>%
  
  ggplot(aes(x = CLNT_AGE, y = Sales_to_visitors)) +
  
  geom_col(aes(fill = CLNT_GENDER), position = 'dodge') +
  
  labs(x = "Age",
       y = "Sales_to_visitors",
       title = "나이대별 방문자 당 판매액",
       subtitle = "",
       caption = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

layout <- matrix(c(1,1,2,2,3,3,
                   1,1,2,2,3,3),
                 2,6,byrow=TRUE)

multiplot(p1, p2, p3, layout=layout)

# 어느 상품군이 가장 돈이 될까?

CLAC1_NM_top10_plot <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  group_by(CLAC1_NM) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  top_n(10, money) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLAC1_NM, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLAC1_NM), position = 'dodge') +
  
  labs(x = "",
       y = "",
       title = "CLAC1 수익이 가장 많이 나는  상품군",
       subtitle = "",
       caption = "")  +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

CLAC2_NM_top15_plot <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  group_by(CLAC2_NM) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  top_n(15, money) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLAC2_NM, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLAC2_NM), position = 'dodge') +
  
  labs(x = "",
       y = "",
       title = "CLAC2 수익이 가장 많이 나는  상품군",
       subtitle = "",
       caption = "")  +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

CLAC3_NM_top20_plot <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  group_by(CLAC3_NM) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  top_n(20, money) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLAC3_NM, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLAC3_NM), position = 'dodge') +
  
  labs(x = "",
       y = "",
       title = "CLAC3 수익이 가장 많이 나는  상품군",
       subtitle = "",
       caption = "")  +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

layout <- matrix(c(1,1,2,2,3,3,
                   1,1,2,2,3,3),
                 2,6,byrow=TRUE)

multiplot(CLAC1_NM_top10_plot, CLAC2_NM_top15_plot, CLAC3_NM_top20_plot, layout=layout)

ggplotly(
  tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    filter(CLAC2_NM %in% c(tr_Product_original %>%
                             
                             left_join(tr_Master_original, c('PD_C')) %>%
                             
                             group_by(CLAC2_NM) %>%
                             
                             summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
                             
                             arrange(desc(money)) %>%
                             
                             top_n(10, money) %>% 
                             
                             select(CLAC2_NM) %>%
                             
                             simplify())) %>%
    
    group_by(CLAC2_NM, CLAC3_NM) %>%
    
    summarise(money = sum(PD_BUY_AM * PD_BUY_CT))  %>%
    
    ggplot(aes(x = reorder(CLAC2_NM, money, FUN = max), y = money)) +
    
    geom_col(aes(fill = reorder(CLAC3_NM, money, FUN = max)), position = 'stack') +
    
    labs(x = "",
         y = "",
         title = "CLAC2 수익이 가장 많이 나는  상품군",
         subtitle = "",
         caption = "") +
    
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
          axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
          legend.position = 'none')
)

# CLAC2 성별 나잇대별 구입 상품군

CLAC2_NM_F <- list()
for(i in seq(10,80,10)){
  
  CLAC2_NM_F[[i/10]] <- tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    left_join(tr_Custom_original, c('CLNT_ID')) %>%
    
    filter(!is.na(CLNT_GENDER) & CLNT_GENDER == 'F' & CLNT_AGE == paste0(i)) %>%
    
    group_by(CLAC2_NM) %>%
    
    summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
    
    top_n(15, money) %>%
    
    arrange(desc(money)) %>%
    
    ggplot(aes(x = reorder(CLAC2_NM, money, FUN = max), y = money)) +
    
    geom_col(aes(fill = CLAC2_NM), position = 'dodge') +
    
    labs(x = "",
         y = "",
         title = paste0("CLAC2"," ",i,"대 여성의 구입 상품군"),
         subtitle = "",
         caption = "")+
    
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
          axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
          legend.position = 'none')
  
  }
CLAC2_NM_F[[1]]


CLAC2_NM_M <- list()
for(i in seq(10,80,10)){
  
  CLAC2_NM_M[[i/10]] <- tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    left_join(tr_Custom_original, c('CLNT_ID')) %>%
    
    filter(!is.na(CLNT_GENDER) & CLNT_GENDER == 'M' & CLNT_AGE == paste0(i)) %>%
    
    group_by(CLAC2_NM) %>%
    
    summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
    
    top_n(15, money) %>%
    
    arrange(desc(money)) %>%
    
    ggplot(aes(x = reorder(CLAC2_NM, money, FUN = max), y = money)) +
    
    geom_col(aes(fill = CLAC2_NM), position = 'dodge') +
    
    labs(x = "",
         y = "",
         title = paste0("CLAC2"," ",i,"대 남성의 구입 상품군"),
         subtitle = "",
         caption = "") +
    
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
          axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
          legend.position = 'none')
  
}
CLAC2_NM_M[[1]]

## 선호하는 상품군을 남성과 여성으로 같이 비교

p1 <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER)) %>%
  
  group_by(CLAC1_NM, CLNT_GENDER, CLNT_AGE) %>%
  
  count() %>%
  
  ggplot(aes(x = reorder(CLAC1_NM, n, FUN = max), y = n)) +
  
  geom_col(aes(fill = CLNT_GENDER), position='stack') +
  
  facet_grid(~ CLNT_AGE) +
  
  coord_flip() +
  
  labs(x = "",
       y = "",
       title = "성별 나이별 상품군",
       subtitle = "",
       caption = "") + 
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

age_plot <- list()
for(i in seq(10,80,10)){

  age_plot[[i/10]] <- tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    left_join(tr_Custom_original, c('CLNT_ID')) %>%
    
    filter(!is.na(CLNT_GENDER), CLNT_AGE == paste0(i)) %>%
  
    group_by(CLAC1_NM, CLNT_GENDER) %>%
    
    count() %>%
    
    ggplot(aes(x = reorder(CLAC1_NM, n, FUN = max), y = n)) +
   
    geom_col(aes(fill = CLNT_GENDER), position='stack') +
    
    geom_label(aes(label = n, colour = CLNT_GENDER), position = position_stack(vjust = 1)) +
      
    coord_flip() +
    
    labs(x = "",
         y = "",
         title = paste0(i, "대 성별 선호하는 상품군"),
         subtitle = "",
         caption = "") +
    
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
          axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))
  }

layout <- matrix(c(1,1,2,2,
                   1,1,2,2),
                 2,4,byrow=TRUE)

multiplot(p1, age_plot[[2]], layout=layout)

# 미상 정보의 결측치를 채워보려고 하였으나 남성이라는 키워드가 들어가는 쪽에보면 오히려 여성의 구매빈도가 높다 따라서 키워드를 기준으로 미상정보를 채우는 것은 타탕해보이지 않는다. 

## 좌표 데이터 추가 

tr_Product_Master_Custom_Session_original <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>% 
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>% 
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID'))

ZON_NM_lat_long <- read.csv("Data/ZON_NM_lat_long.csv", header = T, encoding = "UTF-8")

ZON_NM_lat_long$CITY_NM <- as.character(ZON_NM_lat_long$CITY_NM)

tr_Product_Master_Custom_Session_original$CITY_NM <- as.character(tr_Product_Master_Custom_Session_original$CITY_NM)

# mapping_data_all + CLAC_1

mapping_data_all  <- tr_Product_Master_Custom_Session_original %>%
  
  filter(!is.na(CITY_NM) & !is.na(CLNT_GENDER)) %>%
  
  group_by(CITY_NM, CLAC1_NM) %>%
  
  count() %>%
  
  left_join(ZON_NM_lat_long, c('CITY_NM')) %>%
  
  filter(!is.na(latitude))

mapping_data_all_top5 <- mapping_data_all %>% 
  
  arrange(CITY_NM, desc(n)) %>%
  
  select(CITY_NM, CLAC1_NM, n) %>%
  
  group_by(CITY_NM) %>%
  
  top_n(5, n)

factpal <- colorFactor(topo.colors(5), mapping_data_all$CLAC1_NM)

mapping_data_all_map <- mapping_data_all %>%
  
  leaflet() %>%
  
  addTiles() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  addCircleMarkers(~longitude, ~latitude,
                   
                   fillOpacity = 0.5, color = ~factpal(CLAC1_NM), radius = ~sqrt(n)*3,
                   
                   label = ~CLAC1_NM, popup = ~as.character(n),
                   
                   clusterOptions = markerClusterOptions(),
                   
                   clusterId = 'CLAC1_NM') %>%
  
  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="unfrozen-markers",
        icon="ion-toggle",
        title="Freeze Clusters",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'CLAC1_NM');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
      ),
      easyButtonState(
        stateName="frozen-markers",
        icon="ion-toggle-filled",
        title="UnFreeze Clusters",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'CLAC1_NM');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
      )
    )
  ))

# mapping_data_F + CLAC_1, 지역별 상위 5개, 지도 

mapping_data_F <- list()
mapping_data_F_top5 <- list()
mapping_data_F_map <- list()
for(i in seq(10,80,10)){
  mapping_data_F[[i]]  <- tr_Product_Master_Custom_Session_original %>%
    
    filter(!is.na(CITY_NM) & !is.na(CLNT_GENDER)  & CLNT_GENDER=='F' & CLNT_AGE==paste0(i)) %>%
    
    group_by(CITY_NM, CLAC1_NM) %>%
    
    count() %>%
    
    left_join(ZON_NM_lat_long, c('CITY_NM')) %>%
    
    filter(!is.na(latitude))
  
  mapping_data_F_top5[[i]] <- mapping_data_F[[i]] %>% 
    
    arrange(CITY_NM, desc(n)) %>%
    
    select(CITY_NM, CLAC1_NM, n) %>%
    
    group_by(CITY_NM) %>%
    
    top_n(5, n)
  
  factpal <- colorFactor(topo.colors(5), mapping_data_F[[i]]$CLAC1_NM)
  
  mapping_data_F_map[[i]] <- mapping_data_F[[i]] %>%
    
    leaflet() %>%
    
    addTiles() %>%
    
    addProviderTiles("CartoDB.Positron") %>%
    
    addCircleMarkers(~longitude, ~latitude,
                     
                     fillOpacity = 0.5, color = ~factpal(CLAC1_NM), radius = ~sqrt(n)*3,
                     
                     label = ~CLAC1_NM, popup = ~as.character(n),
                     
                     clusterOptions = markerClusterOptions(),
                     
                     clusterId = 'CLAC1_NM') %>%
    
    addEasyButton(easyButton(
      states = list(
        easyButtonState(
          stateName="unfrozen-markers",
          icon="ion-toggle",
          title="Freeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'CLAC1_NM');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
        ),
        easyButtonState(
          stateName="frozen-markers",
          icon="ion-toggle-filled",
          title="UnFreeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'CLAC1_NM');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
        )
      )
    ))
}

mapping_F <- function(x){
  print(mapping_data_F[[x]])
  print(mapping_data_F_top5[[x]])
  mapping_data_F_map[[x]]
} 

# mapping_data_M + CLAC_1, 지역별 상위 5개, 지도 

mapping_data_M <- list()
mapping_data_M_top5 <- list()
mapping_data_M_map <- list()
for(i in seq(10,80,10)){
  mapping_data_M[[i]]  <- tr_Product_Master_Custom_Session_original %>%
    
    filter(!is.na(CITY_NM) & !is.na(CLNT_GENDER)  & CLNT_GENDER=='M' & CLNT_AGE==paste0(i)) %>%
    
    group_by(CITY_NM, CLAC1_NM) %>%
    
    count() %>%
    
    left_join(ZON_NM_lat_long, c('CITY_NM')) %>%
    
    filter(!is.na(latitude))
  
  mapping_data_M_top5[[i]] <- mapping_data_M[[i]] %>% 
    
    arrange(CITY_NM, desc(n)) %>%
    
    select(CITY_NM, CLAC1_NM, n) %>%
    
    group_by(CITY_NM) %>%
    
    top_n(5, n)
  
  factpal <- colorFactor(topo.colors(5), mapping_data_M[[i]]$CLAC1_NM)
  
  mapping_data_M_map[[i]] <- mapping_data_M[[i]] %>%
    
    leaflet() %>%
    
    addTiles() %>%
    
    addProviderTiles("CartoDB.Positron") %>%
    
    addCircleMarkers(~longitude, ~latitude,
                     
                     fillOpacity = 0.5, color = ~factpal(CLAC1_NM), radius = ~sqrt(n)*3,
                     
                     label = ~CLAC1_NM, popup = ~as.character(n),
                     
                     clusterOptions = markerClusterOptions(),
                     
                     clusterId = 'CLAC1_NM') %>%
    
    addEasyButton(easyButton(
      states = list(
        easyButtonState(
          stateName="unfrozen-markers",
          icon="ion-toggle",
          title="Freeze Clusters",
          onClick = JS("
                       function(btn, map) {
                       var clusterManager =
                       map.layerManager.getLayer('cluster', 'CLAC1_NM');
                       clusterManager.freezeAtZoom();
                       btn.state('frozen-markers');
                       }")
        ),
        easyButtonState(
          stateName="frozen-markers",
          icon="ion-toggle-filled",
          title="UnFreeze Clusters",
          onClick = JS("
                       function(btn, map) {
                       var clusterManager =
                       map.layerManager.getLayer('cluster', 'CLAC1_NM');
                       clusterManager.unfreeze();
                       btn.state('unfrozen-markers');
                       }")
        )
          )
          ))
  }

mapping_M <- function(x){
  print(mapping_data_M[[x]])
  print(mapping_data_M_top5[[x]])
  mapping_data_M_map[[x]]
} 

mapping_data_all
mapping_data_all_top5
mapping_data_all_map

mapping_M(50)

mapping_F(30)

# 어느 지역에만 특별히 잘팔리는 물건이 있을까?

# 지역 별 홍보 전략 세우기(지역의 기후, 홍보 등등이 영향을 끼쳤을 것으로 생각 ) 

# 그 기간에 홍보는 어떻게 했는가? # 판촉의 결과로 판매량으로 이어지지는 않았는가?

#=====================================================================================================================
# Step 4 :  상품군 별 온라인 선호 지수 개발 
#=====================================================================================================================

# 상품군별 온라인 선호 지수란 어떤 사람이 제품을 구매하고 난 이력을 바탕으로 상품군들에 대한 그 사람의 선호하는 정도를 이야기하는 지표이다.

# 상품군별 온라인 선호 지수 

# 일주일 간의 데이터를 바탕으로 
# CLAC1의 상품군을 기준으로
# 해당 상품군의 구매 빈도, 검색빈도를 통해 해당 상품군의 선호정도를 지수로 나타낸다.

CLAC2당 구매수와 CLAC2 개수를 기반으로 최다 구매수를 얻은 상품군의 선호도를 반영한
지수를 통해 일주일간의 어떤 상품 군이 구매자들에게 가장 선호도를 가지는지 파악가능. 지수는 다음을 반영

1) 	일주일 간 CLAC2 상품군 당 평균 구매빈도수, max 빈도수 

2)  CLAC2의 일주일간 비구매 항목은 감점요인 

3) 	평균 CLAC2 구매수는 핵심 CLAC2로 가산요인으로 설정

4) 	최다 구매수 CLAC2는 핵심 CLAC2로 가산 요인으로 설정

5)  CLAC2에서 고른 선호를 받는 상품군(CLAC1)일수록 구매자들들이 선호하는 상품군이다. 

week_mean_max <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
  
  filter(!is.na(SESS_DT)) %>%
  
  mutate(week = week(SESS_DT),
         month = month(SESS_DT),
         count = 1) %>%
  
  group_by(week, CLAC1_NM, CLAC2_NM) %>%
  
  summarise(CLAC2_week_mean = round(sum(count)/7,3)) %>%
  
  left_join(tr_Product_original %>%
              
              left_join(tr_Master_original, c('PD_C')) %>%
              
              left_join(tr_Custom_original, c('CLNT_ID')) %>%
              
              left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
              
              filter(!is.na(SESS_DT)) %>%
              
              mutate(week = week(SESS_DT),
                     month = month(SESS_DT)) %>%
              
              group_by(week, CLAC1_NM, CLAC2_NM) %>%
              
              count() %>%
              
              group_by(week, CLAC1_NM) %>%
              
              mutate(max = max(n)) %>%
              
              filter(n == max) %>%
              
              select(week, CLAC1_NM, CLAC2_NM, max), c('week', 'CLAC1_NM', 'CLAC2_NM')) %>%
  
  group_by(week, CLAC1_NM) %>%
  
  summarise(mean = mean(CLAC2_week_mean)) %>%
  
  left_join(
    
    tr_Product_original %>%
      
      left_join(tr_Master_original, c('PD_C')) %>%
      
      left_join(tr_Custom_original, c('CLNT_ID')) %>%
      
      left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
      
      filter(!is.na(SESS_DT)) %>%
      
      mutate(week = week(SESS_DT),
             month = month(SESS_DT),
             count = 1) %>%
      
      group_by(week, CLAC1_NM, CLAC2_NM) %>%
      
      summarise(CLAC2_week_mean = round(sum(count)/7,3)) %>%
      
      left_join(tr_Product_original %>%
                  
                  left_join(tr_Master_original, c('PD_C')) %>%
                  
                  left_join(tr_Custom_original, c('CLNT_ID')) %>%
                  
                  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
                  
                  filter(!is.na(SESS_DT)) %>%
                  
                  mutate(week = week(SESS_DT),
                         month = month(SESS_DT)) %>%
                  
                  group_by(week, CLAC1_NM, CLAC2_NM) %>%
                  
                  count() %>%
                  
                  group_by(week, CLAC1_NM) %>%
                  
                  mutate(max = max(n)) %>%
                  
                  filter(n == max) %>%
                  
                  select(week, CLAC1_NM, CLAC2_NM, max), c('week', 'CLAC1_NM', 'CLAC2_NM')) %>%
      
      filter(!is.na(max)) %>%
      
      select(week, CLAC1_NM, max) %>%
      
      mutate(max = log(max)+1),
    
    c('week', 'CLAC1_NM'))

week_no_sale <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
  
  filter(!is.na(SESS_DT)) %>%
  
  mutate(week = week(SESS_DT),
         month = month(SESS_DT),
         count = 1) %>%
  
  group_by(week, CLAC1_NM, CLAC2_NM) %>%
  
  summarise(CLAC2_week_mean = round(sum(count)/7,3)) %>%
  
  select(week, CLAC1_NM, CLAC2_NM) %>%
  
  group_by(week, CLAC1_NM) %>%
  
  count() %>%
  
  left_join(tr_Master_original %>%
              
              select(CLAC1_NM, CLAC2_NM) %>%
              
              group_by(CLAC1_NM, CLAC2_NM) %>%
              
              count() %>%
              
              arrange(CLAC1_NM) %>%
              
              select(CLAC1_NM, CLAC2_NM) %>%
              
              group_by(CLAC1_NM) %>%
              
              summarise(count = n()),
            
            c('CLAC1_NM')) %>%
  
  mutate(diff = count - n)

freq_index_material <- week_mean_max %>%
  
  left_join(week_no_sale, c('week','CLAC1_NM')) %>%
  
  mutate(no_sale_pop = diff/count)

save(freq_index_material, file = "C:/Project/GreenBand_Lpoint5/Source/Code/R/freq_index_material.RData")

freq_index <- freq_index_material %>%
  
  mutate(index = (0.8*mean + 0.2*max)*(1-no_sale_pop)) %>%
  
  select(week, CLAC1_NM, index)

save(freq_indexl, file = "C:/Project/GreenBand_Lpoint5/Source/Code/R/freq_index.RData")


#=====================================================================================================================
# Step 5 :  수요 트렌트 예측 
#=====================================================================================================================

time_original <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) %>%
  
  filter(!is.na(SESS_DT))


library(ggthemes)

ggplotly(
time_original %>%
  
  group_by(SESS_DT, CLAC1_NM) %>%
  
  count() %>%
  
  ggplot(aes(x = SESS_DT, y = n)) +
  
  geom_line(size = 0.5, aes(color = CLAC1_NM)) +
  
  facet_wrap(~ CLAC1_NM) +
  
  labs(title = 'CLAC1의 세션 날짜별 상품 판매량') +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')
)  

