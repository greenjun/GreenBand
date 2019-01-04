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
# Step 2 : 데이터 전처리-1, 데이터 타입 처리, 이상치 처리 
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

#=====================================================================================================================
# Step 3 : 탐색적 데이터 분석-1
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

############################# tr_Session_original  #############################

summary(tr_Session_original) ; str(tr_Session_original) ; View(tr_Session_original) ; sapply(tr_Session_original, function(x){sum(is.na(x))})

View(
  tr_Session_original %>%
    
    arrange(CLNT_ID, SESS_SEQ)
)

View(
  tr_Session_original %>%
    
    arrange(CLNT_ID, SESS_SEQ) %>%
    
    left_join(tr_Product_original, c('CLNT_ID','SESS_ID'))
)

# NA의 이유? 

# ans) 





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






############################## tr_Custom_original #############################

summary(tr_Custom_original) ; str(tr_Custom_original) ; View(tr_Custom_original)

# 상품을 구매하는 고객의 나이와 성별 분포 

ggplot(tr_Custom_original, aes(x = CLNT_AGE, fill = CLNT_GENDER)) +
  
  geom_bar(stat='count', position='dodge') +
  
  geom_label_repel(stat='count', aes(label=..count..)) +
  
  ggtitle("사이트 접속 고객 나이대별 성별 분포") + 
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        axis.text.y = element_blank())

############################# tr_Product_original + tr_Custom_original #############################

tr_Product_Customer_original <- left_join(tr_Product_original, tr_Custom_original, c('CLNT_ID'))

summary(tr_Product_Customer_original) ; str(tr_Product_Customer_original) ; View(tr_Product_Customer_original) ; sapply(tr_Product_Customer_original, function(x){sum(is.na(x))})

View(
  tr_Product_Customer_original %>%
    arrange(CLNT_ID)
)

# tr_Product_original의  NA는 방문자의 상품 구매 정보인데, 성별 연령 정보는 있지만 구매이력이 남지 않음. why?

# tr_Custom_original의 NA는 상품 구매자이지만 미상정보제외로, 상품 구매이력만이 남은 경우 

# 나이와 성별로 선호하는 상품군을 파악 가능 


############################# tr_Product_original + tr_Session_original #############################

tr_Product_Session_original <- full_join(tr_Product_original, tr_Session_original, c('CLNT_ID','SESS_ID'))

summary(tr_Product_Session_original) ; str(tr_Product_Session_original) ; View(tr_Product_Session_original) ; sapply(tr_Product_Session_original, function(x){sum(is.na(x))})

View(
  tr_Product_Session_original %>%
    arrange(CLNT_ID)
)

# tr_Product_original의  NA는 방문자의 상품 구매 정보인데, 세션 정보는 있지만 구매이력이 남지 않음. why?

# tr_Session_original의 NA는 상품 구매자이지만 미상정보제외로, 상품 구매이력만이 남은 경우 

############################# tr_Master_original #############################

summary(tr_Master_original) ; str(tr_Master_original) ; View(tr_Master_original) ; sapply(tr_Master_original, function(x){sum(is.na(x))})

############################# tr_Product_original + tr_Master_original #############################

tr_Product_Master_original <- left_join(tr_Product_original, tr_Master_original, c('PD_C'))

summary(tr_Product_Master_original) ; str(tr_Product_Master_original) ; View(tr_Product_Master_original) ; sapply(tr_Product_Master_original, function(x){sum(is.na(x))})

# 상품 부가 정보에는 색과 크기가 포함, 상품군별 선호지수를 파악하는데 큰 도움이 안될꺼같음. 

# ? 색 추출을 통해 선호하는 색의 변화를 보는게 가능할까

#=====================================================================================================================
# Step 4 : 데이터 전처리-2
#=====================================================================================================================

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

## load RData

load(file="Code/R/Integrated_Product.RData")

#=====================================================================================================================
# Step 3 : 탐색적 데이터 분석-2 
#=====================================================================================================================

summary(Integrated_Product) ; str(Integrated_Product) ; View(Integrated_Product) ; sapply(Integrated_Product, function(x){sum(is.na(x))})

## TOT_PAG_VIEW_CT, TOT_SESS_HR_V 의 상관 관계

plot(Integrated_Product$TOT_PAG_VIEW_CT, Integrated_Product$TOT_SESS_HR_V)

# the Orthogonalized Quadrant Correlation estimator

data.frame(Integrated_Product$TOT_PAG_VIEW_CT, Integrated_Product$TOT_SESS_HR_V) %>%
  filter(!is.na(Integrated_Product$TOT_PAG_VIEW_CT) & !is.na(Integrated_Product$TOT_SESS_HR_V)) %>%
  covRob(corr = T) 

cor.test(Integrated_Product$TOT_PAG_VIEW_CT, Integrated_Product$TOT_SESS_HR_V, method = 'pearson')

# 어느 상품군이 가장 돈이 될까?

CLAC1_NM_top10_plot <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  group_by(CLAC1_NM) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  top_n(10, money) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLAC1_NM, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLAC1_NM), position = 'dodge') +
  
  ggtitle("CLAC1 수익이 가장 많이 나는  상품군") + 
  
  labs(x = "", y = "") +
  
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
  
  ggtitle("CLAC2 수익이 가장 많이 나는  상품군") + 
  
  labs(x = "", y = "") +
  
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
  
  ggtitle("CLAC3 수익이 가장 많이 나는  상품군") + 
  
  labs(x = "", y = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')

CLAC1_NM_top10_plot

CLAC2_NM_top15_plot

CLAC3_NM_top20_plot

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
  
  ggtitle("CLAC2 수익이 가장 많이 나는  상품군") + 
  
  labs(x = "", y = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')
)

# 어느 나잇대 성별이 가장 돈이 될까? (추후 이익률을 고려)
# ans) 30대 여성, 40대 여성, 20대 여성, 40대 남성, 50대 여성 순 ..

tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER)) %>%
  
  group_by(CLNT_AGE, CLNT_GENDER) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLNT_AGE, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLNT_GENDER), position = 'dodge') +
  
  ggtitle("판매액이 가장 많은 집단") + 
  
  labs(x = "", y = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

# CUSTOM 정보 누락자 판매액
# ans) 5,859,143,869

tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(is.na(CLNT_GENDER)) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT))



k <- list()
k[[1]] <- c(4,5,6)
k
# 30대 여성의 구입 상품군

CLAC2_NM_top15_F_30_plot <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER) & CLNT_GENDER == 'F' & CLNT_AGE == '30') %>%
  
  group_by(CLAC2_NM) %>%
  
  summarise(money = sum(PD_BUY_AM * PD_BUY_CT)) %>%
  
  top_n(15, money) %>%
  
  arrange(desc(money)) %>%
  
  ggplot(aes(x = reorder(CLAC2_NM, money, FUN = max), y = money)) +
  
  geom_col(aes(fill = CLAC2_NM), position = 'dodge') +
  
  ggtitle("CLAC2 30대 여성의 구입 상품군") + 
  
  labs(x = "", y = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position = 'none')








## 나이와 성별로 선호하는 상품군을 파악 가능 

p1 <- tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER)) %>%
  
  group_by(CLAC1_NM, CLNT_GENDER, CLNT_AGE) %>%
  
  count() %>%
  
  ggplot(aes(x = reorder(CLAC1_NM, n, FUN = min), y = n)) +
  
  geom_col(aes(fill = CLNT_GENDER), position='stack') +
  
  facet_grid(~ CLNT_AGE) +
  
  coord_flip() +
  
  ggtitle("성별 나이별 상품군") + 
  
  labs(x = "", y = "") +
  
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

age_plot <- list()
for(i in seq(10,80,10)){

  age_plot[[i/10]] <- tr_Product_original %>%
    
    left_join(tr_Master_original, c('PD_C')) %>%
    
    left_join(tr_Custom_original, c('CLNT_ID')) %>%
    
    filter(!is.na(CLNT_GENDER), CLNT_AGE == paste0(i)) %>%
  
    group_by(CLAC1_NM, CLNT_GENDER) %>%
    
    count() %>%
    
    ggplot(aes(x = reorder(CLAC1_NM, n, FUN = min), y = n)) +
   
    geom_col(aes(fill = CLNT_GENDER), position='stack') +
    
    geom_label(aes(label = n, colour = CLNT_GENDER), position = position_stack(vjust = 1)) +
      
    coord_flip() +
    
    ggtitle(paste0(i, "대 성별 선호하는 상품군")) + 
    
    labs(x = "", y = "") +
    
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15),
          axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))
  }

layout <- matrix(c(1,1,2,2,
                   1,1,2,2),
                 2,4,byrow=TRUE)

multiplot(p1, age_plot[[3]], layout=layout)

# 미상 정보의 결측치를 채워보려고 하였으나 남성이라는 키워드가 들어가는 쪽에보면 오히려 여성의 구매빈도가 높다 따라서 키워드를 기준으로 미상정보를 채우는 것은 타탕해보이지 않는다. 

## 30대 여성의 지역을 고려해보자, 좌표 데이터 추가 

load(file="Code/R/tr_Product_Master_Custom_Session_original.RData")

ZON_NM_lat_long <- read.csv("Data/ZON_NM_lat_long.csv", header = T, encoding = "UTF-8")

ZON_NM_lat_long$CITY_NM <- as.character(ZON_NM_lat_long$CITY_NM)

tr_Product_Master_Custom_Session_original$CITY_NM <- as.character(tr_Product_Master_Custom_Session_original$CITY_NM)

mapping_data  <- tr_Product_Master_Custom_Session_original %>%
  
  filter(!is.na(CITY_NM) & !is.na(CLNT_GENDER)  & CLNT_GENDER=='F' & CLNT_AGE=='30') %>%
  
  group_by(CITY_NM, CLAC1_NM) %>%
  
  count() %>%
  
  left_join(ZON_NM_lat_long, c('CITY_NM')) %>%
  
  filter(!is.na(latitude))
  
# 지역별 상위 5개 물품 

mapping_data %>% 
  
  arrange(CITY_NM, desc(n)) %>%
  
  select(CITY_NM, CLAC1_NM, n) %>%
  
  group_by(CITY_NM) %>%
  
  top_n(5, n)

# 지도 

factpal <- colorFactor(topo.colors(5), mapping_data$CLAC1_NM)

mapping_data %>%
  
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

# 어느 지역에만 특별히 잘팔리는 물건이 있을까?

# 지역 별 홍보 전략 세우기(지역의 기후, 홍보 등등이 영향을 끼쳤을 것으로 생각 ) 

# 그 기간에 홍보는 어떻게 했는가? # 판촉의 결과로 판매량으로 이어지지는 않았는가?

#=====================================================================================================================
# Step 4 :  상품군 별 온라인 선호 지수 개발 
#=====================================================================================================================

# 상품군별 온라인 선호 지수란 어떤 사람이 제품을 구매하고 난 이력을 바탕으로 상품군들에 대한 그 사람의 선호하는 정도를 이야기하는 지표이다.

# 대분류 지수, 중분류지수, 소분류지수 만들기 



## 대분류지수

# 1. Freq_index 생성

Freq_NNA_index <-  tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(!is.na(CLNT_GENDER)) %>%
  
  group_by(CLAC1_NM,CLNT_GENDER,CLNT_AGE) %>%
  
  count() %>%
  
  mutate(n = round(log(n),3)) %>%
  
  arrange(desc(n))

Freq_NA_index <-  tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  filter(is.na(CLNT_GENDER)) %>%
  
  group_by(CLAC1_NM,CLNT_GENDER,CLNT_AGE) %>%
  
  count() %>%
  
  mutate(n = round(log(n),3)) %>%
  
  arrange(desc(n))







View(
tr_Product_original %>%
  
  left_join(tr_Master_original, c('PD_C')) %>%
  
  left_join(tr_Custom_original, c('CLNT_ID')) %>%
  
  left_join(tr_Session_original, c('CLNT_ID','SESS_ID')) 
)
%>%
  
  filter(!is.na(CLNT_GENDER)) 

  
  group_by(CLAC1_NM,CLNT_GENDER,CLNT_AGE) %>%
  



## 중분류지수










## 키워드 분석전에 검색어를 CLAC3의 분류에 속하도록 검색어를 분류하는 작업을 해야됨 .
# 고려사항 2. 날짜별 키워드 -> 실질적 구매자의 검색어 분석  
Integrated_Product
# ? 고려사항 3. 검색량 - Search2 - 잠재적 구매자의 검색어 분석 

View(tr_Search2_original) ; View(tr_Master_original)
 # KWD_NM 쪼개기
test <- c("abc","def")

KWD_NM <- gsub(c("브랜드 네임") ,"" ,tr_Search2_original$KWD_NM)

View(tr_Search2_original$KWD_NM[tr_Search2_original$KWD_NM %in% tr_Master_original$CLAC3_NM])


# 고려사항 5. 일주일 단위 집계로 선호도의 변화 양상을 살펴본다. 

# 고려사항 6. 방문 빈도 

