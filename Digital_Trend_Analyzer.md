---
title: "Digital Trend Analyzer"
author: "Green Band"
date: '2018-12-16'
output:
  html_document:
    code_folding: hide
    toc_float: true
    toc: true
    toc_depth: 2
    theme: readable
    keep_md: true
---



# Step 0 : 분석 환경 설정




```r
library(dplyr)

library(ggplot2)

library(ggrepel)

library(readr)

library(stringr)
```

# Step 1 : data pre-processing

# Step 2 : EDA

# Step 3 : Modeling

# Step 4 : Insight

# Step 5 : Proposal for New Service








```r
summary(iris)
```

```
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
```




```r
plot(iris$Sepal.Length,iris$Petal.Length)
```

![Image1](Digital_Trend_Analyzer_files/figure-html/unnamed-chunk-4-1.png)


```r
library(GGally)
ggpairs(iris, aes(color=Species), columns=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
```

![Image2](Digital_Trend_Analyzer_files/figure-html/unnamed-chunk-5-1.png)



```r
kable(head(iris), caption = 'Table1', align = 'c')
```



Table: Table1

 Sepal.Length    Sepal.Width    Petal.Length    Petal.Width    Species 
--------------  -------------  --------------  -------------  ---------
     5.1             3.5            1.4             0.2        setosa  
     4.9             3.0            1.4             0.2        setosa  
     4.7             3.2            1.3             0.2        setosa  
     4.6             3.1            1.5             0.2        setosa  
     5.0             3.6            1.4             0.2        setosa  
     5.4             3.9            1.7             0.4        setosa  


*기울임*  
\*기울임*    
**굵게**  
**`형광펜`**  
~~지우기~~--이렇게 수정 


줄바꿈은 이 라인의 끝에 띄어쓰기 두번  
하면 된당 

##### 1. list
  + a;lskdfj
  + asdl;fk
      - asfasf
        
##### 1. list
  1. a;lskdfj
  2. asdl;fk
      i) asfasf
      i) efsgsdg
        
##### box
    safasdfas  


        
        

# SADFSFAS
## sadfsadfa
### sdfasf
#### asdfasfasf
##### sfasfasfasd

내용 내용 내용 

## Quarterly Results {.tabset .tabset-fade .tabset-pills}

### By Product

(tab content)

### By Region

(tab content)

## sdfsa
<http://rmarkdown.rstudio.com>

[name](http://rmarkdown.rstudio.com)
