# -Efficacy-forest-products-hscode
hscode 접해보기

1. 사용함수 및 패키지 로드
options(scipen = 999)
options(sqldf.driver = "SQLite")
.libPaths("/home/ruser02/R/x86_64-redhat-linux-gnu-library/3.5")
source("/home/ruser02/fbig2_cd/000.function.R")
source("/home/ruser02/code/001.library.r")



grs80 = '+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs'
wgs84 = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
epsg5181 = '+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


getwd()
#############################################
# 2. connection
############ connect DB 1
connect_post <- dbConnect(RPostgres::Postgres(),
                          dbname = 'postgres',
                          host = 'localhost',
                          port = 5432,
                          user = 'postgres',
                          password = 'MwIllIuYqor4eX')

############ connect DB 2
post_conn()




############################################

SQL <- "SELECT * FROM forst_prd_hsk_20"
mount <- data.table(post_get_query(SQL))

mount
#############################################




# 임산물만 추출.
mount <- subset(mount,bg_category== "단기임산물")
#hscode_10단위와 식재료명 추출.이유_이외 컬럼은 필요없기 때문. 식재료명을 기준으로 HS코드와 
# 2.고문헌약용생물자원효능 식료품를 추출 하기 위해.
mount <- mount[,c(6,7)]
names(mount)<- c('hscode', '식재료명')








SQL <- "SELECT * FROM TB_MM_CLASSIC_EFFECT_2"
mdicx <- data.table(post_get_query(SQL))

#임산물이 확실히 아닌 그룹 제외.
mdic <- mdicx[(mdicx$ingt_category != '갑각류'& mdicx$ingt_category != '연체류' & mdicx$ingt_category != '식육류'& mdicx$ingt_category !=  '조류' &
                 mdicx$ingt_category != '양념류'&  mdicx$ingt_category != '어류'),]
#식재료 ID와 식재료명만 추출.
mdic1 <- mdic[,-c(1,3,4,6)]
#해당 임산물을 지칭하는 독립적인 명사만을 추출.
#괄호와 그 안의 내용삭제._ 생숙탕(끓인 물에 찬물을 부은것)
mdic1$ingt_nm <- gsub("\\(.*", "", mdic1$ingt_nm)
mdic


# 특수문자 제거.-제호_크림
mdic1$ingt_nm <- gsub("[[:punct:]]", "", mdic1$ingt_nm)

mdic1 <- unique(mdic1)
View(mdic1)# 전체
#마찬가리로 독립적인 명칭에 부수적으로 붙은 명사 제거.
mdic1$ingt_nm <- gsub("나무", "", mdic1$ingt_nm)
mdic1$ingt_nm <- gsub("버섯", "", mdic1$ingt_nm)
mdic1$ingt_nm <- gsub("인", "", mdic1$ingt_nm)
mdic1$ingt_nm<- gsub("나물", "", mdic1$ingt_nm)

#이렇게 생성된 "2.고문헌약용생물자원효능"의 식재료명 텍스트를 기준으로 
#"2020년+기준+산림청+임산물+HSK+코드"의 식재료명과 일부만 일치해도 식재료명은 물론 HSK 코드까지 추출.
table <- data.table()
a<-0
for (i in 1:(nrow(mdic1))) {
  print(i)
  log = mdic1$ingt_nm[[i]]
  mdic1$hscode[i] <- data.table(mount[grep(log , mount$식재료명),])
  tmp2 <- rbind(table,mdic1)
}
View(tmp2)

is.vector(tmp2$hscode)



# 일치하는 항목이 없다면 품목코드에 'character(0)'로 수록된다. 해당 행 제거.
df <- tmp2[!(tmp2$hscode == "character(0)" ), ]

# 벡터안에 한 열로 묶여 있던 품목코드_예시 c( 09889, 03213) 행 별로 분할.
install.packages('splitstackshape')
library('splitstackshape')
df <-cSplit(df, 'hscode', ', ', 'long')
!!!!!!!!
  
  
  #readr::parse_number를 작동시키기 위해서는 factor형인 품목코드를 character형으로 변환.
  
  df$hscode <- as.character(df$hscode )

# 벡터안의 품목코드 숫자형으로 변환

df$hscode <- readr::parse_number(df$hscode) 

df1<-df
#000000
#df1$hscode <- format(df1$hscode, scientific = FALSE)
# df1$hscode로 코딩했을 때 " 0000000"처럼 HS코드안에 빈칸이 존재한다. 오류발생을 여지가 있어 해결.
df1$hscode <- gsub("\\s+", "", df1$hscode)

str(mount$hscode)
install.packages('tidyverse')
library('tidyverse')
#df$hscode <- map_chr(df$hscode, ~paste0('^',.x ))
#벡터안의 HS 코드를 행별로 나열하는 작업에서 HS코드 앞의 '0'9898이 사라지기에 0이 잘려진 HS코드를 기준으로 
#0이 붙은 HS코드 추출.
tabley <- data.table()
a<-0
for (i in 1:(nrow(df1))) {
  print(i)
  log2 = df1$hscode[[i]]
  df1$hscode[i] <- data.table(mount[grep( log2 , mount$hscode),])
  trad1 <- rbind(tabley, df1)
}
View(trad1)


# trad1와 mount의 형식을 통일.
trad1$hscode <- as.character(trad1$hscode)
#hs코드를 기준으로 '2020년+기준+산림청+임산물+HSK+코드'파일에 존재하는 식재료명 추출.
ljoin <- trad1%>% inner_join(mount,by='hscode')

#hs코드의 '0'9898_0이 엑셀파일에서 텍스트로 인식되도록 hs코드에 쌍 따옴표를 붙힌다.
ljoin$hscode<- paste("\'",ljoin$hscode) 

names(ljoin)<- c('INGT_ID', 'INGT_NM', 'HSCODE', 'DTL_INGT_NM')
ljoin$HSCODE <- gsub("\\s+", "", ljoin$HSCODE)
write.csv(ljoin, "FORST_PRD_EFIC_HS.csv",row.names = FALSE)
