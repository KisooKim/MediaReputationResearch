library(RMySQL)
library(ggplot2)
require(reshape2)
library(scales)
library(stargazer)
source("multiplot.R")


#########################################################
##
##   SECTION 0: preparation
##
#########################################################


## Connect to MySQL
## Or you can use the csv file.
##
user <- "root"
password <- ""
database_name <- "critiquers_kr"
host <- "localhost"
con = dbConnect(MySQL(), user=user, password=password, dbname=database_name, host=host, encoding = "UTF-8")
target_table <- dbListTables(con)[1]


## Set encoding
##
dbGetQuery(conn = con, statement = "set session character_set_connection=utf8;")
dbGetQuery(conn = con, statement = "set session character_set_results=utf8;")
dbGetQuery(conn = con, statement = "set session character_set_client=utf8;")


## List of media
##
#media_list <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
#media_list_eng <- c("Joongang","Chosun","NewDaily","Donga","Hankyeore","OhMyNews", "VOP", "MediaToday","JTBC","Hankook","MoneyToday","AsiaToday","EToday","ViewsAndNews")
#media_orientation <- c("Conservative","Conservative","Conservative","Conservative", "Liberal","Liberal", "Liberal", "Centrist","Centrist","Centrist", "Centrist", "Centrist", "Liberal")

media_list <- c("조선일보","중앙일보","동아일보","뉴데일리", "한국경제","매일경제","한겨레신문","오마이뉴스", "민중의소리", "미디어오늘","한국일보","뷰스앤뉴스", "뉴시스", "머니투데이", "JTBC","세계일보","이투데이","아시아투데이","위키트리","허핑턴포스트","노컷뉴스")
media_list_eng <- c("Chosun","Joongang","Donga","NewDaily","HanKyung","MaeilBusiness","Hankyeore","OhMyNews", "VOP", "MediaToday","Hankook","ViewsAndNews", "Newsis", "MoneyToday", "JTBC", "Sekye", "EToday", "AsiaToday","Wikitree","HuffingtonPost", "Nocut")
media_orientation <- c("Conservative","Conservative","Conservative","Conservative", "Conservative","Conservative","Liberal","Liberal", "Liberal", "Liberal","Centrist","Liberal", "Centrist", "Centrist", "Centrist","Centrist","Centrist","Centrist","Centrist","Liberal","Liberal")



#########################################################
##
##   SECTION 1: Save all articles into a data frame
##
#########################################################

## Load data for each media: entire news
##
data_all <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_all[[i]] <- data
}

## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.


#########################################################
##
##   SECTION 2.1: Save all articles with scandal keyword
##                a data frame "data_scandal"
##
#########################################################

## Load data for each media: scandal keyword
#

keyword <- c("최순실", "안종범", "정호성", "송성각", "차은택", "장시호", "김종 차관", "문형표", "류철균", "남궁곤", "김종덕", "정관주", "신동철", "김경숙", "김기춘", "조윤선", "이인성", "박채윤", "최경희", "이재용", "고영태", "농단", "비선실세", "비선 실세", "문고리", "정유라", "미르재단", "미르 재단", "스포츠", "시국선언", "시국 선언", "촛불", "퇴진", "하야", "탄핵", "시위")
keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

## Load data for each media: scandal keyword
##

data_scandal <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_scandal[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.




#########################################################
##
##   SECTION 2.2: Save all articles with keyword 1
##                a data frame "data_keyword_1"
##                normal politics
##
#########################################################

keyword <- c("개헌","선거","국회의원","지방자치","정치","여야","보수","진보","중도","국방위","정보위","송민순","회고록","법무부","법제처","방송통신위원회","방통위","외교부","통일부","행정안전부","행안부","인사혁신처","선관위","중앙선거관리위원회","보건복지부","환경부","노동부","고용노동부","국토부","국토교통부","여가부","여성가정부","공정거래위원회","공정거래위","국방부","사드","북한","北","김정은","미사일","핵실험","미국","美","오바마","트럼프","중국","中","시진핑","일본","日","아베")
keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

## Load data for each media: scandal keyword
##

data_keyword_1 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_keyword_1[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.




#########################################################
##
##   SECTION 2.3: Save all articles with keyword 2
##                a data frame "data_keyword_2"
##                economy
##
#########################################################

keyword <- c("경제성장", "경제 성장", "저성장", "주가", "주식", "물가", "소득", "세금", "재정", "금융", "금리", "부동산","기업","투자","소비자","대기업", "중소기업", "임금", "고용","일자리","실업률", "고용률","가계부채","가계 부채","재정지출","재정 지출", "정부지출","정부 지출","한국은행","기획재정부","기재부")
keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

## Load data for each media: scandal keyword
##

data_keyword_2 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_keyword_2[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.




#########################################################
##
##   SECTION 2.4: Save all articles with keyword 3 into
##                a data frame "data_keyword_3"
##                economy
##
#########################################################

keyword <- c("살인", "절도", "강간", "폭행", "성추행", "성폭행", "사기", "음주운전", "횡령", "배임")
keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

## Load data for each media: scandal keyword
##

data_keyword_3 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_keyword_3[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.




#########################################################
##
##   SECTION 2.5: Save all articles with keyword 4 into
##                a data frame "data_keyword_4"
##                economy
##
#########################################################

keyword <- c("맑음", "흐림", "구름", "기온", "바람", "강우량", "더위", "추위", "날씨", "봄", "여름", "가을", "겨울")
keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

## Load data for each media: scandal keyword
##

data_keyword_4 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-15' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_keyword_4[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle date datatypes in MySQL and convert
## them into numerics.




#########################################################
##
##   SECTION 3: Draw Facebook-Count-Variation accorss
##              pre- and post-scandal periods
##
#########################################################
##
## Define a function to draw facebook / date
## for articles containing keyword during
## the scandal period
##
## Ex: facebook_before_after(c("조선일보", "중앙일보"), "conservative")
##

## Get a empty graph to use as y-axis ticks
empty <- data[0,]
graph_axis <- ggplot(data=empty, aes(x=date_int, y=facebook_sum)) +
    geom_point(color="dodgerblue4") +
    expand_limits(y=0) +
    xlab("") + ylab("") +
    scale_y_log10(labels = comma, limits = c(1, 100000), breaks=c(10,100,1000,10000,100000))+
    theme(plot.margin = unit(c(0.4,0.5,0.4,0), "cm")) +
    theme(plot.title = element_text(colour = "white",size=10)) +
    ggtitle("g")


graph <- NULL
facebook_before_after <- function(group, group_name){
    match <- match(group, media_list)
    j<-1
    for(i in match){
        first <- 2*j-1
        second <- 2*j
        ## Pre-scandal period
        temp <- subset(data_all[[i]], date_int<="2016-10-01")
        # Add 10 to use log scale
        temp$facebook_sum <- temp$facebook_sum+10
        median <- median(temp$facebook_sum)
        graph[[first]] <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
            geom_point(color="dodgerblue4") +
            xlab("Pre-scandal") + ylab("") +
            scale_x_date(date_breaks = "3 month", labels=date_format("%B"))+
            scale_y_log10(labels = comma, limits = c(1, 100000), breaks=c(10,100,1000,10000,100000))+
            geom_hline(yintercept = median, color="red3") +
            theme(plot.margin = unit(c(0,0,0,0), "cm")) +
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank()) +
            ggtitle(paste0(media_list_eng[i])) +
            theme(plot.title = element_text(size = rel(1)))
        
        ## Post-scandal period
        temp <- subset(data_all[[i]], date_int>="2017-03-15")
        # Add 10 to use log scale
        temp$facebook_sum <- temp$facebook_sum+10
        median <- median(temp$facebook_sum)
        if(i==1){
            ylab <- "Facebook Count"
        }else{
            ylab <- ""
        }
        graph[[second]] <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
            geom_point(color="dodgerblue4") +
            xlab("Post-scandal") + ylab("") +
            scale_x_date(date_breaks = "3 month", labels=date_format("%B"))+
            scale_y_log10(labels = comma, limits = c(1, 100000), breaks=c(10,100,1000,10000,100000))+
            geom_hline(yintercept = median, color="red3") +
            theme(plot.margin = unit(c(0,0.4,0,0), "cm")) +
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())+
            theme(plot.title = element_text(colour = "white",size=rel(1), vjust=5)) + 
            ggtitle(paste0(media_list_eng[i]))
        j<-j+1
    }
    
    ## Draw and save the graph
    layout <- matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9), nrow = 1, byrow = TRUE)
    png(paste0("facebook_before_after_", group_name ,".png"), width = 800, height = 200)
    multiplot(graph_axis,graph[[1]],graph[[2]],graph[[3]],graph[[4]],graph[[5]],graph[[6]],graph[[7]],graph[[8]], cols=9, layout=layout)
    dev.off()
}

## Now draw the graphs and save

## Scandal-period-Activeness - Conservative
##
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보")
group_name <- "conservative"
facebook_before_after(group, group_name)
## The error messages come from graph_axis and missing data for
## a few dates when data was not collected.

## Scandal-period-Activeness - Liberal
##
group <- c("한겨레신문","오마이뉴스","민중의소리", "미디어오늘")
group_name <- "liberal"
facebook_before_after(group, group_name)
## The error messages come from graph_axis and missing data for
## June 2nd 2016, when the data was not collected.


## Scandal-period-Activeness - Centrist
##"
group <- c("JTBC","한국일보", "머니투데이", "아시아투데이")
group_name <- "centrist"
facebook_before_after(group, group_name)
## The error messages come from graph_axis and missing data for
## June 2nd 2016, when the data was not collected.



#########################################################
##
##   SECTION 4.1: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts
##
#########################################################
##
## Define a function to draw facebook / date
## for articles containing keyword during
## the scandal period
##
## Ex: correlation_graph(c("조선일보", "중앙일보"))
##
labeltext <- 5

correlation_scandal_all <- function(group){
    media_list_no <- match(group, media_list)
    result <- NULL
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_all[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_all[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_scandal[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add a constant to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 500
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation))
    g
    ggsave("correlation_keyword_all.png", width = 8, height = 8, units = "in", dpi=300)
    result[[1]] <- g
    result[[2]] <- correlation_data
    result
}
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
result <- correlation_scandal_all(group)
graph_scandal_all_original <- result[[1]]
graph_scandal_all_solo <- graph_scandal_all_original +
    geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
    geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
    geom_text(aes(label=name),size=6,hjust=0, vjust=0) +
    theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
    scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
    scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
    xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of Scandal-related Articles)"))))+
    ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
    scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
    theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18)) +
    theme(plot.title = element_text(size = 26, face="bold")) +
    theme(axis.text=element_text(size=6),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
    ggtitle(paste0("Using keywords about Scandal"))
correlation_data <- result[[2]]


## And then save the linear regression
correlation_data[,4] <- log10(correlation_data[,1]+10)*10000
names(correlation_data)[4] <- "variation_log"
linear_scandal_all <- lm(variation_log ~ engagement, correlation_data)
stargazer(linear_scandal_all, title="Results", align=TRUE,font.size="tiny")




#########################################################
##
##   SECTION 4.2: Draw Correlation btw engagement in keyword 1 and
##              Variation in Facebook counts
##
#########################################################
##
##
keyword_name <- "Politics other than scandal"
correlation_keyword1_all <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_all[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_all[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_keyword_1[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 500
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=labeltext,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0,0.1,0,0), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks()) +
        scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
        xlab("")+ylab("")+guides(fill=FALSE)+
        theme(plot.title = element_text(size = 18, face="bold")) +
        theme(axis.text=element_text(size=6),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        ggtitle(paste0("Politics w/o Scandal"))
    g
    ggsave(paste0("correlation_keyword1(",keyword_name,")_all.png"), width = 8, height = 6, units = "in", dpi=300)
    result[[1]] <- g
    result[[2]] <- correlation_data
    result
}
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
result <- correlation_keyword1_all(group)
graph_keyword1_all <- result[[1]]
correlation_data <- result[[2]]

## And then save the linear regression
correlation_data[,4] <- log10(correlation_data[,1]+10)*10000
names(correlation_data)[4] <- "variation_log"
linear_keyword1_all <- lm(variation_log ~ engagement, correlation_data)




#########################################################
##
##   SECTION 4.3: Draw Correlation btw Reporting keyword 2 and
##              Variation in Facebook counts
##
#########################################################
##
##

keyword_name <- "Economy"
correlation_keyword2_all <- function(group){
    media_list_no <- match(group, media_list)
    result <- NULL
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_all[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_all[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_keyword_2[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 500
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=labeltext,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0,0.1,0,0), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks()) +
        scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
        xlab("")+ylab("")+guides(fill=FALSE)+
        theme(plot.title = element_text(size = 18, face="bold")) +
        theme(axis.text=element_text(size=6),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        ggtitle(paste0("Economy"))
    g
    ggsave(paste0("correlation_keyword2(",keyword_name,")_all.png"), width = 8, height = 6, units = "in", dpi=300)
    result[[1]] <- g
    result[[2]] <- correlation_data
    result
}
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
result <- correlation_keyword2_all(group)
graph_keyword2_all <- result[[1]]
correlation_data <- result[[2]]

## And then save the linear regression
correlation_data[,4] <- log10(correlation_data[,1]+10)*10000
names(correlation_data)[4] <- "variation_log"
linear_keyword2_all <- lm(variation_log ~ engagement, correlation_data)




#########################################################
##
##   SECTION 4.4: Draw Correlation btw Reporting keyword 3 and
##              Variation in Facebook counts
##
#########################################################
##
##

keyword_name <- "Crime"
correlation_keyword3_all <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_all[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_all[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_keyword_3[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 500
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=labeltext,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0,0.1,0,0), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks()) +
        scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
        xlab("")+ylab("")+guides(fill=FALSE)+
        theme(plot.title = element_text(size = 18, face="bold")) +
        theme(axis.text=element_text(size=6),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        ggtitle(paste0("Crime"))
    g
    ggsave(paste0("correlation_keyword3(",keyword_name,")_all.png"), width = 8, height = 6, units = "in", dpi=300)
    result[[1]] <- g
    result[[2]] <- correlation_data
    result
}
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
result <- correlation_keyword3_all(group)
graph_keyword3_all <- result[[1]]
correlation_data <- result[[2]]

## And then save the linear regression
correlation_data[,4] <- log10(correlation_data[,1]+10)*10000
names(correlation_data)[4] <- "variation_log"
linear_keyword3_all <- lm(variation_log ~ engagement, correlation_data)




#########################################################
##
##   SECTION 4.5: Draw Correlation btw Reporting keyword 4 and
##              Variation in Facebook counts
##
#########################################################
##
##

keyword_name <- "Weather"
correlation_keyword4_all <- function(group){
    media_list_no <- match(group, media_list)
    result <- NULL
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_all[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_all[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_keyword_4[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 500
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=labeltext,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0,0.1,0,0), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks()) +
        scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
        xlab("")+ylab("")+guides(fill=FALSE)+
        theme(plot.title = element_text(size = 18, face="bold")) +
        theme(axis.text=element_text(size=6),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        ggtitle(paste0("Weather"))
    g
    ggsave(paste0("correlation_keyword4(",keyword_name,")_all.png"), width = 8, height = 6, units = "in", dpi=300)
    result[[1]] <- g
    result[[2]] <- correlation_data
    result
}
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
result <- correlation_keyword4_all(group)
graph_keyword4_all <- result[[1]]
correlation_data <- result[[2]]

## And then save the linear regression
correlation_data[,4] <- log10(correlation_data[,1]+40)*10000
names(correlation_data)[4] <- "variation_log"
linear_keyword4_all <- lm(variation_log ~ engagement, correlation_data)




#########################################################
##
##   SECTION 4.6: Combine graphs and tables
##
#########################################################

stargazer(linear_scandal_all, linear_keyword1_all, linear_keyword2_all,linear_keyword3_all, linear_keyword4_all, title="Comparison of OLS Regressions ((1) Scandal, (2) Politics other than scandal, (3) economy, (4) crime, and (5) weather)", align=TRUE, font.size="tiny")


graph_axis <- graph_axis + theme(plot.margin = unit(c(0.4,0.1,0.4,0), "cm"))

graph_scandal_all_comparison <- graph_scandal_all_original +
    geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
    geom_smooth(method='lm',formula=y~x,color="red3") +
    geom_text(aes(label=name),size=labeltext,hjust=0, vjust=0) +
    theme(plot.margin = unit(c(0,0.1,0,0), "cm")) +
    scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
    scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(100,20500))+
    xlab("")+ylab("")+guides(fill=FALSE)+
    theme(plot.title = element_text(size = 18, face="bold")) +
    theme(axis.text=element_text(size=labeltext),axis.title=element_text(size=19, lineheight=0.9),axis.text.x=element_text(vjust=-10)) +
    scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    ggtitle(paste0("Scandal"))


layout <- matrix(c(1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6), nrow = 1, byrow = TRUE)
png(paste0("comparison_explanatory_variables.png"), width = 1200, height = 300)
multiplot(graph_axis, graph_scandal_all_comparison, graph_keyword1_all,graph_keyword2_all,graph_keyword3_all,graph_keyword4_all, cols=6, layout=layout)
dev.off()



#########################################################
##
##   SECTION 7.1: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 1
##
#########################################################
##
##

keyword_name <- "Politics"
graph_correlation_keyword_1 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_keyword_1[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_keyword_1[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_scandal[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 300
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+ 
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.1,0.1,0.1,0), "cm")) + 
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab("Engagement in Scandal")+
        ylab("Variation in Facebook Counts") +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(plot.title = element_text(size = 26, face="bold")) +
        guides(fill=FALSE)+ 
        theme(axis.text=element_text(size=8),axis.title=element_text(size=16, lineheight=0.9)) +
        ggtitle(paste0("Politics w/o Scandal"))
    g
    ggsave(paste0("correlation_scandal_keyword1(",keyword_name,").png"), width = 8, height = 6, units = "in", dpi=300)
    g
}


group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_1 <- graph_correlation_keyword_1(group)


#########################################################
##
##   SECTION 7.2: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 2
##
#########################################################
##

keyword_name <- "Economy"
graph_keyword_2 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_keyword_2[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_keyword_2[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_scandal[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 300
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+ 
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.1,0.1,0.1,0), "cm")) + 
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab("Engagement in Scandal")+
        ylab("Variation in Facebook Counts") +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(plot.title = element_text(size = 26, face="bold")) +
        guides(fill=FALSE)+ 
        theme(axis.text=element_text(size=8),axis.title=element_text(size=16, lineheight=0.9)) +
        ggtitle(paste0("Economy"))
    g
    ggsave(paste0("correlation_scandal_keyword2(",keyword_name,").png"), width = 8, height = 6, units = "in", dpi=300)
    g
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_2 <- graph_keyword_2(group)





#########################################################
##
##   SECTION 7.3: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 3
##
#########################################################
##
##

keyword_name <- "Crime"
graph_keyword_3 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_keyword_3[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_keyword_3[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_scandal[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 300
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+ 
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.1,0.1,0.1,0), "cm")) + 
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab("Engagement in Scandal")+
        ylab("Variation in Facebook Counts") +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(plot.title = element_text(size = 26, face="bold")) +
        guides(fill=FALSE)+ 
        theme(axis.text=element_text(size=8),axis.title=element_text(size=16, lineheight=0.9)) +
        ggtitle(paste0("Crime"))
    g
    ggsave(paste0("correlation_scandal_keyword3(",keyword_name,").png"), width = 8, height = 6, units = "in", dpi=300)
    g
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_3 <- graph_keyword_3(group)




#########################################################
##
##   SECTION 7.4: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 4
##
#########################################################
##

keyword_name <- "Weather"
graph_keyword_4 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_keyword_4[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_keyword_4[[i]], date_int>="2017-03-15")
        during_scandal <- subset(data_scandal[[i]], date_int<"2017-03-15" & date_int>"2016-10-01")
        
        ## Add constants to use log scale
        temp_variation <- median(after_scandal$facebook_sum)-median(before_scandal$facebook_sum) + 300
        
        variation[j] <- median(temp_variation)
        engagement[j] <- median(during_scandal$facebook_sum)
        name[j] <- media_list_eng[i]
        orientation[j] <- media_orientation[i]
        j <- j+1
    }
    
    ## Construct a data frame to draw
    correlation_data <- data.frame(variation, engagement, name)
    names(correlation_data) <- c("variation", "engagement", "name")
    ## Save as graphs
    g <- ggplot(data=correlation_data, aes(x=engagement, y=variation)) +
        geom_point(stroke=0, color="white", size=12, alpha=0.6,shape=21,aes(fill=factor(orientation)))+ 
        geom_smooth(method='lm',formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.1,0.1,0.1,0), "cm")) + 
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab("Engagement in Scandal")+
        ylab("Variation in Facebook Counts") +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3")) +
        theme(plot.title = element_text(size = 26, face="bold")) +
        guides(fill=FALSE)+
        theme(axis.text=element_text(size=8),axis.title=element_text(size=16, lineheight=0.9)) +
        ggtitle(paste0("Weather"))
    g
    ggsave(paste0("correlation_scandal_keyword4(",keyword_name,").png"), width = 8, height = 6, units = "in", dpi=300)
    g
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_4 <- graph_keyword_4(group)



png(paste0("comparison_dependent_variables.png"), width = 1200, height = 300)
multiplot(graph_1, graph_3, graph_2, graph_4, cols=4)
dev.off()




# Disconnect Connection
dbDisconnect(con)

