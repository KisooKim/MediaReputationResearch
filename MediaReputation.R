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
media_data <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    media_data[[i]] <- data
}
sum <- 0
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
media_no <- match(group, media_list)
for(i in media_no){
    add <- sum(media_data[[i]]$count)
    sum <- sum + add
}
sum

#########################################################
##
##   SECTION 2.1: Save all articles with keywords 1 into
##              a data frame
##
#########################################################

## Load data for each media: Keyword 1
#
keyword_condition_1 <- "(title LIKE '%최순실%' OR title LIKE '%안종범%' OR title LIKE '%정호성%' OR title LIKE '%송성각%' OR title LIKE '%차은택%' OR title LIKE '%장시호%' OR title LIKE '%김종 차고%' OR title LIKE '%문형표%' OR title LIKE '%류철균%' OR title LIKE '%남궁곤%' OR title LIKE '%김종덕%' OR title LIKE '%정관주%' OR title LIKE '%신동철%' OR title LIKE '%김경숙%' OR title LIKE '%김기춘%' OR title LIKE '%조윤선%' OR title LIKE '%이인성%' OR title LIKE '%박채윤%' OR title LIKE '%최경희%' OR title LIKE '%이재용%' OR title OR title LIKE '%고영태%')"

## Load data for each media: keyword 1
##

media_data_keyword_1 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' And", keyword_condition_1, "GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    media_data_keyword_1[[i]] <- data
}



#########################################################
##
##   SECTION 2.2: Save all articles with keywords 1 into
##              a data frame
##
#########################################################

## Load data for each media: Keyword 2
#
keyword_condition_2 <- "(title LIKE '%최순실%' OR title LIKE '%안종범%' OR title LIKE '%정호성%' OR title LIKE '%송성각%' OR title LIKE '%차은택%' OR title LIKE '%장시호%' OR title LIKE '%김종 차고%' OR title LIKE '%문형표%' OR title LIKE '%류철균%' OR title LIKE '%남궁곤%' OR title LIKE '%김종덕%' OR title LIKE '%정관주%' OR title LIKE '%신동철%' OR title LIKE '%김경숙%' OR title LIKE '%김기춘%' OR title LIKE '%조윤선%' OR title LIKE '%이인성%' OR title LIKE '%박채윤%' OR title LIKE '%최경희%' OR title LIKE '%이재용%' OR title OR title LIKE '%고영태%' OR title LIKE '%국정농단%' OR title LIKE '%비선실세%' OR title LIKE '%비선 실세%' OR title LIKE '%문고리%' OR title LIKE '%정유라%' OR title LIKE '%미르재단%' OR title LIKE '%K스포츠%' OR title LIKE '%시국선언%' OR title LIKE '%촛불%' OR title LIKE '%퇴진%' OR title LIKE '%하야%' OR title LIKE '%탄핵%')"

## Load data for each media: keyword 2
##


media_data_keyword_2 <- NULL
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' AND ", keyword_condition_2, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    media_data_keyword_2[[i]] <- data
}
## Note that the warnings are because RMySQL does not know
## how to handle datatypes in MySQL and convert them into
## numerics.



#########################################################
##
##   SECTION 2.5: Draw the usual trend graph
##
#########################################################

graph_media <- function(media){
    i <- match(media, media_list)
    
    temp <- subset(media_data[[i]])
    median <- median(temp$facebook_sum)
    g_1 <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
        geom_point(color="dodgerblue4") +
        expand_limits(y=0) +
        xlab("") + ylab("") +
        ggtitle(paste0("", media_list_eng[i])) +
        scale_y_log10(labels = comma, limits = c(10, 100000),breaks=c(1, 10,100,1000,10000,100000))+
        geom_hline(yintercept = median, color="red3") +
        theme(plot.margin = unit(c(0,0.4,0,0), "cm")) +
        theme(plot.title = element_text(size=10, vjust=2))+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    
    temp <- subset(media_data_keyword_2[[i]])
    median <- median(temp$facebook_sum)
    g_2 <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
        geom_point(color="dodgerblue4") +
        expand_limits(y=0) +
        xlab("") + ylab("") +
        ggtitle(paste0("", media_list_eng[i])) +
        scale_y_log10(labels = comma, limits = c(10, 100000),breaks=c(1, 10,100,1000,10000,100000))+
        geom_hline(yintercept = median, color="red3") +
        theme(plot.margin = unit(c(0,0.4,0,0), "cm")) +
        theme(plot.title = element_text(size=10, vjust=2))+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    layout <- matrix(c(1,2,2,2,2,2,3,3,3,3,3), nrow = 1, byrow = TRUE) 
    multiplot(graph_axis, g_1,g_2, cols=3, layout = layout)
}



#########################################################
##
##   SECTION 3: Draw Keyword-Contribution-Scandal-Period
##              per News groups
##
#########################################################
##
## Define a function to draw facebook / date
## for articles containing keywords during
## the scandal period
##
## Ex: keyword_during_scandal(c("조선일보", "중앙일보"), "conservative")
##
## Get a empty graph to use as y-axis ticks
empty <- media_data[[1]][0,]
graph_axis <- ggplot(data=empty, aes(x=date_int, y=facebook_sum)) +
    geom_point(color="dodgerblue4") +
    expand_limits(y=0) +
    xlab("") + ylab("") +
    scale_y_log10(labels = comma, limits = c(1, 100000), breaks=c(10,100,1000,10000,100000))+
    theme(plot.margin = unit(c(0,0.5,0,0), "cm")) +
    theme(plot.title = element_text(colour = "white",size=10)) +
    ggtitle("g")




## Define the function
keyword_during_scandal <- function(group, group_name){
    media_list_no <- match(group, media_list)
    cols <- length(group)
    graph_1 <- NULL
    j <- 1
    for(i in media_list_no){
        temp <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        median <- median(temp$facebook_sum)
        mean <- mean(temp$facebook_sum)
        graph_1[[j]] <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
            geom_point(color="dodgerblue4") +
            xlab("") + ylab("") +
            ggtitle(paste0("", media_list_eng[i])) +
            scale_y_continuous(trans="log10", labels = comma,breaks=c(1, 10,100,1000,10000,100000), limits=c(NA,100000)) +
            geom_hline(yintercept = median, color="red3") +
            theme(plot.margin = unit(c(0,0.4,0,0), "cm")) +
            theme(plot.title = element_text(size=10, vjust=2))+
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        j <- j+1
    }

    ## Save the image
    layout <- matrix(c(1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5), nrow = 1, byrow = TRUE)
    png(paste0("keyword_", group_name ,".png"), width = 800, height = 200)
    multiplot(graph_axis, graph_1[[1]],graph_1[[2]],graph_1[[3]],graph_1[[4]], cols=5, layout = layout)
    dev.off()
}





## Now draw the graphs and save

## Scandal-period-Activeness - Conservative
##
group <- c("중앙일보","조선일보","동아일보","뉴데일리")
group_name <- "conservative"
keyword_during_scandal(group, group_name)

## Scandal-period-Activeness - Liberal
##
group <- c("한겨레신문","오마이뉴스","민중의소리", "미디어오늘")
group_name <- "liberal"
keyword_during_scandal(group, group_name)

## Scandal-period-Activeness - Centrist
##
group <- c("JTBC","한국일보", "머니투데이", "아시아투데이")
group_name <- "centrist"
keyword_during_scandal(group, group_name)



#########################################################
##
##   SECTION 4: Draw Facebook-Count-Variation accorss
##              pre- and post-scandal periods
##
#########################################################
##
## Define a function to draw facebook / date
## for articles containing keywords during
## the scandal period
##
## Ex: facebook_before_after(c("조선일보", "중앙일보"), "conservative")
##

graph_2 <- NULL
facebook_before_after <- function(group, group_name){
    match <- match(group, media_list)
    j<-1
    for(i in match){
        first <- 2*j-1
        second <- 2*j
        
        ## Pre-scandal period
        temp <- subset(media_data[[i]], date_int<="2016-10-01")
        median <- median(temp$facebook_sum)
        graph_2[[first]] <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
            geom_point(color="dodgerblue4") +
            expand_limits(y=0) +
            xlab("Pre-scandal") + ylab("") +
            scale_x_date(date_breaks = "3 month", labels=date_format("%B"))+
            scale_y_log10(labels = comma, limits = c(10, 100000), breaks=c(10,100,1000,10000,100000))+
            geom_hline(yintercept = median, color="red3") +
            theme(plot.margin = unit(c(0,0,0,0), "cm")) +
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank()) +
            ggtitle(paste0(media_list_eng[i])) +
            theme(plot.title = element_text(size = rel(1)))
        
        ## Post-scandal period
        temp <- subset(media_data[[i]], date_int>="2017-03-01")
        median <- median(temp$facebook_sum)
        if(i==1){
            ylab <- "Facebook Count"
        }else{
            ylab <- ""
        }
        graph_2[[second]] <- ggplot(data=temp, aes(x=date_int, y=facebook_sum)) +
            geom_point(color="dodgerblue4") +
            expand_limits(y=0) +
            xlab("Post-scandal") + ylab("") +
            scale_x_date(date_breaks = "3 month", labels=date_format("%B"))+
            scale_y_log10(labels = comma, limits = c(10, 100000), breaks=c(10,100,1000,10000,100000))+
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
    multiplot(graph_axis,graph_2[[1]],graph_2[[2]],graph_2[[3]],graph_2[[4]],graph_2[[5]],graph_2[[6]],graph_2[[7]],graph_2[[8]], cols=9, layout=layout)
    dev.off()
}


## Now draw the graphs and save

## Scandal-period-Activeness - Conservative
##
group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보")
group_name <- "conservative"
facebook_before_after(group, group_name)

## Scandal-period-Activeness - Liberal
##
group <- c("한겨레신문","오마이뉴스","민중의소리", "미디어오늘")
group_name <- "liberal"
facebook_before_after(group, group_name)

## Scandal-period-Activeness - Centrist
##"
group <- c("JTBC","한국일보", "머니투데이", "아시아투데이")
group_name <- "centrist"
facebook_before_after(group, group_name)



#########################################################
##
##   SECTION 5: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts
##
#########################################################
##
## Define a function to draw facebook / date
## for articles containing keywords during
## the scandal period
##
## Ex: correlation_graph(c("조선일보", "중앙일보"))
##

graph_correlation_keyword_1 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(media_data[[i]], date_int<="2016-10-01")
        after_scandal <- subset(media_data[[i]], date_int>="2017-03-01")
        during_scandal <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        
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
        geom_point(stroke=0, color="white", size=7, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        scale_y_continuous(trans=log_trans(base = exp(1)), labels = comma, breaks=c(10,100,1000,10000,20000), limits = c(30,20500))+
        xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of News Articles about the Scandal during Scandal Period)"))))+
        ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3"))
    g
    ggsave("correlation_keyword_1.png", width = 8, height = 6, units = "in", dpi=300)
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_correlation_keyword_1(group)

## And then get the Regression Table in Latex format
linear1 <- lm(variation ~ engagement, resulting_data)
latex1 <- stargazer(linear1, title="Results", align=TRUE)




#########################################################
##
##   SECTION 6.1: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 1
##
#########################################################
##
##
## Set Keyword 1 = Politics-related words

keyword <- c("경제성장", "경제 성장", "저성장", "주가", "주식", "물가", "소득", "세금", "재정 정책", "금융", "금리", "부동산","소비자","골목상권","대기업", "중소기업", "임금", "고용", "노동", "일자리", "실업률", "고용률")
keyword_name <- "economy"

keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

data_correlation_keyword_1 <- NULL
## Load data from SQL for Keyword 2
##
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_correlation_keyword_1[[i]] <- data
}

graph_keyword_1 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_correlation_keyword_1[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_correlation_keyword_1[[i]], date_int>="2017-03-01")
        during_scandal <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        
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
        geom_point(stroke=0, color="white", size=7, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of News Articles about the Scandal during Scandal Period)"))))+
        ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3"))
    g
    ggsave(paste0("correlation_keyword_",keyword_name,".png"), width = 8, height = 6, units = "in", dpi=300)
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_keyword_1(group)




#########################################################
##
##   SECTION 6.2: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 2
##
#########################################################
##
##
## Set Keyword 2 = Crimes

keyword <- c("살인", "절도", "강간", "폭행", "성추행", "성폭행", "사기", "음주운전", "횡령", "배임")
keyword_name <- "crime"

keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

data_correlation_keyword_2 <- NULL
## Load data from SQL for Keyword 2
##
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_correlation_keyword_2[[i]] <- data
}

graph_keyword_2 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_correlation_keyword_2[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_correlation_keyword_2[[i]], date_int>="2017-03-01")
        during_scandal <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        
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
        geom_point(stroke=0, color="white", size=7, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of News Articles about the Scandal during Scandal Period)"))))+
        ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3"))
    g
    ggsave(paste0("correlation_keyword_",keyword_name,".png"), width = 8, height = 6, units = "in", dpi=300)
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_keyword_2(group)





#########################################################
##
##   SECTION 6.3: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 2
##
#########################################################
##
##
## Set Keyword 3 = Politics-related words

keyword <- c("맑음", "흐림", "구름", "기온", "바람", "강우량", "더위", "추위", "날씨")
keyword_name <- "weather"

keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

data_correlation_keyword_3 <- NULL
## Load data from SQL for Keyword 2
##
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_correlation_keyword_3[[i]] <- data
}

graph_keyword_3 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_correlation_keyword_3[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_correlation_keyword_3[[i]], date_int>="2017-03-01")
        during_scandal <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        
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
        geom_point(stroke=0, color="white", size=7, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of News Articles about the Scandal during Scandal Period)"))))+
        ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3"))
    g
    ggsave(paste0("correlation_keyword_",keyword_name,".png"), width = 8, height = 6, units = "in", dpi=300)
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_keyword_3(group)




#########################################################
##
##   SECTION 6.4: Draw Correlation btw Scandal-Reporting and
##              Variation in Facebook counts for Keyword 4
##
#########################################################
##
##
## Set Keyword 4 = Politics-related words

keyword <- c("정당", "바른정당", "국민의당", "더불어민주당", "민주당", "새누리당", "한나라당", "열린우리당", "열우당", "자유한국당", "박근혜", "노무현", "이명박", "김대중", "박정희", "선거", "대통령", "국회의원", "지방자치", "정치")
keyword_name <- "politics"

keyword_condition <- NULL
for(i in 1:length(keyword)){
    if(i==length(keyword)){
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%'")
    }else{
        keyword_condition <- paste0(keyword_condition, "title LIKE '%", keyword[i], "%' OR ")    
    }
}
keyword_condition <- paste0("(",keyword_condition,")")

data_correlation_keyword_4 <- NULL
## Load data from SQL for Keyword 2
##
for(i in 1:length(media_list)){
    sql <- paste0("SELECT date_int, count(*), sum(facebook_no) from ", target_table ," WHERE date_int>='2016-03-01' AND media='", media_list[i] ,"' AND ", keyword_condition, " GROUP BY date_int ORDER BY date_int DESC")
    print(sql)
    data <- dbGetQuery(conn = con, statement = sql)
    
    ## Change the column names
    names(data)[2] <- "count"
    names(data)[3] <- "facebook_sum"
    
    ## RMySql cannot parse date type column of MySql
    ## So set the date into date type in R
    data$date_int <- (as.Date(data$date_int, format = "%Y-%m-%d"))
    
    ## Store in the variable
    data_correlation_keyword_4[[i]] <- data
}


graph_keyword_4 <- function(group){
    media_list_no <- match(group, media_list)
    correlation_data <- NULL
    variation <- NULL
    engagement <- NULL
    name <- NULL
    orientation <- NULL
    j <- 1
    for(i in media_list_no){
        before_scandal <- subset(data_correlation_keyword_4[[i]], date_int<="2016-10-01")
        after_scandal <- subset(data_correlation_keyword_4[[i]], date_int>="2017-03-01")
        during_scandal <- subset(media_data_keyword_2[[i]], date_int<"2017-03-01" & date_int>"2016-10-01")
        
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
        geom_point(stroke=0, color="white", size=7, alpha=0.6,shape=21,aes(fill=factor(orientation)))+
        geom_smooth(method='lm',se=FALSE,formula=y~x,color="red3") +
        geom_text(aes(label=name),size=4,hjust=0, vjust=0) +
        theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")) +
        scale_x_continuous(labels = comma,breaks=pretty_breaks(), limits=c(0,12000)) +
        xlab(expression(atop("Engagement in Scandal", paste("(Sum of Facebook Counts of News Articles about the Scandal during Scandal Period)"))))+
        ylab(expression(atop("Variation in Facebook Counts", paste("(Pre-scandal Median - Post-scandal Median + 300)")))) +
        scale_fill_manual(name="Political Orientation", values=c("grey57", "firebrick4","steelblue3"))
    g
    ggsave(paste0("correlation_keyword_",keyword_name,".png"), width = 8, height = 6, units = "in", dpi=300)
}

group <- c("중앙일보", "조선일보", "뉴데일리", "동아일보","한겨레신문","오마이뉴스","민중의소리", "미디어오늘","JTBC","한국일보", "머니투데이", "아시아투데이","이투데이","뷰스앤뉴스")
graph_keyword_4(group)


# Disconnect Connection
dbDisconnect(con)