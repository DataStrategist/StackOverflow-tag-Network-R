library(dplyr)
library(ggplot2)
library(tidyr)

## get data (play w/ query specifics there)
a <- read.csv("http://data.stackexchange.com/stackoverflow/csv/543721?TagName=r",stringsAsFactors = F)

## get list of tags
b <- gsub("><",",",a$Tags)
b <-   gsub(">|<","",b)

## throw away r and commaless entries
library(stringr)
b <- gsub(",?r,?","",b)
b <- b[str_count(b,",")!=0]

spl <- function(x) strsplit(as.character(x), ",")[[1]]
eg <- function(aaa, vvv) expand.grid(aaa = spl(aaa), vvv = spl(vvv))
dd <- do.call("rbind", Map(eg, b,b))
xtabs(data = dd) %>% as.matrix -> f
f[lower.tri(f)] <- 0

g <- f %>% data.frame
g <- g[g$aaa !=g$vvv,]
g <- filter(g,Freq>0)

library(d3Network)
s




## Get most popular tags
strsplit(b,",") %>% unlist %>% table() %>% as.data.frame %>% arrange(desc(Freq)) %>% head(6) -> c 

## Get "Score" for 5 most popular tags
d <- c[2:6,1] %>% as.character

a$ggplot2 <- grepl(d[1],a$Tags) * a$Score
a$plot <- grepl(paste("<",d[2],sep=""),a$Tags) * a$Score
a$data.frame <- grepl(d[3],a$Tags) * a$Score
a$data.table <- grepl(d[4],a$Tags) * a$Score
a$shiny <- grepl(d[5],a$Tags) * a$Score

a$date <- gsub("-.+","",a$Date.Asked)

a %>%
  select(date,ggplot2,plot,data.frame,data.table,shiny) %>%
  group_by(date) %>%
  filter(date !=2016) %>%
  summarize(ggplot=sum(ggplot2),
            plot=sum(plot),
            data.frame=sum(data.frame),
            data.table=sum(data.table),
            shiny=sum(shiny)) %>%
  gather(key = date) -> e

names(e) <- c("date","package","value")

ggplot(e,aes(x=date,y=value,group=package,color=package)) + geom_line(size=2) 
  
  
as.character()a$ScoreCat <- cut(a$Score,breaks = c(-10,-1,0,1,5,10,200))


