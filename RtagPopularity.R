## This script will gather tags related to a specific tag and track their popularity over time

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(d3Network)

## get data (play w/ query specifics here: http://data.stackexchange.com/stackoverflow/query/426797/tags-score-views-creationdate-by-tag)
a <- read.csv("http://data.stackexchange.com/stackoverflow/csv/543721?TagName=r",stringsAsFactors = F)

## get list of tags
b <- gsub("><",",",a$Tags)
b <-   gsub(">|<","",b)

## Get most popular tags... we'll use this to come up w/ a shortlist
strsplit(b,",") %>% unlist %>% table() %>% 
  as.data.frame %>% arrange(desc(Freq)) %>% head(70) 

## And pick top lucky tags to investigate and will multiply each of those lines 
## with by views
# a$plot <- grepl("plot",a$Tags)
a$data.frame <- grepl("data.frame",a$Tags)
a$statistics <- grepl("statistics",a$Tags)
a$matrix <- grepl("matrix",a$Tags)
a$knitr <- grepl("knitr",a$Tags)
a$regression <- grepl("regression",a$Tags)
a$python <- grepl("python",a$Tags)
a$parallel.processing <- grepl("parallel-processing",a$Tags)
a$latex <- grepl("latex",a$Tags)
a$machine.learning <- grepl("machine-learning",a$Tags)
a$data.visualization <- grepl("data-visualization",a$Tags)

# a$date <- gsub("-.. .+","",a$Date.Asked)
a$date <- gsub("-.+","",a$Date.Asked)

a[,c(5:15)] %>%
  group_by(date) %>%
  filter(date !=2016) %>%
  summarise_each(funs(sum)) %>%
  gather(key = date) -> e

names(e) <- c("date","package","value")

ggplot(e,aes(x=date,y=value,group=package,color=package)) + geom_line(size=2) 

## Hrm. And what about if we plot the popularity vs the aggregate score?
## first score
c <- data.frame(dataframe="",statistics="",matrix="",knitr="",regression="",python="",latex="",parallel.processing="",machine.learning="",data.visualization="",date=rep(0,50000))

c$dataframe <- grepl("data.frame",a$Tags) * a$Score
# c$statistics <- grepl("statistics",a$Tags) * a$Score
c$matrix <- grepl("matrix",a$Tags) * a$Score
c$knitr <- grepl("knitr",a$Tags) * a$Score
c$regression <- grepl("regression",a$Tags) * a$Score
c$python <- grepl("python",a$Tags) * a$Score
c$parallel.processing <- grepl("parallel-processing",a$Tags) * a$Score
c$latex <- grepl("latex",a$Tags) * a$Score
c$machine.learning <- grepl("machine-learning",a$Tags) * a$Score
c$data.visualization <- grepl("data-visualization",a$Tags) * a$Score
c$date <- gsub("-.+","",a$Date.Asked)

c %>%
  group_by(date) %>%
  filter(date !=2016) %>%
  summarise_each(funs(mean)) %>%
  gather(key = date) -> f

names(f) <- c("date","package","score")

## ... then the views
d <- data.frame(dataframe="",statistics="",matrix="",knitr="",regression="",python="",latex="",parallel.processing="",machine.learning="",data.visualization="",date=rep(0,50000))

d$dataframe <- grepl("data.frame",a$Tags) * a$Views
# d$statistics <- grepl("statistics",a$Tags) * a$Views
d$matrix <- grepl("matrix",a$Tags) * a$Views
d$knitr <- grepl("knitr",a$Tags) * a$Views
d$regression <- grepl("regression",a$Tags) * a$Views
d$python <- grepl("python",a$Tags) * a$Views
d$parallel.processing <- grepl("parallel-processing",a$Tags) * a$Views
d$latex <- grepl("latex",a$Tags) * a$Views
d$machine.learning <- grepl("machine-learning",a$Tags) * a$Views
d$data.visualization <- grepl("data-visualization",a$Tags) * a$Views
d$date <- gsub("-.+","",a$Date.Asked)

d %>%
  group_by(date) %>%
  filter(date !=2016) %>%
  summarise_each(funs(mean)) %>%
  gather(key = date) -> g

names(g) <- c("date","package","Views")

## Now mashup the views and the scores and plot!
left_join(f,g) %>%
  ggplot(aes(x=score,y=Views,group=package,color=package,alpha=date)) + 
  geom_point() + geom_line(size=2)



