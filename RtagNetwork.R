library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(d3Network)

## MAKE SURE YOU SET DIRECTORY OR THE SAVE FILES WILL SAVE TO WHEREVER


## get data from SO Data Explorer. This will download all questions tagged with r, 
## to repeat this exercise for any other tag, replace TagName=r with whateva
## ( or play w/ query specifics here: 
## http://data.stackexchange.com/stackoverflow/query/426797/tags-score-views-creationdate-by-tag)

a <- read.csv("http://data.stackexchange.com/stackoverflow/csv/543721?TagName=r",stringsAsFactors = F)

## get list of tags
b <- gsub("><",",",a$Tags)
b <-   gsub(">|<","",b)

## throw away r and commaless entries
b <- gsub(",r,",",",b)
b <- gsub("^r,","",b)
b <- gsub(",r$",",",b)
b <- b[str_count(b,",")!=0]

c <- b
# ##--------------- filter for one tag + second level. Meh...------------
# Input <- "linux"
# c  <- b[grep(Input,b)]
# 
# ## and grab second level tags related to the one you wanted
# pattern <- gsub("\\|\\|","|",
#                 str_split(c,",") %>% unlist %>%  unique %>% head(300) %>%
#                   paste(collapse="|")) 
# c <- b[grep(pattern,b)]
# 
# ##----

## Now split it up into a huuuge matrix that counts matches (will take a while),
## Then grab only half of it, and data.frameit
spl <- function(x) strsplit(as.character(x), ",")[[1]]
eg <- function(aaa, vvv) expand.grid(aaa = spl(aaa), vvv = spl(vvv))
dd <- do.call("rbind", Map(eg,c,c))
xtabs(data = dd) %>% as.matrix -> f
f[lower.tri(f)] <- 0
g <- f %>% data.frame
g <- g[g$aaa !=g$vvv,]

## output a csv so everyone can play :)
# write.csv(g,"rTagsAndLinkStrengths.csv")

## Select cutoff
h <- filter(g,Freq>3)

## Investigate Link Strengths
h$Freq %>% sort %>% plot(main = "Link Strength Distribution", ylab = "Link Strength",
                         xlab="Question Number",ylim = c(1,50))

########### This part removes connections ---------

conns <- h[,c(1,3)]
names(conns)[1] <- "vvv"
conns <- rbind(conns,h[,c(2,3)])

conns %>%
  group_by(vvv) %>%
  summarize(Sum=sum(Freq)) %>%
  arrange(desc(Sum)) %>% head(35) %>% 
  select(vvv) -> toLose

paste(toLose$vvv,collapse = "|") -> toLose
toLose <- gsub("-","\\\\-",toLose)
  # toLose <- c("ggplot2|plot|data.frame")

## Lose the above nodes:
h <- h[-grep(toLose,h$aaa),]
h <- h[-grep(toLose,h$vvv),]
############ -------------

d3SimpleNetwork(h,fontsize = 15, width = 1800, height = 900,
                linkColour ="#999",file="LS3withoutTop35.html")

# ## @ 4k resolution:
# d3SimpleNetwork(h,fontsize = 15, width = 3,840, height = 2160,
#                 linkColour ="#999",file="Simple1.4k.html")



## or Complex ones:

## get nodes
nodes <- data.frame(name=h[,1:2] %>% unlist %>% as.character() %>% unique(),group=1)

## and match them to their value
h$source <- match(h$aaa,nodes$name)
h$target <- match(h$vvv,nodes$name)

i <- h %>% select(source,target,Freq)
names(i)[3] <- "value"
i$value <- as.integer(i$value)

## network plots are zero indexed because people like me that don't RTFM should suffer and waste extra time thinking "... I don't get it... it should totally work!" :(
i$target <- i$target - 1
i$source <- i$source - 1

# Create graph
d3ForceNetwork(Links = i, Nodes = nodes, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               Group = "group", opacity = 0.8, zoom =T,
               file="Complex100.html")

## Just keep changing the thresholds and then plot simple or complex as you want

