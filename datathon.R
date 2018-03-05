library(httr)

args = list(
  grant_type    = 'client_credentials',
  client_id     = '**',
  client_secret = '**',
  scope         = 'read_content read_financial_data read_product_data read_user_profile'
);

auth_req <- POST("https://idfs.gs.com/as/token.oauth2", 
                 body = args, 
                 encode = "form", 
                 verbose())
access_token = content(auth_req, "parsed")$access_token
#data_req <- GET("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/coverage", 
#                add_headers(Authorization = paste('Bearer', access_token)), 
#                verbose())
data_req <- POST("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/query", 
                 body = '{
                 "where": {
                 "gsid": [ "10516", "10696", "11308", "11896", "13901" ]
                 },
                 "startDate": "2015-01-01",
                 "endDate": "2017-12-31"
                 }', 
                 encode = "json", 
                 add_headers('Content-Type' = 'application/json', Authorization = paste('Bearer', access_token)), 
                 verbose())
content(data_req)
data_req <- POST("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/query", 
                 body = '{
                 "where": {
                 "gsid": [ "10516", "10696", "11308", "11896", "13901" ]
                 },
                 "startDate": "2015-01-01",
                 "endDate": "2016-12-31"
                 }', 
                 encode = "json", 
                 add_headers('Content-Type' = 'application/json', Authorization = paste('Bearer', access_token)), 
                 verbose())
content(data_req)
dat_org01 = data.frame(t(matrix(unlist(content(data_req)),nrow = 7, ncol = nrow(data.frame(unlist(content(data_req))))/7)))
colnames(dat_org01) = c("date", "gsid", "financialReturnsScore", "growthScore", "multipleScore", "integratedScore","updateTime")
dat_org01[,1] = as.Date(dat_org01[,1])
dat_org02 = data.frame(dat_org01)
num = apply(dat_org02[,3:6],2,function(x){as.numeric(as.character(x))})
dat_org03 = cbind(dat_org02[,1:2],num,dat_org02[,7])

library(dplyr)
library(ggplot2)
dat_org03 %>%
  group_by(gsid)%>%
  ggplot() +
  geom_line(aes(x = date, y = financialReturnsScore, color = gsid))
# transpose
df1 = dat_org03 %>%
  filter(gsid == 10516) %>%
  select(financialReturnsScore)
df2 = dat_org03 %>%
  filter(gsid == 10696) %>%
  select(financialReturnsScore)
df3 = dat_org03 %>%
  filter(gsid == 11308) %>%
  select(financialReturnsScore)
df4 = dat_org03 %>%
  filter(gsid == 11896) %>%
  select(financialReturnsScore)
df5 = dat_org03 %>%
  filter(gsid == 13901) %>%
  select(financialReturnsScore)
new_df = rbind(t(df1),t(df2),t(df3),t(df5))
row.names(new_df) = c("10516","10696","11308","13901")
matplot(t(new_df), type = "l",xlab = 'date',ylab = 'financialReturnsScore')
ggplot()
d = data.frame(t(df1))
matplot(dat_org03$financialReturnsScore, type = "l")
financial = kmeans(new_df,3)
financial$cluster
dg1 = dat_org03 %>%
  filter(gsid == 10516) %>%
  select(growthScore)
dg2 = dat_org03 %>%
  filter(gsid == 10696) %>%
  select(growthScore)
dg3 = dat_org03 %>%
  filter(gsid == 11308) %>%
  select(growthScore)
dg4 = dat_org03 %>%
  filter(gsid == 11896) %>%
  select(growthScore)
dg5 = dat_org03 %>%
  filter(gsid == 13901) %>%
  select(growthScore)
new_df1 = rbind(t(dg1),t(dg2),t(dg3),t(dg5))
row.names(new_df1) = c("10516","10696","11308","13901")
growth = kmeans(new_df1,3)
growth$cluster

dh1 = dat_org03 %>%
  filter(gsid == 10516) %>%
  select(multipleScore)
dh2 = dat_org03 %>%
  filter(gsid == 10696) %>%
  select(multipleScore)
dh3 = dat_org03 %>%
  filter(gsid == 11308) %>%
  select(multipleScore)
dh4 = dat_org03 %>%
  filter(gsid == 11896) %>%
  select(multipleScore)
dh5 = dat_org03 %>%
  filter(gsid == 13901) %>%
  select(multipleScore)
new_df2 = rbind(t(dh1),t(dh2),t(dh3),t(dh5))
row.names(new_df2) = c("10516","10696","11308","13901")
multiple = kmeans(new_df2,3)
multiple$cluster

di1 = dat_org03 %>%
  filter(gsid == 10516) %>%
  select(integratedScore)
di2 = dat_org03 %>%
  filter(gsid == 10696) %>%
  select(integratedScore)
di3 = dat_org03 %>%
  filter(gsid == 11308) %>%
  select(integratedScore)
di4 = dat_org03 %>%
  filter(gsid == 11896) %>%
  select(integratedScore)
di5 = dat_org03 %>%
  filter(gsid == 13901) %>%
  select(integratedScore)
new_df3 = rbind(t(dh1),t(dh2),t(dh3),t(dh5))
row.names(new_df3) = c("10516","10696","11308","13901")
integrated = kmeans(new_df3,3)
integrated$cluster
matplot(t(new_df3), type = "l",xlab = 'date',ylab = 'integratedScore')

cluster = data.frame(financial$cluster,growth$cluster, multiple$cluster, integrated$cluster)

par(mfrow = c(2,2))
matplot(t(new_df), type = "l",xlab = 'date',ylab = 'financialReturnsScore')
matplot(t(new_df1), type = "l",xlab = 'date',ylab = 'growthScore')
matplot(t(new_df2), type = "l",xlab = 'date',ylab = 'multipleScore')
matplot(t(new_df3), type = "l",xlab = 'date',ylab = 'integratedScore')


## company names


## stock historical prices

## twitter analysis

## NSDAQ & SP 500
library(rvest)
# https://finance.yahoo.com/quote/^GSPC/history?period1=1420088400&period2=1514696400&interval=1d&filter=history&frequency=1d
# GSPC

url1 <- 'https://finance.yahoo.com/quote/^GSPC/history?period1=1420088400&period2=1514696400&interval=1mo&filter=history&frequency=1mo'
webpage <- read_html(url1)

tbls <- html_nodes(webpage, "table")

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

colnames(tbls_ls[[1]]) <- c("Date",	"Open",	"High",	"Low",	"Close",	"Adj.Close.",	"Volume")
company <- rep('GSPC',nrow(tbls_ls[[1]]))
table <- cbind(tbls_ls[[1]],company)
sp_df <- data.frame(table) 
sp_df$Date = format(as.Date(sp_df$Date, format = "%b %d, %Y"), "%Y-%m-%d") 
cvt_num0 = apply(sp_df[,2:6], 2, function(x){as.numeric(as.character(gsub(",","",x)))})
sp_df = cbind(sp_df[,c(1,7)],cvt_num0)
sp_df = sp_df %>%
  mutate(change_pop = (Close-Open)/Close)

# IXIC
url2 <- 'https://finance.yahoo.com/quote/^IXIC/history?period1=1420088400&period2=1514696400&interval=1mo&filter=history&frequency=1mo'
webpage <- read_html(url2)

tbls2 <- html_nodes(webpage, "table")

tbls_ls2 <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

colnames(tbls_ls2[[1]]) <- c("Date",	"Open",	"High",	"Low",	"Close",	"Adj.Close.",	"Volume")
company2 <- rep('IXIC',nrow(tbls_ls2[[1]]))
table2 <- cbind(tbls_ls2[[1]],company)
ns_df <- data.frame(table) 
ns_df$Date = format(as.Date(ns_df$Date, format = "%b %d, %Y"), "%Y-%m-%d") 
cvt_num = apply(ns_df[,2:6], 2, function(x){as.numeric(as.character(gsub(",","",x)))})
ns_df = cbind(ns_df[,c(1,7)],cvt_num)
ns_df = ns_df %>%
  mutate(change_pop = (Close-Open)/Close)
# company

# symbol = 
# https://finance.yahoo.com/quote/FISV/history?period1=1420088400&period2=1514696400&interval=1mo&filter=history&frequency=1mo
table_scrape <- function(symbol){
  
  baseurl <- "https://finance.yahoo.com/quote/"
  tailurl <- "/history?period1=1420088400&period2=1514696400&interval=1mo&filter=history&frequency=1mo"
  url <- paste0(baseurl,symbol,tailurl)
  webpage <- read_html(url)
  
  ## Identify the table of interest
  tbls <- html_nodes(webpage, "table")
  
  ## Scrape the table
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  
  ## Reformat
  colnames(tbls_ls[[1]]) <- c("Date",	"Open",	"High",	"Low",	"Close",	"Adj.Close.",	"Volume")
  company <- rep(symbol,nrow(tbls_ls[[1]]))
  table <- cbind(tbls_ls[[1]],company)
  
  df <- data.frame(table)
  df$Date = format(as.Date(df$Date, format = "%b %d, %Y"), "%Y-%m-%d") 
  cvt_num = apply(df[,2:6], 2, function(x){as.numeric(as.character(gsub(",","",x)))})
  df = cbind(df[,c(1,7)],cvt_num)
  df = df %>%
    mutate(change_pop = (Close-Open)/Close)

}

symbol = c('Fiserv Inc','KO')
fisv = table_scrape("FISV")
fisv = remove_missing(fisv)
ko = table_scrape('KO')
library(stringr)
ko = remove_missing(ko)
ns_df = remove_missing(ns_df)
sp_df = remove_missing(sp_df)
#https://finance.yahoo.com/quote/^IXIC/history?period1=1420088400&period2=1514696400&interval=1d&filter=history&frequency=1d
## ambest

aa = cbind(fisv,ko,ns_df$change_pop,sp_df$change_pop)
colnames(aa) <- c("Date","Volume","Open", "High","Low", "Close", "Adj.Close.","change_pop","Date1", "Volume1","Open1","High1","Low1","Close1","Adj.Close.1","change_pop1","ns_change_pop", "sp_change_pop")

aa = aa %>%
  #filter(Date >= '2015-01-01' && Date <= '2016-12-31') %>%
  mutate(fisv_ns = change_pop/ns_change_pop,fisv_sp = change_pop/sp_change_pop, ko_ns = change_pop1/ns_change_pop, ko_sp = change_pop1/sp_change_pop) %>%
  arrange(Date)

bb = aa[1:24,]
save(bb,file = "price.Rda")
save(aa,file = "p.Rda")
par(mfrow= c(2,1))
matplot(aa[,19:20], type = "l",ylab = "fisv",xlab = "time")
matplot(aa[,21:22], type = "l",ylab = "ko",xlab = "time")

#heatmap(cor(,aa$fe,aa$ko_ns), col = cm.colors(20), symm = TRUE)

library("PerformanceAnalytics")

#chart.Correlation(],histogram=TRUE,pch=19)