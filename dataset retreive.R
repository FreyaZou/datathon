library(httr)

args = list(
  grant_type    = 'client_credentials',
  client_id     = 'd7a76e446c0846a2b5eb1cbffce3f50a',
  client_secret = 'dcaad73af8aa4f578a623eacbdd17cb6db952a65e0929eb55826982bebc08c10',
  scope         = 'read_financial_data'
);

auth_req <- POST("https://idfs.gs.com/as/token.oauth2", 
                 body = args, 
                 encode = "form", 
                 verbose())

access_token = content(auth_req, "parsed")$access_token

#content()

data_req <- GET("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/coverage", 
                add_headers(Authorization = paste('Bearer', access_token)), 
                verbose())
content(data_req)

auth_req <- POST("https://idfs.gs.com/as/token.oauth2", 
                 body = args, 
                 encode = "form", 
                 verbose())

access_token = content(auth_req, "parsed")$access_token


data_req <- GET("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/coverage", 
                add_headers(Authorization = paste('Bearer', access_token)), 
                verbose())
content(data_req)

#head(data_req)

data_req <- POST("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/query", 
                 body = '{
                 "where": {
                 "gsid": [ "10516", "10696", "11308", "11896", "13901" ]
                 },
                 "startDate": "2015-11-02",
                 "endDate": "2015-12-04"
                 }', 
                 encode = "json", 
                 add_headers('Content-Type' = 'application/json', Authorization = paste('Bearer', access_token)), 
                 verbose())
content(data_req)
#####
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


dat_org01 = data.frame(t(matrix(unlist(content(data_req)),nrow = 7, ncol = nrow(data.frame(unlist(content(data_req))))/7)))
colnames(dat_org01) = c("date", "gsid", "financialReturnsScore", "growthScore", "multipleScore", "integratedScore","updateTime")
dat_org01[,1] = as.Date(dat_org01[,1])
dat_org02 = data.frame(dat_org01)
num = apply(dat_org02[,3:6],2,function(x){as.numeric(as.character(x))})
dat_org03 = cbind(dat_org02[,1:2],num,dat_org02[,7])ro
library(dplyr)
library(ggplot2)

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

dat_org03 %>%
  group_by(gsid)%>%
  ggplot() +
  geom_line(aes(x = date, y = financialReturnsScore, color = gsid))


####
dat_org01 = data.frame(t(matrix(unlist(content(data_req)),nrow = 7, ncol = nrow(data.frame(unlist(content(data_req))))/7)))

colnames(dat_org01) = c("date", "gsid", "financialReturnsScore", "growthScore", "multipleScore", "integratedScore","updateTime")

dat_org02 = data.frame(dat_org01)

dat_org02[,1] = as.Date(dat_org02[,1])

num = apply(dat_org02[,3:6],2,function(x){as.numeric(as.character(x))})

dat_org03 = cbind(dat_org02[,1:2],num,dat_org02[,7])

library(ggplot2)
library(dplyr)
dat_org03 %>%
  ggplot()+
  geom_line(aes(y = financialReturnsScore,x= date))
  #geom_line(aes(y = growthScore,x= date,color = gsid))+
  
matplot(y = financialReturnsScore,x= date,data = dat_org03)
test = dat_org03 %>%
  filter(date >= 2015-11-02 & date <= 2015-11-04)
pairs(~, data = test[,3:6],pch=21)


heatmap(cor(BloodBrain[,c(15,11,12,13,10)]), col = cm.colors(100), symm = TRUE)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               




