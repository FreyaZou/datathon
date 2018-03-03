library(httr)





auth_req <- POST("https://idfs.gs.com/as/token.oauth2", 
                 body = args, 
                 encode = "form", 
                 verbose())

access_token = content(auth_req, "parsed")$access_token


data_req <- GET("https://api.marquee.gs.com/v1/data/USCANFPP_MINI/coverage", 
                add_headers(Authorization = paste('Bearer', access_token)), 
                verbose())
content(data_req)
