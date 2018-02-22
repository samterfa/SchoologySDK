



scrapeSchoologyApiPages = function(objectName){
     
     require(rvest)
     
     url = paste0('https://developers.schoology.com/api-documentation/rest-api-v1/', objectName)
     
     html = read_html(url)
     tables = html %>% html_nodes("table")
     objectDescription = html_table(tables[[1]])
    
     tables = tables[-1]
     
     endpoints = lapply(tables, function(x){html_table(x)$X2[[1]]})
     endpoints = as.character(endpoints)
     endpoints = endpoints[!endpoints == 'NULL']
     
     spans = html %>% html_nodes("span")
     divs = html %>% html_nodes("div")
     return(divs)
     spans = spans[which(html_attr(spans, 'class') == 'mw-headline' & html_attr(spans, 'id') == 'view')]
     
     verbs = html_text(spans)
     return(endpoints)
     return(data.frame(verb = verbs, endpoint = endpoints))
     tables = list()
     for(i in 1:length(tableObjects)){
          print(html_table(tableObjects[[i]]))
          Sys.sleep(1)
          tables = append(tables, html_table(tableObjects[[i]]))
     }
     
     return(tables)
}

htmlTables = function(objectName){
     
     library(rvest)
     myurl = paste0('https://developers.schoology.com/api-documentation/rest-api-v1/', objectName)
     download.file(myurl, destfile="myfile.html", method="curl")
     myhtml <- readChar("myfile.html", file.info("myfile.html")$size)
     myhtml <- gsub("</tr>", "</tr><tr>", myhtml, fixed = TRUE)
     mydata <- read_html(myhtml)
     
     mydf <- mydata %>%
          html_node("table") %>%
          html_table(fill = TRUE)
     
     mydf <- na.omit(mydf)
     
}