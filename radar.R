clock.plot <- function (x, col = rainbow(n), ...) {
   if( min(x)<0 ) x <- x - min(x)
   if( max(x)>1 ) x <- x/max(x)
   n <- length(x)
   if(is.null(names(x))) names(x) <- 0:(n-1)
   m <- 1.05
   par(oma=c(0,0,0,0))
   plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
   a <- pi/2 - 2*pi/200*0:200
   polygon( cos(a), sin(a) )
   v <- .02
   a <- pi/2 - 2*pi/n*0:n
   segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
   segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
   ca <- -2*pi/n*(0:50)/50
   for (i in 1:n) {
      a <- pi/2 - 2*pi/n*(i-1)
      b <- pi/2 - 2*pi/n*i
      polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
      v <- .1
      text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
   }
}

getPalavraMaisCitada <- function(text){
   
   allwords <- unlist(str_extract_all(text, boundary("word")))
 #remover todas as palavras com apenas uma letra 
   docs <- Corpus(VectorSource(allwords))
   toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
   docs <- tm_map(docs, toSpace, "/")
   docs <- tm_map(docs, toSpace, "@")
   docs <- tm_map(docs, toSpace, "\\|")
   # Convert the text to lower case
   docs <- tm_map(docs, content_transformer(tolower))
   # Remove numbers
   docs <- tm_map(docs, removeNumbers)
   # Remove english common stopwords
   docs <- tm_map(docs, removeWords, stopwords("portuguese"))
   # Remove your own stop word
   # specify your stopwords as a character vector
   docs <- tm_map(docs, removeWords, badwords) 
   # Remove punctuations
   docs <- tm_map(docs, removePunctuation)
   # Eliminate extra white spaces
   docs <- tm_map(docs, stripWhitespace)
   dtm <- TermDocumentMatrix(docs)
   m <- as.matrix(dtm)
   v <- sort(rowSums(m),decreasing=TRUE)
   d <- data.frame(word = names(v),freq=v)
   head(d, 1)
}

get50TopPalavras <- function(text){
   
   allwords <- unlist(str_extract_all(text, boundary("word")))
   
   docs <- Corpus(VectorSource(allwords))
   toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
   docs <- tm_map(docs, toSpace, "/")
   docs <- tm_map(docs, toSpace, "@")
   docs <- tm_map(docs, toSpace, "\\|")
   # Convert the text to lower case
   docs <- tm_map(docs, content_transformer(tolower))
   # Remove numbers
   docs <- tm_map(docs, removeNumbers)
   # Remove english common stopwords
   docs <- tm_map(docs, removeWords, stopwords("portuguese"))
   # Remove your own stop word
   # specify your stopwords as a character vector
   docs <- tm_map(docs, removeWords, badwords) 
   # Remove punctuations
   docs <- tm_map(docs, removePunctuation)
   # Eliminate extra white spaces
   docs <- tm_map(docs, stripWhitespace)
   dtm <- TermDocumentMatrix(docs)
   m <- as.matrix(dtm)
   v <- sort(rowSums(m),decreasing=TRUE)
   d <- data.frame(word = names(v),freq=v)
   head(d, 50)
}

badwords <- c("scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","senhor","comentário","perfil","mais","com","está","por","uma","tem","vai","pelo","meu","sobre","não","já","nos","sem","quando","xed","xbd","ser","xbe","xa0","x8f","xb9","xb2","xb0","xb1","xb8","x8c","xa3","xbc","xaa")

library(Rfacebook)
library(fmsb)

id <- "105821366170914"
url <- "https://www.facebook.com/pt.brasil/videos/1516388621780841/"
data_inicio <- "2017/10/11"
data_fim <- "2017/10/13"
load("/home/cdesantana/DataSCOUT/Objectiva/BadogueExcel/fb_oauth")

page <- getPage(id,token=fb_oauth, since=data_inicio, until = data_fim)
idpost <- page %>% filter(link == url) %>% select(id)
post <- getPost(idpost, token=fb_oauth, reactions=TRUE)
posts_pertime <- post$comments %>% select(from_id, created_time, message) %>% left_join(post$reactions, by="from_id") %>% select(-from_name)

beginning <- sort(posts_pertime$created_time)[1]
ending <- sort(posts_pertime$created_time, decreasing = TRUE)[1]

wordspermin <- posts_pertime %>% mutate(
   beginning_day = ymd_hms(beginning) %>% as_datetime %>% format("%d"),
   beginning_month = ymd_hms(beginning) %>% as_datetime %>% format("%m"),
   beginning_year = ymd_hms(beginning) %>% as_datetime %>% format("%Y"),
   beginning_hour = ymd_hms(beginning) %>% as_datetime %>% format("%H"),
   beginning_minute = ymd_hms(beginning) %>% as_datetime %>% format("%M"),
   day = ymd_hms(created_time) %>% as_datetime %>% format("%d"), 
   month = ymd_hms(created_time) %>% as_datetime %>% format("%m"), 
   year = ymd_hms(created_time) %>% as_datetime %>% format("%Y"), 
   hour = ymd_hms(created_time) %>% as_datetime %>%format("%H"), 
   min = ymd_hms(created_time) %>% as_datetime %>% format("%M"),
   min5 = (difftime(
         strptime(paste(paste(day,month,year,sep="/"),paste(hour,min,sep=":"),sep=""), format = "%d/%m/%Y %H:%M"),
         strptime(paste(paste(beginning_day,beginning_month,beginning_year,sep="/"),paste(beginning_hour,beginning_minute,sep=":"),sep=""), format = "%d/%m/%Y %H:%M")
         ,units="mins") %>% as.numeric() %/% 10 ) +1
) 

wordsper5min <- wordspermin %>%
   select(from_id, hour, min, min5, message) %>%
   filter(min5 <= 6) %>%
   group_by(min5) %>%
   summarise(palavra = as.character(getPalavraMaisCitada(message)$word), 
             count = as.numeric(getPalavraMaisCitada(message)$freq))
data=as.data.frame(matrix(as.numeric(wordsper5min$count), ncol=length(as.numeric(wordsper5min$count))))
colnames(data)=as.character(wordsper5min$palavra)
x <- wordsper5min$count
names(x) <- as.character(wordsper5min$palavra)
# Clock plot functio
png("./clockplot_PT_1a.png",width=3200,height=3200,res=300)
clock.plot(x, main = "1a palavras mais citadas a cada 10 minutos")
dev.off()



apoiadores <- wordspermin %>%
   group_by(from_id) %>% 
   summarise(count = n()) %>% tail(50)
p <- ggplot(apoiadores, aes(x=reorder(apoiadores$from_id,as.numeric(apoiadores$count)), y = as.numeric(apoiadores$count))) + geom_bar(stat="identity") + ylab("Número de comentários") + xlab("Apoiadores") + coord_flip()
png("apoiadores_PT.png",width=3200,height=1800,res=300)
print(p)
dev.off();

bolsonarointime <- wordspermin %>%
   select(hour, min, message) %>%
   group_by(hour, min) %>% 
   summarise(count = sum(str_detect(message, "Bolsonaro")))
cirointime <- wordspermin %>%
   select(hour, min, message) %>%
   group_by(hour, min) %>% 
   summarise(count = sum(str_detect(message, "Ciro")))

p <- left_join(cirointime,bolsonarointime,by=c('hour','min')) %>% mutate(date = as.POSIXct(paste(hour,min,sep=":"),format = "%H:%M")) %>% mutate(Ciro = count.x, Bolsonaro = count.y) %>% select(date, Ciro, Bolsonaro) %>% gather(Candidatos,value, Ciro, Bolsonaro) %>% ggplot(aes(x=date, y=value, colour=Candidatos)) + geom_line() + ylab("Número de posts") + xlab("Tempo")
png("PalavrasNoTempo.png",width=3200,height=1800,res=300)
print(p)
dev.off()




wordspermin %>%
   select(from_id, hour, min, min5, message) %>%
   filter(min5 <= 6) %>%
   group_by(min5) %>%
   summarise(palavra = as.character(getPalavraMaisCitada(message)$word), 
             count = as.numeric(getPalavraMaisCitada(message)$freq))

