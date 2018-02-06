getUnigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 1L, n_min = 1L, simplify = TRUE)))
   
   unigram$words[which(!is.na(str_extract(unigram$words,"comentario")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"timeline")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"video")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"photos")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"conta")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"eliminatoria")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"eliminatória")))] <- NA
   
   return(unigram)
}

getTrigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   trigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 3L, n_min = 3L, simplify = TRUE)))
   
   trigram$words[which(!is.na(str_extract(trigram$words,"conta")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"essa")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"bata")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"tres")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"dois")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"apenas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"comentario")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"timeline")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"video")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"photos")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"sou")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"mais")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"2vx9vf4")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"hidrica")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"barragem")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"www.facebook")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"www.boatos.org")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"etapas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamados")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamadas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"mil")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatórias")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatorias")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatoria")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatória")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"convocou")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamou")))] <- NA
   
   
   return(trigram)
}

getBigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   bigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 2L, n_min = 2L, simplify = TRUE)))
   
   bigram$words[which(!is.na(str_extract(bigram$words,"conta")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"essa")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"bata")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"tres")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"dois")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"apenas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"comentario")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"timeline")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"video")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"photos")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"sou")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"mais")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"2vx9vf4")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"hidrica")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"barragem")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"www.facebook")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"www.boatos.org")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"etapas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamados")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamadas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"mil")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"eliminatórias")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"eliminatorias")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"convocou")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamou")))] <- NA
   
   
   return(bigram)
}

function(input, output, session) {

  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)

  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5

  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)

  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  dlCount <- downloadCount(pkgStream)

  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)

  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())

  
  
  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)

    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = "Downloads per sec (last 5 min)",
      icon = icon("area-chart"),
      color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })

  output$count <- renderValueBox({
    valueBox(
      value = dlCount(),
      subtitle = "Total downloads",
      icon = icon("download")
    )
  })

  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique users",
      icon = icon("users")
    )
  })

  eventReactive(input$downloadExcelData,{
        url <- input$urlpost
        id_pagina <- getFBID(url)
        data <- input$date
        
        # command file.path already controls for the OS
        load(paste(workdir,"/fb_oauth",sep=""));
        
        data_inicio <- ymd(as.character(data)) + days(-1);
        data_final <- ymd(as.character(data)) + days(1);
        
        mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final),n=20)
        
        id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
        
        post_dados <- getPost(id_post, token=fb_oauth, n= 1000000, reactions=TRUE)
        post_comments <- left_join(post_dados$comments,post_dados$reactions,by=c("from_id","from_name"));
        post_reactions <- getReactions(id_post, token=fb_oauth)
        counts <- as.numeric(post_reactions[,-1])
        reactions_names <- c("created_time","Likes","Loves","Haha","Wow","Sad", "Angry")
        
        df_reactions <- data.frame(matrix(unlist(list(as.character(Sys.time()), counts)),ncol=length(reactions_names)))
        names(df_reactions) = reactions_names
        
        write.xlsx(df_reactions, file=reactions_timeseries_filename)        
        write.xlsx(post_comments, file=comments_timeseries_filename)    
  })
  
  
  plotReactionsTS = function(){
     if(file.exists(reactions_timeseries_filename)){
        post_reactions <- read_xlsx(reactions_timeseries_filename)
        counts <- as.numeric(post_reactions[,-1])
        reactions <- c("Likes","Loves","Haha","Wow","Sad", "Angry")
        percent <- signif(100*(counts/sum(counts)),1)
        ggplot() + 
           geom_bar(stat="identity", aes(x=reactions, y = counts)) + 
           xlab("Reações") + 
           ylab("Número de Ocorrências") + 
           coord_flip()    
     }
  }
  
  plotSentimentoTS = function(){
     if(file.exists(reactions_timeseries_filename)){
        reactions_dataframe <- read_xlsx(reactions_timeseries_filename)
        
        timeseries <- reactions_dataframe %>% mutate(
           day = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%d"), 
           month = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%m"), 
           year = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%Y"), 
           hour = ymd_hms(created_time) %>%
              format("%H"), 
           min = ymd_hms(created_time) %>%
              format("%M")
        ) %>%
           group_by(year,month,day,hour,min) %>%
           summarise(mediais = mean(is))
        
        timeseries <- timeseries[-c(1:22),]; 
        timeseries$hour <- as.numeric(timeseries$hour) - 3
        timeseries$hour <- as.character(timeseries$hour)
        
        dates <- paste(timeseries$day,timeseries$month,timeseries$year,sep="/");
        times <- paste(timeseries$hour,timeseries$min,sep=":");
        myx <- paste(dates, times)
        mydate <- strptime(myx, "%d/%m/%Y %H:%M")
        myy <- timeseries$mediais
        
        ggplot() + geom_line(stat = "identity", aes(x = mydate, y = myy)) + ylab("Índice de Sentimento") + xlab("Tempo")
     }
  }
  
  plotBigrama = function(){
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        bigram <- getBigram(text)
        bigram %>% 
           filter(!is.na(words)) %>% 
           select(words) %>% 
           group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Pares de Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()     
     }
  }
  
  plotTrigrama = function(){
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        trigram <- getTrigram(text)
        trigram %>%
           filter(!is.na(words)) %>% 
           select(words) %>% 
           group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Trios de Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()       
     }
  }
  
  plotUnigrama = function(){
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        unigram <- getUnigram(text)
        unigram %>% 
           filter(!is.na(words)) %>% 
           select(words) %>% group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()
     }
  }
  
  plotWordcloudTS = function(){
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        mydfm <- getDFMatrix(text);
        set.seed(100)
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(8,"Dark2"))     
     }
  }  
  
  plotComentariosTS = function(){
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)

        timeseries <- comments_dataframe %>% mutate(
           day = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%d"), 
           month = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%m"), 
           year = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%Y"), 
           hour = ymd_hms(created_time) %>%
              format("%H"), 
           min = ymd_hms(created_time) %>%
              format("%M")
        ) %>%
           group_by(year,month,day,hour,min) %>%
           summarise(total = n())
        
        timeseries <- timeseries[-c(1:22),];
        timeseries$hour <- as.numeric(timeseries$hour) - 3#por causa da diferença de fuso com relação ao GMT
        timeseries$hour <- as.character(timeseries$hour)
        
        dates <- paste(timeseries$day,timeseries$month,timeseries$year,sep="/");
        times <- paste(timeseries$hour,timeseries$min,sep=":");
        myx <- paste(dates, times)
        mydate <- strptime(myx, "%d/%m/%Y %H:%M")
        myy <- timeseries$total
        
        ggplot() + geom_line(stat = "identity", aes(x = mydate, y = myy)) + ylab("Comentários por minuto") + xlab("Tempo")
     }
  }
  
  #####################################3 DASHBOARD
  
  output$reactionsPlot <- renderPlot({
     if(file.exists(reactions_timeseries_filename)){
        post_reactions <- read_xlsx(reactions_timeseries_filename)
        counts <- as.numeric(post_reactions[,-1])
        
        reactions <- c("Likes","Loves","Haha","Wow","Sad", "Angry")
        percent <- signif(100*(counts/sum(counts)),1)
        ggplot() + 
           geom_bar(stat="identity", aes(x=reactions, y = counts)) + 
           xlab("Reações") + 
           ylab("Número de Ocorrências") + 
           coord_flip()
     }
  })

  output$sentimentPlot <- renderPlot({
     if(file.exists(reactions_timeseries_filename)){
        reactions_dataframe <- read_xlsx(reactions_timeseries_filename)

        timeseries <- reactions_dataframe %>% mutate(
           day = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%d"), 
           month = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%m"), 
           year = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%Y"), 
           hour = ymd_hms(created_time) %>%
              format("%H"), 
           min = ymd_hms(created_time) %>%
              format("%M")
        ) %>%
           group_by(year,month,day,hour,min) %>%
           summarise(mediais = mean(is))
        
        timeseries <- timeseries[-c(1:22),]; 
        timeseries$hour <- as.numeric(timeseries$hour) - 3
        timeseries$hour <- as.character(timeseries$hour)
        
        dates <- paste(timeseries$day,timeseries$month,timeseries$year,sep="/");
        times <- paste(timeseries$hour,timeseries$min,sep=":");
        myx <- paste(dates, times)
        mydate <- strptime(myx, "%d/%m/%Y %H:%M")
        myy <- timeseries$mediais
        
        ggplot() + geom_line(stat = "identity", aes(x = mydate, y = myy)) + ylab("Índice de Sentimento") + xlab("Tempo")
     }
  })
    
  output$wordcloudPlot <- renderPlot({
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        mydfm <- getDFMatrix(text);
        set.seed(100)
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(8,"Dark2"))
     }
  })
  
  output$unigramaPlot <- renderPlot({
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        unigram <- getUnigram(text)
        unigram %>% 
           filter(!is.na(words)) %>% 
           select(words) %>% group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()
     }
  })
  
  output$bigramaPlot <- renderPlot({
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        bigram <- getBigram(text)
        bigram %>% 
           filter(!is.na(words)) %>% 
           select(words) %>% 
           group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Pares de Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()     
     }
  })
  
  output$trigramaPlot <- renderPlot({
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)
        text <- comments_dataframe %>% select(message) %>% as.character()
        trigram <- getTrigram(text)
        trigram %>%
           filter(!is.na(words)) %>% 
           select(words) %>% 
           group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Trios de Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()       
     }
  })

  output$commentsPlot <- renderPlot({
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read_xlsx(comments_timeseries_filename)

        timeseries <- comments_dataframe %>% mutate(
           day = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%d"), 
           month = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%m"), 
           year = ymd_hms(created_time) %>%
              as.Date() %>%
              format("%Y"), 
           hour = ymd_hms(created_time) %>%
              format("%H"), 
           min = ymd_hms(created_time) %>%
              format("%M")
        ) %>%
           group_by(year,month,day,hour,min) %>%
           summarise(total = n())
        timeseries <- timeseries; 
        timeseries$hour <- as.numeric(timeseries$hour) - 3 #por causa da diferença de fuso com relação ao GMT
        timeseries$hour <- as.character(timeseries$hour)
        
        dates <- paste(timeseries$day,timeseries$month,timeseries$year,sep="/");
        times <- paste(timeseries$hour,timeseries$min,sep=":");
        myx <- paste(dates, times)
        mydate <- strptime(myx, "%d/%m/%Y %H:%M")
        myy <- timeseries$total
        
        ggplot() + geom_line(stat = "identity", aes(x = mydate, y = myy)) + ylab("Comentários por minuto") + xlab("Tempo")
     }
   })
    
  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()

    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)
    
    bubbles(df$n, df$package, key = df$package)
  })

  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Package name" = package, "% of downloads" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)

  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows))
    options(orig)
  })
  
  output$comentariosts = downloadHandler(
     filename = function() {
        paste("comentariosts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotComentariosTS(), device = device)
     }     
  )
  
  output$reactionsts = downloadHandler(
     filename = function() {
        paste("reactionsts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotReactionsTS(), device = device)
     }     
  )

  output$sentimentts = downloadHandler(
     filename = function() {
        paste("sentimentosts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotSentimentoTS(), device = device)
     }     
  )  
    
  output$wordcloudts = downloadHandler(
     filename = function() {
        paste("wordcloudts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloudTS(), device = device)
     }     
  )
  
  output$unigrama = downloadHandler(
     filename = function() {
        paste("unigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotUnigrama(), device = device)
     }     
  )
  
  output$bigrama = downloadHandler(
     filename = function() {
        paste("bigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotBigrama(), device = device)
     }     
  )
  
  output$trigrama = downloadHandler(
     filename = function() {
        paste("trigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotTrigrama(), device = device)
     }     
  )
}


