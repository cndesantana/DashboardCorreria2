library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(dplyr)
library(stylo)
library(tidytext)
library(tm)
library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(httr)


function(input, output, session) {

   output$downloadComments<- downloadHandler(
     filename = function() {
        comments_timeseries_filename;
     },
     content = function(file) {
        url <- input$urlpost
        id_pagina <- getFBID(url)
        data <- input$date
        fb_oauth <- input$token
        
        data_inicio <- ymd(as.character(data)) + days(-1);
        data_final <- ymd(as.character(data)) + days(1);
        mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final),n=20)
        
        id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
        post_dados <- getPost(id_post, token=fb_oauth, n= 10000, api="v2.12")
        allmessages <- post_dados$comments %>% select(created_time,from_id,from_name,message);
        names(allmessages) <- c("Data","Autor ID","Autor Nome","Conteúdo");
        wb<-createWorkbook(type="xlsx")
        TABLE_COLNAMES_STYLE <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
        sheet <- createSheet(wb, sheetName = "facebookdata")
        setColumnWidth(sheet, colIndex=1:length(allmessages[1,]), 25)
        addDataFrame(allmessages, sheet, colnamesStyle = TABLE_COLNAMES_STYLE, colStyle = TABLE_COLNAMES_STYLE, startColumn=1, row.names = FALSE)
        saveWorkbook(wb, file)   
     })
  
  output$downloadReactions<- downloadHandler(
     filename = function() {
        reactions_timeseries_filename;
     },
     content = function(file) {
        url <- input$urlpost
        id_pagina <- getFBID(url)
        data <- input$date
        fb_oauth <- input$token
        
        data_inicio <- ymd(as.character(data)) + days(-1);
        data_final <- ymd(as.character(data)) + days(1);
        
        mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final),n=20)
        
        id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
        
        post_dados <- getPost(id_post, token=fb_oauth, n= 10000, reactions=TRUE, api="v2.12")
        allreactions <- post_dados$reactions %>% select(from_name, from_id, from_type);
        names(allreactions) <- c("Autor Nome","Autor ID","Tipo");
        wb<-createWorkbook(type="xlsx")
        TABLE_COLNAMES_STYLE <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
        sheet <- createSheet(wb, sheetName = "facebookdata")
        setColumnWidth(sheet, colIndex=1:length(allreactions[1,]), 25)
        addDataFrame(allreactions, sheet, colnamesStyle = TABLE_COLNAMES_STYLE, colStyle = TABLE_COLNAMES_STYLE, startColumn=1, row.names = FALSE)
        saveWorkbook(wb, file)       
     })
 
  
  ############## DASHBOARD - ATUALIZAÇÃO DOS DADOS VIA CLIQUE EM BOTÃO 'RUN'
  # When the dashboard button is clicked, save the form data
  observeEvent(input$dashboard, {
     withProgress(message = 'Baixando...', value = 0, {
        incProgress(1/5, detail = paste("0%"))
        url <- input$urlpost
        id_pagina <- getFBID(url)
        data <- input$date
        fb_oauth <- input$token
        data_inicio <- ymd(as.character(data)) + days(-1);
        data_final <- ymd(as.character(data)) + days(1);
        incProgress(1/5, detail = paste("25%"))
        mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final),n=20)
        incProgress(1/5, detail = paste("50%"))
        id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
        incProgress(1/5, detail = paste("75%"))
        post_dados <- getPost(id_post, token=fb_oauth, n= 10000, reactions=TRUE, api="v2.12")
        incProgress(1/5, detail = paste("100%"))
        sufix <- paste(
           as.character(format(Sys.time(),"%d%m%Y")),
           digest(input$url),
           sep="");
        saveData(post_dados$comments,names(post_dados$comments),prefix="comments",sufix)
        saveData(post_dados$reactions,names(post_dados$reactions),prefix="reactions",sufix)
     })
     
  })
  
  randomVals <- eventReactive(input$update, {
     runif(10)
  })
  

  ####### DISTRIBUIÇÃO DAS REAÇÕES

    
  output$reactionsPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix))         
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(reactions_timeseries_filename)){
           post_reactions <- read.csv(reactions_timeseries_filename,sep=",")
           reactions <- c("LIKE","LOVE","HAHA","WOW","SAD", "ANGRY")
           counts <- array(0, length(reactions))
           for(i in 1:length(reactions)){
              reac <- reactions[i]
              pos_reactions <- which(reac == names(table(post_reactions$from_type)))
              if(length(pos_reactions) == 0){
                 counts[i] <- 0
              }else{
                 counts[i] <- as.integer(table(post_reactions$from_type))[pos_reactions]
              }
           }
   
           ggplot() + 
              geom_bar(stat="identity", aes(x=reactions, y = counts)) + 
              xlab("Reações") + 
              ylab("Número de Ocorrências") + 
              coord_flip()
           }        
        }
  })

  plotReactionsTS = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(reactions_timeseries_filename)){
        post_reactions <- read.csv(reactions_timeseries_filename,sep=",")
        reactions <- c("LIKE","LOVE","HAHA","WOW","SAD", "ANGRY")
        counts <- array(0, length(reactions))
        for(i in 1:length(reactions)){
           reac <- reactions[i]
           pos_reactions <- which(reac == names(table(post_reactions$from_type)))
           if(length(pos_reactions) == 0){
              counts[i] <- 0
           }else{
              counts[i] <- as.integer(table(post_reactions$from_type))[pos_reactions]
           }
        }
        
        ggplot() + 
           geom_bar(stat="identity", aes(x=reactions, y = counts)) + 
           xlab("Reações") + 
           ylab("Número de Ocorrências") + 
           coord_flip()
     }
  }
  
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
  
  ####### COMENTÁRIOS NO TEMPO
  
  output$commentsPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix))
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(comments_timeseries_filename)){
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           
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
              hour = as.numeric(hour) - 3,
              hour = as.character(hour), 
              min = ymd_hms(created_time) %>%
                 format("%M")
           ) %>%
              mutate(date = as.POSIXct(paste(paste(day,month,year,sep="/"),paste(hour,min,sep=":")),format = "%d/%m/%Y %H:%M")) %>%
              group_by(date) %>%
              summarise(total = n()) %>% head(72)
           
           myx <- timeseries$date
           mydate <- format(as.POSIXct(myx), format="%d/%m/%Y %H:%M")
           myy <- timeseries$total
           ggplot(timeseries) + geom_line(stat = "identity", aes(x = date, y = total)) + ylab("Comentários por minuto") + xlab("Tempo")
        }
     }
  })
  
  plotComentariosTS = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        
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
           hour = as.numeric(hour) - 3,
           hour = as.character(hour),
           min = ymd_hms(created_time) %>%
              format("%M")
        ) %>%           
           mutate(date = as.POSIXct(paste(paste(day,month,year,sep="/"),paste(hour,min,sep=":")),format = "%d/%m/%Y %H:%M")) %>%
           group_by(date) %>%
           summarise(total = n()) %>% head(72)
        
        myx <- timeseries$date
        mydate <- format(as.POSIXct(myx), format="%d/%m/%Y %H:%M")
        myy <- timeseries$total
        ggplot(timeseries) + geom_line(stat = "identity", aes(x = date, y = total)) + ylab("Comentários por minuto") + xlab("Tempo")
        
     }
  }

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
  
  
  
  ####### WORDCLOUDS
  
  plotWordcloudTS = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        
        text <- as.character(comments_dataframe$message)
        mydfm <- getDFMatrix(text);
        set.seed(100)
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(8,"Dark2"))     
     }
  }  
  
  output$wordcloudPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(comments_timeseries_filename)){
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           
           text <- as.character(comments_dataframe$message)
           mydfm <- getDFMatrix(text);
           set.seed(100)
           textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                              rot.per = .25, 
                              colors = RColorBrewer::brewer.pal(8,"Dark2"))
        }
     }
  })
  
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
  
  ###### UNIGRAM

  output$unigramaPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(comments_timeseries_filename)){
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           text <- as.character(comments_dataframe$message)
           
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
  })
  
  plotUnigrama = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        text <- as.character(comments_dataframe$message)
        
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
  
  
  ###### LOVE
  
  output$loveunigramaPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix))
     
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(comments_timeseries_filename) & file.exists(reactions_timeseries_filename)){      
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           reactions_dataframe <- read.csv(reactions_timeseries_filename,sep=",")
           comments_dataframe <- left_join(comments_dataframe,reactions_dataframe,by=c("from_id","from_name"));
           
           comments_dataframe <- comments_dataframe %>% filter(from_type == "LOVE")
           if(length(comments_dataframe$from_type) > 0){
              text <- as.character(comments_dataframe$message)
           
              unigram <- getUnigram(text)
              unigram %>% 
                 filter(!is.na(words)) %>% 
                 select(words) %>% group_by(words) %>% 
                 summarise(total = n()) %>% 
                 arrange(total) %>% tail(20) %>% 
                 ggplot(aes(reorder(words,total), total)) +
                 geom_bar(stat = "identity", fill = corpositivo) + 
                 xlab("Palavras") + ylab("Frequência") +
                 ggtitle("Palavras mais frequentes + LOVE") +
                 geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
                 coord_flip()
           }
        }
     }
  })
  
  plotLoveUnigrama = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename) & file.exists(reactions_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        reactions_dataframe <- read.csv(reactions_timeseries_filename,sep=",")
        comments_dataframe <- left_join(comments_dataframe,reactions_dataframe,by=c("from_id","from_name"));
        
        comments_dataframe <- comments_dataframe %>% filter(from_type == "LOVE")
        if(length(comments_dataframe$from_type) > 0){
           text <- as.character(comments_dataframe$message)
           
           unigram <- getUnigram(text)
           unigram %>% 
              filter(!is.na(words)) %>% 
              select(words) %>% group_by(words) %>% 
              summarise(total = n()) %>% 
              arrange(total) %>% tail(20) %>% 
              ggplot(aes(reorder(words,total), total)) +
              geom_bar(stat = "identity", fill = corpositivo) + 
              xlab("Palavras") + ylab("Frequência") +
              ggtitle("Palavras mais frequentes + LOVE") +
              geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
              coord_flip()
           }
     }
  }
  
  output$loveunigrama = downloadHandler(
     filename = function() {
        paste("loveunigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotLoveUnigrama(), device = device)
     }     
  )
  

  ############ ANGRY
  
  output$angryunigramaPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     
     cat(paste("Numero de elementos: ",length(randomVals()),sep=""),sep="\n")
     if(length(randomVals()) > 0){
        if(file.exists(comments_timeseries_filename) & file.exists(reactions_timeseries_filename)){      
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           reactions_dataframe <- read.csv(reactions_timeseries_filename,sep=",")
           comments_dataframe <- left_join(comments_dataframe,reactions_dataframe,by=c("from_id","from_name"));
           
           comments_dataframe <- comments_dataframe %>% filter(from_type == "ANGRY")
           if(length(comments_dataframe$from_type) > 0){
              text <- as.character(comments_dataframe$message)
              
              unigram <- getUnigram(text)
              unigram %>% 
                 filter(!is.na(words)) %>% 
                 select(words) %>% group_by(words) %>% 
                 summarise(total = n()) %>% 
                 arrange(total) %>% tail(20) %>% 
                 ggplot(aes(reorder(words,total), total)) +
                 geom_bar(stat = "identity", fill = cornegativo) + 
                 xlab("Palavras") + ylab("Frequência") +
                 ggtitle("Palavras mais frequentes + ANGRY") +
                 geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
                 coord_flip()
           }
        }
     }
  })
  
  plotAngryUnigrama = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$url),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     prefix <- "reactions";
     reactions_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename) & file.exists(reactions_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        reactions_dataframe <- read.csv(reactions_timeseries_filename,sep=",")
        comments_dataframe <- left_join(comments_dataframe,reactions_dataframe,by=c("from_id","from_name"));
        
        comments_dataframe <- comments_dataframe %>% filter(from_type == "ANGRY")
        if(length(comments_dataframe$from_type) > 0){
           text <- as.character(comments_dataframe$message)
           
           unigram <- getUnigram(text)
           unigram %>% 
              filter(!is.na(words)) %>% 
              select(words) %>% group_by(words) %>% 
              summarise(total = n()) %>% 
              arrange(total) %>% tail(20) %>% 
              ggplot(aes(reorder(words,total), total)) +
              geom_bar(stat = "identity", fill = cornegativo) + 
              xlab("Palavras") + ylab("Frequência") +
              ggtitle("Palavras mais frequentes + ANGRY") +
              geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
              coord_flip()
        }
     }
  }
  
  output$angryunigrama = downloadHandler(
     filename = function() {
        paste("angryunigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotAngryUnigrama(), device = device)
     }     
  )
    
  
}


