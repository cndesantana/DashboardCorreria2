dashboardPage(
  dashboardHeader(title = "Monitoramento do Papo Correria"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      textInput("urlpost", 
                "URL do post no facebook", 
                value = ""
      ),
      textInput("pagepost", 
                "URL da página para onde aponta o post", 
                value = ""
      ),
      textInput("messagepost", 
                "Mensagem do post no facebook", 
                value = ""
      ),
      textInput("token", "Acess Token:", value=""),
      dateInput('date',
                label = 'Data do post:',
                value = Sys.Date()
      ),
      actionButton("dashboard", "Raspar", icon("refresh"), class = "btn btn-primary"),
      actionButton(inputId="update", label="Plotar"),      
      tags$hr("Baixar planilha com Comentários",br()),
      downloadButton("downloadComments", "Download Comments"),
      tags$hr("Baixar planilha com Reações",br()),
      downloadButton("downloadReactions", "Download Reactions"),
#      tags$hr("Palavras a excluir das análises",br()),
#      textInput(inputId = "words",
#                label = "(palavras separadas por vírgula)", 
#                value = ""), 
      tags$hr("Dashboard",br())
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
        fluidRow(
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Reações",
              plotOutput("reactionsPlot", width="100%", height= 600),
              downloadButton("reactionsts","Download") 
           ),
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Comentários",
              plotOutput("commentsPlot", width="100%", height= 600),
              downloadButton("comentariosts","Download")
           )#,
#           box(
#              width = 4, status = "info", solidHeader = TRUE,
#              title = "Wordcloud",
#              plotOutput("wordcloudPlot", width="100%", height= 600),
#              downloadButton("wordcloudts","Download")
#           )
        ),
        fluidRow(
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Unigramas",
              plotOutput("unigramaPlot", width="100%", height= 600),
              downloadButton("unigrama","Download")
           ),
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Unigramas x LOVE",
              plotOutput("loveunigramaPlot", width="100%", height= 600),
              downloadButton("loveunigrama","Download")
           ),
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Unigramas x ANGRY",
              plotOutput("angryunigramaPlot", width="100%", height= 600),
              downloadButton("angryunigrama","Download")
           )
        )
      )
    )
  )
)

