dashboardPage(
  dashboardHeader(title = "Monitoramento do Papo Correria"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      textInput("urlpost", 
                "URL do post", 
                value = ""
      ),
      dateInput('date',
                label = 'Data do post:',
                value = Sys.Date()
      ),
      actionButton("downloadExcelData", "Run")
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
          ),
          box(
             width = 4, status = "info", solidHeader = TRUE,
             title = "Wordcloud",
             plotOutput("wordcloudPlot", width="100%", height= 600),
             downloadButton("wordcloudts","Download")
          )
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
              title = "Bigramas",
              plotOutput("bigramaPlot", width="100%", height= 600),
              downloadButton("bigrama","Download")
           ),
           box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Trigramas",
              plotOutput("trigramaPlot", width="100%", height= 600),
              downloadButton("trigrama","Download")
           )
        )
      )
    )
  )
)

