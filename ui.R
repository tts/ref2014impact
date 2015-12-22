sidebar <- dashboardSidebar(
  selectizeInput(
    inputId = "unit", 
    label = "Unit of Assessment",
    multiple  = F,
    choices = sort(unique(kw.df$UnitOfA))
  ),
  
  sidebarMenu(
    menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Data by Unit of Assessment", tabName = "dt", icon = icon("th")),
    menuItem("Sentiment analysis stats", tabName = "sdt", icon = icon("th"))
  ),
  
  HTML("<p></p>"),
  HTML("<p></p>"),
  HTML("<p><a href=\"http://impact.ref.ac.uk/CaseStudies/\">Impact case study data by REF2014</a> <a href=\"https://creativecommons.org/licenses/by/4.0/\">(License)</a></p>"),
  HTML("<p></p>"),
  HTML("<p></p>"),
  HTML("<p><a href=\"http://www.alchemyapi.com/\">Text Analysis by AlchemyAPI</a></p>"),
  imageOutput("alchemyAPI"),
  HTML("<p><a href=\"https://blogs.aalto.fi/suoritin/2015/06/25/looking-at-keywords-in-ref2014-impact-case-studies\">Fore more info, see this blog posting</a></p>"),
  width = "258"
)


body <- dashboardBody(
  tabItems(
    
    tabItem("dashboard",
            fluidRow(
              # Number of case studies
              infoBoxOutput("studycount", width = 4),
              # Number of universities
              infoBoxOutput("universitycount", width = 4),
              # Number of unique keywords
              infoBoxOutput("keywordcount", width = 4)
              ),
              
            fluidRow(
              # Keyword sentiment
              infoBoxOutput("keywordSentPos", width = 4),
              infoBoxOutput("keywordSentNeu", width = 4),
              infoBoxOutput("keywordSentNeg", width = 4)
              ),
              
            fluidRow(
              box(
                width = 6, 
                status = "info", solidHeader = TRUE,
                title = "Keyword relevance and sentiment",
                ggvisOutput("plot")
              ),
                            
              box(
                width = 3, 
                status = "success", 
                title = "Top 20 keywords by relevance",
                tableOutput("kwTable")
              ),
              
              box(
                width = 3, 
                status = "success", 
                title = "Top 20 positive keywords",
                tableOutput("sentTable")
              )
            )
    ),
    
    tabItem("dt",
            DT::dataTableOutput("datatable")
    ),
    
    tabItem("sdt",
            d3heatmapOutput("heat")
            #DT::dataTableOutput("sentdatatable")
    )
  )
)


dashboardPage(
  skin = "black",
  dashboardHeader(title = "Keywords of REF2014 impact case studies",
                  titleWidth = "500"),
  sidebar,
  body
)
  
 