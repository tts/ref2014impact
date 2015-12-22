function(input, output, session) {
  
  unis <- reactive({
    if (!is.null(input$unit) && nrow(kw.df) > 0) {
      univWithThisUnit <- kw.df %>%
        filter(UnitOfA == input$unit)
    }
  })
  
  univCount <- reactive({
    if (!is.null(unis())) {
      length(unique(unis()$University))
    }
  })
  
  studyCount <- reactive({
    if (!is.null(unis())) {
      length(unique(unis()$id))
    }
  })
  
  kwCount <- reactive({
    if (!is.null(unis())) {
      length(unique(unis()$Keyword))
    }
  })
  
  kwMean <- reactive({
    if (!is.null(unis())) {
      round(mean(unis()$Relevance), digits = 3)
    }
  })
  
  negSentProc <- reactive({
    if (!is.null(unis())) {
      paste0(round((nrow(unis()[unis()$SentimentType == 'negative',]) / nrow(unis())) * 100, digits=1), "%")
    }
  })
  
  posSentProc <- reactive({
    if (!is.null(unis())) {
      paste0(round((nrow(unis()[unis()$SentimentType == 'positive',]) / nrow(unis())) * 100, digits=1), "%")
    }
  })
  
  neutSentProc <- reactive({
    if (!is.null(unis())) {
      paste0(round((nrow(unis()[unis()$SentimentType == 'neutral',]) / nrow(unis())) * 100, digits=1), "%")
    }
  })
    
  output$universitycount <- renderInfoBox({
    infoBox(
      value = univCount(),
      title = "Institutions",
      icon = icon("institution"),
      color = if (univCount() >= 200) "orange" else "aqua",
      fill = TRUE
    )
  })
  
  output$studycount <- renderInfoBox({
    infoBox(
      value = studyCount(),
      title = "Case studies",
      icon = icon("bar-chart"),
      fill = TRUE
      )
  })
  
  output$keywordcount <- renderInfoBox({
    infoBox(
      value = paste(kwCount(), kwMean(), sep = " / "),
      title = "Unique keywords / relevance mean",
      icon = icon("language"),
      fill = TRUE
      )
  })
  

  output$keywordSentNeg <- renderInfoBox({
    infoBox(
      value = negSentProc(),
      title = "Negative keywords",
      icon = icon("arrow-down"),
      color = "blue"
    )
  })
  
  output$keywordSentPos <- renderInfoBox({
    infoBox(
      value = posSentProc(),
      title = "Positive keywords",
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$keywordSentNeu <- renderInfoBox({
    infoBox(
      value = neutSentProc(),
      title = "Neutral keywords",
      icon = icon("arrow-right"),
      color = "orange"
    )
  })
  
  
  
  # See http://127.0.0.1:29528/library/xtable/html/xtable.html for digits
  output$kwTable <- renderTable({
    
    ut <- data.frame(
      Keyword = unis()$Keyword,
      Relevance = unis()$Relevance,
      stringsAsFactors=FALSE
    )
    
    ut$Relevance <- as.double(ut$Relevance)
    
    ut %>%
      filter(Relevance >= relThr) %>%
      arrange(desc(Relevance), Keyword) %>%
      head(20)
    
  }, digits = 3, include.rownames = FALSE)
  
  
  
  output$sentTable <- renderTable({
    
    ut <- data.frame(
      Keyword = unis()$Keyword,
      Score = unis()$SentimentScore,
      stringsAsFactors=FALSE
    )
    
    ut$Score <- as.double(ut$Score)
    
    ut %>%
      filter(Score >= sentThr) %>%
      arrange(desc(Score), Keyword) %>%
      head(20)
    
  }, digits = 3, include.rownames = FALSE)
  
  
  
  
  output$datatable <- DT::renderDataTable({
    
    baseurl <- "http://impact.ref.ac.uk/CaseStudies/CaseStudy.aspx?Id="
    
    tb <- unis()      
    tb$id <- lapply(tb$id, function(x) paste0("<a href=\"", baseurl, x, "\">", x, "</a>"))
    tb <- tb %>%
      group_by(University, Keyword, Relevance)    
    tb
    
  }, options = list(
       pageLength = 10
       ))
  
  
  
  output$sentdatatable <- DT::renderDataTable({
    
    sentStat
    
  }, options = list(
    pageLength = 50
    ))
    
  
  output$heat <- renderD3heatmap({
        
    d3heatmap(sentStat, scale = "column", dendrogram = "none", colors = "YlOrBr",
              xaxis_height = 60, yaxis_width = 500,
              xaxis_font_size = "8pt")

  })
  
    

  output$alchemyAPI <- renderImage({
   
   list(src = alchemyapi.image,
        contentType = 'image/png',
        width = 259,
        height = 64,
        alt = "AlchemyAPI")
  }, deleteFile = FALSE)
 
 
 
 kw_tooltip <- function(x) {
   if (is.null(x)) return(NULL)
   if (is.null(x$id)) return(NULL)
   
   # Pick out the keyword with this ID
   all_unis <- isolate(unis())
   uni <- all_unis[all_unis$id == x$id & all_unis$Relevance == x$Relevance, ]
   
   paste0(uni$Keyword, "<br>",
          "<b>", uni$University, "</b><br>",
          "ID: ", uni$id, "<br>")
 }
 
 
 
 vis <- reactive({

   visdf <- unis()
   visdf$Type <- factor(visdf$SentimentType)
   
   xvar_name <- paste0("Relevance (min ", relThr, ")")
   yvar_name <- "Sentiment score"
  
      
   xvar <- ~Relevance
   yvar <- ~SentimentScore
         
   visdf %>%
     filter(Relevance >= relThr) %>%
     ggvis(x = xvar, y = yvar) %>%
     layer_points(size := 50, 
                  fill = ~Type,
                  size.hover := 200,
                  fillOpacity := 0.7, 
                  fillOpacity.hover := 0.5,
                  key := ~id) %>%
     add_tooltip(kw_tooltip, "hover") %>%
     add_axis("x", title = xvar_name) %>%
     add_axis("y", title = yvar_name)
  #   set_options(width = 400)
   
 })
 
 vis %>% bind_shiny("plot")
 
}
