library(shiny)
library(tidyverse)
library(shinyWidgets)

course_data <- read_csv("partial_1000_course_data.csv")
fcm_course <- read_csv("fcm_course_tibble_1000_0104.csv")
dfm_course <- read_csv("dfm_course_tibble_1000_0104.csv")

tags <- function (fcm, keyword, occurrence = 2){
  frame <- fcm %>% select(feat_names, keyword)
  
  for ( i in names(frame)[2:length(frame)]){
    result_tags <- frame %>% 
      filter(.[[i]] >= occurrence) %>% # 找出大於等於 occurrence 的數值
      arrange(desc(.[[i]])) %>%
      .$feat_names
    
    if (length(result_tags) < 10 && length(result_tags) > 0){
      return(result_tags[1:length(result_tags)])
    } else if (length(result_tags) > 10){
      return(result_tags[1:10])
    } else if (length(result_tags) == 10){
      return("None")
    }
  }
}

filter_class <- function (original_data, keywords, tags){
  #建立回傳值的空vector
  ans <- vector()
    
  #轉換資料形態
  original_data <- convert(original_data, to = "data.frame") %>% 
    as_tibble()
    
  for (i in seq_along(tags)) {
    #製造欄位名稱
    score_name <- paste("score_", i)
    #創造代數不然下下行有時候會有問題 ## 這個還是有問題
    strange <- original_data[[keywords]] * 0.5 + original_data[[tags[i]]] * 0.5
    #測試是不是這邊出錯
    #print(strange)
      
    #創造加權平均的分數欄位
    new_data <- mutate(original_data, score_name = strange) %>% 
      #重新排序由大到小
      arrange(desc(score_name)) %>% 
      head(5) %>%
      filter(score_name > 0) %>%
      .$document
      
    ans <- c(ans, new_data)
  }
  return(unique(ans))
}

  
ui <- fluidPage(
  titlePanel("R 分解茶"),
  fluidRow(
    title = "Fold Change Analysis",
    textInput("keyword", label = h3("請輸入一個關鍵字")),
    
    textOutput("class"),
    
    actionButton("dislike", label="dislike"),
    actionButton("like", label="like"),
    align="center"
  )
  
)


server <- function(input, output) {
  
  output$class <- renderText({
    input$text
  })
}

shinyApp(ui, server)
