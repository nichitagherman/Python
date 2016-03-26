library("plotly")
library("shiny")

shinyServer(function(input, output){
  output$starushki <- renderUI({
    x <- list()
    for(i in 1:input$kolicestvo){
      x[i] <- list(selectInput(inputId = paste("starushka_",toString(i),sep = ""),
                               label = paste("Выберите, какой по счету сумасшедшая старушка № ",toString(i)," зайдет в самолет"),
                               choices = 1:200,
                               selected = i))
    }
    return (column(12, x))
  })
  main_func <- function(n, babushka){
    if(length(babushka == 1)){
      if(babushka[1] == n){
        return(n)
      }
    }
    starushki <- c()
    for (i in 1:length(babushka)){
      starushki <- append(starushki,babushka[i])
    }
    min_babushka <- starushki[which.min(starushki)]
    otnositelino_min <- function(y){
      return(y-min_babushka+1)
    }
    starushki <- lapply(starushki, otnositelino_min)
    # Те, кто до первой старушки зашли - точно сели на свои места. Нас не волнует, как они расселись
    result <- min_babushka - 1
    all <- matrix(c(min_babushka:n, sample(1:n, (n + 1 - min_babushka), replace = FALSE)), nrow = (n + 1 - min_babushka))
    # Если место пассажира занято, то пассажир равновероятно садится на оставшиеся места
    # Занятые места для пассажиров будет помечаться NaN в общем массиве all
    # Функция, которая сажает пассажира,чье место занято, на рандомное место
    
    choise <- function(n,babushka){
      r <- 1 : (n + 1 - min_babushka)
      while (TRUE){
        # Пассажир выбирает, на какое место ему сесть
        s <- sample(r, 1)
        # Если оно свободно - садится
        if(!is.nan(all[s,2])){
          all[s,2] <- NaN
          return (all)
        }
      }
    }
    
    all <- choise(n,min_babushka)
    # Может сесть и на свое
    if(is.nan(all[1,2])){
      result <- result + 1 
    }
    # Посмотрим, как рассадятся остальные пассажиры (начиная со 2, так как 1 - бабушка)
    for (i in 2:(n + 1 - min_babushka)){
      # Может сесть и на свое
      if(i %in% starushki){
        if(is.nan(all[i,2])){
          all <- choise(n,min_babushka)
        }
        else{
          all <- choise(n,min_babushka)
          if(is.nan(all[i,2])){
          result <- result + 1 
          }
        }
      }
      else if(is.nan(all[i,2]) || i %in% starushki){
        all <- choise(n,min_babushka)
      }
      else{
        all[i,2] <- NaN
        result <- result+1
      }
    }
    return(result)
  }
  
  output$plot <- renderPlotly({
    starushki <- c()
    for (i in 1:input$kolicestvo){
      starushki <- append(starushki,strtoi(eval(parse(text = paste("input$starushka_",toString(i), sep = "")))))
    }
    n1 <- input$number_passangers[1]
    n2 <- input$number_passangers[2]
    result <- matrix(ncol = 2,nrow = n2)
    for (i in n1:n2){
      s <- 0 # Счетчик случайной величины, который принимает значение количества севших на свое место пассажиров
      for (y in 1:300){ # Количество выборок
        s <- s + main_func(i,starushki)
      }
      result[i,1] <- i
      result[i,2] <- s/300
    }
    result <- list('Yeah' = result[,1], 'Ok' = result[,2])
    result <- data.frame(result)
    plot_ly(result, x = Yeah, y = Ok, name = "График из-за старушки")
      add_trace(x = n1:n2, y = n1:n2, name = "Все cели на свои места")
    layout(p, xaxis = list(title = "Количество пассажиров", range = c(n1-1,n2+1)), 
           yaxis = list(title = "Ожидаемое количество пассажиров, севших на свои места"))
  })
})
