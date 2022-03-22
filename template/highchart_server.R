
# @diff: should differences of two datasets be calculated? Remember to prepare the datasets to be eqaually in demographic characteristics!
# @input1: provide row var input. E.g. input$test
# @input_choices: provide exact set chouces in input. Note for me: set this automatically.
# @input2: provide col var input. E.g. input$thema
# @input_choices: provide exact set choices in input. Note for me: set this automatically in future updates.
# @df_set1: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @df_set2: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @df_set3: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @title: provide list with titles to be used. 
# @diff_positon: provide numeric vector to specify the df_sets, which should be substracted. Default = c(1,3)

# Refactoring Outpout  ----------------------------------------------------
library(purrr)

highchart_server  <- function(diff = FALSE, input1 = character(), input_choices1 = character(), input2 = NULL, input_choices2 = NULL, df_set1 = character(), df_set2 = NULL, df_set3 = NULL, title = character(), diff_position = c(1,3), ...) {
  source("config.R", local =  T)
  try
  walk(list.files( pattern = "*.RData"), function(x) load(x, envir = .GlobalEnv))
  
  input1 <- i18n$t(input1)
  input_choices1 <- i18n$t(input_choices1)
  input2 <- i18n$t(input2)
  input_choices2 <- i18n$t(input_choices2)
  
  index1 <- match(input1, input_choices1)
  print(paste("index1", index1))    #test
  if (!is.null(input2)) {
    index2 <- match(input2, input_choices2)
    print(paste("index2", index2))    #test
  }
  # standard df
  try({
    df_list <- map(list(df_set1,df_set2,df_set3), list)
    print("pre str", str(df))                  #test
    df <- df_list[[index2]][[1]][[index1]]
    #print("structure", str(df))                  # test
    #df <- as_tibble(df)
  }
  )
  
  # cond. df diff variation
  pos1 <-  diff_position[1]
  pos2 <-  diff_position[2]
  if (diff == TRUE & index2 == pos2) {
    try({
      print(paste("position index", pos1,pos2))
      df_pre <- map(list(df_set1,df_set2,df_set3), list)
      print("pre str_pre", str(df_pre))                  #test
      df_pre <- df_pre[[pos1]][[1]][[index1]]         # eventuell als Arg?
      #print("structure", str(df))                  # test
      #df <- as_tibble(df)
      df <- df_pre %>% mutate(Freq = df$Freq - df_pre$Freq, se = se  )
    }
    )
   
  }
  
  try({
   df <-  df %>% mutate(se = SE) # fix für inkonsistente Namenskonvention in Prep
  })
  
  df <- df %>% mutate(Var1 = i18n$t(Var1), Cats = i18n$t(df[[1]]))
  
  print(df)                  # test
  #print("Var1" ,as.character(df[["Var1"]])) # test
  
  # create matrix for title string vector for indexing
  dim(title) <- c(length(input_choices1), length(input_choices2))
  
  if (index1 == 1) {
    hc %>%  hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter = JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'  +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (this.se *2*100).toFixed(1)  + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared = TRUE,footerFormat = '{series.n}{this.se}</table> ',useHTML = TRUE) %>%
    hc_xAxis(categories = i18n$t(df[["Var1"]]))  %>%
    hc_title(text = i18n$t( title[index1,index2])) %>%
    hc_add_series(df, "column",hcaes(x = Var1, y = round(Freq,4)*100), showInLegend = FALSE,
                                                          tooltip = list(enabled = TRUE,pointFormat = '${point.y}'))
  }
  
  else {
    
    hc %>% 
    hc_add_series(df, "column",hcaes(x = Var1, y = round(Freq,4)*100,  group = Cats),showInLegend = FALSE,
                  tooltip = list(enabled = TRUE,pointFormat = '${point.y}'))  %>%
    hc_title(text = i18n$t( title[index1,index2])) %>%
    hc_xAxis(categories = unique(i18n$t(df$Var1))) 
  }
  
  
      

      
  # else if (input$test == i18n_r()$t("Age") & input$thema == i18n_r()$t("2019")) { #vorher MIgration
  #   
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age."))%>%
  #     
  #     hc_add_series(mutate(df_age19,Var1 = i18n$t(df_age19$Var1), Age_Cat = i18n$t(df_age19$Age_Cat)), "column",hcaes(x = Var1, y = Freq*100, group = Age_Cat),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_age$Var1[order(df_age$Var2)])))
  #   
  # }
  # else if (input$test == i18n_r()$t("Gender") & input$thema == i18n_r()$t("2019")) {
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by gender."))%>%
  #     hc_add_series(mutate(df_gender19, Var1= i18n$t(Var1), Gender = i18n$t(df_gender19$Gender)), "column",hcaes(x = Var1, y = Freq*100, group = Gender)) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_gender$Var1[order(df_gender$Var2)])))
  #   
  # }
  # 
  # else if (input$test == i18n_r()$t("Status") & input$thema == i18n_r()$t("2019")) {
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by status."))%>%
  #     hc_add_series(mutate(df_status19,Var1= i18n$t(Var1),  status = i18n$t(df_status19$status)), "column",hcaes(x = Var1, y = Freq*100, group = status),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_status$Var1[order(df_status$Var2)])))
  #   
  #   
  # }
  # 
  # 
  # 
  # 
  # else if (input$test == i18n_r()$t("None") & input$thema == i18n_r()$t("2020")) {  #   
  #   hc %>% 
  #     hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'  +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
  #     
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days."))%>%

  #     hc_add_series(mutate(df020F, Var1= i18n$t(Var1)), "column",hcaes(x = Var1, y = round(Freq,4)*100),showInLegend = F,
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #    hc_xAxis(categories = i18n$t(as.character(df020F$Var1)))
  #   
  #   
  # }
  # 
  # else if (input$test == i18n_r()$t("Status") & input$thema == i18n_r()$t("2020")) { #vorher MIgration
  #   
  #   
  #   hc %>% 
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by status"))%>%
  #     
  #     # hc_xAxis(categories = df2$name) %>% 
  #     #     hc_add_series(name= i18n$t("Student"), data =df2[c("name","y")] )%>% # unnecessary "name?
  #     #     hc_add_series(name= i18n$t("Employed"),data =df2$y1 ) %>%
  #     #     hc_add_series(name= i18n$t("NEET"), data =df2$y2) 
  #     
  #     hc_add_series(mutate(df_status20F,Var1= i18n$t(Var1),  status = i18n$t(df_status20F$status)), "column",hcaes(x = Var1, y = Freq*100, group = status),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_status20F$Var1[order(df_status20F$status)])))
  #   
  #   
  # }
  # else if (input$test == i18n_r()$t("Age") & input$thema == i18n_r()$t("2020")) {
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age."))%>%
  #     
  #     # hc_xAxis(categories = dfx$name) %>%
  #     #     hc_add_series(name= i18n$t("12-15"), data =dfx[c("n","y")]) %>%
  #     #     hc_add_series(name= i18n$t("16-19"),data = dfx$y1 ) %>%
  #     #     hc_add_series(name= i18n$t("20-24"), data =dfx$y2)  %>%
  #     #     hc_add_series(name= i18n$t("25-29"), data =dfx$y3)#%>%
  #     #     #hc_add_series(type= "errorbar",linkedTo = i18n$t("12-16"), data= list(c(20,60),c(20,30),c(20,40)))
  #     #    # hc_add_series(name= i18n$t("24-29"),type= "errorbar", data= map(ma[3,],.f = function(x) x+ c(-1.96,1.96)*sqrt((x/100*(1-x/100)/1000))*100))
  #     # 
  #     hc_add_series(mutate(df_age20F,Var1= i18n$t(Var1),  Age_Cat = i18n$t(df_age20F$Age_Cat)), "column",hcaes(x = Var1, y = Freq*100, group = Age_Cat),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_age20F$Var1[order(df_age20F$Age_Cat)])))
  #   
  #   
  # }
  # 
  # else if (input$test == i18n_r()$t("Gender") & input$thema == i18n_r()$t("2020")) {
  #   
  #   hc %>% 
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by gender."))%>%
  #     
  #     #hc_plotOptions(bar = list(stacking = "percent")) %>% 
  #     # hc_xAxis(categories = df3$name) %>% 
  #     # hc_add_series(name= i18n$t("Female"), data =df3$y )%>%
  #     # hc_add_series(name= i18n$t("Male"),data =df3$y1 )
  #     hc_add_series(mutate(df_gender20F,Var1= i18n$t(Var1),  Gender = i18n$t(df_gender20F$Gender)), "column",hcaes(x = Var1, y = Freq*100, group = Gender),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_gender20F$Var1[order(df_gender20F$Gender)])))
  #   
  #   
  # }
  # 
  # 
  # 
  # #Differences
  # else if (input$test == i18n_r()$t("None") & input$thema == i18n_r()$t("Differences")) {
  #   #dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days."))%>%
  #     hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'  +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
  #     
  #     hc_add_series(mutate(df020,Var1 = i18n$t(Var1), Freq = round(df020$Freq-df019$Freq,4),se = df019$se), "column",hcaes(x = Var1, y = Freq*100),showInLegend = FALSE,
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df0$Var1)) )#, title = list(text = "Konsum"))
  #   
  # }
  # else if (input$test == i18n_r()$t("Age") & input$thema == i18n_r()$t("Differences")) { #vorher MIgration
  #   
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age."))%>%
  #     hc_add_series(mutate(df_age20,Var1= i18n$t(Var1),  Freq = df_age20$Freq-df_age19$Freq,se = df_age19$se, Age_Cat = i18n$t(df_age20$Age_Cat)), "column",hcaes(x = Var1, y = Freq*100, group = Age_Cat),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_age$Var1[order(df_age$Var2)])))
  #   
  # }
  # else if (input$test == i18n_r()$t("Gender") & input$thema == i18n_r()$t("Differences")) {
  #   
  #   hc %>%
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by gender."))%>%
  #     hc_add_series(mutate(df_gender20,Var1= i18n$t(Var1),  Freq = df_gender20$Freq-df_gender19$Freq,se = df_gender19$se,Gender = i18n$t(df_gender20$Gender) ), "column",hcaes(x = Var1, y = Freq*100, group = Gender)) %>%
  #     #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #     #             centerInCategory = TRUE, groupPadding = .68,
  #     #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_gender$Var1[order(df_gender$Var2)])))
  #   
  # }
  # 
  # else if (input$test == i18n_r()$t("Status") & input$thema == i18n_r()$t("Differences")) {
  #   
  #   hc %>%
  #     
  #     hc_title(text = i18n$t("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by status."))%>%
  #     hc_add_series(mutate(df_status20, Var1= i18n$t(Var1), Freq = df_status20$Freq - df_status19$Freq, se = df_status19$se, status = i18n$t(df_status20$status) ), "column",hcaes(x = Var1, y = round(Freq,4)*100, group = status),
  #                   tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #     hc_xAxis(categories = i18n$t(as.character(df_status$Var1[order(df_status$Var2)])))
  #   
  #   
  # }
  
}
