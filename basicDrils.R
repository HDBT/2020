df <- tibble(name = c("Animals", "Fruits"),y = c(5, 2),drilldown = tolower(name))
df
hc <- highchart() %>%hc_title(text = "Basic drilldown") %>%hc_xAxis(type = "category") %>%hc_legend(enabled = FALSE) %>%hc_plotOptions(series = list(borderWidth = 0,dataLabels = list(enabled = TRUE))) %>%
  hc_add_series(data = df,type = "column",hcaes(name = name, y = y),name = "Things",colorByPoint = TRUE)

dfan <- data.frame(name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),value = c(4, 3, 1, 2, 1))
dffru <- data.frame(name = c("Apple", "Organes"),value = c(4, 2))
dsan <- list_parse2(dfan)
dsfru <- list_parse2(dffru)

hc <- hc %>% hc_drilldown(allowPointDrilldown = F,type = "bar",series =  list(list(id = "animals",data = dsan),list(id = "fruits",data = dsfru)))%>% hc_chart(type = "column")  

hc

df <- tibble(name = c("Animals", "Fruits"),drilldown = tolower(name))
df
hc <- highchart() %>%  hc_title(text = "Basic drilldown") %>%hc_xAxis(type = "category") %>%hc_legend(enabled = FALSE) %>%hc_plotOptions(series = list(borderWidth = 0,dataLabels = list(enabled = TRUE))) %>%
  hc_add_series(data = df,name = "Things",colorByPoint = TRUE) %>% 
  hc_add_series(data = tibble(name = c("x1", "x2"),y = c(2, 9),yx= c(9,4) ) )
hc

# über mapping? Nope? Lists - dienen der Adressierung von Javascript Argumenten {}
df <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3),y2= c( 3,4,5,2,1))

highchart() %>% 
  hc_chart(type = "column")%>%
 #hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(categories = df$name) %>% 
  hc_add_series(name= "No migration background", data =df$y )%>%
  hc_add_series(name= "Parents imigrated",data =df$y1 ) %>%
  hc_add_series(name= "Self-Immigrated", data =df$y2) 


df2 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3),y2= c( 3,4,5,2,1),drilldown = tolower(name) )

highchart() %>% 
  hc_chart(type = "column")%>%
  #hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(categories = df2$drilldown) %>% 
  hc_add_series(name= "16-20", data =df2[1,2:5] )%>%
  # hc_add_series(name= "21-25",data =df2$y1 ) %>%
  # hc_add_series(name= "26-29", data =df2$y2) %>%
  hc_drilldown(
    allowPointDrilldown = T,
    series = list(
      list(id = df2$drilldown[1], data = list_parse2(df2), name="16-20")
      ))

# google translate --------------------------------------------------------
library(shiny.i18n)
i18n <- Translator$new(automatic = TRUE)
i18n$set_translation_language("en")

i18n$at("zweiter entwurf")

# google authenticate
googleAuthR::gar_auth_service("location_of_json_files.json")
text <- "zweiter entwurf"
## translate British into Danish
gl_translate(text, target = "en")$translatedText


df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3))

highchart() %>% 
  hc_chart(type = "column")%>%
  hc_title(text = "Titel")%>%
  hc_subtitle(text ="untertitel") %>%
  hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(categories = df3$name, crosshair = T) %>% 
  hc_yAxis(title = list(text = "%")) %>%
  hc_add_series(name= "Female", data =df3$y*10 )%>%
  hc_add_series(name= "Male",data =df3$y1*10 ) %>%
  hc_tooltip(headerFormat = '<span style="font-size:10px">{point.key}</span><table>', pointFormat = '<tr><td style="color:{series.color};padding:0">{series.name}: </td><td style="padding:0"><b>{point.y:.1f} %</b></td></tr>', footerFormat = '</table>', shared = T, useHTML =T) %>%
  hc_exporting(enabled = T, filename = "custom-file-name")

df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3))

highchart() %>% 
  hc_chart(polar = T, type = "line")%>%
  #hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(categories = df3$name,tickmarkPlacement= 'on')%>%
  hc_yAxis(gridLineInterpolation= 'polygon', lineWidth= 0, min= 0)%>% 
  hc_add_series(name= "Female", data =df3$y )%>%
  hc_add_series(name= "Male",data =df3$y1 )


df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3))

highchart() %>% 
  hc_chart(polar = T, type = "line")%>%
  #hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(categories = df3$name,tickmarkPlacement= 'on')%>%
  hc_yAxis(gridLineInterpolation= 'polygon', lineWidth= 0, min= 0)%>% 
  hc_add_series(name= "Female", data =df3$y, pointPlacement = "on" )%>%
  hc_add_series(name= "Male",data =df3$y1,pointPlacement = "on"  )%>%
  hc_tooltip(shared = T,pointFormat = '<span style="color:{series.color}">{series.name}: <b>%{point.y:,.0f}</b><br/>') %>%
  hc_exporting(enabled = T, filename = "custom-file-name")
  

#calc pi aprox with random x ~ U(0,1)


calcf <- function(n){
x <- 0
y <- 0
plot(x,y)
smaller <- 0
bigger  <- 0 

  for (i in 1:n) {
    x <-  runif(1)
    y <-  runif(1)
    result <- sqrt(x**2+y**2)
    ifelse(result <=1, smaller <- smaller +1, bigger <- bigger +1)
    # if (result <= 1) {
    #   points(x,y, col ="blue")
    # } else{
    #   points(x,y,col="red")
    # }
     if (i == n) {
      
      print(4*smaller/(smaller+bigger))
    }
  }
}
calcf(n =1000000)



#    shinyjs::toggle("myBox") <- für einfaches einklappen einer box() <- ui