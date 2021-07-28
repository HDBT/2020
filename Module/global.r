# # create the initial x variable
#   x1 <- rnorm(100, 15, 5)
# 
# # x2, x3, and x4 in a matrix, these will be modified to meet the criteria
#   x234 <- scale(matrix( rnorm(300), ncol=3 ))
# 
# # put all into 1 matrix for simplicity
#   x1234 <- cbind(scale(x1),x234)
# 
# # find the current correlation matrix
#   c1 <- var(x1234)
# 
# # cholesky decomposition to get independence
#   chol1 <- solve(chol(c1))
#   
#   newx <-  x1234 %*% chol1 
# 
# # check that we have independence and x1 unchanged
#   zapsmall(cor(newx))
#   all.equal( x1234[,1], newx[,1] )
# 
# # create new correlation structure (zeros can be replaced with other rvals)
#   newc <- matrix( 
#     c(1  , 0.4, 0.5, 0.6, 
#       0.4, 1  , 0  , 0  ,
#       0.5, 0  , 1  , 0  ,
#       0.6, 0  , 0  , 1  ), ncol=4 )
# 
# # check that it is positive definite
#   eigen(newc)
#   
#   chol2 <- chol(newc)
#   
#   finalx <- newx %*% chol2 * sd(x1) + mean(x1)
# 
# # verify success
#   mean(x1)
#   colMeans(finalx)
#   
#   sd(x1)
#   apply(finalx, 2, sd)
#   
#   zapsmall(cor(finalx))
#   pairs(finalx)
#   
#   all.equal(x1, finalx[,1])

# Zufriedenheit <- as.integer(rnorm(100,5,2))
# sex <- rbinom(100,1,.5)
# einkommen <- as.integer(sample(100,100, T))
# mig <- sample(1:3, 100, replace = T)
# emotion <- sample(c("hoffnung", "angst", "traurig", "wut", "misstrauen"),100,T)
# covid <- sample(c("covid1","covid2"),100, replace = T, prob =c(0.5,.2))
# region <- sample(1:12,100,T)
# id <- 1:100
# df <- data.frame(sex,Zufriedenheit,einkommen,mig,covid,id,emotion,region)
# 
# #leaflet agreggate
# # df_agg <- df %>%
# #   group_by(region) %>%
# #   summarize(mean_weight = mean(einkommen, na.rm = TRUE))
# # class(df_agg$mean_weight)
# # Variables that can be put on the x and y axes
# axis_vars <- c(
#   "Einkommen in Brutto" = "einkommen",
#   "Zufriedenheitskala" = "Zufriedenheit"
# 
# )

#data 

# library(foreign)
# #install.packages("haven")
# data1 <- read.dta("Data.dta")
# library(haven)
# data <- read_dta("Data.dta")
# 
# hchart(df%>% filter(sex == 0), type = "point", hcaes(x = Zufriedenheit, y = einkommen), name = "Männer") %>%
#   hc_add_series(df %>% filter(sex == 1), type = "point", mapping = hcaes(x = Zufriedenheit, y = einkommen), name = "Frauen", fast = FALSE) 
# 
# hchart(data %>% filter(Gender == 1), type = "point", hcaes(x = Age, y = A85), name = "Männer") %>%
#   hc_add_series(data %>% filter(Gender == 2), type = "point", mapping = hcaes(x = Age, y = A85), name = "Frauen", fast = FALSE) 
# 
# dat <- data_to_boxplot(data, A85, Gender,name = "Unterschiede in Dem.Zufriedenheit") #fuer highcharter box
# highchart() %>% hc_xAxis(type = "category") %>% hc_add_series_list(dat)
# 
# hc <- highchart() %>% hc_xAxis(type = "category") %>% hc_add_series_list(dat)
# hc <- hc %>% hc_drilldown(allowPointdtilldown= T, series = list (id = "Gender", data = dat ))
# hc



#ggvis
# data1 %>% 
#   ggvis(props(x = ~Gender))%>% 
#   layer_points(size := 50, size.hover := 200, fillOpacity := 0.2, fillOpacity.hover := 0.5, stroke = ~covid, key := ~id) %>%
#   add_tooltip(genTooltip,"hover") %>%
#   add_legend("stroke",title = "Hatte Corona Erfahrung in soz. Umkreis", values = c("Ja","Nein")) %>%
#   scale_nominal("stroke", domain =  c("Ja","Nein"), range = c("orange","lightblue")) %>%
#   set_options(width = 800, height =  600)
# 
# df <- as.data.frame(data)
#   ggvis(data1, ~Age)

  # Removing Labels because of Problems with GGVis / Haven

#  install.packages("labelled")
  #library(labelled)
  #data1 <- remove_labels(data)
  


# plot type reactive function ---------------------------------------------


# get plot type
# * 2: both numeric variables
# * 1: one numeric, one non-numeric variable
# * 0: both non-numeric variables
# * -1: only one variable provided
# plot_type <- reactive({
#   if (input$y != "None")
#     is.numeric(raw_df[[input$x]]) + is.numeric(raw_df[[input$y]])
#   else
#     -1
# })
#https://github.com/kjytay/misc/blob/master/blog/2020-12-22%20dataset%20explorer/DatasetExplorer1/app.R



# data  -------------------------------------------------------------------

#dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )


# HTML ICONS in choices einbinden
# pickerInput("f","jeifj", choices = c("en","de"), 
#             choicesOpt = list(content = mapply(flags, FUN = function(flags) {
#               HTML(tags$img(src = flags, width = 20, height = 15))
#             }, SIMPLIFY = F, USE.NAMES = F)) )


#load ysl '19

library(haven)
ysl19 <- haven::read_dta("Y:/Identity_Political_Participation/Statistics/Datasets/YSL2019_Final dataset_Full.dta")


ysl19 %>% filter(sex)
library(highcharter)
hchart(
  density(ysl19$Age),
  type = "area", name ="age"
) %>%
  hc_add_series(
    density(
    )
  )


install.packages("hdrcde")
library(hdrcde)
dens <-  cde(ysl19$Age+15, ysl19$Gender)
den <-   density(ysl19$Age)


hchart(
  cde(ysl19$Age, ysl19$Gender),
  type = "area", name ="agel"
)
as(dens, "density")

library(dplyr)
df4 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(47,26,92,92,85), y1 = c (48,34,89,93,94), y2 = c (49,42,84,76,75))

df4 <- tibble(name = c("Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.","Being Born in Lux.") ,y = c(47,26,26,66,34,98,26,26,26,26,92,92,85))
df4 <- tibble(x = c(3,5,7,4,3,5,6,7,5,4,3,3,2) ,y = c(47,26,26,66,34,98,26,26,26,26,92,92,85))

library(purrr)
complete <- ysl19 %>% tidyr::drop_na("A15_5", "Agegroup")
tapply(complete$A15_5,complete$Agegroup, density) %>%
  reduce(.f = hc_add_series, .init = highchart())
  
  hchart(density(df4),
    type = "area", name ="agel"
  )%>%
  hc_plotOptions(area = list(pointStart = 16, pointEnd = 30)) 
 
  
  hchart(data = df4,
    type = "areaspline", name ="agel"
  )%>%
  hc_plotOptions(area = list(pointStart = 16, pointEnd = 30)) 
 
# Beispiel Density Chart.
highchart() %>% 
  hc_chart(type= "areasplinerange") %>% 
  #hc_colors(colors) %>% 
  hc_plotOptions(areasplinerange = list(marker = FALSE, states = list(hover = list(enabled = FALSE)) ))  %>%
  hc_tooltip( shared = T, useHTML =T, crosshairs =TRUE)%>%
  hc_yAxis(title = list(text = NULL), categories = c("Frage 1", "Frage 2", "Frage 3", "Frage 4" ), max= 3, labels = list(formatter = JS("function(){if (this.pos < 4) return this.value}"))) %>%
  hc_add_series(name = " Frage 1", data= densinator(dens,0.0), zIndex ="5")    %>%
  hc_add_series(name = " Frage 2", data= densinator(dens_F2,1),zIndex ="1" ) %>%
  hc_add_series(name = " Frage 3", data= densinator(dens_F3,2),zIndex ="2 ") %>%
  hc_add_series(name = " Frage 4", data= densinator(dens_F4,3),zIndex ="4" ) 
  


#Run JS Code in r for direct HC Options 
highchart() %>% 
hc_chart(events = list(load = JS("function() {
      var chart = this; chart.update({
        chart: {
          backgroundColor: '#FCFFC5'
        }
      }); console.log('Updated chart background color!');
    }
    ")
))

#make density scores 
library(tidyr)
dropped <- drop_na(ysl19)
density(dropped$Age[dropped$A86_7 == 1])
dens <-density(drop_na(ysl19 %>% select("A86_11","Age") %>% filter(A86_11 == 1))$Age+14)
dens$y <- dens$y+3
dens$z <- sapply(dens$y, function(x) return(3))
dens <- list(dens$x, dens$y,dens$z)
dens <- list_parse2(as.data.frame(dens))
dens_F2 <-density(drop_na(ysl19 %>% select("A86_12","Age") %>% filter(A86_12 == 1))$Age+14)$y / (density(drop_na(ysl19 %>% select("A86_12","Age") %>% filter(A86_12 == 1))$Age+14)$y+ density(drop_na(ysl19 %>% select("A86_12","Age") %>% filter(A86_12 == 0))$Age+14)$y)
dens_F3 <-density(drop_na(ysl19 %>% select("A86_13","Age") %>% filter(A86_13 == 1))$Age+14)
dens_F4 <-density(drop_na(ysl19 %>% select("A86_14","Age") %>% filter(A86_14 == 1))$Age+14)

#function densinator
densinator <- function(dens,step) {
  dens[[2]] <-  dens[[2]] +step
  dens[[3]] <- sapply(dens[[3]], function(x) return(step))
  dens <- list(dens[[1]], dens[[2]],dens[[3]])
  
  dens <- list_parse2(as.data.frame(dens))
  
  
  print(dens)
}
densinator(dens_F2,3)

# conditional dens plot
highchart() %>% 
  hc_chart(type= "areaspline") %>% 
  #hc_colors(colors) %>% 
  hc_plotOptions(areasplinerange = list(marker = FALSE, states = list(hover = list(enabled = FALSE)) ))  %>%
  hc_tooltip( shared = T, useHTML =T, crosshairs =TRUE, valueDecimals = 3, headerFormat = NULL, footerFormat= NULL, pointFormat = "<b>{series.name}</b>: {point.y} Prozent <br/>")%>%
  hc_yAxis(plotBands =list(list(from = 0, to= 2, color='rgba(68, 170, 213, 0.1)', label = list(text= "jieofwo") ),list(from = 0, to= 1, color='rgba(68, 170, 213, 0.1)', label = list(text= "jieofwo") )) ,title = list(text = NULL), categories = c("Frage 1", "Frage 2", "Frage 3", "Frage 4" ), max= 3, labels = list(formatter = JS("function(){if (this.pos < 4) return this.value}"))) %>%
  hc_add_series(name = " Frage 1", data= sumdens, zIndex ="5") %>%
  hc_add_series(name = "Frage 2", data = dens_F2+0, zIndex = "3")
  

  #calc dens
dropped <- drop_na(ysl19)
density(dropped$Age[dropped$A86_7 == 1])
dens1 <-density(drop_na(ysl19 %>% select("A86_11","Age") %>% filter(A86_11 == 1))$Age+14)
dens2 <-density(drop_na(ysl19 %>% select("A86_11","Age") %>% filter(A86_11 == 0))$Age+14)
dens3 <-density(drop_na(ysl19 %>% select("A86_12","Age") %>% filter(A86_12 == 1))$Age+14)
dens4 <-density(drop_na(ysl19 %>% select("A86_12","Age") %>% filter(A86_12 == 0))$Age+14)

dens$z <- sapply(dens$y, function(x) return(3))
dens <- list(dens$x, dens$y,dens$z)
dens <- list_parse2(as.data.frame(dens))
hchart(density(drop_na(ysl19 %>% select("A86_11","Age") %>% filter(A86_11 == 0))$Age),
       type = "area", name ="agel"
)%>%
  hc_plotOptions(area = list(pointStart = 16, pointEnd = 30)) 

densdf <- ysl19 
cdplot(unlist(drop_na(ysl19 %>% select("Age"))), 
       as.factor(unlist((drop_na(ysl19 %>% select("A19") %>% filter(!A19 == -98))) ) ))
plot <- cdplot(unlist(drop_na(ysl19 %>% select("Age"))), 
       as.factor(unlist((drop_na(ysl19 %>% select("A19") %>% filter(!A19 == -98))) ) ))

dens <- density(unlist(drop_na(ysl19 %>% select("Age"))))
densa <- density(unlist(drop_na(ysl19 %>% select("Age"))))

sumdens <-  dens1$y / (dens1$y + dens2$y) 
sumdens2 <-  dens3$y / (dens3$y + dens4$y) 
plot(dens$x, dens$y) 
plot(dens$x,sumdens)
(dens$x, dens$y)



# Neuer Versuch -------------------------------------------------------------
# Zwei Versionen



