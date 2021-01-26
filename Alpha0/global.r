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

