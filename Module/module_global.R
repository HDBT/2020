library(pacman)
p_load(haven, tidyverse, gt, gtsummary,highcharter,broom)
# yac20 old style--------------
df <- read_spss("Y:/YSL Wave 2020/Data/Final Dataset/YAC+_Final dataset.sav")
df19 <- read_spss("Y:/Youth Report 2020/YAC Erweiterung/old/Caro 8_11/YSL2019_Final dataset_Clean Including Migrant.sav")

#####  20 full ##############
df20_full <- df %>% mutate(alc = ifelse(A60 == 0, 0,1),
              smoke= ifelse(A59 == 0,0,1),
              cannabis= ifelse(A62 == 0,0,1),
              Age_Cat = case_when(Age >=12 & Age < 16 ~ 1, #Änderung um es vergleichbar zu machen mit '19
                                  Age >=16 & Age < 20  ~ 2,
                                  Age >=20 & Age < 25  ~ 3,
                                  Age >=25 & Age <= 29  ~  4),
              status = case_when( A5_1 == 1~ 1,
                                  A5_2 == 1~ 1,
                                  A5_3 == 1~ 1,
                                  A5_4 == 1~ 1,
                                  A5_5 == 1~ 2,
                                  A5_6 == 1~ 2,
                                  A5_7 == 1~ 2,
                                  A5_8 == 1~ 2,
                                  A5_9 == 1~ 3,
                                  A5_10 == 1~ 3,
                                  A5_11 == 1~ 3,
                                  A5_12 == 1~ 3,
                                  A5_13 == 1~ 3,
                                  A5_14 == 1~ 3,
                                  A5_15 == 1~ 3
              ),
              wealth = A38
              
)

##### 20 diff equalizer ######
# stata check 
#recode Age (16/19 = 1) (20/24 = 2) (25/29 = 3) (else = .), gen (age_cat)
# tabulate A62 age_cat, col row
df20 <- df %>% filter(Age > 15) %>% mutate(alc = ifelse(A60 == 0, 0,1),
              smoke= ifelse(A59 == 0,0,1),
              cannabis= ifelse(A62 == 0,0,1),
              Age_Cat = case_when(Age >=16 & Age < 20 ~ 1, #Änderung um es vergleichbar zu machen mit '19
                                  Age >=20 & Age < 25  ~ 2,
                                  Age >=25 & Age <= 29  ~  3),
              status = case_when( A5_1 == 1~ 1,
                                  A5_2 == 1~ 1,
                                  A5_3 == 1~ 1,
                                  A5_4 == 1~ 1,
                                  A5_5 == 1~ 2,
                                  A5_6 == 1~ 2,
                                  A5_7 == 1~ 2,
                                  A5_8 == 1~ 2,
                                  A5_9 == 1~ 3,
                                  A5_10 == 1~ 3,
                                  A5_11 == 1~ 3,
                                  A5_12 == 1~ 3,
                                  A5_13 == 1~ 3,
                                  A5_14 == 1~ 3,
                                  A5_15 == 1~ 3
                
              ),
              wealth = A38
              
)

# 19 df ----------------
df19 <- df19 %>% mutate(alc = ifelse(A58_2 == 1, 0,1),
              smoke= ifelse(A57_2 == 1,0,1),
              cannabis= ifelse(A61_2 == 1,0,1),
              Age_Cat = case_when(Age >=16 & Age < 20 ~ 1,
                                  Age >=20 & Age < 25  ~ 2,
                                  Age >=25 & Age <= 29  ~  3),
              status = case_when( A4_1 == 1~ 1,
                                  A4_2 == 1~ 1,
                                  A4_3 == 1~ 1,
                                  A4_4 == 1~ 2,
                                  A4_5 == 1~ 2,
                                  A4_6 == 1~ 2,
                                  A4_7 == 1~ 2,
                                  A4_8 == 1~ 3,
                                  A4_9 == 1~ 3,
                                  A4_10 == 1~ 3,
                                  A4_11 == 1~ 3,
                                  A4_12 == 1~ 3,
                                  A4_13 == 1~ 3,
                                  A4_14 == 1~ 3
                                  
              )
)

head(df19$Age_Cat)
summary(df$Age_Cat)

# 2020 table prep old unweighted ----------------
# none
an <- prop.table(table(df$alc)) %>% {.*100} %>% round(2) %>% {.[2]}
sn <- prop.table(table(df$smoke)) %>% {.*100} %>% round(2)%>% {.[2]}
cn <- prop.table(table(df$cannabis)) %>% {.*100} %>% round(2)%>% {.[2]}

asc <- c(an,sn,cn) 

#age
aa <- prop.table(table(df$alc, df$Age_Cat),2) %>% {.*100} %>% round(2) %>% {.[2,1:4]}
as <- prop.table(table(df$smoke, df$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:4]}
ac <- prop.table(table(df$cannabis, df$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:4]}

ma<- matrix(c(aa,as,ac),ncol =3,byrow = FALSE)
ma
#Gender
ag <- prop.table(table(df$alc, df$Gender),2) %>% {.*100} %>% round(2)%>% {.[2,1:2]}
sg <- prop.table(table(df$smoke, df$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}
cg <- prop.table(table(df$cannabis, df$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}

mg<- matrix(c(ag,sg,cg),ncol =3,byrow = FALSE)
mg

#Status
as <- prop.table(table(df$alc, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ss <- prop.table(table(df$smoke, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
cs <- prop.table(table(df$cannabis, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ms<- matrix(c(as,ss,cs),ncol =3,byrow = FALSE)
ms

margin.table(table(df$smoke,df$Age_Cat))

#save objects
save(asc, ma, mg,ms, file= "data.RData")





# ysl19  old style-------------------------------------------------------------------



# none
an <- prop.table(table(df19$alc)) %>% {.*100} %>% round(2) %>% {.[2]}
sn <- prop.table(table(df19$smoke)) %>% {.*100} %>% round(2)%>% {.[2]}
cn <- prop.table(table(df19$cannabis)) %>% {.*100} %>% round(2)%>% {.[2]}

asc19 <- c(an,sn,cn) 

#age
aa <- prop.table(table(df19$alc, df19$Age_Cat),2) %>% {.*100} %>% round(2) %>% {.[2,1:3]}
as <- prop.table(table(df19$smoke, df19$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ac <- prop.table(table(df19$cannabis, df19$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}

ma19<- matrix(c(aa,as,ac),ncol =3,byrow = FALSE)

#Gender
ag <- prop.table(table(df19$alc, df19$Gender),2) %>% {.*100} %>% round(2)%>% {.[2,1:2]}
sg <- prop.table(table(df19$smoke, df19$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}
cg <- prop.table(table(df19$cannabis, df19$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}

mg19<- matrix(c(ag,sg,cg),ncol =3,byrow = FALSE)


#Status
as <- prop.table(table(df19$alc, df19$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ss <- prop.table(table(df19$smoke, df19$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
cs <- prop.table(table(df19$cannabis, df19$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ms19<- matrix(c(as,ss,cs),ncol =3,byrow = FALSE)
ms

margin.table(table(df19$smoke,df19$Age_Cat))
margin.table(table(df$smoke,df$Age_Cat))
margin.table(table(df$smoke))
margin.table(table(df19$cannabis))


#save objects
#save(asc19, ma19, mg19,ms19, file= "data.RData")

# dataframes for 2020 weighted tables


# design 
design <- survey::svydesign(~1, weights = ~Weight, data = df)

svytable(~alc,design = design, Ntotal = 100 )

tableizer <-  function(Var1 = NULL, Var2 = NULL, data =df) {
  Var1 <- enquo(Var1)
  #Var2 <- quote(Var2)
 # Var2 <- enquo(Var2)
  svytable(formula(~ Var1),design = design, Ntotal = 100 )
  
  if(!is.null(data[[Var2]])) {
    print("hallo")
    data[[Var2]]
  }
}

tableizer(Var1 = "alc", Var2 = "smoke")
df[["smoke"]]


#experimental hchart code. create df for 19-----

# none 
df0<- as.data.frame(prop.table(table(df19$alc,df19$Age_Cat),2)) %>%
  filter(Var1 != 0)
df0 <- df0 %>% mutate(Var1 = recode(Var1,"1" = "Alc"), Var2 = case_when(Var2 == 1 ~ "alter1",
                                                                      Var2 == 2 ~ "alter2",
                                                                      Var2 == 3 ~ "alter3"))
#df should look like this
# var1  frequency
# alc       x
# smoke     x
# cannabis  x
prop.table(table(df19$alc))[2]
prop.table(table(df19$alc))[2]
prop.table(table(df19$alc))[2]
 
df0 <- df19 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) round(prop.table(table(x))[2],3)) %>%  as.data.frame() %>% t() %>% as.data.frame()

df0
df0 <- df0 %>% mutate(Var1 = rownames(df0)) %>% rename(Freq = 1) %>% relocate(Var1,Freq ) %>% mutate(Var1 = recode(Var1, alc = "Alcohol",
                                                                                                            cannabis = "Cannabis",
                                                                                                            smoke = "Tobacco"),
                                                                                                     Var2 = "2019")


#Age 
#first group
df <- as.data.frame(prop.table(table(df19$alc,df19$Age_Cat),2)%>% round(3)) %>% 
 filter(Var1 != 0)
df <- df %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                      Var2 == 2 ~ "20-24",
                                                                      Var2 == 3 ~ "25-29"))

df
#second group
df1 <- as.data.frame(prop.table(table(df19$smoke,df19$Age_Cat),2)%>% round(3)) %>%
  filter(Var1 != 0)

df1 <- df1 %>% mutate(Var1 = recode(Var1,"1" = "Tobacco"), Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                          Var2 == 2 ~ "20-24",
                                                                          Var2 == 3 ~ "25-29"))
df1
# third group
df2 <- as.data.frame(prop.table(table(df19$cannabis,df19$Age_Cat),2)%>% round(3)) %>%
  filter(Var1 != 0)

df2 <- df2 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"), Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                             Var2 == 2 ~ "20-24",
                                                                             Var2 == 3 ~ "25-29"))
df2
df_age <- rbind(df, df1,df2) 
df_age

#Gender 
#first group
df <- as.data.frame(prop.table(table(df19$alc,df19$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)
df <- df %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                      Var2 == 2 ~ "Male"))

df
#second group
df1 <- as.data.frame(prop.table(table(df19$smoke,df19$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)

df1 <- df1 %>% mutate(Var1 = recode(Var1,"1" = "Tobacco"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                          Var2 == 2 ~ "Male"))
# third group
df2 <- as.data.frame(prop.table(table(df19$cannabis,df19$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)

df2 <- df2 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                             Var2 == 2 ~ "Male"))
df_gender <- rbind(df, df1,df2) 

df_gender
#Status 
#first group
df <- as.data.frame(prop.table(table(df19$alc,df19$status),2)%>% round(3)) %>%
  filter(Var1 != 0)
df <- df %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                      Var2 == 2 ~ "Employed",
                                                                      Var2 == 3 ~ "Unemployed"))

df
#second group
df1 <- as.data.frame(prop.table(table(df19$smoke,df19$status),2)%>% round(3)) %>%
  filter(Var1 != 0)

df1 <- df1 %>% mutate(Var1 = recode(Var1,"1" = "Tobacco"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                          Var2 == 2 ~ "Employed",
                                                                          Var2 == 3 ~ "Unemployed"))
# third group
df2 <- as.data.frame(prop.table(table(df19$cannabis,df19$status),2)%>% round(3)) %>%
  filter(Var1 != 0)

df2 <- df2 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                             Var2 == 2 ~ "Employed",
                                                                             Var2 == 3 ~ "Unemployed"))
df_status <- rbind(df, df1,df2) 




# df78 <- data.frame(Gender = c("Male", "Male", "Female", "Female"),
#                  ShareType = rep(c("Long", "Short"),2),
#                  InvestedPerAccount = c(10,9,7,8),
#                  lower = c(8,7,6,7),
#                  upper = c(11.5,10,9,8.8))

# df <- data.frame(Gender = c("Male", "Male", "Female", "Female"),
#                  Konsum = c("Alkohol","Gras","Alkohol","Gras"),
#                  Kon = c(60,20,30,10)
# )

# hcchart none
highchart() %>%
  hc_add_series(df0, "column",hcaes(x = Var1, y = Freq*100),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df0$Var1, title = list(text = "Konsum")) 

# hc chart Age

highchart() %>%
  hc_add_series(df_age, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
   #             centerInCategory = TRUE, groupPadding = .68,
    #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_age$Var1[order(df_age$Var2)], title = list(text = "Konsum")) 


# hc chart Gender
highchart() %>%
  hc_add_series(df_gender, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_gender$Var1[order(df_gender$Var2)], title = list(text = "Konsum")) 

# hc chart Status
highchart() %>%
  hc_add_series(df_status, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_status$Var1[order(df_status$Var2)], title = list(text = "Konsum")) 


save(df_gender,df_status, df_age,df0,file= "data19.RData")


# df 19 weighted --------------------


#survey none 
p_load(survey)
design <-  survey::svydesign(ids = ~0, data = df19,weights = ~Weight) 
#df20 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% as.data.frame() %>% tidyr::pivot_longer(everything())



c_table <- function(var1 = NULL, var2 = NULL ) {
  var1 <-  enquo(var1)
  var2 <-  enquo(var2)
  var1 <- formula(var1)
  
  if (!is.null(var2)) {
    var2 <- formula(var2)
    table <- survey::svyby(formula  =  var1 , by= var2, design = design, FUN =svymean, na.rm = TRUE) 
  }
  else  {
    table <- survey::svyby(formula  =  var1, by = NULL, design = design, FUN =svymean, na.rm = TRUE) 
  }
  tbl <- as.tbl(table) %>% rename(Freq = 2)
  return(round(tbl,3))  
}
c_table(alc,Age_Cat)
# new way
#works good
df019<- df19 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>%  purrr::transpose() %>% purrr::flatten_df() %>% t() %>% cbind(
        df19 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% map(function(x) SE(x)) %>%   purrr::transpose() %>% purrr::flatten_df() %>% t() )
df019 <- as_tibble(df019)

#df020 <- df20 %>% dplyr::select(c(alc,smoke,cannabis)) %>% map(function(x) round(prop.table(table(x))[2],3)) %>%  as.data.frame() %>% t() %>% as.data.frame()

df019
df019 <- df019 %>% dplyr::mutate(Var1 = rownames(df0)) %>% rename(Freq = 1, se = 2) %>% relocate(Var1,Freq ) %>% mutate(Var1 = recode(Var1, alc = "Alcohol",
                                                                                                                                      cannabis = "Cannabis",
                                                                                                                                      smoke = "Tobacco"),
                                                                                                                        Var2 = "2020")

df019
#Age 
#first group

df19_Diff_Age_1 <- c_table(alc,Age_Cat) %>% mutate(Var1 = "Alcohol", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                                      "2" = "20-24",
                                                                                      "3" = "25-29" ))

df19_Diff_Age_1
#second group
df19_Diff_Age_2 <- c_table(smoke,Age_Cat) %>%  mutate(Var1 = "Tobacco", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                                         "2" = "20-24",
                                                                                         "3" = "25-29" ))

df19_Diff_Age_2
# third group
df19_Diff_Age_3 <- c_table(cannabis,Age_Cat)  %>% mutate(Var1 = "Cannabis", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                                             "2" = "20-24",
                                                                                             "3" = "25-29" ))
df19_Diff_Age_3
df_age19 <- rbind(df19_Diff_Age_1, df19_Diff_Age_2,df19_Diff_Age_3) 
df_age19

#Gender 
#first group
df19_Diff_Gender_1 <- c_table(alc,Gender) %>% mutate(Var1 = "Alcohol", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df19_Diff_Gender_1
#second group
df19_Diff_Gender_2 <- c_table(smoke,Gender) %>% mutate(Var1 = "Tobacco", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df19_Diff_Gender_2
# third group

df19_Diff_Gender_3 <- c_table(cannabis,Gender) %>% mutate(Var1 = "Cannabis", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df19_Diff_Gender_3
df_gender19 <- rbind(df19_Diff_Gender_1, df19_Diff_Gender_2,df19_Diff_Gender_3) 
df_gender19

#Status 
#first group
df19_Diff_Status_1 <- c_table(alc,status) %>% mutate(Var1 = "Alcohol", status = case_when(status == 1 ~ "Student",
                                                                                          status == 2 ~ "Employed",
                                                                                          status == 3 ~ "Unemployed"))



df19_Diff_Status_1
#second group
df19_Diff_Status_2 <- c_table(smoke,status) %>% mutate(Var1 = "Tobacco", status = case_when(status == 1 ~ "Student",
                                                                                            status == 2 ~ "Employed",
                                                                                            status == 3 ~ "Unemployed"))
df19_Diff_Status_2
# third group
df19_Diff_Status_3 <- c_table(cannabis,status) %>% mutate(Var1 = "Cannabis", status = case_when(status == 1 ~ "Student",
                                                                                                status == 2 ~ "Employed",
                                                                                                status == 3 ~ "Unemployed"))
df19_Diff_Status_3

df_status19 <- rbind(df19_Diff_Status_1, df19_Diff_Status_2,df19_Diff_Status_3) 
#df_status20 <-  df_status20 %>% mutate(n = 1000)
df_status19

save(df_gender19,df_status19, df_age19,df019,file= "data19_Diff.RData")


# df 20 full weighted --------------------


#survey none 
p_load(survey)
design <-  survey::svydesign(ids = ~0, data = df20_full,weights = ~Weight) 
#df20 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% as.data.frame() %>% tidyr::pivot_longer(everything())



c_table <- function(var1 = NULL, var2 = NULL ) {
  var1 <-  enquo(var1)
  var2 <-  enquo(var2)
  var1 <- formula(var1)
  
  if (!is.null(var2)) {
    var2 <- formula(var2)
    table <- survey::svyby(formula  =  var1 , by= var2, design = design, FUN =svymean, na.rm = TRUE) 
  }
  else  {
    table <- survey::svyby(formula  =  var1, by = NULL, design = design, FUN =svymean, na.rm = TRUE) 
  }
  tbl <- as.tbl(table) %>% rename(Freq = 2)
  return(round(tbl,3))  
}
c_table(alc,Age_Cat)
# new way
#works good
df020F<- df20_full %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>%  purrr::transpose() %>% purrr::flatten_df() %>% t() %>% cbind(
  df20_full %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% map(function(x) SE(x)) %>%   purrr::transpose() %>% purrr::flatten_df() %>% t() )
df020F <- as_tibble(df020F)

#df020 <- df20 %>% dplyr::select(c(alc,smoke,cannabis)) %>% map(function(x) round(prop.table(table(x))[2],3)) %>%  as.data.frame() %>% t() %>% as.data.frame()

df020F
df020F <- df020F %>% dplyr::mutate(Var1 = rownames(df0)) %>% rename(Freq = 1, SE = 2) %>% relocate(Var1,Freq ) %>% mutate(Var1 = recode(Var1, alc = "Alcohol",
                                                                                                                                      cannabis = "Cannabis",
                                                                                                                                      smoke = "Tobacco"),
                                                                                                                        Var2 = "2020")

df020F
#Age 
#first group

df20F_Diff_Age_1 <- c_table(alc,Age_Cat) %>% mutate(Var1 = "Alcohol", Age_Cat = recode(Age_Cat,
                                                                                      "1" = "12-15",
                                                                                      "2"= "16-19",
                                                                                      "3" = "20-24",
                                                                                      "4" = "25-29" ))

df20F_Diff_Age_1
#second group
df20F_Diff_Age_2 <- c_table(smoke,Age_Cat) %>%  mutate(Var1 = "Tobacco", Age_Cat = recode(Age_Cat,
                                                                                          "1" = "12-15",
                                                                                          "2"= "16-19",
                                                                                          "3" = "20-24",
                                                                                          "4" = "25-29" ))

df20F_Diff_Age_2
# third group
df20F_Diff_Age_3 <- c_table(cannabis,Age_Cat)  %>% mutate(Var1 = "Cannabis", Age_Cat = recode(Age_Cat,
                                                                                              "1" = "12-15",
                                                                                              "2"= "16-19",
                                                                                              "3" = "20-24",
                                                                                              "4" = "25-29" ))
df20F_Diff_Age_3
df_age20F <- rbind(df20F_Diff_Age_1, df20F_Diff_Age_2,df20F_Diff_Age_3) 
df_age20F

#Gender 
#first group
df20F_Diff_Gender_1 <- c_table(alc,Gender) %>% mutate(Var1 = "Alcohol", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20F_Diff_Gender_1
#second group
df20F_Diff_Gender_2 <- c_table(smoke,Gender) %>% mutate(Var1 = "Tobacco", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20F_Diff_Gender_2
# third group

df20F_Diff_Gender_3 <- c_table(cannabis,Gender) %>% mutate(Var1 = "Cannabis", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20F_Diff_Gender_3
df_gender20F <- rbind(df20F_Diff_Gender_1, df20F_Diff_Gender_2,df20F_Diff_Gender_3) 
df_gender20F

#Status 
#first group
df20F_Diff_Status_1 <- c_table(alc,status) %>% mutate(Var1 = "Alcohol", status = case_when(status == 1 ~ "Student",
                                                                                          status == 2 ~ "Employed",
                                                                                          status == 3 ~ "Unemployed"))



df20F_Diff_Status_1
#second group
df20F_Diff_Status_2 <- c_table(smoke,status) %>% mutate(Var1 = "Tobacco", status = case_when(status == 1 ~ "Student",
                                                                                            status == 2 ~ "Employed",
                                                                                            status == 3 ~ "Unemployed"))
df20F_Diff_Status_2
# third group
df20F_Diff_Status_3 <- c_table(cannabis,status) %>% mutate(Var1 = "Cannabis", status = case_when(status == 1 ~ "Student",
                                                                                                status == 2 ~ "Employed",
                                                                                                status == 3 ~ "Unemployed"))
df20F_Diff_Status_3

df_status20F <- rbind(df20F_Diff_Status_1, df20F_Diff_Status_2,df20F_Diff_Status_3) 
#df_status20 <-  df_status20 %>% mutate(n = 1000)
df_status20F

save(df_gender20F,df_status20F, df_age20F,df020F,file= "data20_full.RData")




# javascript code -----------
highchart() %>%
  hc_chart(events = list(load = JS("function () {
    this.series[0].update({
      id: 'firstColumnSeries'
    }, false);
    this.series[1].update({
      id: 'secondColumnSeries'
    }, false);
    this.series[2].update({
      linkedTo: 'secondColumnSeries'
    }, false);
    this.series[3].update({
      linkedTo: 'firstColumnSeries'
    });
    
    
  }"))) %>%
  hc_add_series(df78, "column",hcaes(x = ShareType, y = InvestedPerAccount, group = Gender),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  hc_add_series(df78, 'errorbar', hcaes(x = ShareType, low = lower, high = upper, group = Gender, grouping = FALSE)) %>%
  hc_xAxis(categories = df78$ShareType, title = list(text = "Share Type")) %>% 
  hc_colors(c("pink","lightblue"))

#prop table gibt ohne margins die zellen prozente raus. insgesamt 100%. Sinvoll für density?


# df20_diff weighted--------------


 
df_none<- as.data.frame(prop.table(table(df20$alc,df20$Age_Cat),2)) %>%
  filter(Var1 != 0)
df_none <- df_none %>% mutate(Var1 = recode(Var1,"1" = "Alc"), Var2 = case_when(Var2 == 1 ~ "alter1",
                                                                        Var2 == 2 ~ "alter2",
                                                                        Var2 == 3 ~ "alter3"))
df_none
#df should look like this
# var1  frequency
# alc       x
# smoke     x
# cannabis  x
prop.table(table(df$alc))[2]
prop.table(table(df$alc))[2]
prop.table(table(df$alc))[2]

#survey none 
p_load(survey)
design <-  survey::svydesign(ids = ~ID, data = df20,weights = ~Weight) 
#df20 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% as.data.frame() %>% tidyr::pivot_longer(everything())


c_table <- function(var1 = NULL, var2 = NULL ) {
    var1 <-  enquo(var1)
    var2 <-  enquo(var2)
    var1 <- formula(var1)
    
  if (!is.null(var2)) {
    var2 <- formula(var2)
    table <- survey::svyby(formula  =  var1 , by= var2, design = design, FUN =svymean, na.rm = TRUE) 
  }
  else  {
    table <- survey::svyby(formula  =  var1, by = NULL, design = design, FUN =svymean, na.rm = TRUE) 
  }
    tbl <- as.tbl(table) %>% rename(Freq = 2, se1= 3)
    return(round(tbl,3))  
}
c_table(alc,Age_Cat) 

# new way
#works good
df020<- df20 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>%  purrr::transpose() %>% purrr::flatten_df() %>% t() %>% cbind(
        df20 %>% select(c(alc,smoke,cannabis)) %>% map(function(x) survey::svymean(x = ~ x, design = design,na.rm = TRUE)) %>% map(function(x) SE(x)) %>%   purrr::transpose() %>% purrr::flatten_df() %>% t() )
df020 <- as_tibble(df020)

#df020 <- df20 %>% dplyr::select(c(alc,smoke,cannabis)) %>% map(function(x) round(prop.table(table(x))[2],3)) %>%  as.data.frame() %>% t() %>% as.data.frame()

df020
df020 <- df020 %>% dplyr::mutate(Var1 = rownames(df0)) %>% rename(Freq = 1, SE = 2) %>% relocate(Var1,Freq ) %>% mutate(Var1 = recode(Var1, alc = "Alcohol",
                                                                                                                   cannabis = "Cannabis",
                                                                                                                 smoke = "Tobacco"),
                                                                                                         Var2 = "2020")

df020
#Age 
#first group
df20_Diff_Age_1 <- as.data.frame(prop.table(table(df20$alc,df20$Age_Cat),2)%>% round(3)) %>% 
  filter(Var1 != 0)

df20_Diff_Age_1 <- df20_Diff_Age_1 %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                          Var2 == 2 ~ "20-24",
                                                                          Var2 == 3 ~ "25-29"))
df20_Diff_Age_1 <- c_table(alc,Age_Cat) %>% mutate(Var1 = "Alcohol", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                          "2" = "20-24",
                                                                          "3" = "25-29" ))

df20_Diff_Age_1
#second group
df20_Diff_Age_2 <- as.data.frame(prop.table(table(df20$smoke,df20$Age_Cat),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Age_2 <- df20_Diff_Age_2 %>% mutate(Var1 = recode(Var1,"1" = "Tobacco"),  Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                                 Var2 == 2 ~ "20-24",
                                                                                 Var2 == 3 ~ "25-29"))
df20_Diff_Age_2 <- c_table(smoke,Age_Cat) %>%  mutate(Var1 = "Tobacco", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                                     "2" = "20-24",
                                                                                     "3" = "25-29" ))

df20_Diff_Age_2
# third group
df20_Diff_Age_3 <- as.data.frame(prop.table(table(df20$cannabis,df20$Age_Cat),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Age_3 <- df20_Diff_Age_3 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"),  Var2 = case_when(Var2 == 1 ~ "16-19",
                                                                                  Var2 == 2 ~ "20-24",
                                                                                  Var2 == 3 ~ "25-29"))
df20_Diff_Age_3 <- c_table(cannabis,Age_Cat)  %>% mutate(Var1 = "Cannabis", Age_Cat = recode(Age_Cat, "1"= "16-19",
                                                                                          "2" = "20-24",
                                                                                          "3" = "25-29" ))
df20_Diff_Age_3
df_age20 <- rbind(df20_Diff_Age_1, df20_Diff_Age_2,df20_Diff_Age_3) 
df_age20

#Gender 
#first group
df20_Diff_Gender_1 <- as.data.frame(prop.table(table(df20$alc,df20$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)
df20_Diff_Gender_1 <- df20_Diff_Gender_1 %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                          Var2 == 2 ~ "Male"))
df20_Diff_Gender_1 <- c_table(alc,Gender) %>% mutate(Var1 = "Alcohol", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20_Diff_Gender_1
#second group
df20_Diff_Gender_2 <- as.data.frame(prop.table(table(df20$smoke,df20$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Gender_2 <- df20_Diff_Gender_2 %>% mutate(Var1 = recode(Var1,"1" = "Tobacco"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                           Var2 == 2 ~ "Male"))
df20_Diff_Gender_2 <- c_table(smoke,Gender) %>% mutate(Var1 = "Tobacco", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20_Diff_Gender_2
# third group
df20_Diff_Gender_3 <- as.data.frame(prop.table(table(df20$cannabis,df20$Gender),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Gender_3 <- df20_Diff_Gender_3 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"), Var2 = case_when(Var2 == 1 ~ "Female",
                                                                             Var2 == 2 ~ "Male"))
df20_Diff_Gender_3 <- c_table(cannabis,Gender) %>% mutate(Var1 = "Cannabis", Gender =  case_when(Gender == 1 ~ "Female", Gender == 2 ~ "Male"))

df20_Diff_Gender_3
df_gender20 <- rbind(df20_Diff_Gender_1, df20_Diff_Gender_2,df20_Diff_Gender_3) 
df_gender20

#Status 
#first group
df20_Diff_Status_1 <- as.data.frame(prop.table(table(df20$alc,df20$status),2)%>% round(3)) %>%
  filter(Var1 != 0)
df20_Diff_Status_1 <- df20_Diff_Status_1 %>% mutate(Var1 = recode(Var1,"1" = "Alcohol"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                          Var2 == 2 ~ "Employed",
                                                                          Var2 == 3 ~ "Unemployed"))
df20_Diff_Status_1 <- c_table(alc,status) %>% mutate(Var1 = "Alcohol", status = case_when(status == 1 ~ "Student",
                                                                                          status == 2 ~ "Employed",
                                                                                          status == 3 ~ "Unemployed"))



df20_Diff_Status_1
#second group
df20_Diff_Status_2 <- as.data.frame(prop.table(table(df20$smoke,df20$status),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Status_2 <- df20_Diff_Status_2%>% mutate(Var1 = recode(Var1,"1" = "Tobacco"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                            Var2 == 2 ~ "Employed",
                                                                            Var2 == 3 ~ "Unemployed"))
df20_Diff_Status_2 <- c_table(smoke,status) %>% mutate(Var1 = "Tobacco", status = case_when(status == 1 ~ "Student",
                                                                                          status == 2 ~ "Employed",
                                                                                          status == 3 ~ "Unemployed"))
df20_Diff_Status_2
# third group
df20_Diff_Status_3<- as.data.frame(prop.table(table(df20$cannabis,df20$status),2)%>% round(3)) %>%
  filter(Var1 != 0)

df20_Diff_Status_3 <- df20_Diff_Status_3 %>% mutate(Var1 = recode(Var1,"1" = "Cannabis"), Var2 = case_when(Var2 == 1 ~ "Student",
                                                                             Var2 == 2 ~ "Employed",
                                                                             Var2 == 3 ~ "Unemployed"))
df20_Diff_Status_3 <- c_table(cannabis,status) %>% mutate(Var1 = "Cannabis", status = case_when(status == 1 ~ "Student",
                                                                                          status == 2 ~ "Employed",
                                                                                          status == 3 ~ "Unemployed"))
df20_Diff_Status_3

df_status20 <- rbind(df20_Diff_Status_1, df20_Diff_Status_2,df20_Diff_Status_3) 
#df_status20 <-  df_status20 %>% mutate(n = 1000)
df_status20

save(df_gender20,df_status20, df_age20,df020,file= "Module/data20_Diff.RData")


# compare
# hcchart none
highchart() %>%
  hc_plotOptions(column = list(grouping = FALSE)) %>%
  hc_add_series(rbind(df020,df0), "column",hcaes(x = Var1, y = Freq*100, group = Var2),pointPlacement = c(0.2,-0.2),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df0$Var1, title = list(text = "Konsum")) 


#age comparison
highchart() %>%
  hc_plotOptions(column = list(grouping = FALSE, borderWidth = 0, shadow =FALSE)) %>%
  hc_add_series(rbind(df_age20,df_age), "column",hcaes(x = Var1, y = Freq*100, group = Var2),pointPadding = c(0.3,0.4,0.3,0.4,0.3,0.4),pointPlacement = c(-0.3,-0.3,0,0,0.3,0.3),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_age$Var1[order(df_age$Var2)], title = list(text = "Konsum")) 



#negative values -----------

# hcchart none
highchart() %>%
  hc_add_series(mutate(df020, Freq = df020$Freq-df019$Freq), "column",hcaes(x = Var1, y = Freq*100),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df0$Var1, title = list(text = "Konsum")) 

# hc chart Age

highchart() %>%
  hc_add_series(mutate(df_age20, Freq = df_age20$Freq-df_age19$Freq), "column",hcaes(x = Var1, y = Freq*100, group = Age_Cat),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_age$Var1[order(df_age$Var2)], title = list(text = "Konsum")) 


# hc chart Gender
highchart() %>%
  hc_add_series(mutate(df_gender20, Freq = df_gender20$Freq-df_gender19$Freq), "column",hcaes(x = Var1, y = Freq*100, group = Gender),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_gender$Var1[order(df_gender$Var2)], title = list(text = "Konsum")) 

# hc chart Status
highchart() %>%
  hc_add_series(mutate(df_status20, Freq = df_status20$Freq - df_status19$Freq), "column",hcaes(x = Var1, y = Freq*100, group = status),
                tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
  #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
  #             centerInCategory = TRUE, groupPadding = .68,
  #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
  hc_xAxis(categories = df_status$Var1[order(df_status$Var2)], title = list(text = "Konsum")) 


#save(df_gender20,df_status20, df_age20,df020,file= "data19.RData")

