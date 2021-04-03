library(pacman)
p_load(haven, tidyverse, gt, gtsummary,highcharter,broom)

df <- read_spss("Y:/YSL Wave 2020/Data/Final Dataset/YAC+_Final dataset.sav")
df19 <- read_spss("Y:/Youth Report 2020/YAC Erweiterung/old/Caro 8_11/YSL2019_Final dataset_Clean Including Migrant.sav")


df <- df %>% mutate(alc = ifelse(A60 == 0, 0,1),
              smoke= ifelse(A59 == 0,0,1),
              cannabis= ifelse(A62 == 0,0,1),
              Age_Cat = case_when(Age >=12 & Age < 17 ~ 1,
                                  Age >=17 & Age < 23  ~ 2,
                                  Age >=23 & Age < 29  ~  3),
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
                
              )
)
df19 <- df19 %>% mutate(alc = ifelse(A58_2 == 0, 0,1),
              smoke= ifelse(A57_2 == 0,0,1),
              cannabis= ifelse(A61_2 == 0,0,1),
              Age_Cat = case_when(Age >=12 & Age < 17 ~ 1,
                                  Age >=17 & Age < 23  ~ 2,
                                  Age >=23 & Age < 29  ~  3),
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

head(df$Age_Cat)
summary(df$Age_Cat)

# none
an <- prop.table(table(df$alc)) %>% {.*100} %>% round(2) %>% {.[2]}
sn <- prop.table(table(df$smoke)) %>% {.*100} %>% round(2)%>% {.[2]}
cn <- prop.table(table(df$cannabis)) %>% {.*100} %>% round(2)%>% {.[2]}

asc <- c(an,sn,cn) 

#age
aa <- prop.table(table(df$alc, df$Age_Cat),2) %>% {.*100} %>% round(2) %>% {.[2,1:3]}
as <- prop.table(table(df$smoke, df$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ac <- prop.table(table(df$cannabis, df$Age_Cat),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}

ma<- matrix(c(aa,as,ac),ncol =3,byrow = FALSE)

#Gender
ag <- prop.table(table(df$alc, df$Gender),2) %>% {.*100} %>% round(2)%>% {.[2,1:2]}
sg <- prop.table(table(df$smoke, df$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}
cg <- prop.table(table(df$cannabis, df$Gender),2)%>% {.*100} %>% round(2)%>% {.[2,1:2]}

mg<- matrix(c(ag,sg,cg),ncol =3,byrow = FALSE)


#Status
as <- prop.table(table(df$alc, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ss <- prop.table(table(df$smoke, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
cs <- prop.table(table(df$cannabis, df$status),2)%>% {.*100} %>% round(2)%>% {.[2,1:3]}
ms<- matrix(c(as,ss,cs),ncol =3,byrow = FALSE)
ms

margin.table(table(df$smoke,df$Age_Cat))

#save objects
save(asc, ma, mg,ms, file= "data.RData")





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

#save objects
save(asc19, ma19, mg19,ms19, file= "data.RData")
