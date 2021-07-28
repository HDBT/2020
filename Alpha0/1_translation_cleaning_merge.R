
#ysl19 --------
#uebersetzungen mergen --------------------------------
p_load(openxlsx, plyr)
path <-"Y:/YSL Wave 2019/Questionnaire/Main field/Final/Other formats/xlsx/" 
flist <- list.files(path)
trans <- list(NULL)
for (i in 1:length(flist)) {
  print(i)
  fpath <- paste0(path,flist[i])
  trans[i] <- list(read.xlsx(paste0(path,flist[i])))
  names(trans)[i] <- substr(flist[i], nchar(flist[i])-6,nchar(flist[i])-5)
}
glimpse(trans)
class(trans)

#transdf <- inner_join(trans$DE, trans$FR,by= "PhraseID")
transdf <- plyr::join_all(trans, by = "PhraseID", type = "inner" )
names(transdf) <- tolower(names(transdf))

#fuer vis trans
transdf <- transdf[- c(1:4),]
transdf <- transdf[,-1]
names(transdf)[names(transdf)== "en-gb"] <- "en" 


transdf <- transdf[complete.cases(transdf),]


write.csv(transdf,file = "translationsR.csv",sep = ",")


#cleaning 

library(rvest)
library(purrr)
library(dplyr)
library(data.table)
strip_html <- function(s) {
  html_text(read_html(charToRaw(s)))
}

#nicht ganz sauber. Glyphen hinterlasssen. 
apply(transdf, function(x) {strip_html(x)})
write.csv(apply(transdf,MARGIN =1, FUN = function(x) {strip_html(x)}), file = "dije.csv", fileEncoding = "UTF-8")

# keep rows only if they dont contain any html elements. Delets a few rows. Quick and dirty. 

transdf_grepl <- transdf  %>% dplyr::filter(across(everything(), ~ !grepl("<",.))) #works good
transdf_grepl <- transdf_grepl[!duplicated(transdf_grepl$de),]
transdf_grepl <- transdf_grepl[!duplicated(transdf_grepl$en),]
transdf_grepl <- transdf_grepl[!duplicated(transdf_grepl$fr),]
transdf_grepl <- transdf_grepl[!duplicated(transdf_grepl$lux),]
transdf_grepl <- transdf_grepl[!duplicated(transdf_grepl$pt),]
transdf_list <- list("languages" = c("de","en","fr","lux","pt"), "translation" = transdf_grepl) 
#export to json for shiny.i18n
library(pacman)
p_load(jsonlite)
jsonlite::write_json(transdf_list, "translation.json")


#yac 20 --------------
library(XML)
yac <- xmlParse("Y:/YSL Wave 2020/Questionnaire 2020/Questionnaires as run/(.xml)/YAC+_Questionnaire_ALL LANGUAGES.xml")
yac <- xmlToDataFrame(yac)
names(yac) <- tolower(names(yac))
names(yac)[names(yac)== "en-gb"] <- "en" 
yac_grepl <- yac  %>% dplyr::filter(across(everything(), ~ !grepl("<",.))) #works good
yac_grepl <- yac_grepl[!duplicated(yac_grepl$de),]
yac_grepl <- yac_grepl[!duplicated(yac_grepl$en),]
yac_grepl <- yac_grepl[!duplicated(yac_grepl$fr),]
yac_grepl <- yac_grepl[!duplicated(yac_grepl$lux),]
yac_grepl <- yac_grepl[!duplicated(yac_grepl$pt),]

yac_list <- list("languages" = c("en","de","fr","lux","pt"), "translation" = yac_grepl) 
jsonlite::write_json(yac_list, "translationYAC.json")


#allgemeine translation file erstellen

trans <- read_csv("translation_en.csv")
trans <- jsonlite::read_json("translation.json")
p_load(editData)
tans <- editData(as_tibble(trans$translation))
trans <- tans

#trans <- tans %>% mutate(fr = "test")
trans <- list("languages" = c("en","de","fr"), "translation" = trans) 

jsonlite::write_json(trans, "translation_general.json")



# Test table reporting ----------------------------------------------------
library(pacman)
p_load(gtsummary)
tbl <- trial %>%dplyr::select(trt, age, grade, response) %>%tbl_summary(by = trt)
# Example 1 ----------------------------------# Add statistic presented to the variable label row
add_stat_label_ex1 <-tbl %>%add_stat_label(# update default statistic label for continuous variables
label = all_continuous() ~ "med. (diqr)")
add_stat_label_ex1

#Example 2 
tbl %>%add_stat_label(# add a new column with statistic labels
  location = "column")
# Example 3 ----------------------------------
trial %>%select(age, grade, trt) %>%
  tbl_summary(by = trt,type = all_continuous() ~ "continuous2",statistic = all_continuous() ~ c("{mean} ({sd})", "{min} - {max}"),) %>%
  add_stat_label(label = age ~ c("Mean (SD)", "Min - Max")) 

# example from stack
mtcars %>%
  select(mpg, cyl, vs, am) %>%
  # create a new variable to display N in table
  mutate(total = 1) %>%
  # this is just to reorder variables for table 
  select(total, everything()) %>%
  tbl_summary(
    by = am, value = list("total" ~ "level to show"),
    # this is to specify you only want N (and no percentage) for new total variable
    statistic = total ~ "N = {N}") %>%
  # this is a gtsummary function that allows you to edit the header
  modify_header(stat_by = "**{level}**")


# felxtable optipn
mtcars %>%
  select(mpg, cyl, vs, am) %>%
  # create a new variable to display N in table
  tbl_summary(
    by = am
    # this is to specify you only want N (and no percentage) for new total variable
  ) %>%
  # this is a gtsummary function that allows you to edit the header
  modify_header(stat_by =  "{level}\nN = {N}") %>%
  as_flex_table()





