setwd("P:/Youth Report 2020/Data/Merge YSL and HBSC")

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(haven,labelled,huxtable,openxlsx,flextable,officer,survey,gtsummary,tidyr,dplyr,effsize, plyr)

df <- read_sav("P:/Youth Report 2020/Data/Merge YSL and HBSC/MergedAllVav.sav")#in tbl

warnings ()

# cleaning up. Dop NAs and Missing Labels
#df <- drop_na(df)
#df <- drop_unused_value_labels(df)

#Datensatz filtern und labels f?r Tabellenband ?ndern
df <- df %>% filter(Filter1129nO==1)

var_label(df$sex)<- paste ("Geschlecht")
val_label(df$sex, 1) <- paste ("M?nnlich")
val_label(df$sex, 2) <- paste ("Weiblich")
var_label(df$agecat)<- paste ("Alter")
val_label(df$agecat, 1) <- paste ("11-12 Jahre")
val_label(df$agecat, 2) <- paste ("13-14 Jahre")
val_label(df$agecat, 3) <- paste ("15-17 Jahre")
val_label(df$agecat, 4) <- paste ("18-20 Jahre")
val_label(df$agecat, 5) <- paste ("21-23 Jahre")
val_label(df$agecat, 6) <- paste ("24-26 Jahre")
val_label(df$agecat, 7) <- paste ("27-29 Jahre")
var_label(df$migrant)<- paste ("Migrationshintergrund")
val_label(df$migrant, 0) <- paste ("Kein Migrationshintergrund")
val_label(df$migrant, 1) <- paste ("min. ein Elternteil eingewandert")
val_label(df$migrant, 2) <- paste ("selbst eingewandert")
var_label(df$finSES_LMH)<- paste ("Finanzielle Resourcen (SES)")
val_label(df$finSES_LMH, 1) <- paste ("geringe finanzielle Resourcen (SES)")
val_label(df$finSES_LMH, 2) <- paste ("mittlere finanzielle Resourcen (SES)")
val_label(df$finSES_LMH, 3) <- paste ("hohe finanzielle Resourcen (SES)")
var_label(df$SubjSES)<- paste ("Subjektiver SES")
val_label(df$SubjSES, 1) <- paste ("niedrige Eigeneinsch?tzung des SES")
val_label(df$SubjSES, 2) <- paste ("mittlere Eigeneinsch?tzung des SES")
val_label(df$SubjSES, 3) <- paste ("hohe Eigeneinsch?tzung des SES")
var_label(df$CultSES)<- paste ("Kulturelles Kapital")
val_label(df$CultSES, 1) <- paste ("niedriges kulturelles Kapital")
val_label(df$CultSES, 2) <- paste ("mittleres kulturelles Kapital")
val_label(df$CultSES, 3) <- paste ("hohes kulturelles Kapital")

var_label(df$healthexc)<- paste ("Subjektive Gesundheit")
val_label(df$healthexc, 1) <- paste ("Ausgezeichnet")
val_label(df$healthexc, 0) <- paste ("Gut, einigermassen oder (sehr) schlecht")

#Insgesamt erstellen und valuelabel Insgesamt mit ungewichtetem n ersetzen. 
df <- df %>% mutate(Insgesamt = 1)
df <- tibble::rowid_to_column(df, "IDX")
val_label(df$Insgesamt, 1) <- paste("N=",sum(!is.na(df$healthexc)))

#remove haven class and convert labels as factors. 
df <- haven::as_factor(df)  #wichtig um keine haven klasse zu haben, ohenhin von Dev als Temp gedacht.

# Apply simple weight to data set. 
design <- survey::svydesign(ids = ~IDX, weights = ~Weight, data= df)

#  CI Function
categorical_ci <- function(variable, tbl,...) {
  b <-  dplyr::filter(tbl$meta_data, variable == .env$variable) %>% glimpse %>%
    purrr::pluck("df_stats", 1) %>%
  plyr::mutate(# calculate and format 95% C
    prop_ci = purrr::map2(n, N, ~prop.test(.x, .y)$conf.int %>% style_percent(symbol = TRUE)),
    ci = purrr::map_chr(prop_ci, ~glue::glue("{.x[1]}, {.x[2]}")) 
  ) %>%
    dplyr::pull(ci) %>% glimpse()
  split <- matrix(b, ncol=length(tbl$df_by$by), byrow =  T)
  #apply(split , 1 , paste, collapse = ";" )
  apply(split , 1 ,function(x) paste0("(",x,")", collapse = ";" ))
}

#confint(svymean(~factor(healthexc), design = design, na.rm=TRUE))

#svyciprop (~migrant, design, method="logit")


#define displayed stats for gtsummary
stats0 <- list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{p}%", Insgesamt ~ "{p}%")


#Loop
# create gt table 
table <- 
  design %>% tbl_svysummary(by = healthexc, include = c(Insgesamt, sex, agecat, healthexc, migrant, finSES_LMH, SubjSES, CultSES), digits  = list(everything() ~ 1), type = all_dichotomous()~ "categorical", missing="no", statistic = stats0, percent = "row") %>% add_p() %>%  modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**",var_label(df$healthexc),"**"))%>%  modify_header(label ~ "** **") %>%
  modify_header(update = all_stat_cols(F) ~ "**{level}**")%>% bold_labels() %>% add_stat(fns = everything() ~ "categorical_ci",location = "level",header = "**95% CI**")



paste2 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  gsub(paste0("(^",sep,"|",sep,"$)"),"",
       gsub(paste0(sep,sep),sep,
            do.call(paste,c(L,list(sep=sep)))))
}



p_load(strex) 
table <- table %>%  modify_table_body(dplyr::mutate, 
                   add_stat_1 = paste2(add_stat_1,";"),
                   stat_1 = paste2(stat_1, sub(";.*", "",add_stat_1 )), 
                   add_stat_1 = paste2( str_after_nth(add_stat_1, ";", 1)),
                   stat_2 =paste2(stat_2, str_before_nth(add_stat_1, ";", 1)),
                   add_stat_1 = paste2( str_after_nth(add_stat_1, ";", 1))
                   ) 



table 

if ("stat_3"  %in% names(table$table_body)) {
 table <- table %>%  modify_table_body(
    dplyr::mutate, stat_3 =paste2(stat_3,str_before_nth(add_stat_1, ";", 1) ),
                                  add_stat_1 = paste2( str_after_nth(add_stat_1, ";", 1))
                                  )
}
table
if ("stat_4"  %in% names(table$table_body)) {
 table <- table %>%  modify_table_body(
    dplyr::mutate, stat_4 =paste2(stat_4, str_before_nth(add_stat_1, ";", 1)),
                                  add_stat_1 = paste2( str_after_nth(add_stat_1, ";", 1))
                                  )
}
table
if ("stat_5"  %in% names(table$table_body)) {
 table <- table %>%  modify_table_body(
    dplyr::mutate, stat_5 =paste2(stat_5,str_before_nth(add_stat_1, ";", 1)) ) 
}
table
table <- table %>% modify_column_hide(add_stat_1)                             
table




setwd("P:/Youth Report 2020/Tabellenband")

#export gt table as excel
hux <- table %>% as_hux_table()

wb <- openxlsx::createWorkbook()
wb <- as_Workbook(hux,wb,sheet = "jd")
saveWorkbook(wb,"wb.xlsx",overwrite = T)

# export gt table as docx
flex1 <- table %>% as_flex_table() %>% flextable::width(width = 1.3)

print(flex1,preview = "docx")
save_as_docx(flex1,
             path = "flex1.docx")

#Loop