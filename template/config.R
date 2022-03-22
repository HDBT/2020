
# @diff: should differences of two datasets be calculated? Remember to prepare the datasets to be eqaually in demographic characteristics!
# @input1: provide row var input. E.g. input$test
# @input_choices: provide exact set chouces in input. Note for me: set this automatically.
# @input2: provide col var input. E.g. input$thema
# @input_choices: provide exact set choices in input. Note for me: set this automatically in future updates.
# @df_set1: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @df_set2: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @df_set3: provide prepared dfs as list() in exact order to be displayed from top to bottom. Note for me: change prep routine to make this obsolete.
# @title: provide vector with titles as strings to be used. 
# @diff_positon: provide numeric vector to specify the df_sets, which should be substracted. Default = c(1,3)



diff = TRUE
input1 = input$test 
input_choices1 = c("None", "Age", "Gender", "Status")
input2 = input$thema
  
input_choices2 = c("2019", "2020", "Differences")
df_set1 = list(df019, df_age19, df_gender19, df_status19)
df_set2 = list(df020F,df_status20F,df_age20F,df_gender20F)
df_set3 = list(df020, df_age20, df_gender20, df_status20)
title = c("Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age.",
"Percentage of those who indicated to have consumed one of the following psychoactive substances at least once in the past 30 days by age."
)
