
##This code reads in an excel file,  recodes variables,creates labels, and frequency tables##


#Load R packages
#Install these packages#
# #install.packages("tidyverse",
#                  "dslabs",
#                  "readxl",
#                  "dplyr",
#                  "gtsummary",
#                   "expss")

#installed.packages("expss")
# install.packages('haven')
# install.packages('writexl')

library(haven)
library(tidyverse)
library(dslabs)
library(readxl)
library(writexl)
library(dplyr)
library(gtsummary)
library(expss)
library(openxlsx)
library(FactoMineR)
library(corrr)
library(homals)
library(epiDisplay)
library(psych)
library(ltm)
library(janitor)


#set up a working directory
getwd()
setwd("C:/Users/38592/OneDrive - ICF/Documents/Documents/DASH SSE Demo/YRBS/")

#import data
yrbs <- read_sav('Data/Raw/2023 High School YRBS - Final w All Translation_August 7, 2023_13.58.sav')
class(yrbs)
glimpse(yrbs)
#view(yrbs)






#explore data
#convert var names to lowercase
#names(yrbs) <- tolower(names(yrbs))
#sch <- c('school', 'school_category', 'school_code', 'school_name', 'school_nces', 'school_type', 'school_code.0')


#export labeled excel dataset
#write_labelled_xlsx(yrbs, "Data/Raw/yrbs_raw.xlsx")

#clean data
# ## change 1: convert haven_labelled variables to factors ----
# mutate_if(haven::is.labelled, haven::as_factor) %>% 
# # change 2: convert variable labels to variable names ----
# sjlabelled::label_to_colnames()
yrbs_clean <- yrbs %>% rename_with(tolower) %>%  
                  filter(school_type %in% c("High School", "Combined Elementary/Secondary School") 
                  & !school %in% c("Groppi Hi","Marshall Hi","Milw Co Youth Educ Center",
                    "Shalom Hi","South Accelerated Acad","Vincent Accelerated Acad")) %>% 
                  mutate(q246=tolower(q246),
                         q246_6_text=tolower(q246_6_text),
                         
                         sex_merged=pmax(q404,q244,na.rm=TRUE), 
                         sex_merged= case_when(sex_merged==1 ~ 3,
                                                 q246=="female" ~ 3,
                                                 q246=="male" ~ 2,
                                                 q246=="nonbinary afab" ~ 5,
                                                 grepl("binary", q246) ~ 1,
                                                 grepl("fluid", q246) ~ 1,
                                                 grepl("queer", q246) ~ 1,
                                                 grepl("conforming", q246) ~ 1,
                                                 grepl("agender", q246) ~ 1,
                                                 q246 %in% c("both","demi-boy", "non-bianary",
                                                             "i don't see me as a woman or male so i use they/them.") ~ 1,
                                                 grepl("trans", q246) ~ 5,
                                                 TRUE ~ sex_merged), 
                         
                         gender_ident=case_when(q90==2 ~ 5,TRUE ~ q2),
                         
                         hispanic=case_when(grepl("hispanic", q246_6_text) ~ 1),
                         
                         race_aian_combined=pmax(q412_1,q246_1,na.rm=TRUE),
                         race_asian_combined=pmax(q412_2,q246_2,na.rm=TRUE),
                         race_black_combined=pmax(q412_3,q246_3,na.rm=TRUE),
                         race_nhpi_combined=pmax(q412_4,q246_4,na.rm=TRUE),
                         race_white_combined=pmax(q412_5,q246_5,na.rm=TRUE),
                         sumrace = rowSums(pick(race_aian_combined:race_white_combined), na.rm = TRUE),
                         
                         
                         race_eth=case_when(q406 == 1 | (q406 == 2 & hispanic==1) ~ 1,
                                            q406 == 2 & sumrace==1 & race_black_combined==1 ~ 2,
                                            q406 == 2 & sumrace==1 & race_white_combined==1 ~ 3,
                                            q406 == 2 & sumrace>1 ~ 4,
                                            q406 == 2 & sumrace==1 & (race_aian_combined==1|
                                            race_asian_combined==1|race_nhpi_combined==1|q246_6==1)  ~ 5),
                                             
                         lgbtq_ident=case_when(q90==2 ~ 1,
                                                q2==1 ~ 1,
                                                q89 %in% 2:5 ~ 1,
                                                q89==1 ~ 0),
                         
                         min_sex_ident=case_when(q89 %in% 2:5 ~ 1,
                                                 q89==1 ~ 0),
                         
                         min_gen_ident=case_when(q90==2 ~ 1,
                                                 q2==1 ~ 1,
                                                 q2 %in% 2:3 ~ 0),
                         
                         sumfamstr = rowSums(pick(q15_1:q15_10), na.rm = TRUE),
                         sumparentfamstr = rowSums(pick(q15_1:q15_3), na.rm = TRUE),
                         
                         oth_fam_str = case_when(q15_10 == 1 & sumfamstr>1 ~ 1,
                                         q15_10 == 1 & sumfamstr==1 ~ 0,
                                         TRUE ~ q15_10),
                         self_fam_str = case_when(q15_9 == 1 & sumfamstr>1 ~ 1,
                                                 q15_9 == 1 & sumfamstr==1 ~ 0,
                                                 TRUE ~ q15_9),
                         oth_parent_fam_str = case_when(q15_10 == 1 & sumparentfamstr>0 ~ 1,
                                                 q15_10 == 1 & sumparentfamstr==0 ~ 0,
                                                 TRUE ~ q15_10),
                         self_parent_fam_str = case_when(q15_9 == 1 & sumparentfamstr>0 ~ 1,
                                                  q15_9 == 1 & sumparentfamstr==0 ~ 0,
                                                  TRUE ~ q15_9),
                         foster_parent_fam_str = case_when(q15_5 == 1 & sumparentfamstr>0 ~ 1,
                                                         q15_5 == 1 & sumparentfamstr==0 ~ 0,
                                                         TRUE ~ q15_5),
                         multigenerat_fam_str = case_when(q15_4 == 1 & sumparentfamstr>0 ~ 1,
                                                           q15_4 == 1 & sumparentfamstr==0 ~ 0,
                                                           TRUE ~ q15_4),
                         
                         famstr_d= case_when(sumparentfamstr>0 & !sumfamstr %in% c(0,10) ~ 1,
                                             sumparentfamstr==0 & !sumfamstr %in% c(0,10) ~ 0),
                         
                         famstr_cat= case_when(sumparentfamstr>0 & !sumfamstr %in% c(0,10) & is.na(q15_4)  ~ 1,
                                               sumparentfamstr>0 & !sumfamstr %in% c(0,10) & q15_4 == 1 ~ 2,
                                               sumparentfamstr==0 & !sumfamstr %in% c(0,10) ~ 3),
                                            
                         
                         sex_merged= case_when(sex_merged == 1 ~ 'Non-Binary',
                                                  sex_merged == 2 ~ 'Male',
                                                  sex_merged == 3 ~ 'Female',
                                                  sex_merged == 5 ~ 'Transgender'),
                         
                          gender_ident= case_when(gender_ident == 1 ~ 'Non-Binary',
                                                  gender_ident == 2 ~ 'Male',
                                                  gender_ident == 3 ~ 'Female',
                                                  gender_ident == 4 ~ 'I do not know what this question is asking',
                                                  gender_ident == 5 ~ 'Transgender'),
                         
                          lgbtq_ident= case_when(lgbtq_ident == 0 ~ 'Not Identifies LGBTQ+',
                                                 lgbtq_ident == 1 ~ 'Identifies LGBTQ+'),

                          min_sex_ident= case_when(min_sex_ident == 0 ~ 'Not Identifies Minority Sexual Identity',
                                                  min_sex_ident == 1 ~ 'Identifies Minority Sexual Identity'),

                          min_gen_ident= case_when(min_gen_ident == 0 ~ 'Not Identifies Minority Gender Identity',
                                                  min_gen_ident == 1 ~ 'Identifies Minority Gender Identity'),
                         
                          race_eth= case_when(race_eth == 1 ~ 'Hispanic/Latino/a',
                                             race_eth == 2 ~ 'Non-Hispanic Black or African American',
                                             race_eth == 3 ~ 'Non-Hispanic White',
                                             race_eth == 4 ~ 'Non-Hispanic Multiracial',
                                             race_eth == 5 ~ 'Non-Hispanic Other'),
                         )


#add variable labels
var_lab(yrbs_clean$gender_ident) = "MM1 Based Gender Identity"
var_lab(yrbs_clean$sex_merged) = "Gender Identity based on Sex Variables"
var_lab(yrbs_clean$race_eth) = "Race/Ethnicity"
var_lab(yrbs_clean$lgbtq_ident) = "Identifies as LGBTQ+"
var_lab(yrbs_clean$min_sex_ident) = "Minority Sexual Identity"
var_lab(yrbs_clean$min_gen_ident) = "Minority Gender Identity"

# glimpse(yrbs_clean)
# table(yrbs_clean$q1, useNA = "ifany")
# unique(yrbs_clean$q1)
# with(yrbs_clean, table(school,school_type, useNA = "ifany"))
# table(yrbs_clean$school_type)
# table(yrbs_clean$sex_merged)
# with(yrbs_clean, table(sex_merged, q246,  useNA = "ifany"))
# table(yrbs_clean$q406)
# table(yrbs_clean$sumrace)
# table(yrbs_clean$race_eth)
# with(yrbs_clean, table(q406, hispanic,  useNA = "ifany"))
# with(yrbs_clean, table(hispanic, q246_6_text))
# hispanic <- yrbs_clean %>% filter(hispanic==1) %>% distinct(q246_6_text) %>% print(n=500)
# gender <- yrbs_clean %>% filter(sex_combined==1) %>% dplyr::select(q246) %>% print(n=500)
# gender2 <- yrbs_clean %>% filter(sex_combined==5) %>% dplyr::select(q246) %>% print(n=500) 
# table(yrbs_clean$lgbtq_ident, useNA = "ifany")
# with(yrbs_clean, table(lgbtq_ident, q89,  useNA = "ifany"))
# with(yrbs_clean, table(lgbtq_ident, q90,  useNA = "ifany"))
# with(yrbs_clean, table(lgbtq_ident, q2,  useNA = "ifany"))
with(yrbs_clean, table(min_sex_ident, q89,  useNA = "ifany"))
with(yrbs_clean, table(min_gen_ident, q2,  useNA = "ifany"))
with(yrbs_clean, table(min_gen_ident, q90,  useNA = "ifany"))
table(yrbs_clean$min_sex_ident, useNA = "ifany")
table(yrbs_clean$min_gen_ident, useNA = "ifany")

#family structure

table(yrbs_clean$sumfamstr, useNA = "ifany")
table(yrbs_clean$q15_10, useNA = "ifany")
with(yrbs_clean, table(q15_10, oth_fam_str,  useNA = "ifany"))
table(yrbs_clean$q15_9, useNA = "ifany")
with(yrbs_clean, table(q15_9, self_fam_str,  useNA = "ifany"))
with(yrbs_clean, table(q15_9, sumfamstr,  useNA = "ifany"))
table(yrbs_clean$sumparentfamstr, useNA = "ifany")
table(yrbs_clean$q15_10, useNA = "ifany")
with(yrbs_clean, table(q15_10, oth_parent_fam_str,  useNA = "ifany"))
table(yrbs_clean$q15_9, useNA = "ifany")
with(yrbs_clean, table(q15_9, self_parent_fam_str,  useNA = "ifany"))
with(yrbs_clean, table(q15_9, sumparentfamstr,  useNA = "ifany"))
table(yrbs_clean$q15_5, useNA = "ifany")
with(yrbs_clean, table(q15_5, foster_parent_fam_str,  useNA = "ifany"))
with(yrbs_clean, table(q15_5, sumparentfamstr,  useNA = "ifany"))
table(yrbs_clean$q15_4, useNA = "ifany")
with(yrbs_clean, table(q15_4, multigenerat_fam_str,  useNA = "ifany"))
with(yrbs_clean, table(q15_4, sumparentfamstr,  useNA = "ifany"))
table(yrbs_clean$famstr_d, useNA = "ifany")
with(yrbs_clean, table(famstr_d, sumfamstr,  useNA = "ifany"))
with(yrbs_clean, table(famstr_d, sumparentfamstr,  useNA = "ifany"))
table(yrbs_clean$famstr_cat, useNA = "ifany")
with(yrbs_clean, table(famstr_cat, q15_4,  useNA = "ifany"))
with(yrbs_clean, table(famstr_cat, sumparentfamstr,  useNA = "ifany"))


##School Climate Items Summary##
climate<-c("q5", "q9",	"q20.0",	"q26",	"q38",	"q356",	"q360",	"q362",	
           "q364",	"q366",	"q368",	"q370",	"q372",	"q374")

frq_table1 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>% 
  dplyr::select(all_of(climate)) %>%
  tbl_summary( missing = "ifany")
frq_table1


# #School Climate - EFA
# climate2<-c("q5", "q9",	"q20.0",	"q26",	"q38")
# yrbs_climate<-select(yrbs_clean, climate2)
# #check for missingness
# colSums(is.na(yrbs_climate))
# glimpse(yrbs_climate)
# 
# 
# #Pearson Correlation
# pear_cor = cor(yrbs_climate)
# cor.plot(pear_cor, numbers=T, upper=FALSE, main = "Pearson Correlation", show.legend = FALSE)
# #Polychoric correlation
# poly_cor = polychoric(yrbs_climate)
# rho = poly_cor$rho
# #save(rho, file = "polychoric")
# ### Thresholds/Scaling results
# #poly_cor$tau
# cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
# # Scree plot
# fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# 
# 
# # Polychoric factor analysis
# poly_fa <-  fa(yrbs_climate, 1, cor="poly", fm="mle", rotate = "promax") #options include "varimax", "promax", and "none"
# fa.diagram(poly_fa)
# poly_fa
# poly_fa$values
# poly_fa2 <-  fa(yrbs_climate, 2, cor="poly", fm="mle", rotate = "promax") #options include "varimax", "promax", and "none"
# fa.diagram(poly_fa2)
# poly_fa2
# poly_fa$values
# poly_fa3 <-  fa(yrbs_climate, 3, cor="poly", fm="mle", rotate = "promax") #options include "varimax", "promax", and "none"
# fa.diagram(poly_fa3)
# poly_fa3
# poly_fa3$values
# 
# 
# 
# #Cronbach's alpha
# #cronbach.alpha(pd, na.rm = TRUE)
# #or for more detailed summary use alpha function from psych package
# psych::alpha(yrbs_climate) #alpha is 0.65
# psych::alpha(yrbs_climate[1:3]) #alpha is 0.5
# psych::alpha(yrbs_climate[4:5]) #alpha is 0.68
# 
# 
# #export plots
# dev.copy(pdf,"fa_plots_climate.pdf")
# mtext("PD Loadings Plots", 1)
# # create a 2X2 grid
# par( mfrow= c(1,2) )
# fa.diagram(poly_fa)
# fa.diagram(poly_fa2)
# dev.off() 
# 
# #save output as text file
# capture.output(print("Climate Construct - Single Factor"),loadings(poly_fa),
#                summary(psych::alpha(yrbs_climate)),
#                print("Policy Climate - Two Factors"),loadings(poly_fa2),
#                summary(psych::alpha(yrbs_climate[1:3])),
#                summary(psych::alpha(yrbs_climate[4:5])),
#                file = "efa_alpha_climate.txt", append=TRUE)


##Our Module and select PF Items Summary##
our_module_plus<-c("q14.0",	"q15_1",	"q15_2",	"q15_3",	"q15_4",	"q15_5",	"q15_6",	"q15_7",
           "q15_8",	"q15_9",	"q15_10",	"q16",	"q18",	"q19",	"q20.0",	"q21.0",
           "q22.0",	"q23.0",	"q24.0",	"q25.0",	"q359",	"q363",	"q365.0")

frq_table2 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>% 
  dplyr::select(all_of(our_module_plus)) %>%
  tbl_summary( missing = "ifany")
frq_table2



##Outcomes Summary##
outcomes<-c("q24",	"q23",	"q25",	"q31",	"q32",	"q36",
            "q37",	"q433",	"q46",	"q47", "q48",	"q49",	
            "q66",	"q68",	"q70",	"q80",	"q78",	"q73",
            "q74",	"q84",	"q85",	"q86",	"q87",	"q315")

frq_table3 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>% 
  dplyr::select(all_of(outcomes)) %>%
  tbl_summary( missing = "ifany")
frq_table3


##Individual and Interpersonal PFs Summary##
pfs<-c("q404",	"q244",	"q246",	"q405",	"q406",	"q412_1",	"q412_2",	"q412_3",
        "q412_4",	"q412_5",	"q246_1",	"q246_2",	"q246_3",	"q246_4",	"q246_5",
        "q246_6",	"q246_6_text",	"q89",	"q90",	"q109",	"q113.0",	"q114.0",
        "q224",	"q2",	"q329",	"q331",	"q333",	"q335", "race_eth","gender_ident", 
        "sex_merged", "min_gen_ident", "min_sex_ident", "lgbtq_ident")


frq_table4 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>% 
  dplyr::select(all_of(pfs)) %>%
  tbl_summary( missing = "ifany")
frq_table4


summaries <- list('school climate' = frq_table1, 'our module' = frq_table2,
                  'outomes' = frq_table3, 'protective factors' = frq_table4)
write.xlsx(summaries, "yrbs_frequency_tables.xlsx")


#Crosstabs#
#Gender Identity#
#Sex Combined#
# with(yrbs_clean, table(q404,q244, useNA = "ifany"))
# with(yrbs_clean, table(sex_merged,q244, useNA = "ifany"))
# with(yrbs_clean, table(sex_merged,q404, useNA = "ifany"))
# with(yrbs_clean,table(gender_ident,sex_merged, useNA = "ifany"))
# unique(yrbs_clean$q246)

crosstab1 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>%
  tabyl(sex_merged, q2)  %>% adorn_title("combined") %>% 
  adorn_totals(c("row", "col")) 

crosstab2 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>%
           tabyl(gender_ident,sex_merged) %>% adorn_title("combined") %>% 
            adorn_totals(c("row", "col")) 

crosstab3 <- yrbs_clean %>% mutate_if(haven::is.labelled, haven::as_factor) %>%
  tabyl(q2,  q90) %>% adorn_title("combined") %>% 
  adorn_totals(c("row", "col")) 


summaries <- list('school climate' = frq_table1, 'our module' = frq_table2,
                  'outomes' = frq_table3, 'protective factors' = frq_table4,
                  "Gender_MM1Only" = crosstab1, 'MM1+Transgender_Gender' = crosstab2,
                  "MM1_Transgender" = crosstab3)
write.xlsx(summaries, "yrbs_frequency_tables.xlsx")

