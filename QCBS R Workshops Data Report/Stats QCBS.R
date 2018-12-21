
# QCBS R workshops stats --------------------------------------------------

# df.reg=read.csv("~/Desktop/QCBS_rworshops_data/r_workshops_registration.csv")
# df.reg=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_registration.csv") 
df.reg=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_registration (1).csv") 
dim(df.reg)
library(dplyr)
people=paste(trimws(df.reg$first_name),trimws(df.reg$last_name),sep="_") %>%  
  tolower() %>% 
  gsub(pattern = "  ",replacement = "_",x = .) %>% 
  gsub(pattern = " ",replacement = "_",x = .) %>% 
  gsub(pattern = "-",replacement = "_",x = .) %>% 
  gsub(pattern = "é",replacement = "e",x = .) %>% 
  gsub(pattern = "ê",replacement = "e",x = .) %>% 
  gsub(pattern = "ë",replacement = "e",x = .) %>% 
  gsub(pattern = "ç",replacement = "c",x = .) %>% 
  gsub(pattern = "è",replacement = "e",x = .) %>% 
  gsub(pattern = "ô",replacement = "o",x = .) %>% 
  gsub(pattern = "ö",replacement = "o",x = .) %>% 
  gsub(pattern = "ï",replacement = "i",x = .) %>% 
  gsub(pattern = "á",replacement = "a",x = .) %>% 
  gsub(pattern = "adriana_aguilar",replacement = "adriana_raquel_aguilar_melo",x = .) %>% 
  gsub(pattern = "jean-marc_kaumbu_kyalamakasa",replacement = "jean-marc_kaumbu",x = .) %>%
  gsub(pattern = "jose_lira",replacement = "jose_jonathas_lira",x = .) %>%
  gsub(pattern = "pablo_menenez",replacement = "pablo_menendez",x = .)
  df.reg$name.corr = people
  df.reg$name.corr %>% 
    unique() %>% 
    sort() %>% 
    .[.!="_"] %>% 
    .[.!="nffwjdhnde_nffwjdhnde"] %>% 
    .[.!="pcrvvcivjr_pcrvvcivjr"] %>% 
    .[.!="pdxaivqkyb_pdxaivqkyb"] %>% 
    .[.!="spcmubxagc_spcmubxagc"]
  # write.csv(df.reg,"~/Desktop/people.csv")
  
  
  # df.workshop=read.csv("~/Desktop/QCBS_rworshops_data/r_workshops_lookup.csv")
  # df.workshop=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_lookup.csv")
  df.workshop=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_lookup (1).csv")
  data.m=base::merge(df.workshop,
              df.reg,
              by.x = "registrant", 
              by.y = "id")
  
  # df.workshop.date=read.csv("~/Desktop/QCBS_rworshops_data/r_workshops_dates.csv")
  # df.workshop.date=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_dates.csv")
  df.workshop.date=read.csv("/Users/Marco/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/r_workshops_dates (1).csv")
  merged.data = base::merge(data.m,
               df.workshop.date,
               by.x = "workshop", 
               by.y = "id")
  merged.data$date = as.character(merged.data$date)
  merged.data$registration_timestamp = as.character(merged.data$registration_timestamp)
  merged.data[merged.data$date == "0000-00-00","date"] <- merged.data[merged.data$date == "0000-00-00","registration_timestamp"]
  merged.data[merged.data$registration_timestamp == "0000-00-00 00:00:00","registration_timestamp"] <-merged.data[merged.data$registration_timestamp == "0000-00-00 00:00:00","date"]
  
  # Add a fake date 
  merged.data[merged.data$registration_timestamp == "0000-00-00 00:00:00","registration_timestamp"] <- "2014-10-10"
  merged.data[merged.data$date == "0000-00-00 00:00:00","date"] <- "2014-10-10"
  
  
  merged.data$year = substr(merged.data$date,1,4)
  merged.data$month = substr(merged.data$date,6,7)
  merged.data$day = substr(merged.data$date,9,10)
  
  # write.csv(merged.data,"~/Desktop/merged.data.csv")
  # dput(levels(merged.data$title))
  merged.data$new.title = as.character(merged.data$title)
  
  at1 = c(
    "Atelier 1 - Intro à R", 
    "Workshop 1 - Intro to R", 
    "Introduction à R ", 
    "Introduction to R") 
    
  at2 = c("Atelier 2 - Charger et explorer des données", 
    "Workshop 2 - Loading and exploring data", 
    "Charger et manipuler des données", 
    " Loading and manipulating data")  

  at3 =   c("Atelier 4 - ggplot et plyr", 
    "Workshop 4 - ggplot and plyr", 
    "Introduction à ggplot2, reshape & plyr ", 
    "Introduction to ggplot2, reshape & plyr")  
    
  at4 =   c("Atelier 3 - Modèles linéaires", 
    "Workshop 3 - Linear models", 
    "Modèles linéaires", 
    "Linear models") 
    
  at5 = c("Atelier 8 - Programmation avec R", 
    "Workshop 8 - Programming in R",
    "Programmation avec R ",
    "Programming in R") 
    
  at6= c("Atelier 5 - Modèles linéaires mixtes", 
    "Workshop 5 - Linear mixed models",
    "Modèles linéaires mixtes ", 
    "Linear mixed models (LMMs)") 
    
  at7=c("Workshop 6 - Generalized linear (mixed) models", 
    "Atelier 6 - Modèles linéaires généralisés (mixtes)", 
    "Modèles linéaires généralisés mixtes", 
    "Generalized linear mixed models (GLMMs)") 
    
  at8=c("Modèles additifs généralisés ", 
    "Generalized additive models (GAMs)")  
    
  at9=c("Atelier 7 - Méthodes d'ordination", 
    "Workshop 7 - Ordination", 
    "Méthodes d'ordination", 
    "Ordination")  
    
  at10=c("Advanced multivariate analyses", 
    "Analyses multivariées avancées")
  merged.data[  merged.data$title%in% at1,"new.title"] <- "intro_R"
  merged.data[  merged.data$title%in% at2,"new.title"] <- "charge_R"
  merged.data[  merged.data$title%in% at3,"new.title"] <- "plot_R"
  merged.data[  merged.data$title%in% at4,"new.title"] <- "lm_R"
  merged.data[  merged.data$title%in% at5,"new.title"] <- "prog_R"
  merged.data[  merged.data$title%in% at6,"new.title"] <- "lmm_R"
  merged.data[  merged.data$title%in% at7,"new.title"] <- "glmm_R"
  merged.data[  merged.data$title%in% at8,"new.title"] <- "gams_R"
  merged.data[  merged.data$title%in% at9,"new.title"] <- "ordi_R"
  merged.data[  merged.data$title%in% at10,"new.title"] <- "advmult_R"
  merged.data$new.title = as.factor(merged.data$new.title)
  merged.data$new.title = factor(merged.data$new.title,levels(merged.data$new.title)[c(5,2,9,6,10,7,4,3,8,1)])
  
  merged.data$new.location = as.character(merged.data$location)
  
  
  merged.data[  merged.data$location%in% c("McGill University/Concordia"),"new.location"] <- "McGill/Concordia"
  merged.data[  merged.data$location%in% c("Université de Montréal/UQAM",
                                           "Université du Québec à Montréal"),"new.location"] <- "UdeM/UQAM"
  merged.data[  merged.data$location%in% c("Université de Sherbrooke"),"new.location"] <- "Sherbrooke"
  merged.data[  merged.data$location%in% c("Université du Québec à Rimouski"),"new.location"] <- "UQAR"
  merged.data[  merged.data$location%in% c("Université Laval"),"new.location"] <- "ULaval"
  merged.data$new.location = as.factor(merged.data$new.location)
  
  merged.data[  merged.data$language%in% c("français"),"language"] <- "Français"
  merged.data$language <-  droplevels(merged.data$language)
  write.csv(merged.data[,c("registration_timestamp", 
                           "r_knowledge", 
                           "member", 
                           "location", "date", 
                           "workshop_no", 
                           "language", 
                           "year", "month", "day", "new.title",
                           "name.corr", 
                           "title", 
                           "time", 
                           "workshop", "registrant", "registration_id", "status", 
                           "first_name", "last_name", "email", "student_id", "supervisor", 
                           "affiliation", 
                           "comments", 
                           "order_id", 
                           "visible")
                        ],"~/Desktop/merged.data2.csv")
  
  
  
  

# Preparing clean data ----------------------------------------------------
  clean.data = merged.data[,c("registration_timestamp", 
                              "r_knowledge", "name.corr","member", 
                              "new.location", "date", "new.title", 
                              "workshop_no", "language",  "status", 
                              "year", "month", "day")]
  # Remove weird names 
  clean.data =  clean.data %>% filter(!(name.corr %in%c("_",
                                          "nffwjdhnde_nffwjdhnde",
                                          "pcrvvcivjr_pcrvvcivjr",
                                          "pdxaivqkyb_pdxaivqkyb",
                                          "spcmubxagc_spcmubxagc"))) 
  
  # save(clean.data,
  #      file = "~/Desktop/QCBS_rworshops_data/stats.data.workshops.QCBS.RData")
  save(clean.data,
       file = "~/École/École - En cours!/McGill University/McGill/QCBS — McGill/R workshops/QCBS_rworshops_data/stats.data.workshops.QCBS.RData")

