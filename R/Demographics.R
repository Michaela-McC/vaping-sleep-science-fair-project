library(googlesheets4)
library(readxl)
library(dplyr)
setwd("/Users/motto/Servicios/QL/Mentees/MichaelaMcCormack")

# https://googlesheets4.tidyverse.org/articles/articles/auth.html
gs4_auth(email="mark.ot2o@gmail.com",)

Demog <- read_sheets(ss="MichaelaMcCormack/Demographics Survey",
                    col_types = c("date",""
)

Demog <- read_excel("Ori/DemographicsSurvey.xlsx",
                    col_types = c("date",
                                  "text",
                                  "text",
                                  "text",
                                  "text",
                                  "text",
                                  "text",
                                  "numeric",
                                  rep("skip",39),
                                  rep("text",14))
                    ) %>%
 filter(!is.na(Timestamp))
