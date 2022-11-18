#######
## GET ICD DIAGNOSIS DATE

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)

df = read_tsv("/data/Wolfson-UKBB-Dobson/UKB_pheno_30_06_21/ukb_pheno_30062021.tsv")

df2 = df %>% filter_at(vars(contains("Diagnoses")), any_vars(.=="G20"))

dateofdiagnosis = function(ROWS, TOW) {
  
  row0 = df2 %>% select(EID, ROWS, TOW)
  row0 = row0 %>% filter_at(vars(contains("Diagnoses - ICD10")),any_vars(.=="G20"))
  names(row0)[1] <- "EID"
  names(row0)[2] <- "ICD"
  names(row0)[3] <- "Date of ICD PD diagnosis"
  row0 = data.frame(row0)
  
  return(list(row0))
  
}

table = c(dateofdiagnosis("Diagnoses - ICD10.0.0", "Date of first in-patient diagnosis - ICD10.0.0"), dateofdiagnosis("Diagnoses - ICD10.0.1", "Date of first in-patient diagnosis - ICD10.0.1")
          , dateofdiagnosis("Diagnoses - ICD10.0.2", "Date of first in-patient diagnosis - ICD10.0.2"), dateofdiagnosis("Diagnoses - ICD10.0.3", "Date of first in-patient diagnosis - ICD10.0.3")
          , dateofdiagnosis("Diagnoses - ICD10.0.4", "Date of first in-patient diagnosis - ICD10.0.4"), dateofdiagnosis("Diagnoses - ICD10.0.5", "Date of first in-patient diagnosis - ICD10.0.5")
          , dateofdiagnosis("Diagnoses - ICD10.0.6", "Date of first in-patient diagnosis - ICD10.0.6"), dateofdiagnosis("Diagnoses - ICD10.0.7", "Date of first in-patient diagnosis - ICD10.0.7")
          , dateofdiagnosis("Diagnoses - ICD10.0.8", "Date of first in-patient diagnosis - ICD10.0.8"), dateofdiagnosis("Diagnoses - ICD10.0.9", "Date of first in-patient diagnosis - ICD10.0.9")
          , dateofdiagnosis("Diagnoses - ICD10.0.10", "Date of first in-patient diagnosis - ICD10.0.10"), dateofdiagnosis("Diagnoses - ICD10.0.11", "Date of first in-patient diagnosis - ICD10.0.11")
          , dateofdiagnosis("Diagnoses - ICD10.0.12", "Date of first in-patient diagnosis - ICD10.0.12"), dateofdiagnosis("Diagnoses - ICD10.0.13", "Date of first in-patient diagnosis - ICD10.0.13")
          , dateofdiagnosis("Diagnoses - ICD10.0.14", "Date of first in-patient diagnosis - ICD10.0.14"), dateofdiagnosis("Diagnoses - ICD10.0.15", "Date of first in-patient diagnosis - ICD10.0.15")
          , dateofdiagnosis("Diagnoses - ICD10.0.16", "Date of first in-patient diagnosis - ICD10.0.16"), dateofdiagnosis("Diagnoses - ICD10.0.17", "Date of first in-patient diagnosis - ICD10.0.17")
          , dateofdiagnosis("Diagnoses - ICD10.0.18", "Date of first in-patient diagnosis - ICD10.0.18"), dateofdiagnosis("Diagnoses - ICD10.0.19", "Date of first in-patient diagnosis - ICD10.0.19")
          , dateofdiagnosis("Diagnoses - ICD10.0.20", "Date of first in-patient diagnosis - ICD10.0.20"), dateofdiagnosis("Diagnoses - ICD10.0.21", "Date of first in-patient diagnosis - ICD10.0.21")
          , dateofdiagnosis("Diagnoses - ICD10.0.22", "Date of first in-patient diagnosis - ICD10.0.22"), dateofdiagnosis("Diagnoses - ICD10.0.23", "Date of first in-patient diagnosis - ICD10.0.23")
          , dateofdiagnosis("Diagnoses - ICD10.0.24", "Date of first in-patient diagnosis - ICD10.0.24"), dateofdiagnosis("Diagnoses - ICD10.0.25", "Date of first in-patient diagnosis - ICD10.0.25")
          , dateofdiagnosis("Diagnoses - ICD10.0.26", "Date of first in-patient diagnosis - ICD10.0.26"), dateofdiagnosis("Diagnoses - ICD10.0.27", "Date of first in-patient diagnosis - ICD10.0.27")
          , dateofdiagnosis("Diagnoses - ICD10.0.28", "Date of first in-patient diagnosis - ICD10.0.28"), dateofdiagnosis("Diagnoses - ICD10.0.29", "Date of first in-patient diagnosis - ICD10.0.29")
          , dateofdiagnosis("Diagnoses - ICD10.0.30", "Date of first in-patient diagnosis - ICD10.0.30"), dateofdiagnosis("Diagnoses - ICD10.0.31", "Date of first in-patient diagnosis - ICD10.0.31")
          , dateofdiagnosis("Diagnoses - ICD10.0.32", "Date of first in-patient diagnosis - ICD10.0.32"), dateofdiagnosis("Diagnoses - ICD10.0.33", "Date of first in-patient diagnosis - ICD10.0.33")
          , dateofdiagnosis("Diagnoses - ICD10.0.34", "Date of first in-patient diagnosis - ICD10.0.34"), dateofdiagnosis("Diagnoses - ICD10.0.35", "Date of first in-patient diagnosis - ICD10.0.35")
          , dateofdiagnosis("Diagnoses - ICD10.0.36", "Date of first in-patient diagnosis - ICD10.0.36"), dateofdiagnosis("Diagnoses - ICD10.0.37", "Date of first in-patient diagnosis - ICD10.0.37")
          , dateofdiagnosis("Diagnoses - ICD10.0.38", "Date of first in-patient diagnosis - ICD10.0.38"), dateofdiagnosis("Diagnoses - ICD10.0.39", "Date of first in-patient diagnosis - ICD10.0.39"))

df3 = bind_rows(table, .id = "column_label")

write.csv(df3, "/data/Wolfson-UKBB-Dobson/drugs/ICDdiagnosesdates.csv")


#######
## GET SR AND DATES

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx00")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x00.csv")


selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx01")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x01.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx02")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x02.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx03")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x03.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx04")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x04.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx05")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x05.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx06")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x06.csv")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx07")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x07.csv")


selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx08")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x08.csv")


selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/chunkx09")
df=selected_vars %>% select(EID,contains("Non-cancer illness code, self-reported."), contains("Interpolated Year when non-cancer illness first diagnosed."))
df=data.frame(df)
df = df %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")), any_vars(.=="1262"))

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2x09.csv")

df0 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x00.csv")
df1 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x01.csv")
df2 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x02.csv")
df3 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x03.csv")
df4 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x04.csv")
df5 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x05.csv")
df6 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x06.csv")
df7 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x07.csv")
df8 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x08.csv")
df9 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2x09.csv")

df = rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9)

write.csv(df,"/data/Wolfson-UKBB-Dobson/drugs/selfreported2cases.csv")

df = read.csv("/data/Wolfson-UKBB-Dobson/drugs/selfreported2cases.csv")

dateofdiagnosis = function(ROWS, TOW) {
  
  row0 = df %>% select(EID, ROWS, TOW)
  row0 = row0 %>% filter_at(vars(contains("Non.cancer.illness.code..self.reported.")),any_vars(.=="1262"))
  names(row0)[1] <- "EID"
  names(row0)[2] <- "SR"
  names(row0)[3] <- "Date of SR PD diagnosis"
  row0 = data.frame(row0)
  
  return(list(row0))
  
}

table = c(dateofdiagnosis("Non.cancer.illness.code..self.reported.0.0", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.0"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.1", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.1"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.2", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.2"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.3", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.3"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.4", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.4"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.5", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.5"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.6", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.6"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.7", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.7"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.8", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.8"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.9", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.9"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.10", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.10"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.11", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.11"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.12", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.12"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.13", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.13"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.14", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.14"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.15", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.15"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.16", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.16"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.17", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.17"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.18", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.18"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.19", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.19"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.20", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.20"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.21", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.21"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.21", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.22"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.23", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.23"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.24", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.24"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.25", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.25"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.0.26", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.26"),dateofdiagnosis("Non.cancer.illness.code..self.reported.0.27", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.0.27"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.0", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.0"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.1", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.1"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.2", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.2"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.3", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.3"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.4", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.4"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.5", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.5"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.6", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.6"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.7", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.7"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.8", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.8"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.9", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.9"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.10", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.10"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.11", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.11"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.12", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.12"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.13", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.13"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.14", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.14"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.15", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.15"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.1.14", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.14"),dateofdiagnosis("Non.cancer.illness.code..self.reported.1.15", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.1.15"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.0", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.0"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.1", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.1"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.2", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.2"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.3", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.3"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.4", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.4"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.5", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.5"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.6", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.6"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.7", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.7"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.8", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.8"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.9", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.9"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.10", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.10"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.11", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.11"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.12", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.12"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.13", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.13"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.2.14", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.14"),dateofdiagnosis("Non.cancer.illness.code..self.reported.2.15", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.2.15"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.0", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.0"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.1", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.1"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.2", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.2"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.3", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.3"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.4", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.4"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.5", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.5"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.6", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.6"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.7", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.7"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.8", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.8"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.9", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.9"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.10", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.10"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.11", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.11"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.12", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.12"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.13", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.13"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.14", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.14"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.15", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.15"),
          dateofdiagnosis("Non.cancer.illness.code..self.reported.3.16", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.16"),dateofdiagnosis("Non.cancer.illness.code..self.reported.3.17", "Interpolated.Year.when.non.cancer.illness.first.diagnosed.3.17"))


df3 = bind_rows(table, .id = "column_label")
str(df3)

#There are duplicated SR for some PD patients on different visits - some have diff dates (below line takes SR from first ever report)

df3 = df3[!duplicated(df3$EID),]

write.csv(df3, "/data/Wolfson-UKBB-Dobson/drugs/SRdiagnosesdates.csv")

#######
## Merge SR and ICD datasets with GP prescription database

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/ICDdiagnosesdates.csv")
df2 = read_csv("/data/Wolfson-UKBB-Dobson/drugs/SRdiagnosesdates.csv")


gp_df = read_tsv("/data/Wolfson-UKBB-Dobson/UKBB_GP_DATA/gp_scripts.txt")

df = data.frame(df)
df2 = data.frame(df2)

df = full_join(df,df2,by=c("EID"="EID"))

df$ICD[is.na(df$ICD)] = 0
df$SR[is.na(df$SR)] = 0

df$ICD = factor(df$ICD)
df$SR = factor(df$SR)

gp_df = data.frame(gp_df)

df$EID = as.character(df$EID)
gp_df$eid = as.character(gp_df$eid)

df = df %>% filter(EID %in% gp_df$eid)

merged_df = left_join(gp_df,df,by=c("eid"="EID"))

write.csv(merged_df,"/data/Wolfson-UKBB-Dobson/drugs/datesanddrugs.csv")

#######
##Get dataset into correct format

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)

gp_df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/datesanddrugs.csv", col_types = "dccccccccddfDddfc")
gp_df = gp_df[!duplicated(gp_df$eid),]

gp_df$Date.of.SR.PD.diagnosis = substr(gp_df$Date.of.SR.PD.diagnosis, 0, 4)
gp_df$Date.of.SR.PD.diagnosis = ymd(gp_df$Date.of.SR.PD.diagnosis, truncated = 2L)

sr = gp_df %>% filter_at(vars(contains("ICD")), any_vars(str_detect(., pattern = "G20"))) %>% mutate("PD_status_fin" = 1)
sr2 = gp_df %>% filter(!eid %in% sr$eid) %>% filter_at(vars(contains("SR")), any_vars(.==1262)) %>% mutate("PD_status_fin" = 1)
sr = rbind(sr,sr2)

gp_df$eid = factor(gp_df$eid)
sr$eid = factor(sr$eid)
sr_ids = sr$eid

sr = filter(gp_df, eid %in% sr_ids)%>% mutate("PD_status_fin"=1)
no_sr= gp_df %>% filter(!(eid %in% sr$eid))
controls = filter(gp_df, eid %in% no_sr$eid) %>% mutate("PD_status_fin"=0)

gp_df = rbind(sr,controls)

sr2 = gp_df %>% filter_at(vars(contains("ICD")), any_vars(str_detect(., pattern = "G20"))) %>% mutate("PD_status_both" = 1)
sr = gp_df %>% filter(eid %in% sr2$eid) %>% filter_at(vars(contains("SR")), any_vars(.==1262)) %>% mutate("PD_status_both" = 1)

gp_df$eid = factor(gp_df$eid)
sr$eid = factor(sr$eid)
sr_ids = sr$eid

sr = filter(gp_df, eid %in% sr_ids)%>% mutate("PD_status_both"=1)
no_sr= gp_df %>% filter(!(eid %in% sr$eid))
controls = filter(gp_df, eid %in% no_sr$eid) %>% mutate("PD_status_both"=0)

gp_df = rbind(sr,controls)

ass_date = read_csv("/data/Wolfson-UKBB-Dobson/drugs/selfreportedcontrols.csv")
ass_date = ass_date %>% select(EID, "Date of attending assessment centre.0.0")
ass_date$EID = as.character(ass_date$EID)

gp_df = left_join(gp_df,ass_date,by=c("eid"="EID"))

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/ukb_pheno_final_PD_1012")

selected_vars = selected_vars %>% select(EID,Sex.0.0,`Year of birth.0.0`,`Month of birth.0.0`,`Ethnic background.0.0`,`Age at recruitment.0.0`,`Townsend deprivation index at recruitment.0.0`)

selected_vars$Sex.0.0 = factor(selected_vars$Sex.0.0)
selected_vars$`Year of birth.0.0` = as.numeric(selected_vars$`Year of birth.0.0`)
selected_vars$`Month of birth.0.0` = factor(selected_vars$`Month of birth.0.0`)

selected_vars$`Ethnic background.0.0`=factor(recode(selected_vars$`Ethnic background.0.0`,African="Non-white",`Any other Asian background`="Non-white",`Other ethnic group`="Non-white",`White and Black Caribbean`="Non-white",`White and Asian`="Non-white",`Any other Black background`="Non-white",`White and Black African`="Non-white",`Any other mixed background`="Non-white",`Bangladeshi`="Non-white",`British Caribbean`="Non-white",`Pakistani`="Non-white",`Indian`="Non-white",`Caribbean`="Non-white",`Chinese`="Non-white",`British`="White",`Irish`="White",`Any other white background`="White",`Do not know`="NA",`Prefer not to answer`="NA"),levels=c("White","Non-white"))

selected_vars$EID = factor(selected_vars$EID)
gp_df$eid = factor(gp_df$eid)

gp_df = left_join(gp_df,selected_vars,by=c("eid"="EID"))

gp_df = gp_df %>% select(eid,ICD,Date.of.ICD.PD.diagnosis,SR,Date.of.SR.PD.diagnosis,PD_status_both,PD_status_fin,`Date of attending assessment centre.0.0`,`Month of birth.0.0`,
                         `Year of birth.0.0`,`Townsend deprivation index at recruitment.0.0`,`Ethnic background.0.0`,Sex.0.0,`Age at recruitment.0.0`)

write.csv(gp_df,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup.csv")

#make case control group (ICD)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

gp_df = read.csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup.csv")

gp_df$SR[is.na(gp_df$SR)] = 0
gp_df$ICD[is.na(gp_df$ICD)] = 0
gp_df$SR=factor(recode(gp_df$SR,`0`="0",`1262`="1"))
gp_df$ICD=factor(recode(gp_df$ICD,`0`="0",`G20`="1"))

#remove individuals with only an SR diangosis so they don't get picked as controls
onlysr = gp_df %>% filter_at(vars(contains("SR")), any_vars(.== 1)) %>% filter_at(vars(contains("PD_status_both")), any_vars(.== 0))
onlysr$eid = factor(onlysr$eid)
gp_df = gp_df %>% filter(!(eid %in% onlysr$eid))

gp_df = data.frame(gp_df)
exit = c("1")
gp_df = cbind(gp_df,exit)

gp_df$PD_status_fin = as.numeric(gp_df$PD_status_fin)
gp_df$exit = as.numeric(gp_df$exit)
gp_df = gp_df %>% drop_na(Year.of.birth.0.0)
gp_df = gp_df %>% drop_na(Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% drop_na(Ethnic.background.0.0)
gp_df$Townsend.deprivation.index.at.recruitment.0.0 = as.numeric(gp_df$Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% mutate(Townsend = ntile(Townsend.deprivation.index.at.recruitment.0.0, 4))

gp_df$Year.of.attending = format(as.Date(gp_df$Date.of.attending.assessment.centre.0.0, format="%Y/%m/%d"),"%Y")
gp_df$Year.of.attending = as.numeric(gp_df$Year.of.attending)

set.seed(435922)

gp_df2 = ccwc(entry    = 0,
              exit=exit,
              fail = ICD,
              origin   = 0,
              controls = 6,
              data     = gp_df,
              include  = list(eid,ICD,Date.of.ICD.PD.diagnosis,SR,Date.of.SR.PD.diagnosis,Month.of.birth.0.0,Year.of.birth.0.0,Age.at.recruitment.0.0,Townsend.deprivation.index.at.recruitment.0.0,
                              Ethnic.background.0.0,Sex.0.0,PD_status_fin), 
              match    = list(Sex.0.0,Year.of.birth.0.0,Townsend,Ethnic.background.0.0),
              silent   = TRUE
)

write.csv(gp_df2,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup2.csv")

## input pseudodiagnosis dates (ICD)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup2.csv")

df2 = df %>% select(eid,Date.of.ICD.PD.diagnosis)
df2 = na.omit(df2)
df3 = df2[rep(seq_len(nrow(df2)), each=6),]

df4 = df %>% select(eid,Date.of.ICD.PD.diagnosis)
df4 = df4 %>% filter(is.na(df4$Date.of.ICD.PD.diagnosis))
df4$Date.of.ICD.PD.diagnosis = df3$Date.of.ICD.PD.diagnosis

df5 = rbind(df2,df4)
df5 = left_join(df,df5, by=c("eid"="eid"))
names(df5)[22]= "ICD_pseudo_dx_date"

write.csv(df5,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup3.csv")

## select cases and controls from GP_script and search for AEDs (ICDs)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup3.csv")
df$eid = as.character(df$eid)

gp_df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/datesanddrugs.csv", col_types = "dccccccccddfDddfc")
gp_df$issue_date = as.Date(gp_df$issue_date, "%d/%m/%Y")

gp_df= gp_df %>% filter(eid %in% df$eid)
str(gp_df)

gp_df = left_join(gp_df,df,by=c("eid"="eid"))
#gp_df$ICD_pseudo_dx_date = ymd(gp_df$ICD_pseudo_dx_date) - years(2)

gp_df3 = gp_df %>% filter(ICD_pseudo_dx_date  > issue_date)

gp_df3 = data.frame(gp_df3)
str(gp_df3)

SV_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium Valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "SODIUM VALPROATE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name6 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "EPILIM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_read = gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnb"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

CBZ_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "CARBAMAZEPINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "TEGRETOL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dn3"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

KEP_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LEVETIRACETAM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "KEPPRA"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dno"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)

LAM_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMOTRIGINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMICTAL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnf"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)

all_medications = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read,CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,
                        CBZ_read,KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read,LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,
                        LAM_read)
medications_1 = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read)
medications_2 = rbind(CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,CBZ_read)
medications_3 = rbind(KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read)
medications_4 = rbind(LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,LAM_read)

gp_df3$eid = factor(gp_df3$eid)
all_medications$eid = factor(all_medications$eid)

issues_ICD = aggregate(AED_drug_status~eid,all_medications,sum)
names(issues_ICD)[2] = 'Issues_ICD'

Sodium_valproate_issues_ICD = aggregate(sodium_valproate_status~eid,all_medications,sum)
names(Sodium_valproate_issues_ICD)[2] = 'Sodium_valproate_issues_ICD'

Carbamazepine_issues_ICD = aggregate(carbamazepine_status~eid,all_medications,sum)
names(Carbamazepine_issues_ICD)[2] = 'Carbamazepine_issues_ICD'

Levetiracetam_issues_ICD = aggregate(levetiracetam_status~eid,all_medications,sum)
names(Levetiracetam_issues_ICD)[2] = 'Levetiracetam_issues_ICD'

Lamotrigine_issues_ICD = aggregate(lamotrigine_status~eid,all_medications,sum)
names(Lamotrigine_issues_ICD)[2] = 'Lamotrigine_issues_ICD'

medication_ids = all_medications$eid

all_medications = filter(gp_df3, eid %in% medication_ids)%>% mutate("AED_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% all_medications$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("AED_ICD_drug_status"=0)

sample_df2 = rbind(all_medications,controls)

sample_df2$eid = factor(sample_df2$eid)

sample_df2 = sample_df2[!duplicated(sample_df2$eid),]

medications_1$eid = factor(medications_1$eid)
medication_ids = medications_1$eid

medications_1 = filter(gp_df3, eid %in% medication_ids)%>% mutate("SV_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_1$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("SV_ICD_drug_status"=0)
medications_1 = rbind(medications_1,controls)

medications_1 = medications_1[!duplicated(medications_1$eid),]

medications_2$eid = factor(medications_2$eid)
medication_ids = medications_2$eid

medications_2 = filter(gp_df3, eid %in% medication_ids)%>% mutate("CB_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_2$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("CB_ICD_drug_status"=0)
medications_2 = rbind(medications_2,controls)

medications_2 = medications_2[!duplicated(medications_2$eid),]

medications_3$eid = factor(medications_3$eid)
medication_ids = medications_3$eid

medications_3 = filter(gp_df3, eid %in% medication_ids)%>% mutate("KEP_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_3$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("KEP_ICD_drug_status"=0)
medications_3 = rbind(medications_3,controls)

medications_3 = medications_3[!duplicated(medications_3$eid),]

medications_4$eid = factor(medications_4$eid)
medication_ids = medications_4$eid

medications_4 = filter(gp_df3, eid %in% medication_ids)%>% mutate("LAM_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_4$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("LAM_ICD_drug_status"=0)
medications_4 = rbind(medications_4,controls)

medications_4 = medications_4[!duplicated(medications_4$eid),]

medications_1 = data.frame(medications_1)
medications_2 = data.frame(medications_2)
medications_3 = data.frame(medications_3)
medications_4 = data.frame(medications_4)
issues_ICD = data.frame(issues_ICD)
Sodium_valproate_issues_ICD = data.frame(Sodium_valproate_issues_ICD)
Carbamazepine_issues_ICD = data.frame(Carbamazepine_issues_ICD)
Levetiracetam_issues_ICD = data.frame(Levetiracetam_issues_ICD)
Lamotrigine_issues_ICD = data.frame(Lamotrigine_issues_ICD)

medications_1 = medications_1 %>% select(eid,`SV_ICD_drug_status`)
medications_2 = medications_2 %>% select(eid,`CB_ICD_drug_status`)
medications_3 = medications_3 %>% select(eid,`KEP_ICD_drug_status`)
medications_4 = medications_4 %>% select(eid,`LAM_ICD_drug_status`)
issues_ICD = issues_ICD %>% select(eid,`Issues_ICD`)
Sodium_valproate_issues_ICD = Sodium_valproate_issues_ICD %>% select(eid,`Sodium_valproate_issues_ICD`)
Carbamazepine_issues_ICD = Carbamazepine_issues_ICD %>% select(eid,`Carbamazepine_issues_ICD`)
Levetiracetam_issues_ICD = Levetiracetam_issues_ICD %>% select(eid,`Levetiracetam_issues_ICD`)
Lamotrigine_issues_ICD = Lamotrigine_issues_ICD %>% select(eid,`Lamotrigine_issues_ICD`)

sample_df2 = left_join(sample_df2,medications_1, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_2, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_3, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_4, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Sodium_valproate_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Carbamazepine_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Levetiracetam_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Lamotrigine_issues_ICD, by=c("eid"="eid"))

sample_df2 = sample_df2 %>% select(eid,SV_ICD_drug_status,CB_ICD_drug_status,KEP_ICD_drug_status,LAM_ICD_drug_status,Issues_ICD,Sodium_valproate_issues_ICD,Carbamazepine_issues_ICD,
                                   Levetiracetam_issues_ICD,Lamotrigine_issues_ICD)

merged_df = left_join(df,sample_df2,by=c("eid"="eid"))

write.csv(merged_df,"/data/Wolfson-UKBB-Dobson/drugs/newICDcasecontrolset.csv")

##stats (ICD)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)
library(abd)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newICDcasecontrolset.csv")

df2 = df %>% filter_at(vars(contains("KEP_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df3 = df %>% filter_at(vars(contains("LAM_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df4 = df %>% filter_at(vars(contains("SV_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df5 = df %>% filter_at(vars(contains("CB_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)

df6 = rbind(df2,df3,df4,df5)

df$eid = factor(df$eid)
df6$eid = factor(df6$eid)
df6_ids = df6$eid

df6 = filter(df, eid %in% df6_ids)%>% mutate("AED_ICD_status"=1)
no_df6= df %>% filter(!(eid %in% df6$eid))
controls = filter(df, eid %in% no_df6$eid) %>% mutate("AED_ICD_status"=0)

df = rbind(df6,controls)

df$AED_ICD_status[is.na(df$AED_ICD_status)] = 0
df$KEP_ICD_drug_status[is.na(df$KEP_ICD_drug_status)] = 0
df$LAM_ICD_drug_status[is.na(df$LAM_ICD_drug_status)] = 0
df$SV_ICD_drug_status[is.na(df$SV_ICD_drug_status)] = 0
df$CB_ICD_drug_status[is.na(df$CB_ICD_drug_status)] = 0


ICD_AED=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$AED_ICD_status),
            family=binomial(link="logit"))
summary(ICD_AED)

ICD_KEP=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$KEP_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_KEP)

ICD_LAM=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$LAM_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_LAM)

ICD_SV=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
             factor(df$SV_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_SV)

ICD_CB=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
             factor(df$CB_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_CB)

df2 = data.frame(rbind(summary(ICD_AED)$coefficients,
                       summary(ICD_KEP)$coefficients,
                       summary(ICD_CB)$coefficients,
                       summary(ICD_LAM)$coefficients,
                       summary(ICD_SV)$coefficients))
df2$name=rownames(df2)
df2=df2[order(df2$name),]

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolICDOR.csv")

epil = read_csv("/data/Wolfson-UKBB-Dobson/drugs/epilepsy_dx_06_21.csv")

df$eid=factor(df$eid)
epil$EID=factor(epil$EID)
df2 = left_join(df,epil, by=c("eid"="EID"))


ICD_AED=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+`Epilepsy_status_06_21`+
              factor(df$AED_ICD_status),
            family=binomial(link="logit"))
summary(ICD_AED)

ICD_KEP=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+`Epilepsy_status_06_21`+
              factor(df$KEP_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_KEP)

ICD_LAM=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+`Epilepsy_status_06_21`+
              factor(df$LAM_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_LAM)

ICD_SV=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+`Epilepsy_status_06_21`+
             factor(df$SV_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_SV)

ICD_CB=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+`Epilepsy_status_06_21`+
             factor(df$CB_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_CB)

df2 = data.frame(rbind(summary(ICD_AED)$coefficients,
                       summary(ICD_KEP)$coefficients,
                       summary(ICD_CB)$coefficients,
                       summary(ICD_LAM)$coefficients,
                       summary(ICD_SV)$coefficients))
df2$name=rownames(df2)
df2=df2[order(df2$name),]

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolICDORepilepsy.csv")


##Number of different AEDs vs PD
df2= df
df2$AED_num = rowSums(df2[,25:28] )

df2$AED_num=recode(df2$AED_num,`0`="0",`1`="1",`2`="2",`3`="2",`4`="2")

AED_num_OR =glm(data=df2,
                Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
                  factor(df2$AED_num),
                family=binomial(link="logit"))

df2 = data.frame(summary(AED_num_OR)$coefficients)

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolAEDnumOR.csv")

##Number of AEDs issues vs PD

df3 = df %>% filter_at(vars(contains("Issues_ICD")), any_vars(. != 0)) 
df3 = df3 %>% mutate(Quartile = ntile(Issues_ICD, 4))

Quartile_OR =glm(data=df3,
                 Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
                   factor(df3$Quartile),
                 family=binomial(link="logit"))

df2 = data.frame(summary(Quartile_OR)$coefficients)

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolIssuesOR.csv")


#make case control group (SR)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

gp_df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup.csv")

gp_df$SR[is.na(gp_df$SR)] = 0
gp_df$ICD[is.na(gp_df$ICD)] = 0
gp_df$SR=factor(recode(gp_df$SR,`0`="0",`1262`="1"))
gp_df$ICD=factor(recode(gp_df$ICD,`0`="0",`G20`="1"))

table(gp_df$ICD,gp_df$SR)

#remove individuals with only an ICD diangosis so they don't get picked as controls
onlysr = gp_df %>% filter_at(vars(contains("ICD")), any_vars(.== 1)) %>% filter_at(vars(contains("PD_status_both")), any_vars(.== 0))
onlysr$eid = factor(onlysr$eid)
gp_df = gp_df %>% filter(!(eid %in% onlysr$eid))

table(gp_df$ICD,gp_df$SR)

gp_df = data.frame(gp_df)
exit = c("1")
gp_df = cbind(gp_df,exit)

gp_df$PD_status_fin = as.numeric(gp_df$PD_status_fin)
gp_df$exit = as.numeric(gp_df$exit)
gp_df = gp_df %>% drop_na(Year.of.birth.0.0)
gp_df = gp_df %>% drop_na(Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% drop_na(Ethnic.background.0.0)
gp_df$Townsend.deprivation.index.at.recruitment.0.0 = as.numeric(gp_df$Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% mutate(Townsend = ntile(Townsend.deprivation.index.at.recruitment.0.0, 4))
#gp_df = gp_df %>% mutate( Age = cut_width(Year.of.birth.0.0,width=2))

gp_df$Year.of.attending = format(as.Date(gp_df$Date.of.attending.assessment.centre.0.0, format="%Y/%m/%d"),"%Y")
gp_df$Year.of.attending = as.numeric(gp_df$Year.of.attending)

set.seed(435922)

gp_df2 = ccwc(entry    = 0,
              exit=exit,
              fail = SR,
              origin   = 0,
              controls = 6,
              data     = gp_df,
              include  = list(eid,ICD,Date.of.ICD.PD.diagnosis,SR,Date.of.SR.PD.diagnosis,Month.of.birth.0.0,Year.of.birth.0.0,Townsend.deprivation.index.at.recruitment.0.0,
                              Ethnic.background.0.0,Sex.0.0,PD_status_fin), 
              match    = list(Sex.0.0,Year.of.birth.0.0,Townsend,Ethnic.background.0.0),
              silent   = TRUE
)

write.csv(gp_df2,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup4.csv")


## input pseudodiagnosis dates (SR)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup4.csv")

df2 = df %>% select(eid,Date.of.SR.PD.diagnosis)
df2 = na.omit(df2)
df3 = df2[rep(seq_len(nrow(df2)), each=6),]

df4 = df %>% select(eid,Date.of.SR.PD.diagnosis)
df4 = df4 %>% filter(is.na(df4$Date.of.SR.PD.diagnosis))
df4$Date.of.SR.PD.diagnosis = df3$Date.of.SR.PD.diagnosis

df5 = rbind(df2,df4)
df5 = left_join(df,df5, by=c("eid"="eid"))
names(df5)[21]= "SR_pseudo_dx_date"

write.csv(df5,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup5.csv")

## select cases and controls from GP_script and search for AEDs (SR)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup5.csv")
df$eid = as.character(df$eid)

gp_df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/datesanddrugs.csv", col_types = "dccccccccddfDddfc")
gp_df$issue_date = as.Date(gp_df$issue_date, "%d/%m/%Y")

gp_df= gp_df %>% filter(eid %in% df$eid)
str(gp_df)

gp_df = left_join(gp_df,df,by=c("eid"="eid"))
#gp_df$SR_pseudo_dx_date = ymd(gp_df$SR_pseudo_dx_date) - years(2)

gp_df2 = gp_df %>% filter(SR_pseudo_dx_date  > issue_date)

gp_df2 = data.frame(gp_df2)

SV_name = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name1 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium Valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name2 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name3 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "SODIUM VALPROATE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name4 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name5 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name6 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "EPILIM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_read = gp_df2 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnb"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

CBZ_name = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name1 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name2 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "CARBAMAZEPINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name3 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name4 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name5 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "TEGRETOL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_read= gp_df2 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dn3"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

KEP_name = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name1 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name2 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LEVETIRACETAM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name3 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name4 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name5 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "KEPPRA"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_read= gp_df2 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dno"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)

LAM_name = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name1 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name2 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMOTRIGINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name3 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name4 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name5 = gp_df2 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMICTAL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_read= gp_df2 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnf"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)

all_medications = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read,CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,
                        CBZ_read,KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read,LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,
                        LAM_read)
medications_1 = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read)
medications_2 = rbind(CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,CBZ_read)
medications_3 = rbind(KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read)
medications_4 = rbind(LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,LAM_read)

gp_df2$eid = factor(gp_df2$eid)
all_medications$eid = factor(all_medications$eid)

issues_SR = aggregate(AED_drug_status~eid,all_medications,sum)
names(issues_SR)[2] = 'Issues_SR'

Sodium_valproate_issues_SR = aggregate(sodium_valproate_status~eid,all_medications,sum)
names(Sodium_valproate_issues_SR)[2] = 'Sodium_valproate_issues_SR'

Carbamazepine_issues_SR = aggregate(carbamazepine_status~eid,all_medications,sum)
names(Carbamazepine_issues_SR)[2] = 'Carbamazepine_issues_SR'

Levetiracetam_issues_SR = aggregate(levetiracetam_status~eid,all_medications,sum)
names(Levetiracetam_issues_SR)[2] = 'Levetiracetam_issues_SR'

Lamotrigine_issues_SR = aggregate(lamotrigine_status~eid,all_medications,sum)
names(Lamotrigine_issues_SR)[2] = 'Lamotrigine_issues_SR'

medication_ids = all_medications$eid

all_medications = filter(gp_df2, eid %in% medication_ids)%>% mutate("AED_SR_drug_status"=1)
no_medication= gp_df2 %>% filter(!(eid %in% all_medications$eid))
controls = filter(gp_df2, eid %in% no_medication$eid) %>% mutate("AED_SR_drug_status"=0)

sample_df = rbind(all_medications,controls)

sample_df$eid = factor(sample_df$eid)

sample_df = sample_df[!duplicated(sample_df$eid),]

medications_1$eid = factor(medications_1$eid)
medication_ids = medications_1$eid

medications_1 = filter(gp_df2, eid %in% medication_ids)%>% mutate("SV_SR_drug_status"=1)
no_medication= gp_df2 %>% filter(!(eid %in% medications_1$eid))
controls = filter(gp_df2, eid %in% no_medication$eid) %>% mutate("SV_SR_drug_status"=0)
medications_1 = rbind(medications_1,controls)

medications_1 = medications_1[!duplicated(medications_1$eid),]

medications_2$eid = factor(medications_2$eid)
medication_ids = medications_2$eid

medications_2 = filter(gp_df2, eid %in% medication_ids)%>% mutate("CB_SR_drug_status"=1)
no_medication= gp_df2 %>% filter(!(eid %in% medications_2$eid))
controls = filter(gp_df2, eid %in% no_medication$eid) %>% mutate("CB_SR_drug_status"=0)
medications_2 = rbind(medications_2,controls)

medications_2 = medications_2[!duplicated(medications_2$eid),]

medications_3$eid = factor(medications_3$eid)
medication_ids = medications_3$eid

medications_3 = filter(gp_df2, eid %in% medication_ids)%>% mutate("KEP_SR_drug_status"=1)
no_medication= gp_df2 %>% filter(!(eid %in% medications_3$eid))
controls = filter(gp_df2, eid %in% no_medication$eid) %>% mutate("KEP_SR_drug_status"=0)
medications_3 = rbind(medications_3,controls)

medications_3 = medications_3[!duplicated(medications_3$eid),]

medications_4$eid = factor(medications_4$eid)
medication_ids = medications_4$eid

medications_4 = filter(gp_df2, eid %in% medication_ids)%>% mutate("LAM_SR_drug_status"=1)
no_medication= gp_df2 %>% filter(!(eid %in% medications_4$eid))
controls = filter(gp_df2, eid %in% no_medication$eid) %>% mutate("LAM_SR_drug_status"=0)
medications_4 = rbind(medications_4,controls)

medications_4 = medications_4[!duplicated(medications_4$eid),]

medications_1 = data.frame(medications_1)
medications_2 = data.frame(medications_2)
medications_3 = data.frame(medications_3)
medications_4 = data.frame(medications_4)
issues_SR = data.frame(issues_SR)
Sodium_valproate_issues_SR = data.frame(Sodium_valproate_issues_SR)
Carbamazepine_issues_SR = data.frame(Carbamazepine_issues_SR)
Levetiracetam_issues_SR = data.frame(Levetiracetam_issues_SR)
Lamotrigine_issues_SR = data.frame(Lamotrigine_issues_SR)

medications_1 = medications_1 %>% select(eid,`SV_SR_drug_status`)
medications_2 = medications_2 %>% select(eid,`CB_SR_drug_status`)
medications_3 = medications_3 %>% select(eid,`KEP_SR_drug_status`)
medications_4 = medications_4 %>% select(eid,`LAM_SR_drug_status`)
issues_SR = issues_SR %>% select(eid,`Issues_SR`)
Sodium_valproate_issues_SR = Sodium_valproate_issues_SR %>% select(eid,`Sodium_valproate_issues_SR`)
Carbamazepine_issues_SR = Carbamazepine_issues_SR %>% select(eid,`Carbamazepine_issues_SR`)
Levetiracetam_issues_SR = Levetiracetam_issues_SR %>% select(eid,`Levetiracetam_issues_SR`)
Lamotrigine_issues_SR = Lamotrigine_issues_SR %>% select(eid,`Lamotrigine_issues_SR`)


head(issues_SR)
head(Sodium_valproate_issues_SR)
head(Carbamazepine_issues_SR)
head(sample_df)

sample_df = left_join(sample_df,medications_1, by=c("eid"="eid"))
sample_df = left_join(sample_df,medications_2, by=c("eid"="eid"))
sample_df = left_join(sample_df,medications_3, by=c("eid"="eid"))
sample_df = left_join(sample_df,medications_4, by=c("eid"="eid"))
sample_df = left_join(sample_df,issues_SR, by=c("eid"="eid"))
sample_df = left_join(sample_df,Sodium_valproate_issues_SR, by=c("eid"="eid"))
sample_df = left_join(sample_df,Carbamazepine_issues_SR, by=c("eid"="eid"))
sample_df = left_join(sample_df,Levetiracetam_issues_SR, by=c("eid"="eid"))
sample_df = left_join(sample_df,Lamotrigine_issues_SR, by=c("eid"="eid"))

sample_df = sample_df %>% select(eid,SV_SR_drug_status,CB_SR_drug_status,KEP_SR_drug_status,LAM_SR_drug_status,Issues_SR,Sodium_valproate_issues_SR,Carbamazepine_issues_SR,
                                 Levetiracetam_issues_SR,Lamotrigine_issues_SR)

merged_df = left_join(df,sample_df,by=c("eid"="eid"))

write.csv(merged_df,"/data/Wolfson-UKBB-Dobson/drugs/newSRcasecontrolset.csv")

##stats (SR)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)
library(abd)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newSRcasecontrolset.csv")

df2 = df %>% filter_at(vars(contains("KEP_SR_drug_status")), any_vars(.==1)) %>% mutate("AED_SR_status" = 1)
df3 = df %>% filter_at(vars(contains("LAM_SR_drug_status")), any_vars(.==1)) %>% mutate("AED_SR_status" = 1)
df4 = df %>% filter_at(vars(contains("SV_SR_drug_status")), any_vars(.==1)) %>% mutate("AED_SR_status" = 1)
df5 = df %>% filter_at(vars(contains("CB_SR_drug_status")), any_vars(.==1)) %>% mutate("AED_SR_status" = 1)

df6 = rbind(df2,df3,df4,df5)

df$eid = factor(df$eid)
df6$eid = factor(df6$eid)
df6_ids = df6$eid

df6 = filter(df, eid %in% df6_ids)%>% mutate("AED_SR_status"=1)
no_df6= df %>% filter(!(eid %in% df6$eid))
controls = filter(df, eid %in% no_df6$eid) %>% mutate("AED_SR_status"=0)

df = rbind(df6,controls)

df$AED_SR_status[is.na(df$AED_SR_status)] = 0
df$KEP_SR_drug_status[is.na(df$KEP_SR_drug_status)] = 0
df$LAM_SR_drug_status[is.na(df$LAM_SR_drug_status)] = 0
df$SV_SR_drug_status[is.na(df$SV_SR_drug_status)] = 0
df$CB_SR_drug_status[is.na(df$CB_SR_drug_status)] = 0


SR_AED=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
             factor(df$AED_SR_status),
           family=binomial(link="logit"))
summary(SR_AED)

df2 = data.frame(rbind(summary(SR_AED)$coefficients))

df2$name=rownames(df2)
df2=df2[order(df2$name),]

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolSROR.csv")

#make case control group (PD medication sensitivity analysis)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

gp_df = read.csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgroup.csv")

gp_df$SR[is.na(gp_df$SR)] = 0
gp_df$ICD[is.na(gp_df$ICD)] = 0
gp_df$SR=factor(recode(gp_df$SR,`0`="0",`1262`="1"))
gp_df$ICD=factor(recode(gp_df$ICD,`0`="0",`G20`="1"))

pdmeds = read_csv("/data/Wolfson-UKBB-Dobson/drugs/AEDPDldopasens.csv")
pdmeds = pdmeds %>% filter_at(vars("L_dopa_status"), any_vars(. !=0))
pdmeds = pdmeds %>% filter_at(vars("Issues_l_dopa"), any_vars(. >1))

pdmeds$eid = factor(pdmeds$eid)
gp_df$eid = factor(gp_df$eid)

gp_df = left_join(gp_df,pdmeds, by=c("eid"="eid"))
gp_df$L_dopa_status[is.na(gp_df$L_dopa_status)] = 0
gp_df$L_dopa_status = factor(gp_df$L_dopa_status)

PD_med_ICD = gp_df %>% filter_at(vars(contains("ICD")), any_vars(.== 1)) %>% filter_at(vars(contains("L_dopa_status")), any_vars(.== 1))
PD_med_ICD$eid = factor(PD_med_ICD$eid)
PD_med_ICD_ids = PD_med_ICD$eid

PD_med_ICD = filter(gp_df, eid %in% PD_med_ICD_ids)%>% mutate("PD_med_ICD"=1)
no_PD_med = gp_df %>% filter(!(eid %in% PD_med_ICD$eid))
controls = filter(gp_df, eid %in% no_PD_med$eid) %>% mutate("PD_med_ICD"=0)
PD_med_ICD = rbind(PD_med_ICD,controls)
PD_med_ICD = PD_med_ICD %>% select(eid,`PD_med_ICD`)
gp_df = left_join(gp_df,PD_med_ICD, by=c("eid"="eid"))

#remove individuals with HES but no PD meds so they don't get picked as controls
HESnoPDmeds = gp_df %>% filter_at(vars(contains("ICD")), any_vars(.== 1)) %>% filter_at(vars(contains("L_dopa_status")), any_vars(.== 0))
HESnoPDmeds$eid = factor(HESnoPDmeds$eid)
gp_df = gp_df %>% filter(!(eid %in% HESnoPDmeds$eid))

#remove individuals with only an SR diangosis so they don't get picked as controls
onlysr = gp_df %>% filter_at(vars(contains("SR")), any_vars(.== 1)) %>% filter_at(vars(contains("PD_status_both")), any_vars(.== 0))
onlysr$eid = factor(onlysr$eid)
gp_df = gp_df %>% filter(!(eid %in% onlysr$eid))

gp_df = data.frame(gp_df)
exit = c("1")
gp_df = cbind(gp_df,exit)

gp_df$PD_status_fin = as.numeric(gp_df$PD_status_fin)
gp_df$exit = as.numeric(gp_df$exit)
gp_df = gp_df %>% drop_na(Year.of.birth.0.0)
gp_df = gp_df %>% drop_na(Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% drop_na(Ethnic.background.0.0)
gp_df$Townsend.deprivation.index.at.recruitment.0.0 = as.numeric(gp_df$Townsend.deprivation.index.at.recruitment.0.0)
gp_df = gp_df %>% mutate(Townsend = ntile(Townsend.deprivation.index.at.recruitment.0.0, 4))

gp_df$Year.of.attending = format(as.Date(gp_df$Date.of.attending.assessment.centre.0.0, format="%Y/%m/%d"),"%Y")
gp_df$Year.of.attending = as.numeric(gp_df$Year.of.attending)

set.seed(435922)

gp_df2 = ccwc(entry    = 0,
              exit=exit,
              fail = PD_med_ICD,
              origin   = 0,
              controls = 6,
              data     = gp_df,
              include  = list(eid,ICD,Date.of.ICD.PD.diagnosis,SR,Date.of.SR.PD.diagnosis,Month.of.birth.0.0,Year.of.birth.0.0,Age.at.recruitment.0.0,Townsend.deprivation.index.at.recruitment.0.0,
                              Ethnic.background.0.0,Sex.0.0,PD_status_fin), 
              match    = list(Sex.0.0,Year.of.birth.0.0,Townsend,Ethnic.background.0.0),
              silent   = TRUE
)

write.csv(gp_df2,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgrouppdmed.csv")

## input pseudodiagnosis dates (ICD)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgrouppdmed.csv")

df2 = df %>% select(eid,Date.of.ICD.PD.diagnosis)
df2 = na.omit(df2)
df3 = df2[rep(seq_len(nrow(df2)), each=6),]

df4 = df %>% select(eid,Date.of.ICD.PD.diagnosis)
df4 = df4 %>% filter(is.na(df4$Date.of.ICD.PD.diagnosis))
df4$Date.of.ICD.PD.diagnosis = df3$Date.of.ICD.PD.diagnosis

df5 = rbind(df2,df4)
df5 = left_join(df,df5, by=c("eid"="eid"))
names(df5)[22]= "ICD_pseudo_dx_date"

write.csv(df5,"/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgrouppdmed2.csv")

## select cases and controls from GP_script and search for AEDs (ICDs)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolgrouppdmed2.csv")
df$eid = as.character(df$eid)

gp_df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/datesanddrugs.csv", col_types = "dccccccccddfDddfc")
gp_df$issue_date = as.Date(gp_df$issue_date, "%d/%m/%Y")

gp_df= gp_df %>% filter(eid %in% df$eid)
str(gp_df)

gp_df = left_join(gp_df,df,by=c("eid"="eid"))

gp_df3 = gp_df %>% filter(ICD_pseudo_dx_date  > issue_date)

gp_df3 = data.frame(gp_df3)
str(gp_df3)

SV_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium Valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Sodium valproate"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "SODIUM VALPROATE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Epilim"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_name6 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "EPILIM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
SV_read = gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnb"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 1) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

CBZ_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Carbamazepine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "CARBAMAZEPINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Tegretol"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "TEGRETOL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)
CBZ_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dn3"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 1) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 0)

KEP_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Levetiracetam"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LEVETIRACETAM"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Keppra"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "KEPPRA"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)
KEP_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dno"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 1) %>% mutate("lamotrigine_status" = 0)

LAM_name = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name1 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamotrigine"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name2 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMOTRIGINE"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name3 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name4 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "Lamictal"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_name5 = gp_df3 %>% filter_at(vars(contains("drug_name")), any_vars(str_detect(., pattern = "LAMICTAL"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)
LAM_read= gp_df3 %>% filter_at(vars(contains("read_2")), any_vars(str_detect(., pattern = "^dnf"))) %>% mutate("AED_drug_status" = 1) %>% mutate("sodium_valproate_status" = 0) %>% mutate("carbamazepine_status" = 0) %>% mutate("levetiracetam_status" = 0) %>% mutate("lamotrigine_status" = 1)

all_medications = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read,CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,
                        CBZ_read,KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read,LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,
                        LAM_read)
medications_1 = rbind(SV_name,SV_name1,SV_name2,SV_name3,SV_name4,SV_name5,SV_name6,SV_read)
medications_2 = rbind(CBZ_name,CBZ_name1,CBZ_name2,CBZ_name3,CBZ_name4,CBZ_name5,CBZ_read)
medications_3 = rbind(KEP_name,KEP_name1,KEP_name2,KEP_name3,KEP_name4,KEP_name5,KEP_read)
medications_4 = rbind(LAM_name,LAM_name1,LAM_name2,LAM_name3,LAM_name4,LAM_name5,LAM_read)

gp_df3$eid = factor(gp_df3$eid)
all_medications$eid = factor(all_medications$eid)

issues_ICD = aggregate(AED_drug_status~eid,all_medications,sum)
names(issues_ICD)[2] = 'Issues_ICD'

Sodium_valproate_issues_ICD = aggregate(sodium_valproate_status~eid,all_medications,sum)
names(Sodium_valproate_issues_ICD)[2] = 'Sodium_valproate_issues_ICD'

Carbamazepine_issues_ICD = aggregate(carbamazepine_status~eid,all_medications,sum)
names(Carbamazepine_issues_ICD)[2] = 'Carbamazepine_issues_ICD'

Levetiracetam_issues_ICD = aggregate(levetiracetam_status~eid,all_medications,sum)
names(Levetiracetam_issues_ICD)[2] = 'Levetiracetam_issues_ICD'

Lamotrigine_issues_ICD = aggregate(lamotrigine_status~eid,all_medications,sum)
names(Lamotrigine_issues_ICD)[2] = 'Lamotrigine_issues_ICD'

medication_ids = all_medications$eid

all_medications = filter(gp_df3, eid %in% medication_ids)%>% mutate("AED_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% all_medications$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("AED_ICD_drug_status"=0)

sample_df2 = rbind(all_medications,controls)

sample_df2$eid = factor(sample_df2$eid)

sample_df2 = sample_df2[!duplicated(sample_df2$eid),]

medications_1$eid = factor(medications_1$eid)
medication_ids = medications_1$eid

medications_1 = filter(gp_df3, eid %in% medication_ids)%>% mutate("SV_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_1$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("SV_ICD_drug_status"=0)
medications_1 = rbind(medications_1,controls)

medications_1 = medications_1[!duplicated(medications_1$eid),]

medications_2$eid = factor(medications_2$eid)
medication_ids = medications_2$eid

medications_2 = filter(gp_df3, eid %in% medication_ids)%>% mutate("CB_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_2$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("CB_ICD_drug_status"=0)
medications_2 = rbind(medications_2,controls)

medications_2 = medications_2[!duplicated(medications_2$eid),]

medications_3$eid = factor(medications_3$eid)
medication_ids = medications_3$eid

medications_3 = filter(gp_df3, eid %in% medication_ids)%>% mutate("KEP_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_3$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("KEP_ICD_drug_status"=0)
medications_3 = rbind(medications_3,controls)

medications_3 = medications_3[!duplicated(medications_3$eid),]

medications_4$eid = factor(medications_4$eid)
medication_ids = medications_4$eid

medications_4 = filter(gp_df3, eid %in% medication_ids)%>% mutate("LAM_ICD_drug_status"=1)
no_medication= gp_df3 %>% filter(!(eid %in% medications_4$eid))
controls = filter(gp_df3, eid %in% no_medication$eid) %>% mutate("LAM_ICD_drug_status"=0)
medications_4 = rbind(medications_4,controls)

medications_4 = medications_4[!duplicated(medications_4$eid),]

medications_1 = data.frame(medications_1)
medications_2 = data.frame(medications_2)
medications_3 = data.frame(medications_3)
medications_4 = data.frame(medications_4)
issues_ICD = data.frame(issues_ICD)
Sodium_valproate_issues_ICD = data.frame(Sodium_valproate_issues_ICD)
Carbamazepine_issues_ICD = data.frame(Carbamazepine_issues_ICD)
Levetiracetam_issues_ICD = data.frame(Levetiracetam_issues_ICD)
Lamotrigine_issues_ICD = data.frame(Lamotrigine_issues_ICD)

medications_1 = medications_1 %>% select(eid,`SV_ICD_drug_status`)
medications_2 = medications_2 %>% select(eid,`CB_ICD_drug_status`)
medications_3 = medications_3 %>% select(eid,`KEP_ICD_drug_status`)
medications_4 = medications_4 %>% select(eid,`LAM_ICD_drug_status`)
issues_ICD = issues_ICD %>% select(eid,`Issues_ICD`)
Sodium_valproate_issues_ICD = Sodium_valproate_issues_ICD %>% select(eid,`Sodium_valproate_issues_ICD`)
Carbamazepine_issues_ICD = Carbamazepine_issues_ICD %>% select(eid,`Carbamazepine_issues_ICD`)
Levetiracetam_issues_ICD = Levetiracetam_issues_ICD %>% select(eid,`Levetiracetam_issues_ICD`)
Lamotrigine_issues_ICD = Lamotrigine_issues_ICD %>% select(eid,`Lamotrigine_issues_ICD`)

sample_df2 = left_join(sample_df2,medications_1, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_2, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_3, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,medications_4, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Sodium_valproate_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Carbamazepine_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Levetiracetam_issues_ICD, by=c("eid"="eid"))
sample_df2 = left_join(sample_df2,Lamotrigine_issues_ICD, by=c("eid"="eid"))

sample_df2 = sample_df2 %>% select(eid,SV_ICD_drug_status,CB_ICD_drug_status,KEP_ICD_drug_status,LAM_ICD_drug_status,Issues_ICD,Sodium_valproate_issues_ICD,Carbamazepine_issues_ICD,
                                   Levetiracetam_issues_ICD,Lamotrigine_issues_ICD)

merged_df = left_join(df,sample_df2,by=c("eid"="eid"))

write.csv(merged_df,"/data/Wolfson-UKBB-Dobson/drugs/newICDcasecontrolsetpdmed.csv")

##PD meds stats

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Epi)
library(abd)

df = read_csv("/data/Wolfson-UKBB-Dobson/drugs/newICDcasecontrolsetpdmed.csv")

df2 = df %>% filter_at(vars(contains("KEP_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df3 = df %>% filter_at(vars(contains("LAM_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df4 = df %>% filter_at(vars(contains("SV_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)
df5 = df %>% filter_at(vars(contains("CB_ICD_drug_status")), any_vars(.==1)) %>% mutate("AED_ICD_status" = 1)

df6 = rbind(df2,df3,df4,df5)

df$eid = factor(df$eid)
df6$eid = factor(df6$eid)
df6_ids = df6$eid

df6 = filter(df, eid %in% df6_ids)%>% mutate("AED_ICD_status"=1)
no_df6= df %>% filter(!(eid %in% df6$eid))
controls = filter(df, eid %in% no_df6$eid) %>% mutate("AED_ICD_status"=0)

df = rbind(df6,controls)

df$AED_ICD_status[is.na(df$AED_ICD_status)] = 0
df$KEP_ICD_drug_status[is.na(df$KEP_ICD_drug_status)] = 0
df$LAM_ICD_drug_status[is.na(df$LAM_ICD_drug_status)] = 0
df$SV_ICD_drug_status[is.na(df$SV_ICD_drug_status)] = 0
df$CB_ICD_drug_status[is.na(df$CB_ICD_drug_status)] = 0


ICD_AED=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$AED_ICD_status),
            family=binomial(link="logit"))
summary(ICD_AED)

ICD_KEP=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$KEP_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_KEP)

ICD_LAM=glm(data=df,
            Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
              factor(df$LAM_ICD_drug_status),
            family=binomial(link="logit"))
summary(ICD_LAM)

ICD_SV=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
             factor(df$SV_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_SV)

ICD_CB=glm(data=df,
           Fail~`Sex.0.0`+`Year.of.birth.0.0`+`Townsend.deprivation.index.at.recruitment.0.0`+
             factor(df$CB_ICD_drug_status),
           family=binomial(link="logit"))
summary(ICD_CB)

df2 = data.frame(rbind(summary(ICD_AED)$coefficients,
                       summary(ICD_KEP)$coefficients,
                       summary(ICD_CB)$coefficients,
                       summary(ICD_LAM)$coefficients,
                       summary(ICD_SV)$coefficients))
df2$name=rownames(df2)
df2=df2[order(df2$name),]

write.csv(df2, "/data/Wolfson-UKBB-Dobson/drugs/newcasecontrolICDORpdmed.csv")


