# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Input -------------------------------------------------------------

nic_acid <- read_excel("~/Documents/GitHub/nicotinic_acid/data/Nicotinic Acid Summary.xlsx")

# Data Cleaning ----------------------------------------------------------

nic_hplc <- nic_acid[1:6,]
nic_hplc[,4:8] <- sapply(nic_hplc[,4:8], as.numeric)

nic_hplc_long <- tidyr::gather(data = nic_hplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-16-0184`, `VITB01-17-0001`, `VITB01-17-0004`, `VITB01-17-0005`, `VITB01-17-0006`)

nic_uplc <- nic_acid[12:18,1:6]
nic_uplc[,4:6] <- sapply(nic_uplc[,4:6], as.numeric)
nic_uplc_long <- tidyr::gather(data = nic_uplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-16-0184`, `VITB01-17-0001`, `VITB01-17-0004`)

nic_long <- rbind(nic_hplc_long,nic_uplc_long)
nic_long <- na.omit(nic_long)
write_csv(nic_long,"~/Documents/GitHub/nicotinic_acid/data/long_data.csv" )

# ANOVA ------------------------------------------------------------------

sample_no <- unique(nic_long$Sample)

nic_hplc_aov <- nic_long %>% 
        filter(Sample == sample_no[1] & Method == "HPLC")
nic_hplc_anova <- aov(Result ~ Batch, data = nic_hplc_aov)
summary(nic_hplc_anova)

#Check for significant differences
TukeyHSD(nic_hplc_anova)

#Repeatability & Interim Precision
mean.sqr <- summary(nic_hplc_anova)[1][[1]][[3]]
ncount <- as.numeric(length(nic_hplc_anova$effects))/as.numeric(length(nic_hplc_anova$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR