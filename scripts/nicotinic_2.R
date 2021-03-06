# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Input -------------------------------------------------------------

#nic_acid <- read_excel("~/Documents/GitHub/nicotinic_acid/data/Nicotinic Acid Summary.xlsx")
nic_acid <- read_excel("H:/GitHub Projects/nicotinic_acid/data/Nicotinic Acid Summary.xlsx")

# Data Cleaning ----------------------------------------------------------

nic_hplc <- nic_acid[1:6,1:8]
nic_hplc[,4:8] <- sapply(nic_hplc[,4:8], as.numeric)

nic_hplc_long <- tidyr::gather(data = nic_hplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-16-0184`, `VITB01-17-0001`, `VITB01-17-0004`, `VITB01-17-0005`, `VITB01-17-0006`)

nic_uplc <- nic_acid[7:12,c(1:3,6:8)]

nic_uplc[,4:6] <- sapply(nic_uplc[,4:6], as.numeric)
nic_uplc_long <- tidyr::gather(data = nic_uplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-17-0004`, `VITB01-17-0005`, `VITB01-17-0006`)

nic_long <- rbind(nic_hplc_long,nic_uplc_long)
nic_long <- na.omit(nic_long)
#write_csv(nic_long,"~/Documents/GitHub/nicotinic_acid/data/long_data.csv" )

# ANOVA nic_hplc ---------------------------------------------------------

sample_no <- unique(nic_long$Sample)

nic_hplc_aov <- nic_long %>% 
        filter(Sample == sample_no[2] & Method == "HPLC")

boxplot(nic_hplc_aov$Result~nic_hplc_aov$Batch)

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

# ANOVA nic_uplc ---------------------------------------------------------

sample_no <- unique(nic_long$Sample)

nic_uplc_aov <- nic_long %>% 
        filter(Sample == sample_no[2] & Method == "UPLC")

boxplot(nic_uplc_aov$Result~nic_uplc_aov$Batch)

nic_uplc_anova <- aov(Result ~ Batch, data = nic_uplc_aov)
summary(nic_uplc_anova)

#Check for significant differences
TukeyHSD(nic_uplc_anova)

#Repeatability & Interim Precision
mean.sqr <- summary(nic_uplc_anova)[1][[1]][[3]]
ncount <- as.numeric(length(nic_uplc_anova$effects))/as.numeric(length(nic_uplc_anova$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

# Combined ANOVA & Boxplot -----------------------------------------------


nic_both_aov <- nic_long %>% 
        filter(Sample == sample_no[1])

nic_both_anova <- aov(Result ~ Batch*Method, data = nic_both_aov)
summary(nic_both_anova)

ggplot(nic_both_aov, aes(Batch, Result, fill = Method)) + 
        geom_point(size=4, shape=21, colour = "Black") + 
        labs(subtitle = "Sample Number: 9297682", title = "Nicotinic Acid Comparison Analysis") +
        theme_bw()

