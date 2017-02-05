# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Input -------------------------------------------------------------

nic_acid <- read_excel("~/Documents/GitHub/nicotinic_acid/data/Nicotinic Acid Summary.xlsx", 
             skip = 2)

# Data Cleaning ----------------------------------------------------------

nic_acid <- nic_acid[,2:7]
colnames(nic_acid)[1] <- "Sample"

nic_uplc <- nic_acid[12:18, 3:6]
colnames(nic_uplc)[1] <- "Sample"
nic_uplc[, 2:4] <- sapply(nic_uplc[, 2:4], as.numeric)
nic_uplc$Method <- "UPLC"
nic_uplc <- nic_uplc[,c(1,5,2:4)]
nic_uplc <- tidyr::gather(data = nic_uplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-17-0004`, `VITB01-17-0005`, `VITB01-17-0006`)

nic_hplc <- nic_acid[1:6,]
nic_hplc[, 2:6] <- sapply(nic_hplc[, 2:6], as.numeric)
nic_hplc$Method <- "HPLC"
nic_hplc <- nic_hplc[,c(1,7,2:6)]
nic_hplc <- tidyr::gather(data = nic_hplc, key = Batch, value = Result, na.rm = FALSE, `VITB01-16-0184`, `VITB01-17-0001`, `VITB01-17-0004`, `VITB01-17-0005`, `VITB01-17-0006`)

nic_data <- rbind(nic_hplc, nic_uplc)

nic_data_wide <- spread(nic_data, Method, Result)

# Visualising Data -------------------------------------------------------

nic_plot <- ggplot(nic_data_wide, aes(x=HPLC, y=UPLC)) +
        geom_point() +
        geom_abline(slope=1, intercept = 0, lty=2, col = "red")
nic_plot
