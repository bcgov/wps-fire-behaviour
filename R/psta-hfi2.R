library(tidyverse)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

setwd("C:/Users/lnoble/OneDrive - Government of BC/Programming/R/PSTA") #Set to your working folder in R

###Create output directory if one doesn't already exist###
output_dir <- "outputs"
if (!dir.exists(output_dir)){
dir.create(output_dir)
} else {
  print("Directory already exists")
}


data_hfi <- read.csv("data/Sample_hfi_90_FT_ExportTable_ExportTable_20250220.csv") %>%
  rename(sta_code = Stations_90Percentile_2000_2023) %>%
  mutate(pc_hfi_21_23E = (HFI_2023E - HFI_2021) / HFI_2021 * 100, #calc percent change b/n yrs of interest
         pc_hfi_21_23C = (HFI_2023C - HFI_2021) / HFI_2021 * 100,
         pc_hfi_21_24E = (HFI_2024E - HFI_2021) / HFI_2021 * 100, 
	   pc_hfi_21_24C = (HFI_2024C - HFI_2021) / HFI_2021 * 100,
	   pc_hfi_23_24E = (HFI_2024E - HFI_2023E) / HFI_2023E * 100,
	   pc_hfi_23_24C = (HFI_2024C - HFI_2023C) / HFI_2023E * 100) %>%
  arrange(sta_code)

##################################################
## Percent Change HFI
##################################################

####################
## HFI: 2023 vs 2021
####################

## Extended record 1979 - present
p1a <- 
ggplot(subset(data_hfi, pc_hfi_21_23E > 0)) +
  geom_point(aes(sta_code, pc_hfi_21_23E), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p1c <- 
ggplot(subset(data_hfi, pc_hfi_21_23E < 0)) +
  geom_point(aes(sta_code, pc_hfi_21_23E), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "Percent Change HFI")


## Constrained (20 yr)
p1b <-
ggplot(subset(data_hfi, pc_hfi_21_23C > 0)) +
  geom_point(aes(sta_code, pc_hfi_21_23C), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p1d <- 
ggplot(subset(data_hfi, pc_hfi_21_23C < 0)) +
  geom_point(aes(sta_code, pc_hfi_21_23C), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "Percent Change HFI")

####################
## HFI: 2024 vs 2021
####################

## Extended record 1979 - present
p2a <- 
ggplot(subset(data_hfi, pc_hfi_21_24E > 0)) +
  geom_point(aes(sta_code, pc_hfi_21_24E), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p2c <- 
ggplot(subset(data_hfi, pc_hfi_21_24E < 0)) +
  geom_point(aes(sta_code, pc_hfi_21_24E), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "Percent Change HFI")


## Constrained (20 yr)
p2b <-
ggplot(subset(data_hfi, pc_hfi_21_24C > 0)) +
  geom_point(aes(sta_code, pc_hfi_21_24C), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p2d <- 
ggplot(subset(data_hfi, pc_hfi_21_24C < 0)) +
  geom_point(aes(sta_code, pc_hfi_21_24C), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "Percent Change HFI")

####################
## HFI: 2024 vs 2023
####################

## Extended record 1979 - present
p3a <- 
ggplot(subset(data_hfi, pc_hfi_23_24E > 0)) +
  geom_point(aes(sta_code, pc_hfi_23_24E), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p3c <- 
ggplot(subset(data_hfi, pc_hfi_23_24E < 0)) +
  geom_point(aes(sta_code, pc_hfi_23_24E), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Extended",
       x = "Station Code",
       y = "Percent Change HFI")


## Constrained (20 yr)
p3b <-
ggplot(subset(data_hfi, pc_hfi_23_24C > 0)) +
  geom_point(aes(sta_code, pc_hfi_23_24C), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 12000),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 10000),
                     labels = comma) +
  geom_hline(yintercept = 10) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12.5, label = "1.1x") +
  geom_hline(yintercept = 50) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 62.5, label = "1.5x") +
  geom_hline(yintercept = 100) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 125, label = "2x") +
  geom_hline(yintercept = 300) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 360, label = "4x") +
  geom_hline(yintercept = 900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 1100, label = "10x") +
  geom_hline(yintercept = 3900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 4600, label = "40x") +
  geom_hline(yintercept = 9900) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = 12000, label = "100x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p3d <- 
ggplot(subset(data_hfi, pc_hfi_23_24C < 0)) +
  geom_point(aes(sta_code, pc_hfi_23_24C), col = "blue") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  geom_hline(yintercept = -25) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -23, label = "3/4x") +
  geom_hline(yintercept = -50) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -48, label = "1/2x") +
  geom_hline(yintercept = -75) +
    annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -73, label = "1/4x") +
  geom_hline(yintercept = -90) +
   annotate("text", x = ceiling(max(data_hfi$sta_code)/200)*200, y = -88, label = "1/10x") +
  labs(title = "Constrained",
       x = "Station Code",
       y = "Percent Change HFI")

####################
## Save plots
####################

ggsave(grid.arrange(p1a, p1b, p1c, p1d, nrow = 2,
	top = textGrob("PSTA: HFI Comparison 2023 vs. 2021", gp=gpar(fontsize=20, font=3))),
	 path = output_dir,
	 filename = "fig-psta-90pct_hfi-2023_2021-pc.png")

ggsave(grid.arrange(p2a, p2b, p2c, p2d, nrow = 2,
	top = textGrob("PSTA: HFI Comparison 2024 vs. 2021", gp=gpar(fontsize=20, font=3))),
	 path = output_dir,
	 filename = "fig-psta-90pct_hfi-2024_2021-pc.png")

ggsave(grid.arrange(p3a, p3b, p3c, p3d, nrow = 2,
	top = textGrob("PSTA: HFI Comparison 2024 vs. 2023", gp=gpar(fontsize=20, font=3))),
	 path = output_dir,
	 filename = "fig-psta-90pct_hfi-2024_2023-pc.png")






