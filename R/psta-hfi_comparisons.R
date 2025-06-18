library(tidyverse)
library(ggplot2)
library(scales)

setwd("C:/Users/lnoble/OneDrive - Government of BC/Programming/R/PSTA") #Set to your working folder in R

###Create output directory if one doesn't already exist###
output_dir <- "outputs"
if (!dir.exists(output_dir)){
dir.create(output_dir)
} else {
  print("Directory already exists")
}


data_hfi <- read.csv("data/HFI_Station_Sample_Comparison_FT.csv") %>%
  select(-c(OBJECTID, STATION_ACRONYM, HFI_C2_SL, HFI_Station_Sample_Stations_90Percentile_2000_2023)) %>%
  rename(sta_code = STATION_CODE,
         sta_name = STATION_NAME,
         lat = LATITUDE,
         long = LONGITUDE,
         elev_m = ELEVATION,
         temp = temperature,
         rh = relative_humidity,
         wd = wind_direction,
         ws = wind_speed,
         prec = precipitation,
         ffmc = FFMC,
         isi = ISI,
         dmc = DMC,
         dc = DC,
         bui = BUI,
         fwi = FWI,
         vri_full_label_18 = FULL_LABEL_2018,
         ft_18 = Fuel_Type_CD_2018,
         hfi_90pct_19 = HFI_Station_Sample_HFI_2019,
         vri_full_label_20 = FULL_LABEL_2020,
         ft_20 = Fuel_Type_CD_2020,
         hfi_90pct_21 = HFI_Station_Sample_HFI_2021,
         vri_full_label_24 = FULL_LABEL_2024,
         ft_24 = Fuel_Type_CD_2024,
         hfi_90pct_24 = HFI_Station_Sample_HFI_2024) %>%
  mutate(pc_hfi_90pct_19_21 = (hfi_90pct_21 - hfi_90pct_19) / hfi_90pct_19 * 100, #calc percent change b/n yrs of interest
         pc_hfi_90pct_19_24 = (hfi_90pct_24 - hfi_90pct_19) / hfi_90pct_19 * 100,
         pc_hfi_90pct_21_24 = (hfi_90pct_24 - hfi_90pct_21) / hfi_90pct_21 * 100) %>%
  mutate(lat = str_sub(lat, start = 1, end = 7),
         across(c(wd, ffmc, dmc, dc, bui, fwi, hfi_90pct_19, hfi_90pct_21, hfi_90pct_24), round, 0),
         across(c(temp, ws, prec, isi), round, 1),
         across(c(pc_hfi_90pct_19_21, pc_hfi_90pct_19_24, pc_hfi_90pct_21_24), round, 2)) %>%
  select(sta_code, sta_name,
         lat, long, elev_m,
         temp, rh, wd, ws, prec,
         ffmc, isi, dmc, dc, bui, fwi,
         vri_full_label_18, ft_18, hfi_90pct_19,
         vri_full_label_20, ft_20, hfi_90pct_21,
         vri_full_label_24, ft_24, hfi_90pct_24,
         pc_hfi_90pct_19_21, pc_hfi_90pct_19_24, pc_hfi_90pct_21_24) %>%
  select(!c(vri_full_label_18, vri_full_label_20, vri_full_label_24)) %>%
  arrange(sta_code)

# names(data_hfi)

##################################################
##################################################
## Percent Change HFI
##################################################
##################################################

####################
## HFI: 2024 vs 2021
####################

p1a <-
ggplot(subset(data_hfi, pc_hfi_90pct_21_24 > 0)) +
  geom_point(aes(sta_code, pc_hfi_90pct_21_24), col = "red") +
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
  labs(title = "PSTA: HFI Comparisons (2024 vs 2021)",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p1b <-
ggplot(subset(data_hfi, pc_hfi_90pct_21_24 < 0)) +
  geom_point(aes(sta_code, pc_hfi_90pct_21_24), col = "blue") +
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
  labs(title = "PSTA: HFI Comparisons (2024 vs 2021)",
       x = "Station Code",
       y = "Percent Change HFI")

####################
## HFI: 2021 vs 2019
####################

p2a <-
ggplot(subset(data_hfi, pc_hfi_90pct_19_21 > 0)) +
  geom_point(aes(sta_code, pc_hfi_90pct_19_21), col = "red") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 4600),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 4000),
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
  labs(title = "PSTA: HFI Comparisons (2021 vs 2019)",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p2b <-
ggplot(subset(data_hfi, pc_hfi_90pct_19_21 < 0)) +
  geom_point(aes(sta_code, pc_hfi_90pct_19_21), col = "blue") +
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
  labs(title = "PSTA: HFI Comparisons (2021 vs 2019)",
       x = "Station Code",
       y = "Percent Change HFI")

####################

ggsave(p1a,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2024_v_2021-pc-positive.png")
 
ggsave(p1b,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2024_v_2021-pc-negative.png")

ggsave(p2a,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2021_v_2019-pc-positive.png")

ggsave(p2b,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2021_v_2019-pc-negative.png")

##################################################
##################################################
## Fuel Type Changes
##################################################
##################################################

labels_y_axis <- c("NF","S-3","S-2","S-1","O-1a/b","M-1/2","D-1/2",
                   "C-7","C-6","C-5","C-4","C-3","C-2","C-1",
                   "",
                   "C-1","C-2","C-3","C-4","C-5","C-6","C-7",
                   "D-1/2","M-1/2","O-1a/b","S-1","S-2","S-3","NF")

####################
## FT: 2024 vs 2020
####################

data_hfi_2 <- data_hfi %>%
  mutate(ft_20_code = case_when(ft_20 == "C-1" ~ 1,
                                ft_20 == "C-2" ~ 2,
                                ft_20 == "C-3" ~ 3,
                                ft_20 == "C-4" ~ 4,
                                ft_20 == "C-5" ~ 5,
                                ft_20 == "C-6" ~ 6,
                                ft_20 == "C-7" ~ 7,
                                ft_20 == "D-1/2" ~ 8,
                                ft_20 == "M-1/2" ~ 9,
                                ft_20 == "O-1a/b" ~ 10,
                                ft_20 == "S-1" ~ 11,
                                ft_20 == "S-2" ~ 12,
                                ft_20 == "S-3" ~ 13,
                                ft_20 == "N" ~ 14,
                                T ~ NA),
         ft_24_code = case_when(ft_24 == "C-1" ~ -1,
                                ft_24 == "C-2" ~ -2,
                                ft_24 == "C-3" ~ -3,
                                ft_24 == "C-4" ~ -4,
                                ft_24 == "C-5" ~ -5,
                                ft_24 == "C-6" ~ -6,
                                ft_24 == "C-7" ~ -7,
                                ft_24 == "D-1/2" ~ -8,
                                ft_24 == "M-1/2" ~ -9,
                                ft_24 == "O-1a/b" ~ -10,
                                ft_24 == "S-1" ~ -11,
                                ft_24 == "S-2" ~ -12,
                                ft_24 == "S-3" ~ -13,
                                ft_24 == "N" ~ -14,
                                T ~ NA),
         ft_change = factor(if_else(ft_20 == ft_24, "No", "Yes", NA),
                            levels = c("Yes", "No")))

p3a <-
ggplot(data_hfi_2, aes(x = sta_code,
                       xend = sta_code,
                       y = ft_20_code,
                       yend = ft_24_code,
                       colour = ft_change)) +
  geom_segment() +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(limits = c(-14,14),
                     breaks = seq(-14,14,1),
                     labels = labels_y_axis) +
  geom_hline(yintercept = 0) +
  labs(title = "PSTA: FBP Fuel Type Comparisons (2024 vs 2020)",
       x = "Station Code",
       y = "FBP Fuel Type",
       colour = "FT Change") +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank()) +
  geom_label(aes(ceiling(max(data_hfi$sta_code)/200)*200, 13, label = "2020"), colour = "black") +
  geom_label(aes(ceiling(max(data_hfi$sta_code)/200)*200, -13, label = "2024"), colour = "black")

####################
## FT: 2020 vs 2018
####################

data_hfi_3 <- data_hfi %>%
  mutate(ft_18_code = case_when(ft_18 == "C-1" ~ 1,
                                ft_18 == "C-2" ~ 2,
                                ft_18 == "C-3" ~ 3,
                                ft_18 == "C-4" ~ 4,
                                ft_18 == "C-5" ~ 5,
                                ft_18 == "C-6" ~ 6,
                                ft_18 == "C-7" ~ 7,
                                ft_18 == "D-1/2" ~ 8,
                                ft_18 == "M-1/2" ~ 9,
                                ft_18 == "O-1a/b" ~ 10,
                                ft_18 == "S-1" ~ 11,
                                ft_18 == "S-2" ~ 12,
                                ft_18 == "S-3" ~ 13,
                                ft_18 == "N" ~ 14,
                                T ~ NA),
         ft_20_code = case_when(ft_20 == "C-1" ~ -1,
                                ft_20 == "C-2" ~ -2,
                                ft_20 == "C-3" ~ -3,
                                ft_20 == "C-4" ~ -4,
                                ft_20 == "C-5" ~ -5,
                                ft_20 == "C-6" ~ -6,
                                ft_20 == "C-7" ~ -7,
                                ft_20 == "D-1/2" ~ -8,
                                ft_20 == "M-1/2" ~ -9,
                                ft_20 == "O-1a/b" ~ -10,
                                ft_20 == "S-1" ~ -11,
                                ft_20 == "S-2" ~ -12,
                                ft_20 == "S-3" ~ -13,
                                ft_20 == "N" ~ -14,
                                T ~ NA),
         ft_change = factor(if_else(ft_18 == ft_20, "No", "Yes", NA),
                            levels = c("Yes", "No")))

p3b <-
ggplot(data_hfi_3, aes(x = sta_code,
                       xend = sta_code,
                       y = ft_18_code,
                       yend = ft_20_code,
                       colour = ft_change)) +
  geom_segment() +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(limits = c(-14,14),
                     breaks = seq(-14,14,1),
                     labels = labels_y_axis) +
  geom_hline(yintercept = 0) +
  labs(title = "PSTA: FBP Fuel Type Comparisons (2020 vs 2018)",
       x = "Station Code",
       y = "FBP Fuel Type",
       colour = "FT Change") +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank()) +
  geom_label(aes(ceiling(max(data_hfi$sta_code)/200)*200, 13, label = "2018"), colour = "black") +
  geom_label(aes(ceiling(max(data_hfi$sta_code)/200)*200, -13, label = "2020"), colour = "black")

####################

ggsave(p3a,
       path = output_dir,
       filename = "fig-psta-fuel_types-2024_v_2020.png")

ggsave(p3b,
       path = output_dir,
       filename = "fig-psta-fuel_types-2020_v_2018.png")

##################################################
##################################################
## HFI & Fuel Type Changes
##################################################
##################################################

####################
## HFI: 2024 vs 2021
####################

p11a <-
ggplot() +
  geom_point(data = subset(data_hfi, pc_hfi_90pct_21_24 > 0),
             aes(x = sta_code, y = pc_hfi_90pct_21_24),
             col = "red") +
  geom_point(data = subset(data_hfi_2, pc_hfi_90pct_21_24 > 0 & ft_change == "Yes"),
             aes(x = sta_code, y = pc_hfi_90pct_21_24),
             col = "black") +
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
  labs(title = "PSTA: HFI Comparisons (2024 vs 2021)",
       subtitle = "FT Change: No (red), Yes (black)",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")
  
p11b <-
ggplot() +
  geom_point(data = subset(data_hfi, pc_hfi_90pct_21_24 < 0),
             aes(x = sta_code, y = pc_hfi_90pct_21_24),
             col = "blue") +
  geom_point(data = subset(data_hfi_2, pc_hfi_90pct_21_24 < 0 & ft_change == "Yes"),
             aes(x = sta_code, y = pc_hfi_90pct_21_24),
             col = "black") +
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
  labs(title = "PSTA: HFI Comparisons (2024 vs 2021)",
       subtitle = "FT Change: No (blue), Yes (black)",
       x = "Station Code",
       y = "Percent Change HFI")

####################
## HFI: 2021 vs 2019
####################

p12a <-
ggplot() +
  geom_point(data = subset(data_hfi, pc_hfi_90pct_19_21 > 0),
             aes(x = sta_code, y = pc_hfi_90pct_19_21),
             col = "red") +
  geom_point(data = subset(data_hfi_2, pc_hfi_90pct_19_21 > 0 & ft_change == "Yes"),
             aes(x = sta_code, y = pc_hfi_90pct_19_21),
             col = "black") +
  scale_x_continuous(limits = c(0, ceiling(max(data_hfi$sta_code)/200)*200),
                     breaks = seq(0, ceiling(max(data_hfi$sta_code)/200)*200, 200),
                     labels = comma) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(NA, 4600),
                     breaks = c(10, 25, 50, 100, 250, 500, 1000, 4000),
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
  labs(title = "PSTA: HFI Comparisons (2021 vs 2019)",
       subtitle = "FT Change: No (red), Yes (black)",
       x = "Station Code",
       y = "log10 [Percent Change HFI]")

p12b <-
ggplot() +
  geom_point(data = subset(data_hfi, pc_hfi_90pct_19_21 < 0),
             aes(x = sta_code, y = pc_hfi_90pct_19_21),
             col = "blue") +
  geom_point(data = subset(data_hfi_2, pc_hfi_90pct_19_21 < 0 & ft_change == "Yes"),
             aes(x = sta_code, y = pc_hfi_90pct_19_21),
             col = "black") +
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
  labs(title = "PSTA: HFI Comparisons (2021 vs 2019)",
       subtitle = "FT Change: No (blue), Yes (black)",
       x = "Station Code",
       y = "Percent Change HFI")

####################

ggsave(p11a,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2024_v_2021-pc-positive-w_ft_change.png")

ggsave(p11b,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2024_v_2021-pc-negative-w_ft_change.png")

ggsave(p12a,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2021_v_2019-pc-positive-w_ft_change.png")

ggsave(p12b,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2021_v_2019-pc-negative-w_ft_change.png")

##################################################
##################################################
## HFI pathways by sta, by year
##################################################
##################################################

data_hfi <- data_hfi %>%
  left_join(select(data_hfi_3, sta_code, ft_change),
            by = "sta_code") %>%
  rename(ft_change_18_20 = ft_change) %>%
  left_join(select(data_hfi_2, sta_code, ft_change),
            by = "sta_code") %>%
  rename(ft_change_20_24 = ft_change) %>%
  select(!pc_hfi_90pct_19_24) %>%
  mutate(rln_hfi_90pct_19_21 = factor(case_when(hfi_90pct_21 > hfi_90pct_19 ~ "Increase",
                                                hfi_90pct_21 < hfi_90pct_19 ~ "Decrease",
                                                T ~ "No Change"),
                                      levels = c("Decrease", "Increase", "No Change")),
         rln_hfi_90pct_21_24 = factor(case_when(hfi_90pct_24 > hfi_90pct_21 ~ "Increase",
                                                hfi_90pct_24 < hfi_90pct_21 ~ "Decrease",
                                                T ~ "No Change"),
                                      levels = c("Decrease", "Increase", "No Change")))

####################
## HFI: 2024 vs 2021
####################

p21a <-
ggplot(data_hfi,
       aes(x = "2021",
           xend = "2024",
           y = hfi_90pct_21,
           yend = hfi_90pct_24,
           colour = rln_hfi_90pct_21_24)) +
  geom_segment() +
  scale_color_manual(values = c("#00BFC4", "#F8766D", "black")) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(1, 100000),
                     breaks = c(1, 10, 100, 1000, 10000, 100000),
                     labels = comma) +
  labs(title = "PSTA: HFI Comparisons (2024 vs 2021)",
       x = "Year",
       y = "log10 [HFI (kW/m)]",
       colour = "HFI (2024:2021)") +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank())

####################
## HFI: 2021 vs 2019
####################

p21b <-
ggplot(data_hfi,
       aes(x = "2019",
           xend = "2021",
           y = hfi_90pct_19,
           yend = hfi_90pct_21,
           colour = rln_hfi_90pct_19_21)) +
  geom_segment() +
  scale_color_manual(values = c("#00BFC4", "#F8766D", "black")) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(1, 100000),
                     breaks = c(1, 10, 100, 1000, 10000, 100000),
                     labels = comma) +
  labs(title = "PSTA: HFI Comparisons (2021 vs 2019)",
       x = "Year",
       y = "log10 [HFI (kW/m)]",
       colour = "HFI (2021:2019)") +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank())

rm(data_hfi_2, data_hfi_3, labels_y_axis)

####################

ggsave(p21a,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2024_v_2021.png")

ggsave(p21b,
       path = output_dir,
       filename = "fig-psta-90pct_hfi-2021_v_2019.png")

##################################################
##################################################

names(data_hfi)

df <- data_hfi %>%
  filter(is.infinite(pc_hfi_90pct_21_24) & ft_24 == "N") %>%
  # filter(pc_hfi_90pct_21_24 == -100 & ft_24 != "N") %>%
  # filter(ft_24 == "N" & hfi_90pct_24 != 0) %>%
  arrange(sta_code)

print("ft change 2020 vs 2018")
table(data_hfi$ft_change_18_20)

print("ft change 2024 vs 2020")
table(data_hfi$ft_change_20_24)

