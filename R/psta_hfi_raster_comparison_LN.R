## PSTA HFI Raster comparison  
## Created by: N McLoughlin, L Noble, K Corrigan
## Last modified: 2025-02-24
#############################

library(terra)
library(raster)
library(tidyverse)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)


setwd("Z:/!Project/Predictive Services/10. PROJECT WORK/BC FGM/PSTA") #Set to your working folder in R

##Create output directory if one doesn't already exist
output_dir <- "outputs"
if (!dir.exists(output_dir)){
dir.create(output_dir)
} else {
  print("Directory already exists")
}

## Load Rasters condensed (c) and extended (e) 90th percentile weather
#################### Raster files already exist so can just load classified files
#hfi_23c <- rast(x="HFI_2023_FW_2000-2022.tif")
#hfi_23e <- rast(x="HFI_2023_FW_1970-2023.tif")
#hfi_24c <- rast(x="HFI_2024_FW_2000-2023.tif")
#hfi_24e <- rast(x="HFI_2024_FW_1970-2023.tif")

# Reclassified files
hfi_24cc <-  rast(x="HFI_2024_FW_2000-2023_classified.tif")
hfi_24ec <-  rast(x="HFI_2024_FW_1970-2023_classified.tif")

hfi_21c <- rast(x="HFI_2021_classified.tif")


## Reclassify rasters to HFI class
################################ WRITE STEP TO CHECK IF RECLASSIFIED RASTERS ALREADY EXIST
m <- c(0,10,1,
	10,500,2,
	500,2000,3,
	2000,4000,4,
	4000,10000,5,
	1000,1000000,6)

	rclmat <- matrix(data=m, ncol=3, byrow=TRUE)

hfi_24cc <- classify(
	x=hfi_24c, 
	rcl=rclmat, include.lowest=FALSE, 
	right=FALSE, 
	filename="HFI_2024_FW_2000-2023_classified.tif",
	gdal=c("COMPRESS=DEFLATE", "TWF=YES")
	)

hfi_24ec <- classify(
	x=hfi_24e, 
	rcl=rclmat, include.lowest=FALSE, 
	right=FALSE, 
	filename="HFI_2024_FW_1970-2023_classified.tif",
	gdal=c("COMPRESS=DEFLATE", "TWF=YES")
	)

## Raster Math - compare difference in HFI class
################################################
chfi <- hfi_24cc - hfi_24ec #direct difference

rms_hfi <- sqrt(mean(chfi))
abs_chfi <- abs(chfi)
scaled_chfi <- round(((abs_chfi+0.1)/3.1)^0.5, 1) # Create opacity layer for change in hfi class
scaled_chfi <- subst(scaled_chfi, NA, 0)

## Visualize
###############################################
stack <- c(hfi_24ec, hfi_24cc)	#comparing same year c vs e
names(stack) <- list("Extended", "Constrained")

png("outputs/fig-psta-hfi_class-2024.png")
plot(stack)
dev.off()

png("outputs/fig-psta-hfi_change-2024.png")
plot(chfi)
dev.off()

png("outputs/fig-psta-hfi_opacity-2024.png")
plot(hfi_24cc, alpha=scaled_chfi)
dev.off()
	






