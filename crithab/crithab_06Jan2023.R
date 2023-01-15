---
title: "Critical Habitat Identification and ECAN Assessment"
author: "Supsup et al."
date: "06 January 2023"
output:
  html_document:
    df_print: paged
  pdf_document: default
indent: no
---
```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
```
```{r, load require packages, results='hide'}
#install packages
list.of.packages <- c("raster", "terra", "sf", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

#load packages
library(terra)
library(raster)
library(sf)
```

**1. Load vegetation and sdm data**
```{r, load images}
#read data
veg <- rast("data/balabac/veg.tif") #vegetation data from classification analysis
sdm <- rast("data/balabac/sdm.tif") #species richness data from species distribution modeling

#repoject data to utm
#data.crs <- "+proj=utm +zone=50 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
#veg.utm <- rast(projectRaster(veg, crs = data.crs))
#sdm.utm <- rast(projectRaster(sdm, crs = data.crs))

#view data
plot(veg, col = terrain.colors(255))
plot(sdm)
#convert data to polygon
#system.time( veg.poly <- as.polygons(veg.utm)) #time stamp

veg.poly <- as.polygons(veg)
sdm.poly <- as.polygons(sdm)

writeVector(veg.poly, "shapefiles/veg.poly.shp", overwrite = TRUE)
writeVector(sdm.poly, "shapefiles/sdm.poly.shp", overwrite = TRUE)
```

**2. Intersect vegetation and sdm data**
```{r, read shape}
#read data
veg.shp <- st_read("shapefiles/veg.poly.shp", quiet = TRUE)
sdm.shp <- st_read("shapefiles/sdm.poly.shp", quiet = TRUE)

#intersect vegetation and sdm data
sdm.veg.int <- intersect(veg.poly, sdm.poly)

#export as a shapefile
writeVector(sdm.veg.int, "shapefiles/sdm.veg.shp", overwrite = TRUE)
```

**3. Identify important habitat**
```{r, important hab}
#read data
library(tidyverse)
ih <- st_read("shapefiles/sdm.veg.shp", quiet = TRUE)

#rename sdm column name
names(ih)[2] <- "sdm"

#identify important habitat using the condition below

#CVT + <16 SPP = Not Important
#CVT + >16 SPP = Very Low Importance
#ESG + < 16 SPP = Low Importance 
#ESG + >16 SPP =  Moderate Importance 
#ASG/OGF + MGR + <16 SPP = High Importance
#ASG/OGF + MGR + >16 SPP = Highest Importance

ih.id <- mutate(ih, impthab = if_else(veg==3 & sdm<16, "Not Important", if_else(veg==3 & sdm>=16, "Very Low Importance", if_else(veg==2 & sdm<16, "Low Importance", if_else(veg==2 & sdm>=16, "Moderate Importance", if_else(veg==1 & sdm<16, "High Importance", if_else(veg==1 & sdm>=16, "Highest Importance", if_else(veg==4 & sdm<16, "High Importance", if_else(veg==4 & sdm>=16, "Highest Importance", "Not Data")))))))))

p <- vect(ih.id)

#plot
plot(p, "impthab", lwd = 0.01, main = "Important habitat", mar=c(3.1, 3.1, 2.1, 9.1))

#export classified important habitat as a shapefile
st_write(ih.id, "shapefiles/impthab.shp", driver="ESRI Shapefile")
```

**4. Identify mismatch between identified critical habitat and ecan core zone**
```{r, identify mismatch}
library(units)
library(lwgeom)
library(smoothr)
library(rgdal)

#read data
ih.s <- st_read("shapefiles/impthab.shp", quiet = TRUE)
#get important habitat with high and highest importance
high <- subset(ih.s, impthab == 'Highest Importance' | impthab == 'High Importance')
st_write(high, "shapefiles/high.shp", driver="ESRI Shapefile")
#read and drop
high.ogr <- readOGR(dsn = "shapefiles/", layer = "high")
#drop areas < 100m2
high.dropped <- drop_crumbs(high.ogr, set_units(0.1, km^2))
#export
writeOGR(high.dropped, "shapefiles/", "ch", driver="ESRI Shapefile")

#read critical habitat
ch <- st_read("shapefiles/ch.shp", quiet = TRUE)
#merge fragmented polygons
ch.u <- ch %>% group_by(impthab) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
ch.val <- st_make_valid(ch.u)
#fill holes
ch.filled <- fill_holes(ch.val, set_units(0.1, km^2))

#read ecan core zone
core <- st_read("shapefiles/ecan_core.shp", quiet = TRUE)

#ensure that data crs is uniform
st_crs(ch.filled) <- 4326
st_crs(core) <- 4326

#install_github("a-benini/sfhelpers")
library(sfhelpers) 

#identify mismatch by getting the difference between the core zone and critical habitat
#condition: Highest Importance + Core Zone = "Matched, with ch and core zone"; Highest Importance + No Core Zone = "Mismatched, with ch but no protection"; No Important Habitat + Core Zone = "Mismatched, with protection but no ch"

sf_use_s2(FALSE)

ch.core <- st_or(ch.filled, core) #use union function similar to QGIS
#replace NAs with 0
ch.core.r.na <- ch.core %>% replace(is.na(.), 0)

#apply condition
ch.core.m <- mutate(ch.core.r.na, diff = if_else(impthab %in% "Highest Importance" & ECAN_CODE==1, "Matched, with CH and CZ",if_else(impthab %in% "High Importance" & ECAN_CODE==1, "Matched, with CH and CZ", if_else(impthab %in% "Highest Importance" & ECAN_CODE==0, "Mismatched, with CH but no CZ", if_else(impthab %in% "High Importance" & ECAN_CODE==0, "Mismatched, with CH but no CZ", if_else(impthab==0 & ECAN_CODE==1, "Mismatched, with CZ but no CH", "Not data"))))))

#plot
p.ch <- vect(ch.core.m)
plot(p.ch, "diff", lwd = 0.01, main = "CH and ECAN core zone assessment", mar=c(3.1, 3.1, 2.1, 12.1))

#export critical habitat and ecan assessment
st_write(ch.core.m, "shapefiles/ch.core.m.shp", driver="ESRI Shapefile")
```

