---
title: "Species Distribution Modeling"
author: "Supsup et al."
date: "03 January 2023"
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
list.of.packages <- c("devtools", "SSDM", "raster", "sp", "sf", "ggplot2", "sdmpredictors", "ecodist", "spThin", "rJava")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages))
  install.packages(new.packages)

#load packages
library(SSDM)
library(raster)
library(sp)
library(sf)
library(ggplot2)
library(sdmpredictors)
library(ecodist)
library(spThin)
library(rJava)  #needs java openjdk; set JAVA_HOME by editing javaconf via linux "sudo nano /usr/lib/R/etc/javaconf"
```
**1. Load data**
```{r, load data}
#read environmetal layers (should be in .tif format with uniform extent and resolution)
envi <- load_var(path = "pal_v2/",files = NULL, format = ".tif", categorical = c("pal_esalc_2019_rev"), Norm = FALSE, tmp = FALSE)
envi
#check layers (e.g.BIO1)
plot(envi[[2]])

#read species data (in csv)
#make sure all points are within the envi layers' extent
spp <- load_occ(path = "spp/", envi, Xcol = 'Longitude', Ycol='Latitude', Spcol = 'Species', sep=",", GeoRes = FALSE, header = TRUE)

#read palawan's admin boundary
adm <- st_read("palawan_prov.shp", quiet = TRUE)

#check species data by plotting
ggplot() +
  geom_sf(data = adm, fill = "light gray", color = NA) +
  geom_point(aes(x=spp$Longitude, y=spp$Latitude), size = 0.7)+
  labs(title = "Species records") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.key = element_rect(fill = "transparent"))
```

**2. Filter species records**
```{r, filter species records}
#a. method 1 - remove duplicate records to retain only one per pixel

#export each species as shapefile
#define coordinate reference system (crs)
prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)
#create sf object
sp.sf <- st_as_sf(subset(spp, Species == "Anthracoceros marchei"), coords = c("Longitude", "Latitude"), crs = my.projection)
#set crs
st_crs(sp.sf)
#export shapefile
st_write(sp.sf, "sp.shp", driver="ESRI Shapefile")
#read shapefile
sp <- shapefile("sp.shp")
st_crs(sp)
#sp <- st_as_sf(sp)
#extract raster position of points
pos <- cellFromXY(envi, sp)
#create raster/pixel ID column
sp$raster_id <- pos
sp.rid <- st_as_sf(sp)
sp.rid$raster_id <- as.factor(sp.rid$raster_id)
#remove duplicate raster ID
sp.uni.records <- sp.rid[!duplicated(sp.rid$raster_id),]
#check retained records/features
summary(sp.rid)
summary(sp.uni.records)
#export cleaned records as a csv
st_write(sp.uni.records, "sp.uni.records.csv", layer_options = "GEOMETRY=AS_XY")
#repeat steps to all species
```
```{r, filter by distance, results = 'hide', fig.show = 'hide'}
#. method 2 - filter records by distance
# load spatialautocorrelation functions from https://github.com/jorgeassis/spatialAutocorrelation
source("functions.R")
#subset species data
am <- subset(spp, Species == "Anthracoceros marchei")
#get longitude and latitude cols
occurrenceRecords <- data.frame(Lon=as.numeric(as.character(am[,"Longitude"])), Lat=as.numeric(as.character(am[,"Latitude"])))
#define the distance class
autocorrelationClassDistance <- 1
#define the maximum distance
autocorrelationMaxDistance <- 5
#define the significance level of the test
autocorrelationSignif <- 0.05

distanceUncorr <- data.frame(Predictor=names(envi),Distance=NA)

for( i in 1:length(names(envi)))
  distanceUncorr[i,2] <- spatialAutocorrelation(occurrenceRecords=occurrenceRecords,subset(envi,i), autocorrelationClassDistance,autocorrelationMaxDistance,autocorrelationSignif)
```
```{r, plot correlations}
distanceUncorrPlot <- ggplot(distanceUncorr[sort(distanceUncorr[,2],decreasing = TRUE,index.return=TRUE)$ix,]) +
  geom_bar( aes(x= reorder(Predictor, Distance) , y=Distance), stat="identity", fill="black", alpha=0.5) +
  coord_flip() + theme(axis.text=element_text(size=12), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0) , size=12), axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0) , size=12), panel.border = element_blank(), panel.background = element_rect(fill = "#EFEFEF", colour = "#EFEFEF",size = 0, linetype = "solid"), panel.grid.major = element_line(size = 0, linetype = 'solid',colour = "#EFEFEF"),  panel.grid.minor = element_line(size = 0, linetype = 'solid',colour = "#EFEFEF")) + labs(x = "Predictor") +  labs(y = "Spatial correlation (km)") + geom_hline(aes(yintercept=round(mean(distanceUncorr[,2]),digits=2)),color="Black", linetype="dashed", size=0.3) + annotate("text", y = round(mean(distanceUncorr[,2]),digits=2) + 2 , x = 1 , label = paste0( round(mean(distanceUncorr[,2]),digits=2)," Km") , hjust = 0)

distanceUncorrPlot

meanCorrDistance <- mean(distanceUncorr[,2])

# Prune occurrence recors 
occurrenceRecords <- spatialThinning(occurrenceRecords,envi, meanCorrDistance )
#export filtered records
write.csv(occurrenceRecords, "filtered.csv")

#repeat steps to all species
```

**3. Filter environmental variables**
```{r, filter var}
var.cor <- pearson_correlation_matrix(envi, cachesize = 20, same_mask = FALSE)
plot_correlation(var.cor)
#remove highly correlated variables
```

**4. Perform modeling**
```{r, model}
#read filtered/final variables
envi.fl <- load_var(path = "envi_fl/",files = NULL, format = ".tif", categorical = c("pal_esalc_2019_rev"), Norm = FALSE, tmp = FALSE)
#read filtered species records
spp.fl <- load_occ(path = "spp_fl/", envi.fl, Xcol = 'Longitude', Ycol='Latitude', Spcol = 'Species', sep=",", GeoRes = FALSE, header = TRUE)
#check data
plot(Envi$pal_esalc_2019)
points(spp$Longitude, spp$Latitude)

#perform stacked species distribution modeling
#sum of binary maps
#BSSDM <- stack_modelling(c('MAXENT', 'CTA', 'GLM', 'RF', 'SVM'), spp.fl, envi.fl, Xcol = 'Longitude', Ycol = 'Latitude', verbose = TRUE, metric = "SES", ensemble.metric = "AUC", rep = 1, Spcol = "Species", method = 'bSSDM', save = TRUE, path = getwd(), tmp = TRUE)

#sum of probabilities of habitat suitability
PSSDM <- stack_modelling(c('MAXENT', 'CTA', 'GLM', 'RF', 'SVM'), spp.fl, envi.fl, Xcol = 'Longitude', Ycol = 'Latitude', verbose = TRUE, ensemble.thres = 0, rep = 1, Spcol = "Species", method = 'pSSDM', save = TRUE, path = getwd(), tmp = TRUE)

#view results
#plot(PSSDM)

#plot diversity map
plot(PSSDM@diversity.map

#save model
save.stack(PSSDM, name = "PStacked", path = getwd(), verbose = TRUE, GUI = FALSE)
```
