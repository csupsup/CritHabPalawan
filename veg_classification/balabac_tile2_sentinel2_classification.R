---
title: "Satellite Imagery Classification"
author: "Supsup et al."
date: "22 December 2022"
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
list.of.packages <- c("raster", "tidyverse", "sf", "rpart", "rpart.plot", "rasterVis", "mapedit", "mapview", "caret", "forcats", "patchwork", "rgdal", "maptools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

#load packages
library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
library(forcats)
library(patchwork)
library(rgdal)
library(maptools)
```
**1. Load and explore images**
```{r, load images}
#load all data at once
sat.img <- list.files("images/tile02/", pattern = "tif", full.names=TRUE)
#read admin boundary
adm <- st_read("adm/balabac_t2_adm_utm.shp", quiet = TRUE)
#stack images and mask
stack.img <- stack(sat.img)
stack.img <- mask(stack.img, adm)
#check image coordinate reference system(crs) and resolution
nlayers(stack.img)
#crs
crs(stack.img)
#resolution
res(stack.img)

#plot true color composite: 4-3-2 ##remember to check the sequence of bands
par(col.axis="white",col.lab="white",tck=0)
plotRGB(stack.img, r = 6, g = 5, b = 4, axes = TRUE, 
        stretch = "lin", main = "True color composite")
box(col="white")
#plot false color composite: 5-4-3
par(col.axis="white",col.lab="white",tck=0)
plotRGB(stack.img, r = 10, g = 6, b = 5, axes = TRUE, stretch = "lin", main = "False color composite")
box(col="white")

#calculate NDVI
ndvi <- (stack.img[[10]] - stack.img[[6]])/(stack.img[[10]] + stack.img[[6]])

#minimum
min(ndvi@data@values, na.rm = T)
#maximum
max(ndvi@data@values, na.rm = T)
```
```{r, ndvi, fig.width = 5}
#plot NDVI
as(ndvi, "SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  ggplot(data = .) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "NDVI", 
       x = " ", 
       y = " ") +
  scale_fill_gradient(high = "#CEE50E", 
                      low = "#087F28",
                      name = "NDVI")
```

**2. Perform supervised classification**
```{r, supclass}
#read training points
training_points <- st_read("rois/balabact2_roi_centroids.shp", quiet = TRUE)

#assign color
veg_cols <- c("#aaaa00", "#ffff7f", "#005500", "#00aa00")

#plot points
ggplot() +
  geom_sf(data = adm, fill = "light gray", color = NA) +
  geom_sf(data = training_points, aes(color = C_info), size = 0.7) +
  scale_color_manual(values = veg_cols) +
  labs(title = "Classification points by vegetation type") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.key = element_rect(fill = "transparent"))

#extract spectral values from the raster
training_points <- as(training_points, 'Spatial')

df <- raster::extract(stack.img, training_points) %>% round()

head(df)

#exploratory analysis: plotting the spectral profile
profiles <- df %>% 
  as.data.frame() %>% 
  cbind(., training_points$C_ID) %>% 
  rename(id = "training_points$C_ID") %>% 
  na.omit() %>% 
  group_by(id) %>% 
  summarise(balabact2_b10 = mean(balabact2_b10),
            balabact2_b11 = mean(balabact2_b11),
            balabact2_b12 = mean(balabact2_b12),
            balabact2_b2 = mean(balabact2_b2),
            balabact2_b3 = mean(balabact2_b3),
            balabact2_b4 = mean(balabact2_b4),
            balabact2_b5 = mean(balabact2_b5),
            balabact2_b6 = mean(balabact2_b6),
            balabact2_b7 = mean(balabact2_b7),
            balabact2_b8 = mean(balabact2_b8),
            balabact2_b8a = mean(balabact2_b8a)) %>% 
  mutate(id = case_when(id == 1 ~ "Pristine Forest",
                        id == 2 ~ "Secondary Growth",
                        id == 3 ~ "Open Area",
                        id == 4 ~ "Mangrove")) %>% 
  as.data.frame()
head(profiles)
#rename colnames
colnames(profiles)[2] <- "band10"
colnames(profiles)[3] <- "band11"
colnames(profiles)[4] <- "band12"
colnames(profiles)[5] <- "band2"
colnames(profiles)[6] <- "band3"
colnames(profiles)[7] <- "band4"
colnames(profiles)[8] <- "band5"
colnames(profiles)[9] <- "band6"
colnames(profiles)[10] <- "band7"
colnames(profiles)[11] <- "band8"
colnames(profiles)[12] <- "band8a"

profiles %>% 
  select(-id) %>% 
  gather() %>% 
  mutate(class = rep(c("Pristine Forest", "Secondary Growth", "Open Area/Cultivation", "Mangrove"), 11)) %>% 
  ggplot(data = ., aes(x = fct_relevel(as.factor(key), levels = c("band10", "band11", "band12","band2", "band3", "band4", "band5", "band6", "band7", "band8", "band8a")), y = value, group=class, color = class)) +
  geom_point(size = 2.5) +
  geom_line(lwd = 1.2) +
  scale_color_manual(values=veg_cols) +
  labs(title = "Spectral profile",
       x = "Bands",
       y = "Reflectance") +
  #scale_y_continuous(limits=c(5000, 15000)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5),
        axis.ticks = element_blank())

#density histograms of spec profiles
profiles %>% 
  select(-id) %>% 
  gather() %>% 
  mutate(class = rep(c("Pristine Forest", "Secondary Growth", "Open Area/Cultivation", "Mangrove"), 11)) %>% 
  ggplot(., aes(x=value, group=as.factor(class), fill=as.factor(class))) + 
  geom_density(alpha = 0.75) + 
  geom_vline(data = . %>% group_by(class) %>% summarise(grp.mean = mean(value)),
             aes(xintercept=grp.mean, color = class), linetype="dashed", size=1) +
  scale_fill_manual(values=veg_cols,
                    name = "class") +
  scale_color_manual(values=veg_cols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5),
        axis.ticks = element_blank()) +
  labs(x = "Reflectance value",
       y = "Density",
       title = "Density histograms of spectral profiles",
       subtitle = "Vertical lines represent mean group reflectance values")

#perform classification
comb <- data.frame(training_points$C_ID, df)

mod.class <- rpart(as.factor(training_points.C_ID)~., data = comb, method = 'class')

#plot decission tree
rpart.plot(mod.class, box.palette = 0, main = "Classification tree")

#set class to vegetation types
veg.pr <- predict(stack.img, mod.class, type ='class')
veg_cols2 <- c("#005500","#00aa00", "#ffff7f", "#aaaa00")
plot(veg.pr, legend = FALSE, col = veg_cols2)
legend("topleft", legend = c("Pristine Forest","Secondary Growth","Open Area/Cultivation", "Mangrove"), fill = veg_cols2, cex = 0.7)

#evaluate classification using confusion matrix
test <- raster::extract(veg.pr, training_points) %>% 
  as.data.frame() %>% 
  rename(id = ".")

testProbs <- data.frame(
  obs = as.factor(training_points$C_ID),
  pred = as.factor(test$id)
) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix

#export tif file
writeRaster(veg.pr, "balabac_t2_veg2019.tif", overwrite=TRUE)

```
