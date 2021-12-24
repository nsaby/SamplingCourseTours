library(sf)
library(raster)
library(tmap)
library(dplyr)
library(fields)


# Preparer les donn?es 
parc <- read_sf("../data/parcelle_adj.shp")

r <- raster("../data/mnt_par_adj/")
plot(r)
s <- terrain(r,opt = c("slope", "aspect", "TPI", "TRI") )
plot(s)

tm_shape(r) +
  tm_raster()+
  tm_shape(parc) + tm_polygons(alpha = 0)

scrop <- crop(s,parc)
scrop <- mask(scrop,parc)

rcrop <- crop(r,parc)
rcrop <- crop(rcrop,parc)
plot(scrop)

# Choix du nombre d'échantillons

nStrates <- 8
nEchantillons <- 24
  
# échantillonnage aléatoire simple

MonSpSI  <- st_sample(parc , size= nEchantillons, type =  'random')

tm_shape(parc) + tm_polygons() +
  tm_shape(MonSpSI) + tm_dots(size=4)


# échantillonnage grille
MonSpSY  <- st_sample(parc , size= nEchantillons, type =  'regular')




maGrille <- cbind.data.frame(coordinates(rcrop),alt = getValues(rcrop),getValues(scrop))
maGrille  <- na.omit(maGrille)


# Echantillonnage spatiale

k1<- kmeans(x= maGrille[,1:2] ,
            centers= nEchantillons,
            nstart=10, iter.max = 500)

strat0<- k1$cluster
maGrille$stratKM <- factor(strat0)

rdist.out <- rdist(x1 = k1$centers,
                   x2 = maGrille[,1:2]
                   )
ids.mindist <- apply(rdist.out,MARGIN=1,which.min)

mySampleKM <- maGrille[ids.mindist,]

rm(k1)

mySampleKM <- st_as_sf(mySampleKM,
                        coords = c('x','y'),
                       crs = crs(parc),
                       agr = "constant")

tm_shape(r) + tm_raster() +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(MonSpSY) + tm_dots(size=4) +
  tm_shape(MonSpSI) + tm_dots(size=2, col="red")+
   tm_shape(mySampleKM) + tm_dots(size=2, col="green")


## strate avec RK ----
k1<- kmeans(x= scale(maGrille[,1:7]) ,
            centers= nStrates,
            nstart=10, 
            iter.max = 500000)

strat0<- k1$cluster

maGrille$stratKM2 <- factor(strat0)


#compute stratum sample sizes for proportional allocation
Nh <- tapply(maGrille$stratKM2,INDEX=maGrille$stratKM2,FUN=length)
wh <- Nh/sum(Nh)


nh <- round(wh*nEchantillons)
sum(nh)

#minimum stratum sample size is 2, so increase nh for stratum 2 by 1, and reduce sample szie of stratum 6 by 1
nh[nh<2] <- 2
nh
sum(nh)

library(fields)
units<-strata(maGrille,
              stratanames="stratKM2",
              size=nh,
              method="srswr")

mysampleSTSI <-getdata(maGrille,units)

# select random location within selected pixel
resolution <-res(r)
mysampleSTSI$x <- jitter(mysampleSTSI$x,resolution[1]/2)
mysampleSTSI$y <- jitter(mysampleSTSI$y,resolution[1]/2)

mysampleSTSI <- st_as_sf(mysampleSTSI,
                       coords = c('x','y'),
                       crs = crs(parc),
                       agr = "constant")


StrateSTSI <- rasterFromXYZ(maGrille[,c("x","y","stratKM2")],
                            crs = crs(parc))

StrateSTSI <- rasterFromXYZ(maGrille[,c("x","y","stratKM2")],
                            crs = crs(parc))


pal8 <- c("deepskyblue", "brown1", "brown2",
           "brown3", "brown4", "darkgrey",
           "darkolivegreen", "darkolivegreen1", "darkseagreen1")

tm_shape(StrateSTSI) + tm_raster(style = "cat",
                                 palette = pal8) +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(MonSpSY) + tm_dots(size=4) +
  tm_shape(MonSpSI) + tm_dots(size=2, col="red")+
  tm_shape(mySampleKM) + tm_dots(size=2, col="green") +
   tm_shape(mysampleSTSI) + tm_dots(size=2, col="grey")



