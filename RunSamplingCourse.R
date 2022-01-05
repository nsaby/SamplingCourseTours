library(sf)
library(raster)
library(spatialEco)
library(tmap)
library(dplyr)
library(fields)
library(sampling)

# Preparer les donn?es 
parc <- read_sf("../data/parcelle_adj.shp")

r <- raster("../data/mnt_par_adj/")
plot(r)


ragg <- aggregate(r, fact = 8)
plot(ragg)


curv <-curvature(ragg,"planform")
plot(curv)
contour(ragg,add=TRUE)

s <- terrain(ragg,
             opt = c("slope", "TPI", "TRI","roughness") 
             )
plot(s)

tm_shape(ragg) +
  tm_raster()+
  tm_shape(parc) + tm_polygons(alpha = 0)


tm_shape(s) +
  tm_raster("tri")+
  tm_shape(parc) + tm_polygons(alpha = 0)

scrop <- crop(s,parc)
scrop <- mask(scrop,parc)

rcrop <- crop(ragg,parc)
rcrop <- mask(rcrop,parc)
plot(scrop)

curvcrop <- crop(curv,parc)
curvcrop <- mask(curvcrop,parc)


scrop <- stack(scrop, curvcrop)
plot(scrop)


# Choix du nombre d'échantillons

nStrates <- 8
nEchantillons <- 48
  
# échantillonnage aléatoire simple

MonSpSI  <- st_sample(parc , size= nEchantillons, type =  'random')

tm_shape(parc) + tm_polygons() +
  tm_shape(MonSpSI) + tm_dots(size=4)


# échantillonnage grille
MonSpSY  <- st_sample(parc , size= nEchantillons, type =  'regular')




maGrille <- cbind.data.frame(coordinates(rcrop),
                             alt = getValues(rcrop),
                             getValues(scrop))
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


tm_shape(r) + tm_raster() +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(MonSpSY) + tm_dots(size=4) 


StrateKM <- rasterFromXYZ(maGrille[,c("x","y","stratKM")],
                            crs = crs(parc))

tm_shape(StrateKM) + tm_raster() +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(mySampleKM) + tm_dots(size=4) 


tm_shape(r) + tm_raster() +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(MonSpSY) + tm_dots(size=4) +
  tm_shape(MonSpSI) + tm_dots(size=2, col="red")+
  tm_shape(mySampleKM) + tm_dots(size=2, col="green")

## strate avec RK ----
nStrates <- 5
nEchantillons <- 32

k1<- kmeans(x= scale(maGrille[,c(3,7,8)]) ,
            centers= nStrates,
            nstart=10, 
            iter.max = 500000)

maGrille$stratKM2 <- k1$cluster

StrateSTSI <- rasterFromXYZ(maGrille[,c("x","y","stratKM2")],
                            crs = crs(parc))

pal8 <- c("deepskyblue", "brown1", 
           "brown4", "darkgrey",
          "darkolivegreen1", "darkseagreen1")

tm_shape(StrateSTSI) + tm_raster(style = "cat",
                                 palette = pal8) 
  
#compute stratum sample sizes for proportional allocation
Nh <- tapply(maGrille$stratKM2,INDEX=maGrille$stratKM2,FUN=length)
wh <- Nh/sum(Nh)


nh <- round(wh*nEchantillons)
sum(nh)

#minimum stratum sample size is 2, so increase nh for stratum 2 by 1, and reduce sample szie of stratum 6 by 1
nh[nh<2] <- 2
nh
sum(nh)

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




tm_shape(StrateSTSI) + tm_raster(style = "cat",
                                 palette = pal8[-6]) +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(mysampleSTSI) + tm_symbols (col = 'stratKM2',
                                   size=2,
                                   palette = pal8[-6],
                                   border.col = "black")




tm_shape(StrateSTSI) + tm_raster(style = "cat",
                                 palette = pal8) +
  tm_shape(parc) + tm_polygons(alpha = 0) +
  tm_shape(MonSpSY) + tm_dots(size=4) +
  tm_shape(MonSpSI) + tm_dots(size=2, col="red")+
  tm_shape(mySampleKM) + tm_dots(size=2, col="green") +
   tm_shape(mysampleSTSI) + tm_dots(size=2, col="grey")



