load("C:/Users/nsaby/Dropbox/fac/tp/sampling/data/DonneeCoursTours.RData")

tm_shape(champSP) +
  tm_raster(style="fisher")+
  tm_layout(legend.outside = TRUE)

library(sp)
plot(sp)


n = 40

#fixer la graine pour les tirages aléatoires
set.seed(1974)

# Creation des identifiants des lignes
id = 1:10000

# tire au hasard n valeurs parmi 10000
MesIds <- sample( x = id  , size = n)

MesIds <- sort(MesIds)

MesIds

champ[ MesIds , ]


library(dplyr)
filter(champ , MesIds)


# SI spatial
MonEch <- spsample( x = champSP ,
                    n  = n ,
                    type = "random")


SI <- tm_shape(champSP) +
  tm_raster()+
  tm_shape(MonEch)+
  tm_symbols()+
  tm_layout(legend.outside = TRUE)

SI
