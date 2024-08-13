install.packages("sf")
library(sf)

# El shapefile usado en este proyecto se descargo en el siguiente link https://www.naturalearthdata.com/downloads/

#Primero cargemos el shp
shp_path <- "D:/NIXON/MI MUNDO PROPIO/08 SIG/R CON GEE/Libreria SF/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp"
shp <- st_read(shp_path)

## Otro Shape que sirve como ejemplo
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
plot(nc)

#Vizualicemos el shp
plot(shp)

#Manipulacion de datos espaciales

## Filtrado por atributo

### En este caso se va a filtrar segun el nombre del estado a analizar
filtrado <- shp[shp$name_nl == "Minnesota",]
plot(filtrado)

## Transformar el sistema de coordenadas

transformar <- st_transform(filtrado , crs = 4326)# CRS 4326 es WGS 84

## Guardar datos espaciales

st_write(transformar, "D:/NIXON/MI MUNDO PROPIO/08 SIG/R CON GEE/Libreria SF/Minnesota.shp")


#Creacion de puntos geo espaciales individuales y posterior fusión 

p1 <- st_point(c(-8.395835, 43.37087))
p2 <- st_point(c(-7.555851, 43.01208))
p3 <- st_point(c(-7.864641, 42.34001))
p4 <- st_point(c(-8.648053, 42.43362))
sfc <- st_sfc(list(p1, p2, p3, p4))
cprov <- st_sf(names = c('Coruña (A)', 'Lugo', 'Ourense', 'Pontevedra'),habitantes = c(123,123123,43242342,2342342),
               geom = sfc)
plot(cprov)

########################################################################################################
# Ejercicio 2.1 (Creación de una columna de geometrías) Crear una geometría (un objeto sfc) formada por: dos puntos en las posiciones (1,5) y (5,5), una línea entre los puntos (1,1) y (5,1), y un polígono, con vértices {(0,0), (6,0), (6,6), (0,6), (0,0)} y con un agujero con vértices {(2,2), (2,4), (4,4), (4,2), (2,2)}

## Los puntos 
punto1 <- st_point(c(1, 5))
punto2 <- st_point(c(5, 5))
sfc2 <- st_sfc(list(punto1, punto2))
cprov2 <- st_sf(names = c('uno', 'dos'), 
               geom = sfc2)
plot(cprov2)


## la linea

linea <- st_linestring(rbind(c(1, 1), c(5, 1)))
plot(linea)

## poligono con agujero interno

poligono_exterior <- st_polygon(list(rbind( c(0, 0), c(6, 0), c(6, 6), c(0, 6), c(0, 0))))
poligono_agujero <- st_polygon(list(rbind(  c(2, 2), c(2, 4), c(4, 4), c(4, 2), c(2, 2))))


poligono <- st_polygon(list(rbind(c(0, 0), c(6, 0), c(6, 6), c(0, 6), c(0, 0)),
                            rbind(c(2, 2), c(2, 4), c(4, 4), c(4, 2), c(2, 2))))

plot(poligono)


## Vizualizar todas las geometrias creadas en un solo ambiente
geometrias <- st_sfc( poligono, linea, punto1, punto2)

plot(geometrias, col = c('red', 'blue', 'green', 'yellow'))


#########################################################################################################

# Visualizacion de datos meuse
library(sp)

help(meuse) #para mas informacion sobre el dataset

data(meuse, package="sp")#aqui podremos ver como puntos las medidas de metales pesados cerca de un rio
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")

data(meuse.riv, package="sp")
str(meuse.riv) #aqui podemos ver los vectores relacionados al rio
meuse_riv <- st_sfc(st_polygon(list(meuse.riv)), crs = 28992)

data(meuse.grid, package="sp") #con esto podremos vizualizar la regilla de los datos
meuse_grid <- st_as_sf(meuse.grid, coords = c("x", "y"), 
                       crs = 28992, agr = "constant")

plot(meuse_sf["zinc"], pch = 16, cex = 1.5, main = "",
     breaks = "quantile", key.pos = 4, reset = FALSE)

plot(meuse_riv, col = "lightblue", add = TRUE)

plot(st_geometry(meuse_grid), pch = 3, cex = 0.2, col = "lightgray", add = TRUE)


help(aquifer.RData)

###############################################################################################
#### 2.2.1 Sistemas de referencia de coordenadas ####

#Lo primero que se recalca es entender la diferencia entre 
#coordenadas geodesicas y proyectadas

# Como identificar el CRS  de un shapefile
sf::st_crs(shp)


################################################################################################
#### 2.3 Representación de datos espaciales ####

library(viridis)
plot(x = nc[c("SID74", "SID79" ,"BIR79")],max.plot = 2
     , pal = viridis, border = 'grey70', logz = TRUE, 
     breaks = seq(0, 4, len = 10), at = c(0, 0.5, 1, 1.5, 2,2.5,3,3.5,4), 
     key.pos = 1, key.width = lcm(1.2), key.length = 0.8) 

help(viridis)

# Creacon de mapas tematico usando tmap

install.packages("tmap")
library(tmap)

#los datos los ponemos en tm_shape , pero la secuancia mediante la cual se optendran los
#colores se saca basandose en el numero de clases que tenga 


tm_shape(nc) + tm_polygons("SID79")
tmap_mode("view")#con view se activa el modo interactivo
tmap_last()

tm_shape(shp) + tm_polygons("longitude")
tmap_mode(mode = "plot") #aqui se activa el modo de mapas interactivos
tmap_last() #esta linea solo se usa si necesitamos reactivar las ultimas modificaciones o cracteriscias del mapa

# Esta es una version mejorada para crear mapas interactivos
library(mapview)
mapview(nc, zcol = "SID79") # solo necesita los datos shape , la columna con la que se establecera la paleta de colores, y dentro de este programa ya se establecen los datos extras de la tabla de atributos de la misma


################################################################################################
#### 2.4 Operaciones con datos espaciales ####


dir <- system.file("shape", package="sf")
list.files(dir, pattern="^[nc]")

# ESRI Shapefile, consta de por lo menos de 3 ficheros, el principal .shp
file <- paste0(dir, "/nc.shp") #con esto puedo saber donde se guarda los shp
file #Esto tambien se puede usar en teoria cuando se necesite ingresar la direccion de un archivo

nc_sf <- st_read(file)

drivers <- st_drivers()
str(drivers)
