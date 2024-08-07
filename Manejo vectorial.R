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




