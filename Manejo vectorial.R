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



line <- st_linestring(x = matrix(numeric(0), 0, 2), dim = "XYZ")
cprov2 <- st_sf(names = c('Coruña (A)'),
               geom = line)

plot(line)
help(st_linestring)
