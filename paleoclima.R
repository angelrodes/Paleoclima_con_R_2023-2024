
#### PRíCTICA DE PALEOCLIMA CON R
# En esta práctica vamos a construir una herramienta nueva para "detectar" paleoglaciares ibí©ricos
# (lugares que fueron ocupados por glaciares en el pasado) durante el Plesitoceno superior y el Holoceno.
# Para ello, debemos recordar la definición de glaciar, y cuales son los factores que condicionan la existencia de estos.
# Discutir la distribución de glaciares en el mundo (https://eoimages.gsfc.nasa.gov/images/imagerecords/83000/83918/global_glaciers_rgi_lrg.jpg),
# la influencia de la latitud, altitud, precipitación, temperatura, viento, etc.
# Â¿Cuales son los principales factores que condicionan la aparición, o no, de un glaciar en un lugar concreto?

## PAQUETES QUE NECESTIAREMOS
# install.packages(c("terra","sf","giscoR","tidyverse","tidyterra"))
library(terra)
library(sf)
library(tidyverse)
library(giscoR)
library(tidyterra)

## LOCALIZAR LOS GLACIARES IBíRICOS

# Descargar base de datos de masas de hielo en Iberia
# https://nsidc.org/data/glacier_inventory/query.html
# Latitud de 35N a 45N
# Longitud de -10E (10W) a 5E
# Formato: CSV
# Movel el archivo "glacier_inventory_query.csv" al directorio de trabajo
# Cargar los datos
glaciares<-read_csv("glacier_inventory_query.csv") 
# Esto es loq ue hemos importado:
glaciares
# La íºltima columna (primary_class) indica quí© tipo de masa de hielo es.
# Consultar págs 9-10 de la documentación: https://nsidc.org/sites/default/files/g01130-v001-userguide_1_0.pdf
# En esta base de datos, Â¿son todo realmente glaciares?

# Crear referencias de tipos de hielo basadas en los valores para primary_class
tipos_hielo<-as.character(glaciares$primary_class) %>%
  str_replace_all(c('0'  = 'Varios',
                    '1' = 'Casquete glaciar continental',
                    '2' = 'Banquisa',
                    '3' = 'Glaciar de escape',
                    '4' = 'Glaciar de escape',
                    '5' = 'Glaciar de valle',
                    '6' = 'Glaciar de montaí±a',
                    '7' = 'Nicho de nivación',
                    '8' = 'Plataforma de hielo',
                    '9' = 'Glaciar rocoso'))  %>%
  as.vector()

# Representar masas de hielo en un "mapa":
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  geom_point(aes(colour = tipos_hielo))+
  scale_colour_manual(values=c("#f1c40f", "#FF5733", "#58d68d"))    

## DISTRIBUCIONES DE PRECIPITACIíN Y TEMPERATURA

# Archivos para la práctica de paleoclima: registros de precipitación y temperatura de E-OBS Copernicus (https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php)
# https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_0.1deg_reg_ensemble/rr_ens_mean_0.1deg_reg_v28.0e.nc
# https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_0.1deg_reg_ensemble/tg_ens_mean_0.1deg_reg_v28.0e.nc
temperature <- rast("~/Downloads/tg_ens_mean_0.1deg_reg_v28.0e.nc")
precipitation <- rast("~/Downloads/rr_ens_mean_0.1deg_reg_v28.0e.nc")

# Representar los datos descargados
plot(precipitacion)
plot(temperature)

# Calcular las medias de precipitación y temperatura en toda europa para toda la base de datos
# OJO: esto tarda MUCHO. Mejor cargar medias europeas ya calculadas (unas 10 lí­neas más abajo)
temp_mean<-app(temperature,mean)
prec_mean<-app(precipitation,mean)

# Exporta los mapas de precipitación y temperatura medias de Europa
writeRaster(temp_mean,"temp_mean_europe.tiff")
writeRaster(prec_mean,"prec_mean_europe.tiff")
writeRaster(temp_mean,"temp_mean_europe.nc")
writeRaster(prec_mean,"prec_mean_europe.nc")

# Cargar medias europeas ya calculadas.
# Se pueden descargar de https://github.com/angelrodes/Paleoclima_con_R_2023-2024
temp_mean<-rast("temp_mean_europe.tiff") # temperatura media de todos los datos de E-OBS Copernicus
prec_mean<-rast("prec_mean_europe.tiff") # precipitación mensual media de todos los datos de E-OBS Copernicus
# (Es conveniente leer los comentarios antes de ejecutar las ordenes sin mirar. Ahorrarás tiempo...)

# Dibuja los mapas de precipitación y temperatura medias de Europa
plot(temp_mean)
plot(prec_mean)

# límites de la PIB (Peninsula Iberica y Baleares) 
limites_pib <- gisco_get_countries(country = c("Spain", "Portugal"), resolution = 03)

# Podemos excluir las islas atlanticas, para ello aplicamos la funcion st_crop al objeto creado 
# Hemos de especificar la extension (funcion ext) 
limites_pib <- st_crop(limites_pib, ext(-10, 5, 35, 45))  #xmin(lon min) xmax(lon max) ymin (lat min) ymax (lat max)


# Aplicamos el recorte (crop; paquete terra) a nuestros SpatRaster de los limites establecidos para la PIB.
temperature_pib <- crop(temp_mean, limites_pib)
precipitation_pib <- crop(prec_mean, limites_pib)

# Comprobamos que está bien
plot(temperature_pib)
plot(precipitation_pib)

# Comparar la distribución de las masas de hielo con la temperatura media
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = temperature_pib)+
  geom_point(aes(colour = tipos_hielo)) +
  scale_colour_manual(values=c("#f1c40f", "#FF5733", "#58d68d")) 
  
# Comparar la distribución de las masas de hielo con la precipitación media
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = precipitation_pib)+
  geom_point(aes(colour = tipos_hielo)) +
  scale_colour_manual(values=c("#f1c40f", "#FF5733", "#58d68d")) 

# Para intentar entender dónde aparecen glaciers, vamos a buscar con quí© condiciones de humedad y temperatura encontramos glaciares actualemente.

# Extraer precipitaciones del mapa para las coordinadas donde tenemos glaciares
precip_media<-terra::extract(precipitation_pib,
        as.data.frame(select(glaciares, lon, lat)))$mean

# Extraer temperaturas del mapa para las coordinadas donde tenemos glaciares
temp_media<-terra::extract(temperature_pib,
                                 as.data.frame(select(glaciares, lon, lat)))$mean
             
# Aí±adir estos datos a nuestra base de datos de glaciares
glaciares<-add_column(glaciares,temp_media,precip_media)

# Comprobamos que está bien
glaciares

# Representa los valores de precipitación y temperatura correspondientes a la posición de los "glaciares"
ggplot(data = glaciares,                      
       aes(glaciares$temp_media,glaciares$precip_media)) +  
  geom_point(aes(colour = tipos_hielo,size=tipos_hielo))+
  scale_size_manual(values=c(5, 4, 2))+
  scale_colour_manual(values=c("#f1c40f", "#FF5733", "#58d68d"))    

## CREAR NUESTRO MODELO DE "PREDICCIíN DE GALCIARES"

# Â¿En que combinación de condiciones de precipitación (mí­nima) y temperatura (máxima) hay glaciares?
max_temp<-max(filter(glaciares,primary_class<7)$temp_media)
min_precip<-min(filter(glaciares,primary_class<7)$precip_media)

# Â¡Ya tenemos nuestro buscador de glaciares!
prediccion_actual<-precipitation_pib$mean>min_precip & temperature_pib<max_temp
este_ano<-as.numeric(format(Sys.time(), "%Y"))
plot(prediccion_actual,main=este_ano) # en el tí­tulo ponemos el aí±o al que corresponde la predicción

# Comparamos con glaciares reales

ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prediccion_actual)+
  geom_point(aes(shape = tipos_hielo),alpha=0.5, size=2)+
  scale_shape_manual(values=c(15,17,1))

# Â¿Se predicen correctamente los lugares donde encontramos glaciares?
# Â¿Se predicen correctamente los lugares donde NO encontramos glaciares?
# Â¿Quí© utilidad tiene este modelo?

## VIAJANDO EN EL TIEMPO

# Ahora intentaremos usar nuestro modelo de predicción de glaciares 
# para otros momentos de la historia de la Tierra en los que conococemos 
# las variaciones de temperatura respecto a la temperatura actual.

# Para ello, descargaremos los datos del registro de temperaturas
# que aparece en la Fig. 2 de este artí­culo: https://www.nature.com/articles/s41586-021-03984-4#MOESM3
# El enlace lo encontramos al final del artí­culo como "Source Data Fig. 2"
# (En caso de no tener acceso a Nature, el archivo xlsx se puede descargar del mismo repositorio que los tiff de arriba.)

# Importamos los datos de excel
datos_nature<-readxl::read_excel("41586_2021_3984_MOESM3_ESM.xlsx")

# En realidad solo nos interesan la primera y la septima columna,
# que se correponden con los "Aí±os BP" y la variación media de la temperatura en la Tierra 
rango_edad<-datos_nature[2:121,1]
delta_temp<-datos_nature[2:121,7]

# Como las edades se dan en un rango, calcularemos la media del rango,
# y el aí±o en la referencia de "Era Comíºn", ya que BP significa "antes de 1950".
# Para ello, primero declaramos unos vectores vací­os...
annos <- vector('numeric', nrow(rango_edad))
variacion_temp<-vector('numeric', nrow(rango_edad))
# ...y luego hacemos los cálculos en un bucle:
for (n in 1:nrow(rango_edad)) {
  rango_inf<-as.numeric(gsub("([0-9]+)-([0-9]+)", "\\1",rango_edad[n,]))
  rango_sup<-as.numeric(gsub("([0-9]+)-([0-9]+)", "\\2",rango_edad[n,]))
  annos[n]<-1950-(rango_inf+rango_sup)/2
  variacion_temp[n]<-as.numeric(delta_temp[n,])
}

# Ya tenemos las variables annos (aí±os) y la variación de temperatura correspondiente a esos aí±os (variacion_temp).
# Ahora solo necesitamos variar la temperatura en nuesro modelo para generar "mapas" de predicción de glaciares para esto aí±os.
# Para ello, usaremos la función "jpeg" para guardar archivos de todas nuestras predicciones en nuestro directorio de trabajo
jpeg(file = "Prediccion_%d.jpeg")
# Luego creamos un monton de figuras en un bucle (que se guardarán como jpeg)
for (n in 1:nrow(rango_edad)) {
  prediccion<-precipitation_pib$mean>min_precip & temperature_pib<max_temp-variacion_temp[n]
  plot(prediccion,main=annos[n])
}
# y finalmente le decimos a R que deje de guardar archivos
dev.off()

# Segíºn estas predicciones:
# Â¿Demuestra este modelo que hubo paleoglaciares en Galicia?
# Â¿En que otros lugares de Iberia pudo haber glaciares en los íºltimos 22.000 aí±os?
# Â¿Cuanto hace que en Galicia se dieron las condiciones necesarias para albergar glaciares?
# En los íºltimos 24.000 aí±os, Â¿Cuándo se dieron las condiciones más favorables para el glaciarismo ibí©rico?
# Â¿Quí© se podrí­a mejorar en este modelo de predicción de glaciares?
