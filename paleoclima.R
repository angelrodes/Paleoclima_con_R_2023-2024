
#### PRÁCTICA DE PALEOCLIMA CON R
# En esta práctica vamos a construir una herramienta nueva para "detectar" paleoglaciares ibéricos
# (lugares que fueron ocupados por glaciares en el pasado) durante el Plesitoceno superior y el Holoceno.
# Para ello, debemos recordar la definición de glaciar, y cuales son los factores que condicionan la existencia de estos.
# Discutir la distribución de glaciares en el mundo (https://eoimages.gsfc.nasa.gov/images/imagerecords/83000/83918/global_glaciers_rgi_lrg.jpg),
# la influencia de la latitud, altitud, precipitación, temperatura, viento, etc.
# ¿Cuales son los principales factores que condicionan la aparición, o no, de un glaciar en un lugar concreto?

## PAQUETES QUE NECESTIAREMOS
# install.packages(c("terra","sf","giscoR","tidyverse","tidyterra"))
library(terra)
library(sf)
library(tidyverse)
library(giscoR)
library(tidyterra)

## LOCALIZAR LOS GLACIARES europeos

# Descargar base de datos de masas de hielo
# https://nsidc.org/data/glacier_inventory/query.html
# Latitud de 35N a 72N
# Longitud de -11E (10W) a 39E
# Formato: CSV
# Movel el archivo "glacier_inventory_query.csv" al directorio de trabajo 
# y renombrarlo a "glaciares_europeos.csv"
# Cargar los datos
glaciares<-read_csv("glaciares_europeos.csv") 
# Esto es loq ue hemos importado:
glaciares
# La última columna (primary_class) indica qué tipo de masa de hielo es.
# Consultar págs 9-10 de la documentación: https://nsidc.org/sites/default/files/g01130-v001-userguide_1_0.pdf
# En esta base de datos, ¿son todo realmente glaciares?

# Crear referencias de tipos de hielo basadas en los valores para primary_class
tipos_hielo<-as.character(glaciares$primary_class) %>%
  str_replace_all(c('0'  = 'Varios',
                    '1' = 'Casquete glaciar continental',
                    '2' = 'Banquisa',
                    '3' = 'Glaciar de escape',
                    '4' = 'Glaciar de escape',
                    '5' = 'Glaciar de valle',
                    '6' = 'Glaciar de montaña',
                    '7' = 'Nicho de nivación',
                    '8' = 'Plataforma de hielo',
                    '9' = 'Glaciar rocoso'))  %>%
  as.vector()

# Filtramos los que no son glaciares de verdad
filter(glaciares,primary_class<7) -> glaciares
# (Podemos usar el asignador al rev'es!)
tipos_hielo<-as.character(glaciares$primary_class) %>%
  str_replace_all(c('0'  = 'Varios',
                    '1' = 'Casquete glaciar continental',
                    '2' = 'Banquisa',
                    '3' = 'Glaciar de escape',
                    '4' = 'Glaciar de escape',
                    '5' = 'Glaciar de valle',
                    '6' = 'Glaciar de montaña'))  %>%
  as.vector()

# Representar masas de hielo en un "mapa":
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  geom_point(aes(colour = tipos_hielo))    

## DISTRIBUCIONES DE PRECIPITACI'ON Y TEMPERATURA

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


# Comparar la distribución de las masas de hielo con la temperatura media
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = temp_mean)+
  geom_point(aes(colour = tipos_hielo)) 
  
# Comparar la distribución de las masas de hielo con la precipitación media
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prec_mean)+
  geom_point(aes(colour = tipos_hielo)) 

# Para intentar entender dónde aparecen glaciers, vamos a buscar con qué condiciones de humedad y temperatura encontramos glaciares actualemente.

# Extraer precipitaciones del mapa para las coordinadas donde tenemos glaciares
precip_media<-terra::extract(prec_mean,
        as.data.frame(select(glaciares, lon, lat)))$mean

# Extraer temperaturas del mapa para las coordinadas donde tenemos glaciares
temp_media<-terra::extract(temp_mean,
                                 as.data.frame(select(glaciares, lon, lat)))$mean
             
# Añadir estos datos a nuestra base de datos de glaciares
glaciares<-add_column(glaciares,temp_media,precip_media)

# Nos deshacemos de los glaciares en los que no se ha podido extraer temp_media por estar en el mar (banquisas del noroeste)
filter(glaciares,!is.na(temp_media)) -> glaciares
tipos_hielo<-as.character(glaciares$primary_class) %>%
  str_replace_all(c('0'  = 'Varios',
                    '1' = 'Casquete glaciar continental',
                    '2' = 'Banquisa',
                    '3' = 'Glaciar de escape',
                    '4' = 'Glaciar de escape',
                    '5' = 'Glaciar de valle',
                    '6' = 'Glaciar de montaña'))  %>%
  as.vector()

# Comprobamos que está bien
glaciares

# Representa los valores de precipitación y temperatura correspondientes a la posición de los "glaciares"
ggplot(data = glaciares,                      
       aes(glaciares$temp_media,glaciares$precip_media)) +  
  geom_point(aes(colour = tipos_hielo))

## CREAR NUESTRO MODELO DE "PREDICCI'ON DE GALCIARES"

# ¿En que combinación de condiciones de precipitación (m'inima) y temperatura (máxima) hay glaciares?
max_temp<-max(glaciares$temp_media)
min_precip<-min(glaciares$precip_media)

# ¡Construimos nuestro buscador de glaciares!
prediccion_actual<-prec_mean$mean>min_precip & temp_mean<max_temp
este_ano<-as.numeric(format(Sys.time(), "%Y"))
plot(prediccion_actual,main=este_ano) # en el tí­tulo ponemos el año al que corresponde la predicción

# Es realista?
# Quizas estemos sobreestimando la cantidad de glaciares 
# debido a la aproximaci'on de condiciones en areas de unos 100 km2?
# Vamos a redefinir los l'imites:
ggplot(data = glaciares,                      
       aes(glaciares$temp_media,glaciares$precip_media)) +  
  geom_point(aes(colour = tipos_hielo))

# Algo as'i?
max_temp<-quantile(glaciares$temp_media,0.99)
min_precip<-quantile(glaciares$precip_media,0.01)
# Ignorar el 1% m'as alto o m'as bajo de los datos es
# una manera tosca, pero efectiva, de eliminar "datoa at'ipicos" (outliers)

# ¡Re-construimos nuestro buscador de glaciares!
prediccion_actual<-prec_mean$mean>min_precip & temp_mean<max_temp
este_ano<-as.numeric(format(Sys.time(), "%Y"))
plot(prediccion_actual,main=este_ano) # en el título ponemos el año al que corresponde la predicción

# Comparamos con glaciares reales

ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prediccion_actual)+
  geom_point(alpha=0.1, size=0.1, shape=1)

# Vamos a ver el Pirineo y los Alpes
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prediccion_actual)+
  geom_point(alpha=0.1, size=0.1, shape=1)+
  xlim(-1, 16)+ylim(42,48)

# Solo los Alpes
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prediccion_actual)+
  geom_point(alpha=0.5, size=0.1, shape=1)+
  xlim(5, 16)+ylim(43,48)

# Solo el Pirineo
ggplot(data = glaciares,                      
       aes(glaciares$lon,glaciares$lat)) +  
  stat_spatraster(data = prediccion_actual)+
  geom_point(alpha=0.5, size=0.1, shape=1)+
  xlim(-3, 4)+ylim(40,44)

# ¿Se predicen correctamente los lugares donde encontramos glaciares?
# ¿Se predicen correctamente los lugares donde NO encontramos glaciares?
# ¿Qué utilidad tiene este modelo?

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
# que se correponden con los "Años BP" y la variación media de la temperatura en la Tierra 
rango_edad<-datos_nature[2:121,1]
delta_temp<-datos_nature[2:121,7]

# Como las edades se dan en un rango, calcularemos la media del rango,
# y el año en la referencia de "Era Común", ya que BP significa "antes de 1950".
# Para ello, primero declaramos unos vectores vací­os...
annos <- vector('numeric', nrow(rango_edad))
variacion_temp<-vector('numeric', nrow(delta_temp))
# ...y luego hacemos los cálculos en un bucle:
for (n in 1:nrow(rango_edad)) {
  rango_inf<-as.numeric(gsub("([0-9]+)-([0-9]+)", "\\1",rango_edad[n,]))
  rango_sup<-as.numeric(gsub("([0-9]+)-([0-9]+)", "\\2",rango_edad[n,]))
  annos[n]<-1950-(rango_inf+rango_sup)/2
  variacion_temp[n]<-as.numeric(delta_temp[n,])
}

# Esta es la curva de [ tiempo - varaici'on de temperatura ] que vamos a usar
plot(annos,variacion_temp)

# Ya tenemos las variables annos (años) y la variación de temperatura correspondiente a esos años (variacion_temp).
# Ahora solo necesitamos variar la temperatura en nuesro modelo para generar "mapas" de predicción de glaciares para esto años.
for (n in 1:nrow(rango_edad)) {
  prediccion<-prec_mean$mean>min_precip & temp_mean+variacion_temp[n]<max_temp
  plot(prediccion,main=annos[n])
}
# y finalmente le decimos a R que deje de guardar archivos
dev.off()
# Para generar una animaci'on "bonita" podriamos usar la librer'ia "gganimate", que funciona bien con ggplot2

# Ahora repetimos los mismo, pero guardando los gráficos en archivos:
# Para ello, usaremos la función "jpeg" para guardar archivos de todas nuestras predicciones en nuestro directorio de trabajo
jpeg(file = "Prediccion_%d.jpeg")
# Luego creamos un monton de figuras en un bucle (que se guardarán como jpeg)
for (n in 1:nrow(rango_edad)) {
  prediccion<-prec_mean$mean>min_precip & temp_mean+variacion_temp[n]<max_temp
  plot(prediccion,main=annos[n])
}
# y finalmente le decimos a R que deje de guardar archivos
dev.off()
# Para generar una animaci'on "bonita" podriamos usar la librer'ia "gganimate", que funciona bien con ggplot2

# Repetimos, pero solo para Iberia
jpeg(file = "Prediccion_Iberia_%d.jpeg")
for (n in 1:nrow(rango_edad)) {
  prediccion<-prec_mean$mean>min_precip & temp_mean<max_temp-variacion_temp[n]
  plot(prediccion,main=annos[n], xlim=c(-10, 4),ylim=c(36,44))
}
dev.off()

# Según estas predicciones:
# ¿Demuestra este modelo que hubo paleoglaciares en Galicia?
# ¿En que otros lugares de Iberia pudo haber glaciares en los últimos 22.000 años?
# ¿Cuanto hace, como m'inimo, que en Galicia se dieron las condiciones necesarias para albergar glaciares?
# En los últimos 24.000 años, ¿Cuándo se dieron las condiciones más favorables para el glaciarismo ibérico?
# ¿Qué se podría mejorar en este modelo de predicción de glaciares?
