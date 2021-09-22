
# Web scraping La Tercera -------------------------------------------------

# Mostrar como extraer textos de un sitio web. Crear una función para escalar procedimiento.
# Analizar palabras frecuentes y temas principales.

library(rvest)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(tidytext)
library(topicmodels)
library(glue)
library(readr)
library(robotstxt)

# ver si podemos escrapear la página.
robotstxt::get_robotstxt("https://www.latercera.com/etiqueta/tecnologia/page/1")

# Hacer una prueba que nos permita extraer info de una página. Si nos resulta, escalamos para el resto.
enlace_p1 <- read_html("https://www.latercera.com/etiqueta/tecnologia/page/1")

# Ver en qué etiqueta está la info que nos interesa.
texto_titulares <- enlace_p1 %>% 
  html_nodes("h3") %>% 
  html_text(trim = TRUE) %>% #indicamos que queremos solo el texto.
  .[-1] #sacar primer elemento del vector. El . indica que viene de lo anterior.

# Lo que nos interesa sería tener los enlaces, para tener la lista de todas las páginas que queremos escrapear. 
enlace_titulares <- enlace_p1 %>% 
  html_nodes("h3 > a") %>% #dame, del elemento h3, la etiqueta a.
  html_attr("href") #rescatar el atributo. La url de cada elemento.

# separar, en el caso del texto y completar el enlace.
tibble(titular = texto_titulares, 
       enlace_noticia = enlace_titulares) %>% 
  separate(col = titular, 
           into = c("seccion", "titular"),
           sep = "  ", 
           extra = "merge") %>% #encuentra un doble espacio. Todo lo que queda despues, dejalo en la misma columna
  mutate(enlace_noticia = paste0("https://www.latercera.com", enlace_noticia))

#Convertir todo esto en una funcion
obtener_titulares <- function(numero_pagina) {
  Sys.sleep(5)
  enlace <- paste0("https://www.latercera.com/etiqueta/tecnologia/page/",
                   numero_pagina)
  html <- read_html(enlace)
  
  texto_titulares <- html %>% 
    html_nodes("h3") %>% 
    html_text(trim = TRUE) %>% 
    .[-1] 
  
  enlace_titulares <- html %>% 
    html_nodes("h3 > a") %>% 
    html_attr("href") 
  
  tibble(titular = texto_titulares, 
         enlace_noticia = enlace_titulares) %>% 
    separate(col = titular, 
             into = c("seccion", "titular"),
             sep = "  ", 
             extra = "merge") %>% #encuentra un doble espacio. Todo lo que queda despues, dejalo en la misma columna
    mutate(enlace_noticia = paste0("https://www.latercera.com", enlace_noticia))
  
  
}

obtener_titulares(2)

map(2:4, obtener_titulares) 
map_df(2:4, obtener_titulares)
lapply(2:4, obtener_titulares)

for (i in 1:5) {
  x <- obtener_titulares(i)
}

# importar los titulares
titulares_tecnologia <- read_csv("https://bit.ly/titulares-tecnologia")
hoy <- today()
write_csv(titulares_tecnologia, paste0("titulares_tecnologia_", hoy, ".csv"))
write_csv(titulares_tecnologia, glue("titulares_tecnologia_{today()}.csv")) #otra opcion

# busquemos cuáles son los bigramas más frecuentes:
algunas_stopwords <- readr::read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
mas_stopwords <- tibble(palabra = c("va", "tiene", "ser", "puede", "puede", "dice",
                                    "hacer", "hace")) #sacar verbos auxiliares.

# verbos auxiliares: 
# no tienen un contenido que es sobre la accion del verbo, sino que nos ayuda a conjugarlo.
# no tienen un significado en si mismo. Marcan el tiempo verbal.
# He comido. El "He" no dice nada sobre la acción comer.

las_stopswords <- bind_rows(algunas_stopwords, mas_stopwords)

titulares_tecnologia %>% 
  unnest_tokens(input = titular,
                output = palabra,
                token = "ngrams", #secuencia de n palabras
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra, 
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% las_stopswords$palabra) %>% 
  filter(!palabra_2 %in% las_stopswords$palabra) 
#filtrar todas las palabras que no estén dentro de nuestra lista de stopwords

