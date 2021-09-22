library(rvest)
library(tidyverse)
library(robotstxt)

# Asignar URL -------------------------------------------------------------
url <- "https://www.amazon.com.mx/b/ref=s9_acsd_hfnv_hd_bw_bAcAgpX_ct_x_ct00_w?_encoding=UTF8&node=9725407011&pf_rd_m=AVDBXBAVVSXLQ&pf_rd_s=merchandised-search-4&pf_rd_r=Q47SQG6GBGGECT1SXNZY&pf_rd_t=101&pf_rd_p=8cc61471-874f-5f7f-96d7-e2b634466523&pf_rd_i=9725377011"

# Preguntar a robot si está permitido realizar scrap ----------------------
robotstxt::paths_allowed(paths = c(url))

# Obtener código HTML -----------------------------------------------------
pagina_web <- read_html(url)

# Asignar la clase para rescatar texto de cada aviso --------------------------------------------------------
css_producto <- ".a-text-normal"

# Obtener código HTML que contiene nombre del producto --------------------
producto_html <- html_nodes(pagina_web,css_producto)
producto_texto <- html_text(producto_html, trim = TRUE)

# Mostrar los datos -------------------------------------------------------
producto_texto
length(producto_texto)
tail(producto_texto)

# Limpiar y rescatar descripción del producto --------------------------------------------------

producto_texto <- producto_texto[-1] # 12 productos, 
typeof(producto_texto)

descripcion <- producto_texto[seq(from = 1, to = length(producto_texto), by = 2)]

# Limpiar y rescatar marca del producto -----------------------------------

# Extraer la marca por cada producto (deben ser 12)
marca_producto <- stringr::str_subset(producto_texto, pattern = "^de\\s*\\w+")
marca_producto <- stringr::str_extract_all(producto_texto, pattern = "^de\\s+\\w+")

# Clase para obtener precio por cada producto -----------------------------
css_precio <- "span.a-price .a-offscreen"

# Obtener el contenido de la clase en código html
precio_html <- html_nodes(pagina_web,css_precio)

# Limpiar el código para obtener el texto
precio_texto <- html_text(precio_html)
precio_texto
# Eliminar el signo de peso
precio_limpio <- readr::parse_number(precio_texto) #Eliminar coma y signo peso
rm(precio_texto)

# Opción con stringr
# precio_limpio <- stringr::str_remove_all(precio_texto, "\\$") 

# Opciones con gsub
# precio_limpio <- gsub("\\$","",precio_texto) 
# precio_limpio <- gsub(",","",precio_limpio) 

# Consultar tipo de dato
data.class(precio_limpio)  ## numeric
length(precio_limpio)
tail(precio_limpio)

# Verificar los precios con los productos seleccionados.

precio_limpio
precios_vigentes <- precio_limpio[seq(1, length(precio_limpio), 2)]
precios_vigentes <- precios_vigentes[c(1,2,3,4,NA,5,NA,6,7,8,9,10)]

# Unimos los datos

data_productos <- tibble::as.tibble(data.frame(marca = marca_producto,
                                               descripcion = descripcion,
                                               precios = precios_vigentes))


# Limpiar data de los productos completa

data_productos <- data_productos %>% 
         dplyr::mutate(marca = stringr::str_remove_all(marca, "de\\s*"),
         marca = stringr::str_to_title(marca),
         descripcion = stringr::str_to_sentence(descripcion))

View(data_productos)




