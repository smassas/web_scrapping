library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)

# Asignar URL -------------------------------------------------------------

url_departamentos <- "https://www.portalinmobiliario.com/arriendo/departamento/propiedades-usadas/metropolitana#filter"

# Robot txt ---------------------------------------------------------------

robotstxt::paths_allowed(url_departamentos)
rvest::session(url_departamentos)

# Código HTML -------------------------------------------------------------

pag_web <- rvest::read_html(url_departamentos)

# Clases -------------------------------------------------------------------

pag_web %>% 
  rvest::html_nodes("*") %>% 
  rvest::html_attr("class") %>% 
  base::unique()

# Rescate precio (50 departamentos)

precio <- pag_web %>% 
  rvest::html_nodes(".price-tag-fraction") %>% 
  rvest::html_text(trim = TRUE)

# Rescate dormitorios y metros cuadrados

metros_cuadrados <- pag_web %>% 
  rvest::html_nodes(".ui-search-card-attributes__attribute") %>% 
  rvest::html_text() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(stringr::str_detect(value, "m²")) %>% 
  dplyr::rename(mt2 = "value") 

dormitorios <- pag_web %>% 
  rvest::html_nodes(".ui-search-card-attributes__attribute") %>% 
  rvest::html_text() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(grepl("dormitori[o|os]", value))

# Rescate ubicación

ubicacion <- pag_web %>% 
  rvest::html_nodes(".ui-search-item__location") %>% 
  rvest::html_text()

# Rescate descripción genérica

descripcion <- pag_web %>% 
  rvest::html_nodes(".ui-search-item__subtitle") %>% 
  rvest::html_text()

# Construir tibble --------------------------------------------------------

arriendos <- tibble::tibble(precio,
               dormitorios,
               metros_cuadrados,
               ubicacion,
               descripcion)

# Limpiar base  -----------------------------------------------------------

arriendos %>% 
  dplyr::mutate(precio = as.numeric(stringr::str_remove_all(precio, "\\.")),
                mt2 = as.numeric(stringr::str_remove_all(mt2, "\\s\\w*\\D")),
                comuna = stringr::str_extract(ubicacion, "\\w*$"),
                ubicacion = stringr::str_remove(ubicacion, ", \\w+$"),
                ubicacion = stringr::str_extract(ubicacion, "^[^\\)]*\\,"),
                ubicacion = gsub(",", ".", ubicacion),
                fecha = lubridate::today(),
                value = as.integer(str_remove(value, "\\s\\w*"))) %>% 
  dplyr::rename(dormitorios = value,
         precio_departamento = precio,
         direcion = ubicacion, 
         categoria = descripcion,
         comuna = comuna,
         fecha_scrap = fecha) 

# Cálculo UF --------------------------------------------------------------

UF_hoy <- function() {
  valor_uf <- rvest::html_session("https://valoruf.cl") %>% 
    rvest::html_nodes(".vpr") %>% 
    rvest::html_text() %>%
    stringr::str_remove("\\.") %>%
    stringr::str_replace(",", ".") %>%
    readr::parse_number()
  
  if (is.numeric(valor_uf) & valor_uf > 10000) {
    valor_uf <- valor_uf
  } else {
    valor_uf <- rvest::html_session("https://www.uf-hoy.com") %>%
      rvest::html_nodes("#valor_uf") %>% 
      rvest::html_text() %>%
      stringr::str_remove("\\.") %>%
      stringr::str_replace(",", ".") %>%
      readr::parse_number()
  }
  return(valor_uf)
}





