library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)

# Asignar URL -------------------------------------------------------------

url_departamentos <- "https://www.portalinmobiliario.com/arriendo/departamento/propiedades-usadas/metropolitana#filter"

# Bloques HTML -------------------------------------------------------------

pag_web <- rvest::read_html(url_departamentos)

# Ver clases -------------------------------------------------------------------

pag_web %>% 
  rvest::html_nodes("*") %>% 
  rvest::html_attr("class") %>% 
  base::unique()

# Rescatar precios --------------------------------------------------------

precio <- pag_web %>% 
  rvest::html_nodes(".poly-component__price") %>% 
  rvest::html_text(trim = TRUE) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(precio = value)

# Rescatar características ------------------------------------------------

caracteristicas <- pag_web %>%
  rvest::html_nodes(".poly-component__attributes-list") %>%
  rvest::html_text() %>%
  tibble::enframe(name = NULL, value = "caracteristicas") %>% 
  dplyr::mutate(extraccion = str_extract_all(caracteristicas, "\\d+"),
                dormitorios = purrr::map_int(extraccion, ~ as.integer(.x[1] %||% NA)),
                baños = purrr::map_int(extraccion, ~ as.integer(.x[2] %||% NA)),
                m2 = purrr::map_int(extraccion, ~ as.integer(.x[3] %||% NA))) %>%
  dplyr::select(-c(extraccion, caracteristicas)) 

# Rescatar ubicación -------------------------------------------------------

ubicacion <- pag_web %>% 
  rvest::html_nodes(".poly-component__location") %>% 
  rvest::html_text() %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(ubicacion = value)

# Rescatar href -----------------------------------------------------------

enlaces <- pag_web %>% 
  rvest::html_elements(".poly-card__content a") %>%  
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(enlace = value)

# Rescate UF --------------------------------------------------------------

calcular_uf_hoy <- function() {
  
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


# Construir tibble --------------------------------------------------------

datos <- dplyr::bind_cols(precio, caracteristicas, ubicacion, enlaces) %>% 
         dplyr::mutate(precio = stringr::str_replace_all(precio, "[\\.,\\$]", ""),
                precio = stringr::str_trim(precio),
                precio_uf = dplyr::case_when(str_detect(precio, "UF") ~ as.numeric(stringr::str_extract(precio, "\\d+")),
                                      TRUE ~ NA),
                precio_uf = as.integer(precio_uf * calcular_uf_hoy()),
                precio_unificado = dplyr::coalesce(as.numeric(precio), precio_uf)) %>% 
         dplyr::select(-c(precio, precio_uf)) %>% 
         dplyr::relocate(precio_unificado, .before = dormitorios) 


# Limpiar datos -----------------------------------------------------------

limpieza_strings <- function(df) {
  
  if (!tibble::is_tibble(df)) {
    stop("El input debe ser un tibble")
  }
  
  df <- df %>%
    dplyr::mutate(across(
      where(is.character),
      ~ stringr::str_trim(stringr::str_to_upper(
        stringi::stri_trans_general(
        stringr::str_to_sentence(str_squish(.)),
          "Latin-ASCII"
        )
      )))
    )
  
  return(df)
  
}

datos <- limpieza_strings(df = datos)


