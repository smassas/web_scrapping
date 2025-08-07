library(rvest)
library(tidyverse)
library(robotstxt)
library(RSelenium)
library(netstat)

driver <- rsDriver(browser = "firefox",
                   verbose = FALSE,
                   port = free_port())

remDr <- driver[["client"]]

url <- "https://www.latercera.com/etiqueta/boric/"
remDr$navigate(url)
Sys.sleep(3)

titulos_total <- c()
enlaces_total <- c()
descripcion_total <- c()
noticias_lista <- list()
bloque <- 1

while (TRUE) {
  Sys.sleep(5)
  
  # Leer HTML completo de la página actual
  page_source <- remDr$getPageSource()[[1]]
  pagina <- read_html(page_source)
  
  # Extraer todas las tarjetas visibles dentro del contenedor principal
  tarjetas <- pagina %>%
    html_node(".result-list") %>%
    html_nodes(".story-card")
  
  # Extraer contenido de cada tarjeta
  df_actual <- purrr::map_dfr(tarjetas, function(t) {
    titulo <- t %>% html_node(".story-card__headline a") %>% html_text(trim = TRUE)
    enlace <- t %>% html_node(".story-card__headline a") %>% html_attr("href")
    descripcion <- t %>% html_node(".story-card__description a") %>% html_text(trim = TRUE)
    
    if (!is.na(enlace) && !grepl("^https?://", enlace)) {
      enlace <- paste0("https://www.latercera.com", enlace)
    }
    
    if (!is.na(titulo) && !is.na(enlace)) {
      tibble(
        titulo = titulo,
        enlace = enlace,
        descripcion = ifelse(is.null(descripcion), NA, descripcion)
      )
    } else {
      NULL
    }
  }) %>% distinct(enlace, .keep_all = TRUE)
  
  # Filtrar noticias nuevas comparando contra acumulado
  df_nuevas <- df_actual %>%
    filter(!enlace %in% enlaces_total)
  
  if (nrow(df_nuevas) == 0) {
    message("No se encontraron noticias nuevas, pero hay posibilidad de más. Intentando nuevamente...")
  } else {
    # Agregar a los vectores acumulados
    titulos_total <- c(titulos_total, df_nuevas$titulo)
    enlaces_total <- c(enlaces_total, df_nuevas$enlace)
    descripcion_total <- c(descripcion_total, df_nuevas$descripcion)
    
    noticias <- tibble(
      id = paste0("id_", seq_along(titulos_total)),
      titulo = titulos_total,
      enlace = enlaces_total,
      descripcion = descripcion_total
    )
    
    noticias_lista[[bloque]] <- noticias
    cat("Bloque", bloque, "- Nuevas encontradas:", nrow(df_nuevas), "- Total acumulado:", nrow(noticias), "\n")
    bloque <- bloque + 1
  }
  
  # Hacer clic en "Ver más"
  Sys.sleep(5)
  tryCatch({
    ver_mas_button <- remDr$findElement(using = "css selector", ".result-list__see-more")
    ver_mas_button$clickElement()
  }, error = function(e) {
    message("No se pudo hacer clic en 'Ver más'. Probablemente no hay más noticias.")
    break
  })
}