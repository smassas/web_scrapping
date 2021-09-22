# install.packages("emayili")
# install.packages("RSelenium")

# Librerías ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(rvest)
library(emayili)
library(RSelenium) # Librería para hacer scraping de forma dinámica

# Página web --------------------------------------------------------------

pagina <- rvest::read_html("https://www.mercadolibre.cl/ofertas?car=MLC1051&page=1")

pagina %>% 
  rvest::html_nodes(xpath = "//li[@class = 'andes-pagination__button']") %>% 
  rvest::html_attr("tabindex") %>% 
  as.numeric() %>% 
  max()

# Con xpath le doy el nombre de la caja y luego el atributo con las comillas.
# e.g: //nombre_caja[@class = 'atributo']
# '' no es lo mismo que " ", en el caso del scraping.

# Extraer precio producto -------------------------------------------------

pagina %>% 
  rvest::html_nodes(xpath = "//div[@class = 'promotion-item__description']/div[2]/span") %>% 
  rvest::html_text2()

# si nos queremos mover a algo más interno. Ejemplo: extraer solo el precio,
# utilizaré el /, para moverme a una caja más interna.

# Extraer marca ----------------------------------------------------------

pagina %>% 
  rvest::html_node(xpath = "//*[@id = 'root-app']/div/section[2]/div/div[2]/
                    div/ol/li[1]/a/div/div/p") %>% 
  rvest::html_text2()

# También podemos recurrir directamente a copiar XPATH en la inspección de elementos 
# en una página web. No obstante, no se recomienda, ya que la página puede cambiar.




