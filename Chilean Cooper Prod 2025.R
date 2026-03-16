library(tidyverse)
library(chilemapas)
library(readxl)
library(sf)
library(RColorBrewer) # Para paletas neutrales
library(ggrepel)
library(cowplot)
library(magick)

# 1. Cargar la data consolidada de Cochilco y nuestras coordenadas
data_raw <- read_excel("Chile_Cooper_Prod.xls", skip = 2) %>%
  pivot_longer(cols=-1, names_to='yacimiento', values_to='prod') %>%
  mutate(Code = yacimiento |> recode_values(
            "Codelco - Gaby" ~ "GM",
            "Codelco - Salvador" ~"SALV",
            "Codelco - Chuqui, RT y MH" ~ "COD_CHUQ",
            "Escondida" ~ "ESCOND",                                        
            "Collahuasi" ~ "COLLAH",
            "Zaldivar" ~ "ZALD",
            "El Abra" ~ "ABRA",
            "Candelaria" ~ "CANDEL",
            "Cerro Colorado" ~ "CC",
            "El Tesoro" ~ "CENT",
            "Quebrada Blanca" ~ "QB",
            "Lomas Bayas" ~ "LOM_BAY",
            "Esperanza" ~ "CENT", 
            "Spence" ~ "SPENCE")) %>%
  filter(!is.na(Code))


minner <- read_excel("Cooper_Minners.xlsx") %>%
  st_as_sf(coords = c("Longitud", "Latitud"), 
    crs = 4326)  # Sistema WGS84

#Mapa norte Chile
# Obtenemos el mapa de todas las regiones y filtramos
mapa_norte <- generar_regiones() %>% 
  filter(codigo_region %in% c("01","02","03"))

plt1 <- ggplot() +
  #Layout 1: Mapa base en gris muy claro
  geom_sf(data = mapa_norte, fill = "#f9f9f9", color = "grey60") +
  #Layout 2: Puntos con colores neutrales
  geom_sf(data = minner, aes(fill = Operador), shape = 21, size = 2, 
          color = "white", alpha = 0.7) +
  #Layout 3: Etiquetas con ggrepel para evitar solapamiento
  geom_text_repel(
    data = minner,
    aes(label = Name, geometry = geometry),
    stat = "sf_coordinates",
    size = 2,
    fontface = "bold",
    color = "#444444",      # Gris oscuro en lugar de negro puro
    box.padding = 0.6,      # Mayor separación entre textos
    point.padding = 0.4,
    min.segment.length = 0, # Siempre dibuja la línea si se aleja
    segment.color = "grey70"
  ) +
  
  # Ajuste de ancho de ejes (más espacio a los costados)
  coord_sf(xlim = c(-74, -66)) + 
  
  # Escala de colores neutrales
  scale_fill_brewer(palette = "Paired") + 
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    axis.text = element_text(size = 8, color = "grey60"),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey30", face = "italic")
  ) +
  labs(
    title = "Operaciones Mineras Zona Norte - Chile",
    subtitle = "Ubicación principales yacimientos de cobre",
    fill = "Operadores",
    x = "Longitud", 
    y = "Latitud",
    caption = "Fuente: Elaboración propia."
  )

##Calcular nuevos puntos en Codelco CHUQ (CHUQ, MH, RT) y Centinela (TESOR, ESPZ)
cent_code <- minner %>%
  group_by(Code) %>% 
  summarise(geometry = st_union(geometry)) %>% # Une los puntos que comparten el mismo código
  st_centroid()                                 # Calcula el punto central geográfico

# 2. Ver el resultado
minner_df <- minner %>% st_drop_geometry() %>% select(Code, Operador, Región) %>%
  distinct() %>% left_join(cent_code) %>% st_as_sf()

###New chart
plt2 <- ggplot() +
  #Layout 1: Mapa base en gris muy claro
  geom_sf(data = mapa_norte, fill = "#f9f9f9", color = "grey60") +
  #Layout 2: Puntos con colores neutrales
  geom_sf(data = minner_df, aes(fill = Operador), shape = 21, size = 2, 
          color = "white", alpha = 0.7) +
  #Layout 3: Etiquetas con ggrepel para evitar solapamiento
  geom_text_repel(
    data = minner_df,
    aes(label = Code, geometry = geometry),
    stat = "sf_coordinates",
    size = 2,
    fontface = "bold",
    color = "#444444",      # Gris oscuro en lugar de negro puro
    box.padding = 0.6,      # Mayor separación entre textos
    point.padding = 0.4,
    min.segment.length = 0, # Siempre dibuja la línea si se aleja
    segment.color = "grey70"
  ) +
  
  # Ajuste de ancho de ejes (más espacio a los costados)
  coord_sf(xlim = c(-74, -66)) + 
  
  # Escala de colores neutrales
  scale_fill_brewer(palette = "Paired") + 
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    axis.text = element_text(size = 8, color = "grey60"),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey30", face = "italic")
  ) +
  labs(
    title = "Operaciones Mineras Zona Norte - Chile",
    subtitle = "Ubicación principales yacimientos de cobre",
    fill = "Operadores",
    x = "Longitud", 
    y = "Latitud",
    caption = "Fuente: Elaboración propia. \nNota: COD_CHUQ incluye CHUQ, RT y MH. \nNota: CENT incluye ESPEZ y TESOR"
  )

###merge with production
df <- data_raw %>% group_by(Period, Code) %>% summarize(prod=sum(prod)) %>%
  left_join(minner_df) %>% st_as_sf()

df_2025 <- df %>% filter(Period==2025) %>% 
  arrange(desc(prod)) %>% st_as_sf()

plt3 <-  ggplot() +
  #Layout 1: Mapa base en gris muy claro
  geom_sf(data = mapa_norte, fill = "#f9f9f9", color = "grey20") +
  #Layout 2: Puntos con colores neutrales
  geom_sf(data = df_2025, aes(size = prod, fill = Operador), shape = 21, 
          color = "grey90", alpha = 0.7) +
  #Layout 3: Etiquetas con ggrepel para evitar solapamiento
  geom_text_repel(
    data = df_2025,
    aes(label = Code, geometry = geometry),
    stat = "sf_coordinates",
    size = 2,
    fontface = "bold",
    color = "#444444",      # Gris oscuro en lugar de negro puro
    box.padding = 0.6,      # Mayor separación entre textos
    point.padding = 0.4,
    min.segment.length = 0, # Siempre dibuja la línea si se aleja
    segment.color = "grey70"
  ) +
  
  # Ajuste de ancho de ejes (más espacio a los costados)
  coord_sf(xlim = c(-74, -66)) + 
  scale_size_continuous(range = c(2, 9), name = "Producción año 2025 (kT)") +
  # Escala de colores neutrales
  scale_fill_brewer(palette = "Paired") + 
  
  guides(size = guide_legend(override.aes = list(color = "white", fill = "grey60")), 
         fill = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
  
           theme_minimal() +
  theme(
    panel.background = element_rect(
      fill = "white", color = NA),
    legend.position = "right",
    axis.text = element_text(size = 10, color = "grey50"),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey30", face = "italic")
  ) +
  labs(
    title = "Operaciones Mineras Zona Norte - Chile",
    subtitle = "Producción Cu año 2025 (kT / miles de toneladas métricas)",
    fill = "Operadores",
    caption = "Fuente: Elaboración propia basada en datos de Cochilco. \nNota: COD_CHUQ incluye CHUQ, RT y MH.",
    x = "Longitud", 
    y = "Latitud"
  )

#Insert watermarks

watermark <- magick::image_read("github_sign.png") %>%
  magick::image_colorize(opacity = 70, color = "white") 

cfg_watermark <- draw_image(watermark, 
  x = 0.8,  y = 0.07,
  scale = 0.13,
  hjust = 0.52, vjust = 0.48
)

plt1_a <- ggdraw(plt1) + cfg_watermark
plt2_a <- ggdraw(plt2) + cfg_watermark
plt3_a <- ggdraw(plt3) + cfg_watermark

#Guardar graficos
ggsave(filename = "plt1.jpg", plot = plt1_a, bg = "gray95", width = 8, height = 6)
ggsave(filename = "plt2.jpg", plot = plt2_a, bg = "gray95", width = 8, height = 6)
ggsave(filename = "plt3.jpg", plot = plt3_a, bg = "gray95", width = 8, height = 6)