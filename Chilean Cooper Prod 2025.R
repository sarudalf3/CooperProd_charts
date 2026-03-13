library(tidyverse)
library(chilemapas)
library(readxl)
library(sf)
library(RColorBrewer) # Para paletas neutrales
library(ggrepel)

# 1. Cargar la data consolidada de Cochilco y nuestras coordenadas
data_raw <- read_excel("ChileChart/Chile_Cooper_Prod.xls", skip = 2) %>%
  pivot_longer(cols=-1, names_to='yacimiento', values_to='prod') %>%
  mutate(Code = yacimiento |> recode_values(
            "Codelco - Gaby" ~ "GABY",
            "Codelco - Salvador" ~"SALV",
            "Codelco - Chuqui, RT y MH" ~ "COD_CHUQ",
            "Escondida" ~ "ESC",                                        
            "Collahuasi" ~ "COLLAH",
            "Zaldivar" ~ "CMZ",
            "El Abra" ~ "ABRA",
            "Candelaria" ~ "CANDEL",
            "Cerro Colorado" ~ "CC",
            "El Tesoro" ~ "CENT",
            "Quebrada Blanca" ~ "QB",
            "Lomas Bayas" ~ "LB",
            "Esperanza" ~ "CENT", 
            "Spence" ~ "SPENCE")) %>%
  filter(!is.na(Code))


minner <- read_excel("ChileChart/Cooper_Minners.xlsx") %>%
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
    axis.text = element_text(size = 8, color = "grey60")
  ) +
  labs(
    title = "Operaciones Mineras Zona Norte - Chile",
    subtitle = "Ubicación principales yacimientos de cobre",
    fill = "Operadores",
    x = "Longitud", 
    y = "Latitud",
    caption = "Fuente: Elaboración propia."
  )


##Calculate new points in Codelco CHUQ (CHUQ, MH, RT) y Centinela (TESOR, ESPZ)
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
    axis.text = element_text(size = 8, color = "grey60")
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
  scale_size_continuous(range = c(2, 9), name = "Producción año 2025") +
  # Escala de colores neutrales
  scale_fill_brewer(palette = "Paired") + 
  
  guides(size = guide_legend(override.aes = list(color = "white", fill = "grey60")), 
         fill = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
  
           theme_minimal() +
  theme(
    panel.background = element_rect(
      fill = "white", color = NA),
    legend.position = "right",
    axis.text = element_text(size = 10, color = "grey50")
  ) +
  labs(
    title = "Operaciones Mineras Zona Norte - Chile",
    subtitle = "Producción Cu año 2025 (kMT)",
    fill = "Operadores",
    caption = "Fuente: Elaboración propia basada en datos de Cochilco. \nNota: COD_CHUQ incluye CHUQ, RT y MH. \nNota: CENT incluye ESPEZ y TESOR",
    x = "Longitud", 
    y = "Latitud"
  )

ggsave(filename = "ChileChart/plt1.jpg", plot = plt1)
ggsave(filename = "ChileChart/plt2.jpg", plot = plt2)
ggsave(filename = "ChileChart/plt3.jpg", plot = plt3)