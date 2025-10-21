source('global.R')

df_08 <- dbGetQuery(
  con,
  "
  SELECT * FROM sicredi_silver_t
  "
)



library(leaflet)
# Teste de mapa
df_08$idade_classificada <- factor(
  df_08$idade_classificada,
  levels = c("Indefinido", "Não-Silver", "Pré-Silver", "Silver")
)

# Paleta de cores associada a cada categoria
pal <- colorFactor(
  palette = c("gray60", "orange", "deepskyblue", "darkblue"),
  domain = df_08$idade_classificada
)

# Filtrar e preparar os dados
df_filtrado <- df_08 |> 
  group_by(municipio_y) |> 
  mutate(freq_mu = n()) |> 
  ungroup() |> 
  filter(
    freq_mu >= 10,
    !is.na(municipio_y),
    !is.na(estado_y),
    estado_y %in% c("TO", "MS", "BA"),
    estado_x %in% c("TO", "MS", "BA"),
    (`F` | `M` != 0),
    (`Mais de 60` + `Menos de 50` + `Menos de 60` > 0),
    status_associado == 'ATIVO'
  ) |> 
  group_by(municipio_x) |> 
  mutate(freq_mu = n()) |> 
  ungroup() |> 
  filter(freq_mu >= 10) |> 
  mutate(
    sexo_final = case_when(
      `F` == 50 ~ "Ambos",
      `F` > 50 ~ "Feminino",
      `F` < 50 ~ "Masculino",
      TRUE ~ "Ignorado"
    ),
    predominancia = case_when(
      `Mais de 60` >= 50 & `F` == 50 ~ "Ambíguo - Idosos",
      `Mais de 60` >= 50 & `F` >= 51 ~ "Mulheres idosas",
      `Mais de 60` >= 50 & `F` <= 51 ~ "Homens idosos",
      TRUE ~ "Classificar"
    ),
    assoc_desde = as_date(ymd_hms(assoc_desde)),
    tempo_relacionamento = interval(assoc_desde, ymd("2025-05-31")) / months(1)
  )

# Criar mapa base
mapa <- leaflet(options = leafletOptions(minZoom = 2)) |>
  addProviderTiles(providers$CartoDB.Positron)

# Adicionar uma camada (grupo) para cada categoria de idade_classificada
for(cat in levels(df_filtrado$idade_classificada)) {
  mapa <- mapa |> 
    addCircleMarkers(
      data = df_filtrado |> filter(idade_classificada == cat, !is.na(lat)),
      lng = ~lon, lat = ~lat,
      radius = 1.2, stroke = FALSE, fillOpacity = 0.8,
      color = pal(cat),
      group = cat,
      popup = ~paste0(
        "<b>", idade_classificada, "</b><br>",
        "Município: ", municipio_x, "<br>",
        "Estado: ", estado_x, "<br>",
        "Sexo: ", sexo_final
      )
    )
}

# Adicionar controle de camadas (checkboxes)
mapa |>
  addLayersControl(
    overlayGroups = levels(df_filtrado$idade_classificada),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(
    "bottomright",
    pal = pal,
    values = df_filtrado$idade_classificada,
    title = "Faixa Etária Classificada",
    opacity = 1
  )
