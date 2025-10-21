source('global.R')
source('auxiliares/municipios_sicredi.R')

# Base Silver
df_08 <- readxl::read_excel('dados/r20250819_silver_associados_pj.xlsx') %>% distinct()


# Geolocalizacao dos enderecos - selecionando as variáveis
df_08_enderecos <- df_08 |> 
  select(cnpj_associado, endereco, numero, bairro, municipio_y, cep, estado_y)

# Ajustes das variáveis para geolocalizar melhor
df_08_enderecos <- df_08_enderecos |> 
  mutate(
    endereco = str_replace(endereco, "^R ", "RUA "),
    endereco = str_replace(endereco, "^R. ", "RUA "),
    endereco = str_replace(endereco, "^TRAV ", "TRAVESSA "),
    endereco = str_replace(endereco, "^AV ", "AVENIDA "), 
    endereco = str_replace(endereco, "^AV. ", "AVENIDA "), 
    endereco = str_replace(endereco, "^ROD ", "RODOVIA ")
  ) |> 
  mutate(
    numero = if_else(
      numero %in% c("", "S/N", "SN", "0", "SN.", "SNR", "Sn", "sn", "S/N.",
                       "S/NR", "s/n", "S/A", "S.N", "S N", "S / N", "S-N", "s-n",
                       "SEM NM", "SEM NM?", "SEM N?", "s n"),
      NA,
      numero
    )
  )


# Primeiro passo: inidicar o nome das colunas com cada campo dos enderecos
campos <- geocodebr::definir_campos(
  logradouro = "endereco",
  numero = "numero",
  cep = "cep",
  localidade = "bairro",
  municipio = "municipio_y",
  estado = "estado_y"
)


# Segundo passo: geolocalizar
df_08_enderecos <- geocodebr::geocode(
  enderecos = df_08_enderecos,
  campos_endereco = campos,
  resultado_completo = FALSE,
  resolver_empates = FALSE,
  resultado_sf = FALSE,
  verboso = FALSE,
  cache = TRUE,
  n_cores = 3
)

# Selecionando as certezas
df_08_enderecos <- df_08_enderecos |> 
  filter(empate == FALSE) |> 
  select(cnpj_associado, lat, lon, tipo_resultado, precisao)

# Junção com a base original
df_08 <- df_08 |> 
  left_join(df_08_enderecos, by='cnpj_associado')


# Variáveis de interesse - criação
df_08 <- df_08 |> 
  mutate(
    sexo_final = case_when(
      `F` == 50 ~ "Ambos",
      `F` >50 ~"Feminino",
      `F` <50 ~"Masculino",
      TRUE~"Ignorado"
    ),
    
    predominancia = case_when(
      `Mais de 60` >= 50 & `F`==50 ~ "Ambiguo - Silver",
      `Mais de 60` >= 50 & `F`>=51 ~ "Mulheres Silver",
      `Mais de 60` >= 50 & `F`<=51 ~ "Homens Silver",
      TRUE ~ "Classificar"
    ),
    
    idade_classificada = case_when(
      `Mais de 60` >= 50 ~ "Silver",
      `Menos de 50` >= 50 ~ "Não-Silver",
      `Menos de 60` >= 50  ~ "Pré-Silver",
      TRUE ~ "Indefinido"
    ),
    
    # Converter para data/hora corretamente
    assoc_desde = as_date(ymd_hms(assoc_desde)),  # converte para Date
    tempo_relacionamento = interval(assoc_desde, ymd("2025-05-31")) / months(1)
  )




# Upload no servidor com latlong
#dbWriteTable(con, 'sicredi_silver_t', df_08, overwrite = TRUE)




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
