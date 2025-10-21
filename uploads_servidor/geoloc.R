source('global.R')
source('auxiliares/municipios_sicredi.R')

# Base Silver
df_08 <- readxl::read_excel('dados/r20250819_silver_associados_pj.xlsx') %>% distinct()



df_08_enderecos <- df_08 |> 
  select(cnpj_associado, endereco, numero, bairro, municipio_y, cep, estado_y)

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

df_08_enderecos <- df_08_enderecos |> 
  filter(empate == FALSE) |> 
  select(cnpj_associado, lat, lon, tipo_resultado, precisao)


df_08 <- df_08 |> 
  left_join(df_08_enderecos, by='cnpj_associado')


#dbWriteTable(con, 'sicredi_silver_t', df_08, overwrite = TRUE)




library(leaflet)
# Criar paleta baseada na variável "empate"
pal <- colorFactor(
  palette = c("red", "orange", "green", "blue"), # ajuste as cores conforme necessário
  domain = df$empate
)

leaflet(data = df_08 |> 
          group_by(municipio_y) |> 
          mutate(freq_mu = n()) |> 
          ungroup() |> 
          filter(
            freq_mu >= 10,
            !is.na(municipio_y),
            !is.na(estado_y),
            estado_y %in% c("TO", "MS", "BA"),
            estado_x %in% c("TO", "MS", "BA"),
            (`F`|`M` != 0),
            (`Mais de 60` + `Menos de 50` + `Menos de 60`  > 0),
            status_associado=='ATIVO'
          ) |>
          group_by(municipio_x) |> 
          mutate(freq_mu = n()) |> 
          ungroup() |> 
          filter(
            freq_mu >= 10
          ) |> 
          mutate(
            sexo_final = case_when(
              `F` == 50 ~ "Ambos",
              `F` >50 ~"Feminino",
              `F` <50 ~"Masculino",
              TRUE~"Ignorado"
            ),
            
            predominancia = case_when(
              `Mais de 60` >= 50 & `F`==50 ~ "Ambiguo - Idosos",
              `Mais de 60` >= 50 & `F`>=51 ~ "Mulheres idosas",
              `Mais de 60` >= 50 & `F`<=51 ~ "Homens idosos",
              TRUE ~ "Classificar"
            ),
            
            # Converter para data/hora corretamente
            assoc_desde = as_date(ymd_hms(assoc_desde)),  # converte para Date
            tempo_relacionamento = interval(assoc_desde, ymd("2025-05-31")) / months(1)
          ), options = leafletOptions(minZoom = 2)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 0.7, stroke = FALSE, fillOpacity = 0.8,
    #color = ~pal(empate),  # <- cor definida pela variável "empate"
    #popup = ~endereco_encontrado
  ) |>
  addLegend(
    "bottomright", 
    pal = pal, values = ~empate,
    title = "Empate",
    opacity = 1
  ) |>
  addScaleBar(position = "bottomleft")
