source('global.R')
source('auxiliares/municipios_sicredi.R')

df_08 <- readxl::read_excel('dados/r20250819_silver_associados_pj.xlsx') %>% distinct()

df_08_final <- df_08 |> 
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
  )

  


