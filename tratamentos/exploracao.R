source('global.R')
source('auxiliares/municipios_sicredi.R')

df_08 <- dbGetQuery(con,
                    "SELECT * FROM sicredi_silver_t")

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
  )
  
