source('global.R')
source('querys/georreferenciamento_rfb_ms_to_ba.R')
library(sf)

end_rfb <- dbGetQuery(
  con,
  georreferenciamento_rfb_ms_to_ba
)

# Ajustes das variáveis para geolocalizar melhor
end_rfb <- end_rfb |> 
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
  municipio = "descricao_ibge",
  estado = "uf"
)


# Segundo passo: geolocalizar
end_rfb_temp <- geocodebr::geocode(
  enderecos = end_rfb,
  campos_endereco = campos,
  resultado_completo = FALSE,
  resolver_empates = FALSE,
  resultado_sf = FALSE,
  verboso = FALSE,
  cache = TRUE,
  n_cores = 3
)

# Selecionando as certezas
end_rfb_temp <- end_rfb_temp |> 
  filter(empate == FALSE) |> 
  select(cnpj, lat, lon, tipo_resultado, precisao)

# Converte o data frame para um objeto sf com coluna geométrica
end_rfb_sf <- end_rfb_temp |> 
  filter(!is.na(lat) & !is.na(lon)) |> 
  mutate(lat_rfb = lat, long_rfb = lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



# Escreve no banco como tabela espacial
st_write(
  end_rfb_sf,
  dsn = con,                   # conexão DBI ativa (ex: RPostgres::dbConnect(...))
  layer = "silver_cnpjs_geo",  # nome da tabela
  driver = "PostgreSQL"#,
  #overwrite = TRUE
)
