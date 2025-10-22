source('querys/sicredi_ampliado.R')

# Base tunada com dados da Receita Federal - sicrediampliado.R
receita <- dbGetQuery(
  con,
  query_sicredi_ampliado
)

# Recorte para comparação
receita2 <- receita |> 
  select(cep_rfb,
         cep_associado,
         cd_ibge_rfb,
         status_associado_sicredi,
         estado_agencia, 
         estado_associado,
         municipio_associado,
         municipio_agencia
         ) |> 
  mutate(verificacao = cep_rfb==cep_associado) 

# lista de municipios com mais de 10 associados ativos com cep igual
municipios <- receita2 |>
  group_by(municipio_associado) |>
  mutate(freq_mu = n()) |>
  ungroup() |> 
  filter(
    freq_mu >=10,
    status_associado_sicredi=='ATIVO',
    verificacao==TRUE,
    estado_agencia%in%c('BA', 'TO', 'MS'),
    estado_associado%in%c('BA', 'TO', 'MS')
  ) |>
  select(cd_ibge_rfb) |>
  distinct() |> 
  arrange(cd_ibge_rfb)

for (v in municipios$cd_ibge_rfb) {
  cat("'", v, "',\n", sep = "")
}






