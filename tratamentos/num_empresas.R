library(dplyr)
library(lubridate)
df <- readxl::read_excel('dados/r20250819_silver_associados_pj.xlsx') %>% distinct()

df <- df %>% 
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


df_idosos <- df %>%
  filter(
    status_associado=="ATIVO", 
    `Mais de 60`>=50) %>% 
  distinct()3014+
  
  df_idosos %>% vitaltable::tab_1(predominancia)


df_idosos %>%
  mutate(isa=as.character(isa)) %>%
  vitaltable::tab_2(isa, predominancia)


library(ggplot2)

ggplot(df, aes(x = tempo_relacionamento, y = isa)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Dispersão entre Tempo de Relacionamento e ISA",
    x = "Tempo de Relacionamento (meses)",
    y = "ISA"
  ) +
  theme_minimal()

cor(df$tempo_relacionamento, df$isa)

df_duplicados <- df %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE))

# linhas únicas (sem duplicatas)
df_unicos <- df %>% 
  filter(!duplicated(.) & !duplicated(., fromLast = TRUE))



df %>% distinct() %>% nrow()

table(df$prod_cesta_relacionamento)

