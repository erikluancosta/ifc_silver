query_sicredi_ampliado <- 
  "
WITH ss_base AS (
    SELECT
        ss.*,
        SUBSTRING(ss.cnpj_associado FROM 1 FOR 8) AS cnpj_basico,
        SUBSTRING(ss.cnpj_associado FROM 9 FOR 4) AS cnpj_ordem,
        SUBSTRING(ss.cnpj_associado FROM 13 FOR 2) AS cnpj_dv
    FROM sicredi_silver_t ss
)
SELECT 
    ss.cnpj_associado,
    em.natureza_juridica AS nat_ju_rfb,
    em.capital_social AS capital_social_rfb,
    em.porte_empresa AS porte_empresa_rfb,
    ss.status_associado AS status_associado_sicredi,
    ss.nom_associado AS nom_associado_sicredi,
    em.razao_social as razao_social_rfb,
    es.nome_fantasia as nome_fantasia_rfb,
    ss.nivel_risco AS nivel_risco_sicredi,
    ss.porte_padrao AS porte_padrao_sicredi,
    ss.publico_estrategico AS publico_estrategico_sicredi,
    ss.isa AS isa_sicredi,
    ss.faixa_principalidade AS faixa_principalidade_sicredi,
    ss.ultimo_contato AS ultimo_contato_sicredi,
    ss.estado_x AS estado_agencia,
    ss.municipio_x AS municipio_agencia,
    ss.sistema_abertura_conta AS sistema_abertura_conta_sicredi,
    ss.endereco AS endereco_associado,
    ss.numero AS numero_associado,
    ss.bairro AS bairro_associado,
    ss.municipio_y AS municipio_associado,
    ss.cep AS cep_associado,
    ss.estado_y AS estado_associado,
    ss.lat AS latitude_associado,
    ss.lon AS longitude_associado,
    ss.precisao AS precisao_associado,
    ss.sexo_final AS sexo_final_sicredi,
    ss.predominancia AS predominancia_sicredi,
    ss.idade_classificada AS idade_sicredi,
    ss.tempo_relacionamento AS tempo_relacionamento_sicredi,
    es.situacao_cadastral AS situacao_cadastral_rfb,
    es.motivo_situacao_cadastral AS motivo_situacao_cadastral_rfb,
    es.data_situacao_cadastral AS data_situacao_cadastral_rfb,
    es.data_inicio_atividade AS data_inicio_atividade_rfb,
    es.cnae_fiscal_principal AS cnae_rfb,
    es.ddd_1 as ddd_1_rfb,
    es.telefone_1 as telefone_1_rfb,
    es.correio_eletronico as email_rfb,
    TRIM(es.tipo_logradouro || ' ' || es.logradouro) AS endereco_rfb,
    es.numero AS numero_rfb,
    es.complemento AS complemento_rfb,
    es.bairro AS bairro_rfb,
    es.cep::numeric AS cep_rfb,
    es.uf AS uf_rfb,
    mc.cd_mun_7 as cd_ibge_rfb,
    mc.descricao_ibge as muni_ibge_rfb,
    qc.qtd_fem as qtd_fem_rfb,
    eg.sexo as sexo_me_rfb
FROM ss_base ss
LEFT JOIN estabelecimento es
    ON ss.cnpj_basico = es.cnpj_basico
    AND ss.cnpj_ordem  = es.cnpj_ordem
    AND ss.cnpj_dv     = es.cnpj_dv
LEFT JOIN empresa em
    ON ss.cnpj_basico = em.cnpj_basico
LEFT JOIN munic_comp mc
    ON es.municipio = mc.codigo
LEFT JOIN quali_cnpj qc
	ON qc.cnpj_basico = es.cnpj_basico
LEFT join empresa_genero eg 
	ON es.cnpj_basico = eg.cnpj_basico;
"
