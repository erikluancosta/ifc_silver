georreferenciamento_rfb_ms_to_ba <- "
      SELECT 
         cnpj
       , TRIM(tipo_logradouro || ' ' || logradouro) as endereco
       , numero
       , bairro
       , cep
       , uf
       , cd_mun_7
       , descricao_ibge
      FROM silver_cnpjs
"


