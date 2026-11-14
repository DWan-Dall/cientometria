########################################################################
## 01_metadados.R - (A) metadados das fontes e verificação de unidades ERA5
## Sem modelagem. Tudo é recalculado a partir dos dados de entrada.
########################################################################
SCRIPT <- "01_metadados.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
b <- ler_bases()
dic <- read_csv(file.path(dir_dados, "dicionario_mensal.csv"), show_col_types = FALSE)
mun <- b$municipios

######@> Cobertura de cada variável, calculada nos dados...
## Período = anos com ao menos um valor não ausente; nº de municípios =
## municípios com ao menos um valor; % ausentes em dois recortes:
## (i) base inteira (37 municípios x 1940-2026, mensal) e
## (ii) janela de análise (35 municípios com Atlas x 1991-2025, mensal).
cobertura_var <- function(dados, v) {
  x <- dados[[v]]; ok <- !is.na(x)
  jan <- dados$tem_atlas & dados$ano >= ano_ini_atlas & dados$ano <= ano_fim_atlas
  tibble(variavel = v,
         ano_inicio = if(any(ok)) min(dados$ano[ok]) else NA,
         ano_fim = if(any(ok)) max(dados$ano[ok]) else NA,
         n_municipios = n_distinct(dados$geocodigo[ok]),
         pct_ausentes_base = 100 * mean(!ok),
         pct_ausentes_janela_1991_2025 = 100 * mean(is.na(x[jan])))
}
vars_m <- intersect(dic$variavel, names(b$mensal))
cob <- map_dfr(vars_m, ~ cobertura_var(b$mensal, .x))

## Papel na dissertação (proposta; decisão final é da autora)
papel_de <- function(v) case_when(
  v %in% c("atlas_n_eventos_hidrologico") ~ "resposta (hidrológico total)",
  grepl("^atlas_n_eventos_tip_(alagamentos|enxurradas|inundacoes|movimento_de_massa|chuvas_intensas)$", v) ~ "resposta (componente tipológico)",
  grepl("^atlas_", v) ~ "contexto/não usada como resposta",
  v %in% c("era5_tp", "era5_tp_mm_dia", "era5_tp_mm_mes", "era5_cp", "era5_cp_mm_dia",
           "era5_lsrr", "era5_lsrr_mm_dia", "era5_tcrw") ~ "preditora (chuva)",
  v %in% c("era5_si10", "era5_vento_vel_m_s", "era5_u10", "era5_v10",
           "era5_vento_dir_graus") ~ "controle negativo (vento)",
  grepl("_(dp|min|max)$", v) ~ "auxiliar (dispersão entre pixels)",
  grepl("^era5_(cvh|cvl|tvh|tvl|lai)", v) ~ "descritor estático/climatológico (não é série)",
  grepl("^era5_", v) ~ "preditora candidata (clima)",
  grepl("^mapb_risco", v) ~ "preditora candidata (exposição, constante no tempo)",
  grepl("^mapb_", v) ~ "preditora candidata (uso da terra)",
  TRUE ~ "chave/controle da base")

meta_var <- dic %>%
  select(variavel, fonte, bloco, unidade, descricao, ressalva) %>%
  inner_join(cob, by = "variavel") %>%
  mutate(papel = papel_de(variavel),
         resolucao_temporal = case_when(
           fonte == "ERA5" & grepl("^era5_(cvh|cvl|tvh|tvl)", variavel) ~ "estática (constante)",
           fonte == "ERA5" & grepl("^era5_lai", variavel) ~ "climatologia mensal fixa",
           fonte == "ERA5" ~ "mensal (média mensal da reanálise)",
           fonte == "Atlas" ~ "evento (data) agregado a mês",
           fonte == "MapBiomas" ~ "anual (repetido nos meses)",
           TRUE ~ "-"),
         resolucao_espacial = case_when(
           fonte == "ERA5" ~ "grade 0,25° (~28 km); média dos pixels do município (1 a 8 pixels)",
           fonte == "Atlas" ~ "município",
           fonte == "MapBiomas" ~ "pixel 30 m (Landsat) agregado ao município",
           TRUE ~ "município"))

salvar_tab(meta_var, "A02_metadados_por_variavel",
           "Uma linha por variável da base mensal consolidada: unidade, período, nº de municípios, % ausentes e papel proposto")

######@> Tabela-síntese por fonte (pedido do orientador)...
resumo_fonte <- function(fonte_, vars) {
  m <- meta_var %>% filter(variavel %in% vars)
  tibble(fonte = fonte_,
         n_variaveis = nrow(m),
         ano_inicio = min(m$ano_inicio, na.rm = TRUE),
         ano_fim = max(m$ano_fim, na.rm = TRUE),
         n_municipios = max(m$n_municipios),
         pct_ausentes_base_mediana = median(m$pct_ausentes_base),
         pct_ausentes_janela_mediana = median(m$pct_ausentes_janela_1991_2025))
}
n_pix <- range(mun$era5_n_pixels)
sint <- bind_rows(
  resumo_fonte("Atlas Digital de Desastres", meta_var$variavel[meta_var$fonte == "Atlas"]),
  resumo_fonte("ERA5 (Copernicus/ECMWF)", meta_var$variavel[meta_var$fonte == "ERA5"]),
  resumo_fonte("MapBiomas - cobertura", meta_var$variavel[grepl("^mapb_cob", meta_var$variavel)]),
  resumo_fonte("MapBiomas - urbanização", meta_var$variavel[grepl("^mapb_urb", meta_var$variavel)]),
  resumo_fonte("MapBiomas - áreas de risco", meta_var$variavel[grepl("^mapb_risco", meta_var$variavel)]),
  resumo_fonte("MapBiomas - superfície de água", meta_var$variavel[grepl("^mapb_agua", meta_var$variavel)])) %>%
  mutate(variaveis_principais = c(
    "nº de eventos por grupo (hidrológico, meteorológico, climatológico, outros) e por tipologia; danos humanos e materiais",
    "precipitação total e convectiva, chuva de larga escala, evaporação, temperatura a 2 m, vento a 10 m, nuvens, radiação, fluxo latente",
    "área (ha) e % do município por classe de cobertura, níveis 1 a 4",
    "área urbanizada, % urbanizada, vegetada x não vegetada",
    "área urbana em risco alto, muito alto e médio (ha)",
    "faixas de tendência de superfície de água (ha)"),
    unidade = c("contagem de eventos (registros)", "ver A02 (m, K, m s-1, J m-2, 0-1; convertidas: mm, °C, W m-2, %)",
                "ha e %", "ha e %", "ha", "ha"),
    resolucao_temporal = c("evento com data; agregado a mês e ano", "mensal",
                           "anual", "anual", "anual (valor constante em todos os anos)", "anual"),
    resolucao_espacial = c("município", paste0("0,25° (~28 km); ", n_pix[1], " a ", n_pix[2], " pixels por município"),
                           "30 m agregado ao município", "30 m agregado ao município",
                           "agregado ao município", "30 m agregado ao município"),
    origem = c("MIDR/S2ID, compilação UFSC/CEPED (Atlas Digital, download 2026)",
               "Copernicus CDS: reanalysis-era5-single-levels-monthly-means, product_type monthly_averaged_reanalysis (script_ERA5_copernicus_ver00.R)",
               "MapBiomas (coleção não registrada na base consolidada)", "MapBiomas", "MapBiomas", "MapBiomas"),
    papel = c("RESPOSTA: grupo hidrológico (732 eventos 1991-2025) e versões sem/só 'Chuvas Intensas'",
              "PREDITORAS: chuva e derivados; vento como controle negativo",
              "preditoras candidatas (uso da terra)", "preditoras candidatas (exposição urbana)",
              "preditora candidata (exposição; atributo do terreno)",
              "contexto (tendência, não estado)")) %>%
  bind_rows(tibble(
    fonte = "Shapefile municipio_abrangencia_PMCAP-SC", n_variaveis = NA,
    ano_inicio = NA, ano_fim = NA, n_municipios = nrow(mun),
    pct_ausentes_base_mediana = 0, pct_ausentes_janela_mediana = 0,
    variaveis_principais = "polígonos municipais, área (km²), centroide",
    unidade = "graus (WGS 84, EPSG:4326)", resolucao_temporal = "estática",
    resolucao_espacial = "limite municipal", origem = "recorte do projeto PMCAP-SC",
    papel = "unidade espacial; vizinhança para I de Moran; mapas"),
  tibble(
    fonte = "Climbra", n_variaveis = 0, ano_inicio = NA, ano_fim = NA, n_municipios = 0,
    pct_ausentes_base_mediana = NA, pct_ausentes_janela_mediana = NA,
    variaveis_principais = "nenhuma variável Climbra na base consolidada atual",
    unidade = "-", resolucao_temporal = "-", resolucao_espacial = "-",
    origem = "usada no pipeline anterior (repositório consolidacao-bd-mestrado)",
    papel = "SUBSTITUÍDA pelo ERA5 na base Consolidado_SC (ago/2026) - motivo a confirmar pela autora"))

salvar_tab(sint, "A01_metadados_por_fonte",
           "Tabela-síntese de metadados por fonte de dados (pedido A)")

######@> Cobertura municipal: quem tem Atlas...
cob_mun <- mun %>%
  transmute(geocodigo, municipio, tem_atlas, atlas_n_eventos_total,
            atlas_ano_primeiro, atlas_ano_ultimo, era5_n_pixels, era5_area_km2,
            area_km2_shape, lat_centroide, lon_centroide)
salvar_tab(cob_mun, "A03_cobertura_municipal",
           "Cobertura por município; Camboriú e Paulo Lopes sem Atlas")

######@> Verificação de unidades do ERA5 (NÃO decide; só mostra a conta)...
## O produto baixado é 'monthly_averaged_reanalysis'. Nele, variáveis
## acumuladas (tp, cp, e, pev) vêm como média da acumulação DIÁRIA, em m
## (documentação ERA5: "monthly means of daily accumulations"). Logo:
##   mm/dia = tp * 1000 ; mm/mês = tp * 1000 * dias_no_mes.
## O fator x24 só caberia no produto 'monthly_averaged_reanalysis_by_hour_of_day'
## (acumulação horária). A tabela abaixo mostra as três hipóteses lado a lado.
u <- b$mensal %>%
  filter(ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
  group_by(geocodigo, ano) %>%
  summarise(tp_m_media = mean(era5_tp),
            H1_x1000xdias = sum(era5_tp * 1000 * dias_no_mes),
            H2_x1000 = sum(era5_tp * 1000),
            H3_x1000x24xdias = sum(era5_tp * 1000 * 24 * dias_no_mes),
            base_tp_mm_mes = sum(era5_tp_mm_mes), .groups = "drop")
razoes <- b$mensal %>%
  summarise(razao_tp_mm_dia_sobre_tp = mean(era5_tp_mm_dia / era5_tp),
            razao_tp_mm_mes_sobre_mm_dia_media = mean(era5_tp_mm_mes / era5_tp_mm_dia),
            dias_no_mes_medio = mean(dias_no_mes),
            razao_lsrr_mm_dia_sobre_lsrr = mean(era5_lsrr_mm_dia / era5_lsrr),
            razao_slhf_W_sobre_J = mean(era5_slhf_W_m2 / era5_slhf),
            dif_max_t2m_C_menos_K = max(abs(era5_t2m_C - (era5_t2m - 273.15))))
unid <- tibble(
  hipotese = c("H1: tp(m/dia) x 1000 x dias do mês (usada na base)",
               "H2: tp x 1000 (trata o valor mensal como total do mês)",
               "H3: tp x 1000 x 24 x dias (trata como acumulação horária)"),
  precipitacao_anual_media_mm = c(mean(u$H1_x1000xdias), mean(u$H2_x1000), mean(u$H3_x1000x24xdias)),
  precipitacao_anual_min_mm = c(min(u$H1_x1000xdias), min(u$H2_x1000), min(u$H3_x1000x24xdias)),
  precipitacao_anual_max_mm = c(max(u$H1_x1000xdias), max(u$H2_x1000), max(u$H3_x1000x24xdias)),
  confere_com_coluna_da_base = c(isTRUE(all.equal(u$H1_x1000xdias, u$base_tp_mm_mes)),
                                 isTRUE(all.equal(u$H2_x1000, u$base_tp_mm_mes)),
                                 isTRUE(all.equal(u$H3_x1000x24xdias, u$base_tp_mm_mes))),
  referencia_externa = "normal climatológica do litoral de SC na ordem de 1.300-2.000 mm/ano (INMET, a confirmar pela autora)",
  decisao = "PENDENTE - decisão da autora")
salvar_tab(unid, "A04_unidades_era5_hipoteses",
           "Precipitação anual 1991-2025 (37 municípios) sob três hipóteses de conversão; a base usa H1")
salvar_tab(razoes, "A05_unidades_era5_razoes_na_base",
           "Razões entre colunas convertidas e originais na base mensal (mostram as contas aplicadas na consolidação)")

fechar_registro()
