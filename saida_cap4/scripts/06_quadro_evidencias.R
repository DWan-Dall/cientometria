########################################################################
## 06_quadro_evidencias.R - (D) quadro de evidências e perguntas
## Lê APENAS tabelas já geradas pelos scripts 01-05 e organiza as
## evidências descritivas por hipótese. Cada número traz a tabela de
## origem. Não há conclusão causal: cada linha tem uma leitura
## alternativa e uma pergunta em aberto. Também gera o índice de saídas.
########################################################################
SCRIPT <- "06_quadro_evidencias.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
lt <- function(x) read_csv(file.path(dir_tab, paste0(x, ".csv")), show_col_types = FALSE)
f1 <- function(x) formatC(x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
f2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
f3 <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")

q01 <- lt("D01_quebra_2012_2013") %>% filter(corte == 2013)
v <- function(ind, col) q01[[col]][q01$indicador == ind]
c03 <- lt("C03_composicao_tipologica_por_bloco")
d02 <- lt("D02_tendencia_descritiva_mk_sen")
mk <- function(ind, per) d02 %>% filter(indicador == ind, periodo == per)
d04 <- lt("D04_correlacao_anual_registro_clima")
r4 <- function(a, rel) d04 %>% filter(alvo == a, relacao == rel)
d06 <- lt("D06_forca_dose_resposta")
r6 <- function(a, d, p) d06$razao_alta_baixa[d06$alvo == a & d06$descritor == d & d06$periodo == p]
d07 <- lt("D07_indicadores_de_registro")
d09 <- lt("D09_associacao_municipal_bruta")
r9 <- function(r, d) d09 %>% filter(resposta == r, descritor == d)
d10 <- lt("D10_moran_municipal")
m10 <- function(cn) d10 %>% filter(coluna == cn)
c13 <- lt("C11_correlacao_preditoras_municipio_ano")
rv <- c13$rho_spearman[c13$var1 == "vento_medio" & c13$var2 == "prec_conv_anual"]
c10 <- lt("C10_variacao_entre_dentro_municipios")

quadro <- tribble(
  ~hipotese, ~evidencia, ~valor, ~tabela_origem, ~leitura_alternativa, ~pergunta_em_aberto,
  "H1 coleta/classificação", "Quebra de classificação em 2013: 'Chuvas Intensas' inexistente até 2012",
  paste0("média anual só chuvas intensas: 0 antes x ", f2(v("Só chuvas intensas", "media_anual_depois")), " depois de 2013; total: ",
         f2(v("Hidrológico total", "media_anual_antes")), " -> ", f2(v("Hidrológico total", "media_anual_depois")),
         " (razão ", f2(v("Hidrológico total", "razao_depois_antes")), "); sem chuvas intensas: razão ",
         f2(v("Hidrológico sem chuvas intensas", "razao_depois_antes"))),
  "D01_quebra_2012_2013", "Parte do aumento pode ser evento novo de fato (mais chuva intensa), não só nova categoria",
  "Os registros de 'Chuvas Intensas' substituem tipologias antigas ou são registros que antes não eram feitos?",
  "H1 coleta/classificação", "Enxurradas recuam enquanto 'Chuvas Intensas' cresce",
  paste0("% de enxurradas no bloco: ", f1(c03$pct_enxurradas[c03$bloco == "2006-10"]), "% (2006-10) -> ",
         f1(c03$pct_enxurradas[c03$bloco == "2021-25"]), "% (2021-25); % chuvas intensas: ",
         f1(c03$pct_chuvas_intensas[c03$bloco == "2021-25"]), "% em 2021-25; Mann-Kendall enxurradas 2013-2025: tau ",
         f3(mk("Enxurradas", "2013-2025")$tau), " (p ", f3(mk("Enxurradas", "2013-2025")$p_valor), ")"),
  "C03_composicao_tipologica_por_bloco; D02_tendencia_descritiva_mk_sen; D03_enxurradas_x_chuvas_intensas",
  "Enxurradas podem ter diminuído por obras de drenagem ou por anos menos chuvosos",
  "Existe nota técnica da Defesa Civil/COBRADE (2012) explicando a nova codificação? Há como reclassificar retroativamente?",
  "H1 coleta/classificação", "Eventos sobem com o ano; chuva extrema não",
  paste0("Spearman eventos totais x ano: ", f3(r4("Hidrológico total", "eventos x ano")$rho),
         "; meses extremos x ano: ", f3(d04$rho[d04$relacao == "meses extremos x ano"]),
         "; MK precipitação anual 1991-2025: tau ", f3(mk("Precipitação anual média (mm)", "1991-2025")$tau),
         "; MK meses extremos: tau ", f3(mk("Meses de chuva extrema (soma)", "1991-2025")$tau)),
  "D04_correlacao_anual_registro_clima; D02_tendencia_descritiva_mk_sen",
  "A chuva mensal do ERA5 (0,25°) pode não captar extremos diários/horários que causam o evento",
  "Com dados diários (ERA5 diário, CHIRPS ou estações INMET/ANA), a tendência de extremos se mantém nula?",
  "H1 coleta/classificação", "Mais municípios registram algo por ano",
  paste0("municípios com registro hidrológico: média ", f1(mean(d07$municipios_com_registro_hid[d07$ano < 2013])),
         " (1991-2012) x ", f1(mean(d07$municipios_com_registro_hid[d07$ano >= 2013])), " (2013-2025); MK 2013-2025 tau ",
         f3(mk("Municípios com registro no ano", "2013-2025")$tau)),
  "D07_indicadores_de_registro; D02_tendencia_descritiva_mk_sen",
  "Mais municípios atingidos também ocorreria com mais eventos reais",
  "A adoção do S2ID (2012/2013) e a capacitação municipal mudaram a propensão a registrar?",
  "H1 coleta/classificação", "Reconhecimento federal x registro simples muda no tempo (todos os grupos)",
  paste0("% reconhecidos: ", f1(100 * sum(d07$reconhecidos_todos_grupos[d07$ano %in% 2009:2011]) /
                                 sum(d07$eventos_todos_grupos[d07$ano %in% 2009:2011])), "% em 2009-11 x ",
         f1(100 * sum(d07$reconhecidos_todos_grupos[d07$ano >= 2021]) / sum(d07$eventos_todos_grupos[d07$ano >= 2021])),
         "% em 2021-25"),
  "D07_indicadores_de_registro",
  "Indicador inclui todos os grupos (não só hidrológico); a base não separa reconhecimento por grupo",
  "É possível obter reconhecimento por evento hidrológico no Atlas bruto?",
  "H1 / H2", "Sinal da chuva sobre o hidrológico 'sem chuvas intensas' se enfraquece depois de 2013",
  paste0("razão P(evento) maior/menor classe de precipitação anual: ",
         f2(r6("Hidrológico sem chuvas intensas", "Precipitação anual", "1991-2012")), " (1991-2012) x ",
         f2(r6("Hidrológico sem chuvas intensas", "Precipitação anual", "2013-2025")), " (2013-2025); só chuvas intensas 2013-2025: ",
         f2(r6("Só chuvas intensas", "Maior acumulado de 3 meses", "2013-2025")), " (acumulado de 3 meses)"),
  "D06_forca_dose_resposta; D05_dose_resposta_descritiva",
  "Coerente com eventos sensíveis à chuva migrando para a nova categoria",
  "Separar a resposta muda o sinal climático: qual versão da resposta a dissertação deve adotar?",
  "Controle negativo", "Vento também 'discrimina' eventos em 1991-2012",
  paste0("razão P(evento) vento >P90/<=P25: ", f2(r6("Hidrológico total", "Vento médio (controle negativo)", "1991-2012")),
         " (1991-2012) x ", f2(r6("Hidrológico total", "Vento médio (controle negativo)", "2013-2025")),
         " (2013-2025); compare precipitação anual: ", f2(r6("Hidrológico total", "Precipitação anual", "1991-2012")),
         " e ", f2(r6("Hidrológico total", "Precipitação anual", "2013-2025")),
         "; rho vento x precipitação convectiva no município-ano: ", f2(rv)),
  "D06_forca_dose_resposta; C11_correlacao_preditoras_municipio_ano",
  "Vento e chuva covariam (sistemas frontais/ciclones trazem os dois): o vento não é controle 'limpo'",
  "Que variável serviria de controle negativo sem covariar com a chuva (ex.: radiação, temperatura defasada)?",
  "H2 outros fatores", "Padrão espacial dos eventos (I de Moran)",
  paste0("I densidade total = ", f3(m10("dens_total")$I), " (p ", f3(m10("dens_total")$p_valor), "); sem chuvas intensas = ",
         f3(m10("dens_sem_chuva")$I), "; só chuvas intensas = ", f3(m10("dens_so_chuva")$I), "; precipitação média = ",
         f3(m10("prec_anual_media")$I), "; % urbanizada = ", f3(m10("urbano_pct")$I)),
  "D10_moran_municipal",
  "Agrupamento espacial pode vir do clima regional, da hidrografia (bacias do Itajaí/Tijucas) ou da prática de registro de uma mesma regional da Defesa Civil",
  "Os agrupamentos coincidem com bacias hidrográficas ou com coordenadorias regionais da Defesa Civil?",
  "H2 outros fatores", "Uso da terra x eventos entre municípios (correlação bruta, n = 35)",
  paste0("rho eventos totais x floresta % = ", f2(r9("Eventos - total", "Floresta (%)")$rho),
         " (floresta x latitude = ", f2(r9("Eventos - total", "Floresta (%)")$rho_descritor_x_latitude),
         "); x área urbanizada % = ", f2(r9("Eventos - total", "Área urbanizada (%)")$rho),
         "; x ganho urbano = ", f2(r9("Eventos - total", "Ganho de área urbanizada (p.p.)")$rho),
         "; x área urbana em risco = ", f2(r9("Eventos - total", "Área urbana em risco (ha)")$rho),
         "; x área do município = ", f2(r9("Eventos - total", "Área do município (km²)")$rho),
         "; x latitude = ", f2(r9("Eventos - total", "Latitude do centroide")$rho)),
  "D09_associacao_municipal_bruta; D08_perfil_municipal_uso_terra",
  "Floresta acompanha a latitude e a área (municípios do norte, maiores e com serra); a correlação pode ser o gradiente norte-sul visto duas vezes",
  "Sem modelar: os municípios com maior ganho urbano são os mesmos com aumento de eventos 'sem chuvas intensas'? (ver tau em D08)",
  "H2 outros fatores", "Uso da terra varia quase só entre municípios",
  paste0("% da variação entre municípios: área urbanizada ", f1(c10$pct_variacao_entre_municipios[c10$coluna == "mapb_cob_n2_area_urbanizada_pct"]),
         "%, floresta ", f1(c10$pct_variacao_entre_municipios[c10$coluna == "mapb_cob_n1_floresta_pct"]),
         "%; precipitação anual ", f1(c10$pct_variacao_entre_municipios[c10$coluna == "prec_anual"]), "%"),
  "C10_variacao_entre_dentro_municipios",
  "Pouca variação temporal limita o que o uso da terra pode explicar sobre a tendência no tempo",
  "O uso da terra deve entrar como característica do município (vulnerabilidade) e não como série temporal?")
salvar_tab(quadro, "D00_quadro_evidencias_hipoteses",
           "Quadro de evidências descritivas por hipótese, com tabela de origem, leitura alternativa e pergunta em aberto")
fechar_registro()

######@> Índice de todas as saídas...
regs <- list.files(dir_tab, pattern = "^_registro_.*\\.csv$", full.names = TRUE)
indice <- map_dfr(regs, ~ read_csv(.x, show_col_types = FALSE)) %>% arrange(arquivo)
write_csv(indice, file.path(dir_tab, "00_indice_saidas.csv"))
message("Índice: ", nrow(indice), " saídas")
