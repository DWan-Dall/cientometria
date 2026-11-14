########################################################################
## 04_preditoras.R - (C, parte 2) descrição das preditoras candidatas
## ERA5 (descritores anuais derivados do mensal + resumo mensal) e
## MapBiomas (uso da terra, exposição). Estatísticas, distribuição,
## ausentes e correlação de Spearman ENTRE preditoras (só descrição).
## O VIF da Etapa 14 do script original foi CORTADO (exige regressão).
########################################################################
SCRIPT <- "04_preditoras.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
b <- ler_bases()
an <- montar_analise(b)

preditoras <- tribble(
  ~coluna, ~rotulo, ~fonte, ~unidade, ~nota,
  "prec_anual", "Precipitação anual", "ERA5", "mm/ano", "soma dos 12 meses (conversão H1, ver A04)",
  "prec_max_mes", "Maior precipitação mensal do ano", "ERA5", "mm/mês", "",
  "prec_3m_max", "Maior acumulado de 3 meses", "ERA5", "mm/3 meses", "janela móvel",
  "meses_ext_chuva", "Meses de chuva extrema no ano", "ERA5", "meses (0-12)", "> P95 local município x mês",
  "prec_conv_anual", "Precipitação convectiva anual", "ERA5", "mm/ano", "",
  "evap_anual", "Evaporação anual", "ERA5", "mm/ano", "negativo = perda",
  "temp_media", "Temperatura média a 2 m", "ERA5", "°C", "",
  "nuvens_pct", "Cobertura de nuvens", "ERA5", "%", "",
  "vento_medio", "Velocidade do vento a 10 m", "ERA5", "m/s", "controle negativo",
  "mapb_cob_n2_area_urbanizada_pct", "Área urbanizada", "MapBiomas", "% da área", "",
  "mapb_cob_n1_floresta_pct", "Floresta", "MapBiomas", "% da área", "",
  "mapb_cob_n1_agropecuaria_pct", "Agropecuária", "MapBiomas", "% da área", "",
  "mapb_cob_n1_area_nao_vegetada_pct", "Área não vegetada", "MapBiomas", "% da área", "composição fechada no nível 1",
  "mapb_urb_pct_vegetada", "Vegetação intraurbana", "MapBiomas", "% da mancha urbana", "",
  "mapb_risco_total_ha", "Área urbana em risco (alto+muito alto+médio)", "MapBiomas", "ha", "constante no tempo")

######@> D1 - Estatísticas descritivas e ausentes (município-ano, janela)...
desc <- map_dfr(seq_len(nrow(preditoras)), function(i) {
  descrever(an[[preditoras$coluna[i]]]) %>%
    mutate(coluna = preditoras$coluna[i], rotulo = preditoras$rotulo[i],
           fonte = preditoras$fonte[i], unidade = preditoras$unidade[i],
           nota = preditoras$nota[i], .before = 1)
})
salvar_tab(desc, "C09_descritivas_preditoras",
           "Estatísticas descritivas e ausentes das preditoras, município-ano 1991-2025 (35 municípios)")

######@> Variação entre e dentro dos municípios...
## Quanto da variação de cada preditora é entre municípios (espaço) e
## quanto é entre anos dentro do município (tempo): decomposição de
## soma de quadrados, só descritiva.
decomp <- map_dfr(preditoras$coluna, function(v) {
  d <- an %>% select(geocodigo, x = all_of(v)) %>% filter(!is.na(x))
  m <- d %>% group_by(geocodigo) %>% mutate(mu = mean(x)) %>% ungroup()
  sq_tot <- sum((m$x - mean(m$x))^2); sq_entre <- sum((m$mu - mean(m$x))^2)
  tibble(coluna = v, pct_variacao_entre_municipios = 100 * sq_entre / sq_tot,
         pct_variacao_dentro_municipio = 100 * (1 - sq_entre / sq_tot))
}) %>% left_join(preditoras %>% select(coluna, rotulo), by = "coluna")
salvar_tab(decomp, "C10_variacao_entre_dentro_municipios",
           "Parcela da variação de cada preditora entre municípios x entre anos")

######@> Figura - distribuições...
p1 <- map_dfr(seq_len(nrow(preditoras)), function(i)
  tibble(rotulo = paste0(preditoras$rotulo[i], " (", preditoras$unidade[i], ")"),
         fonte = preditoras$fonte[i], x = an[[preditoras$coluna[i]]])) %>%
  filter(!is.na(x)) %>%
  mutate(rotulo = factor(rotulo, levels = unique(rotulo))) %>%
  ggplot(aes(x, fill = fonte)) +
  geom_histogram(bins = 30, colour = "white", linewidth = .1) +
  facet_wrap(~ rotulo, scales = "free", ncol = 3, labeller = label_wrap_gen(32)) +
  scale_fill_manual(values = c(ERA5 = "#0072B2", MapBiomas = "#009E73"), name = NULL) +
  labs(title = "Distribuição das preditoras candidatas no município-ano (1991-2025)",
       subtitle = "35 municípios; MapBiomas sem 2025", x = NULL, y = "Município-anos") +
  theme(strip.text = element_text(size = 8))
salvar_fig(p1, "C4_distribuicao_preditoras", 11, 12,
           paste(FONTE_ERA5, FONTE_MAPB, sep = "; "),
           "Histogramas das preditoras", "C09_descritivas_preditoras")

######@> Correlação de Spearman entre preditoras (descritiva)...
X <- an %>% select(all_of(preditoras$coluna))
cm <- suppressWarnings(cor(X, method = "spearman", use = "pairwise.complete.obs"))
cor_long <- as.data.frame(as.table(cm)) %>%
  setNames(c("var1", "var2", "rho_spearman")) %>%
  mutate(across(c(var1, var2), as.character)) %>%
  left_join(preditoras %>% select(var1 = coluna, rotulo1 = rotulo), by = "var1") %>%
  left_join(preditoras %>% select(var2 = coluna, rotulo2 = rotulo), by = "var2") %>%
  mutate(n_pares = map2_int(var1, var2, ~ sum(complete.cases(X[[.x]], X[[.y]]))))
salvar_tab(cor_long, "C11_correlacao_preditoras_municipio_ano",
           "Matriz de correlação de Spearman entre preditoras no município-ano (formato longo)")
pares_altos <- cor_long %>% filter(var1 < var2, abs(rho_spearman) >= .7) %>%
  arrange(desc(abs(rho_spearman)))
salvar_tab(pares_altos, "C12_pares_correlacao_alta",
           "Pares de preditoras com |rho| >= 0,7 (redundância potencial, município-ano)")

## Entre municípios (médias do período): onde o gradiente espacial domina
mm <- an %>% group_by(geocodigo) %>%
  summarise(across(all_of(preditoras$coluna), ~ mean(.x, na.rm = TRUE))) %>%
  left_join(b$municipios %>% select(geocodigo, lat_centroide), by = "geocodigo")
cm2 <- cor(mm %>% select(-geocodigo), method = "spearman")
cor_mun <- as.data.frame(as.table(cm2)) %>% setNames(c("var1", "var2", "rho_spearman")) %>%
  mutate(across(c(var1, var2), as.character))
salvar_tab(cor_mun, "C13_correlacao_preditoras_entre_municipios",
           "Correlação de Spearman entre médias municipais 1991-2025 (n = 35), incluindo latitude")

ordem <- preditoras$rotulo
p2 <- cor_long %>%
  mutate(rotulo1 = factor(rotulo1, levels = ordem), rotulo2 = factor(rotulo2, levels = rev(ordem))) %>%
  ggplot(aes(rotulo1, rotulo2, fill = rho_spearman)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", rho_spearman)), size = 2.4) +
  scale_fill_gradient2(low = "#0072B2", mid = "grey97", high = "#D55E00",
                       limits = c(-1, 1), name = "rho de Spearman") +
  labs(title = "Correlação entre preditoras candidatas (município-ano, 1991-2025)",
       subtitle = "Descrição de redundância; não implica relação causal", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
salvar_fig(p2, "C5_correlacao_preditoras", 11, 9.5,
           paste(FONTE_ERA5, FONTE_MAPB, sep = "; "),
           "Mapa de calor da correlação entre preditoras", "C11_correlacao_preditoras_municipio_ano")

######@> ERA5 mensal - descritivas no município-mês (1991-2025)...
vars_mes <- c(era5_tp_mm_mes = "Precipitação total (mm/mês)",
              era5_cp_mm_dia = "Precipitação convectiva (mm/dia)",
              era5_t2m_C = "Temperatura a 2 m (°C)", era5_si10 = "Vento a 10 m (m/s)",
              era5_tcc_pct = "Cobertura de nuvens (%)", era5_tp_dp = "Desvio entre pixels da precipitação (m/dia)")
mes <- b$mensal %>% filter(ano >= ano_ini_atlas, ano <= ano_fim_atlas, tem_atlas)
desc_mes <- map_dfr(names(vars_mes), ~ descrever(mes[[.x]]) %>%
                      mutate(coluna = .x, rotulo = vars_mes[[.x]], .before = 1))
salvar_tab(desc_mes, "C14_descritivas_era5_municipio_mes",
           "Descritivas das principais variáveis ERA5 no município-mês, 35 municípios, 1991-2025")

fechar_registro()
