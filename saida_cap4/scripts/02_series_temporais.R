########################################################################
## 02_series_temporais.R - (B) séries temporais por base
## Atlas: anual 1991-2025, 35 municípios (Camboriú e Paulo Lopes sem Atlas).
## ERA5: anual e mensal (37 municípios; janela 1991-2025 destacada).
## MapBiomas: anual. Uma figura por base + figura-síntese. Sem modelagem.
########################################################################
SCRIPT <- "02_series_temporais.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
b <- ler_bases()
an <- montar_analise(b)
faixa_atlas <- annotate("rect", xmin = ano_ini_atlas - .5, xmax = ano_fim_atlas + .5,
                        ymin = -Inf, ymax = Inf, fill = "grey85", alpha = .45)

######@> B1 - Atlas: série anual dos três alvos...
serie_atlas <- an %>% group_by(ano) %>%
  summarise(n_municipios = n_distinct(geocodigo),
            across(c(hid_total, hid_sem_chuva, hid_so_chuva,
                     all_of(unname(tip_hidro))), sum)) %>%
  rename_with(~ sub("atlas_n_eventos_tip_", "tip_", .x))
salvar_tab(serie_atlas, "B01_serie_anual_atlas_hidrologico",
           "Eventos hidrológicos por ano (soma dos 35 municípios), três alvos e cinco tipologias")

p1 <- serie_atlas %>%
  pivot_longer(c(hid_total, hid_sem_chuva, hid_so_chuva), names_to = "col", values_to = "n") %>%
  mutate(alvo = factor(names(alvos)[match(col, alvos)], levels = names(alvos))) %>%
  ggplot(aes(ano, n, colour = alvo)) +
  geom_vline(xintercept = ano_corte - .5, linetype = 2, colour = "grey40") +
  annotate("text", x = ano_corte - .3, y = Inf, vjust = 1.5, hjust = 0, size = 3,
           label = "2013: surge 'Chuvas Intensas'", colour = "grey30") +
  geom_line(linewidth = .7) + geom_point(size = 1.4) +
  scale_colour_manual(values = cores_alvo, name = NULL) +
  scale_x_continuous(breaks = seq(1991, 2025, 4)) +
  labs(title = "Desastres hidrológicos registrados por ano, litoral de SC (35 municípios)",
       subtitle = "Total = sem chuvas intensas + só chuvas intensas; 1991-2025",
       x = "Ano", y = "Nº de eventos registrados")
salvar_fig(p1, "B1_serie_anual_atlas", 10, 5.5, FONTE_ATLAS,
           "Série anual do Atlas, três alvos", "B01_serie_anual_atlas_hidrologico")

######@> B2 - ERA5: séries anuais 1940-2025 (média dos 37 municípios)...
serie_era5 <- b$anual %>% filter(ano_completo) %>%
  group_by(ano) %>%
  summarise(n_municipios = n_distinct(geocodigo),
            prec_anual_mm = mean(era5_tp_mm_ano),
            prec_conv_anual_mm = mean(era5_cp_mm_ano),
            temp_media_C = mean(era5_t2m_C),
            vento_medio_m_s = mean(era5_si10),
            nuvens_pct = mean(era5_tcc_pct))
salvar_tab(serie_era5, "B02_serie_anual_era5",
           "ERA5 anual: média entre os 37 municípios, anos completos 1940-2025")

rot_era5 <- c(prec_anual_mm = "Precipitação total (mm/ano)",
              prec_conv_anual_mm = "Precipitação convectiva (mm/ano)",
              temp_media_C = "Temperatura a 2 m (°C)",
              vento_medio_m_s = "Vento a 10 m (m/s)")
p2 <- serie_era5 %>% pivot_longer(names(rot_era5)) %>%
  mutate(name = factor(rot_era5[name], levels = rot_era5)) %>%
  ggplot(aes(ano, value)) + faixa_atlas +
  geom_line(colour = "#0072B2", linewidth = .6) +
  facet_wrap(~ name, ncol = 1, scales = "free_y", strip.position = "top") +
  labs(title = "ERA5: séries anuais, média dos 37 municípios do litoral de SC",
       subtitle = "Faixa cinza = janela do Atlas (1991-2025); 2026 excluído por estar incompleto",
       x = "Ano", y = NULL)
salvar_fig(p2, "B2_serie_anual_era5", 10, 9, FONTE_ERA5,
           "Séries anuais ERA5 1940-2025", "B02_serie_anual_era5")

######@> B3 - ERA5: série mensal 1991-2025 e climatologia mensal...
mensal_reg <- b$mensal %>% filter(ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
  group_by(ano, mes) %>%
  summarise(prec_mm_mes = mean(era5_tp_mm_mes), temp_C = mean(era5_t2m_C),
            vento_m_s = mean(era5_si10), .groups = "drop") %>%
  mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))
salvar_tab(mensal_reg, "B03_serie_mensal_era5",
           "ERA5 mensal 1991-2025, média dos 37 municípios")
clima_mensal <- mensal_reg %>% group_by(mes) %>%
  summarise(prec_media = mean(prec_mm_mes), prec_mediana = median(prec_mm_mes),
            prec_p10 = quantile(prec_mm_mes, .1), prec_p90 = quantile(prec_mm_mes, .9),
            prec_max = max(prec_mm_mes), temp_media = mean(temp_C),
            vento_medio = mean(vento_m_s))
salvar_tab(clima_mensal, "B04_climatologia_mensal_era5",
           "Climatologia mensal 1991-2025 (média regional)")

p3a <- ggplot(mensal_reg, aes(data, prec_mm_mes)) +
  geom_line(colour = "#0072B2", linewidth = .35) +
  geom_smooth(method = "loess", span = .15, se = FALSE, colour = "grey20",
              linewidth = .6, formula = y ~ x) +
  labs(title = "Precipitação mensal (média regional), 1991-2025",
       subtitle = "Linha preta: suavização local (LOESS, span 0,15) apenas para leitura visual",
       x = NULL, y = "mm/mês")
p3b <- ggplot(mensal_reg, aes(factor(mes, labels = month.abb), prec_mm_mes)) +
  geom_boxplot(fill = "#56B4E9", alpha = .5, outlier.size = .8, width = .6) +
  scale_x_discrete(labels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")) +
  labs(title = "Distribuição por mês do ano", x = NULL, y = "mm/mês")
salvar_fig(p3a / p3b, "B3_serie_mensal_era5", 10, 8, FONTE_ERA5,
           "Série mensal e climatologia de precipitação ERA5",
           "B03_serie_mensal_era5; B04_climatologia_mensal_era5")

######@> B4 - Atlas mensal: sazonalidade dos eventos hidrológicos...
## A base mensal traz o grupo hidrológico (não as tipologias) por mês.
saz <- b$mensal %>% filter(tem_atlas, ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
  group_by(mes, periodo = ifelse(ano < ano_corte, "1991-2012", "2013-2025")) %>%
  summarise(eventos_hid = sum(atlas_n_eventos_hidrologico), .groups = "drop") %>%
  group_by(periodo) %>% mutate(pct_no_periodo = 100 * eventos_hid / sum(eventos_hid)) %>%
  ungroup()
salvar_tab(saz, "B05_sazonalidade_atlas_mensal",
           "Eventos hidrológicos por mês do ano e período (base mensal do Atlas, grupo hidrológico)")
p4 <- ggplot(saz, aes(factor(mes), pct_no_periodo, fill = periodo)) +
  geom_col(position = position_dodge(.8), width = .75) +
  scale_fill_manual(values = c("1991-2012" = "#E69F00", "2013-2025" = "#0072B2"), name = NULL) +
  scale_x_discrete(labels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")) +
  labs(title = "Sazonalidade dos desastres hidrológicos registrados",
       subtitle = "% dos eventos de cada período que caem em cada mês; 35 municípios",
       x = NULL, y = "% dos eventos do período")
salvar_fig(p4, "B4_sazonalidade_atlas", 9, 5, FONTE_ATLAS,
           "Sazonalidade mensal do grupo hidrológico", "B05_sazonalidade_atlas_mensal")

######@> B5 - MapBiomas: séries anuais de uso da terra...
serie_mapb <- b$anual %>% filter(!is.na(mapb_cob_n1_floresta_pct)) %>%
  group_by(ano) %>%
  summarise(n_municipios = n_distinct(geocodigo),
            urbanizada_pct = mean(mapb_cob_n2_area_urbanizada_pct),
            floresta_pct = mean(mapb_cob_n1_floresta_pct),
            agropecuaria_pct = mean(mapb_cob_n1_agropecuaria_pct),
            nao_vegetada_pct = mean(mapb_cob_n1_area_nao_vegetada_pct),
            agua_pct = mean(mapb_cob_n1_agua_e_ambiente_marinho_pct),
            urbanizada_ha_total = sum(mapb_cob_n2_area_urbanizada_ha))
salvar_tab(serie_mapb, "B06_serie_anual_mapbiomas",
           "MapBiomas: % médio do município por classe e área urbanizada total, por ano")
rot_mapb <- c(urbanizada_pct = "Área urbanizada", floresta_pct = "Floresta",
              agropecuaria_pct = "Agropecuária", nao_vegetada_pct = "Não vegetada (total)")
p5 <- serie_mapb %>% pivot_longer(names(rot_mapb)) %>%
  mutate(name = factor(rot_mapb[name], levels = rot_mapb)) %>%
  ggplot(aes(ano, value)) + faixa_atlas +
  geom_line(colour = "#009E73", linewidth = .7) +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "MapBiomas: cobertura da terra, média dos municípios",
       subtitle = "% da área municipal; faixa cinza = janela do Atlas",
       x = "Ano", y = "% da área do município")
salvar_fig(p5, "B5_serie_anual_mapbiomas", 10, 6.5, FONTE_MAPB,
           "Séries anuais de cobertura MapBiomas", "B06_serie_anual_mapbiomas")

######@> B6 - Síntese: as três bases na janela 1991-2025...
sint <- serie_atlas %>% select(ano, hid_total, hid_sem_chuva, hid_so_chuva) %>%
  left_join(an %>% group_by(ano) %>%
              summarise(prec_anual_media_mm = mean(prec_anual),
                        meses_ext_chuva_total = sum(meses_ext_chuva),
                        vento_medio = mean(vento_medio)), by = "ano") %>%
  left_join(serie_mapb %>% select(ano, urbanizada_pct), by = "ano")
salvar_tab(sint, "B07_sintese_anual_1991_2025",
           "Série anual integrada (35 municípios com Atlas): eventos, chuva ERA5, meses extremos, vento, % urbanizada")
painel <- function(v, titulo, cor) {
  ggplot(sint, aes(ano, .data[[v]])) + geom_line(colour = cor, linewidth = .7, na.rm = TRUE) +
    geom_point(colour = cor, size = 1, na.rm = TRUE) +
    geom_vline(xintercept = ano_corte - .5, linetype = 2, colour = "grey50") +
    scale_x_continuous(limits = c(1991, 2025), breaks = seq(1991, 2025, 4)) +
    labs(title = titulo, x = NULL, y = NULL) + theme(plot.title = element_text(size = 10))
}
p6 <- painel("hid_total", "Eventos hidrológicos registrados (Atlas)", "#0072B2") /
  painel("prec_anual_media_mm", "Precipitação anual média, mm (ERA5)", "#56B4E9") /
  painel("meses_ext_chuva_total", "Meses de chuva extrema, soma dos municípios (ERA5, > P95 local)", "#E69F00") /
  painel("urbanizada_pct", "Área urbanizada, % médio do município (MapBiomas; até 2024)", "#009E73") +
  plot_annotation(title = "Síntese: as três bases na janela de análise (1991-2025)",
                  subtitle = "Linha tracejada: 2013. Cada painel tem sua própria escala.")
salvar_fig(p6, "B6_sintese_tres_bases", 10, 10,
           paste(FONTE_ATLAS, FONTE_ERA5, FONTE_MAPB, sep = "; "),
           "Síntese das três bases", "B07_sintese_anual_1991_2025")

fechar_registro()
