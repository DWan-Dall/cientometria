########################################################################
## 05_evidencias_hipoteses.R - (D) material descritivo para hipóteses
##  H-coleta: a série reflete melhora/mudança na coleta e classificação?
##  H-outros: há fatores além da chuva (uso da terra, exposição)?
## Reaproveita Etapas 04, 05, 10, 12, 13 (só correlação bruta) e 14 (só
## Moran e mapas) de script_AED_Hidrologico_ver02.R. Mann-Kendall e I de
## Moran usados apenas como DESCRIÇÃO. Nada aqui é teste causal.
########################################################################
SCRIPT <- "05_evidencias_hipoteses.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
suppressPackageStartupMessages({library(spdep); library(sf)})
b <- ler_bases()
an <- montar_analise(b)

serie <- an %>% group_by(ano) %>%
  summarise(municipios_com_registro = sum(hid_total > 0),
            across(c(all_of(unname(alvos)), all_of(unname(tip_hidro))), sum),
            meses_ext_chuva = sum(meses_ext_chuva), prec_media = mean(prec_anual),
            vento_medio = mean(vento_medio), .groups = "drop")

######@> D01 - Quebra de 2012/2013: médias antes x depois (Etapa 04)...
quebra <- map_dfr(c(2012, 2013), function(corte) {
  vars <- c(alvos, "Enxurradas" = tip_hidro[["Enxurradas"]],
            "Municípios com registro no ano" = "municipios_com_registro",
            "Meses de chuva extrema (soma)" = "meses_ext_chuva",
            "Precipitação anual média (mm)" = "prec_media",
            "Vento médio (m/s)" = "vento_medio")
  map_dfr(names(vars), function(a) {
    y <- serie[[vars[[a]]]]
    tibble(corte = corte, indicador = a,
           media_anual_antes = mean(y[serie$ano < corte]),
           media_anual_depois = mean(y[serie$ano >= corte]),
           razao_depois_antes = ifelse(mean(y[serie$ano < corte]) > 0,
                                       mean(y[serie$ano >= corte]) / mean(y[serie$ano < corte]), NA))
  })
})
salvar_tab(quebra, "D01_quebra_2012_2013",
           "Médias anuais antes e depois do corte (2012 da AED original; 2013 = 1º ano de 'Chuvas Intensas')")

######@> D02 - Tendência descritiva (Mann-Kendall/Sen) por período (Etapa 05)...
periodos <- tribble(~periodo, ~ini, ~fim,
                    "1991-2025", 1991, 2025, "1991-2012", 1991, 2012, "2013-2025", 2013, 2025)
ind_tend <- c(alvos, setNames(unname(tip_hidro), names(tip_hidro)),
              "Municípios com registro no ano" = "municipios_com_registro",
              "Meses de chuva extrema (soma)" = "meses_ext_chuva",
              "Precipitação anual média (mm)" = "prec_media",
              "Vento médio (m/s)" = "vento_medio")
tend <- pmap_dfr(periodos, function(periodo, ini, fim) {
  s <- serie %>% filter(ano >= ini, ano <= fim)
  map_dfr(names(ind_tend), function(a) {
    y <- s[[ind_tend[[a]]]]
    mann_kendall(y) %>% mutate(indicador = a, periodo = periodo,
                               sen_por_decada = declividade_sen(y, s$ano), .before = 1)
  })
})
salvar_tab(tend, "D02_tendencia_descritiva_mk_sen",
           "Mann-Kendall (tau, p) e inclinação de Sen por década, séries anuais agregadas; uso descritivo")

######@> D03 - Enxurradas caindo enquanto 'Chuvas Intensas' nasce...
troca <- serie %>% filter(ano >= 2008) %>%
  transmute(ano, enxurradas = atlas_n_eventos_tip_enxurradas,
            chuvas_intensas = atlas_n_eventos_tip_chuvas_intensas,
            inundacoes = atlas_n_eventos_tip_inundacoes,
            alagamentos = atlas_n_eventos_tip_alagamentos,
            pct_chuvas_intensas = 100 * chuvas_intensas / pmax(1, hid_total),
            meses_ext_chuva)
salvar_tab(troca, "D03_enxurradas_x_chuvas_intensas",
           "Série anual 2008-2025 das tipologias e da % de 'Chuvas Intensas' no total")
p_troca <- serie %>% select(ano, all_of(unname(tip_hidro))) %>%
  pivot_longer(-ano) %>% mutate(name = names(tip_hidro)[match(name, tip_hidro)]) %>%
  filter(name %in% c("Enxurradas", "Chuvas intensas", "Alagamentos", "Inundações")) %>%
  ggplot(aes(ano, value, colour = name)) +
  geom_vline(xintercept = 2012.5, linetype = 2, colour = "grey50") +
  geom_line(linewidth = .7) + geom_point(size = 1.2) +
  scale_colour_manual(values = cores_tip, name = NULL) +
  scale_x_continuous(breaks = seq(1991, 2025, 4)) +
  labs(title = "Tipologias do grupo hidrológico por ano",
       subtitle = "'Chuvas intensas' (desde 2013) cresce enquanto 'Enxurradas' recua",
       x = "Ano", y = "Nº de eventos (35 municípios)")
salvar_fig(p_troca, "D1_enxurradas_x_chuvas_intensas", 10, 5.5, FONTE_ATLAS,
           "Séries das tipologias", "D03_enxurradas_x_chuvas_intensas")

######@> D04 - Correlação anual eventos x clima x ano (Etapa 04)...
cor_reg <- map_dfr(names(alvos), function(a) {
  y <- serie[[alvos[[a]]]]
  bind_rows(spearman_seguro(serie$ano, y) %>% mutate(relacao = "eventos x ano"),
            spearman_seguro(serie$meses_ext_chuva, y) %>% mutate(relacao = "eventos x meses extremos de chuva"),
            spearman_seguro(serie$prec_media, y) %>% mutate(relacao = "eventos x precipitação média"),
            spearman_seguro(serie$vento_medio, y) %>% mutate(relacao = "eventos x vento médio (controle negativo)")) %>%
    mutate(alvo = a, .before = 1)
}) %>% bind_rows(spearman_seguro(serie$ano, serie$meses_ext_chuva) %>%
                   mutate(alvo = "(clima)", relacao = "meses extremos x ano", .before = 1))
salvar_tab(cor_reg, "D04_correlacao_anual_registro_clima",
           "Spearman entre séries anuais agregadas (n = 35 anos); descritivo")

######@> D05 - Dose-resposta descritiva e controle negativo (Etapa 10)...
## P(ao menos 1 evento no município-ano) por classe do percentil LOCAL do
## descritor (município, entre anos). Vento é o controle negativo: espera-se
## que não "explique" o hidrológico tanto quanto a chuva.
dic_desc <- c(prec_anual = "Precipitação anual", prec_max_mes = "Maior precipitação mensal",
              prec_3m_max = "Maior acumulado de 3 meses",
              meses_ext_chuva = "Meses de chuva extrema",
              vento_medio = "Vento médio (controle negativo)")
an_p <- an %>% group_by(geocodigo) %>%
  mutate(across(c(prec_anual, prec_max_mes, prec_3m_max, vento_medio),
                ~ rank(.x) / n(), .names = "{.col}_p")) %>% ungroup()
cortes_p <- c(0, .25, .50, .75, .90, 1)
rot_p <- c("<=P25", "P25-50", "P50-75", "P75-90", ">P90")
dose <- map_dfr(names(dic_desc), function(v) map_dfr(names(alvos), function(a) {
  d <- an_p %>% mutate(y = .data[[alvos[[a]]]])
  d <- if(v == "meses_ext_chuva") d %>% mutate(classe = cut(meses_ext_chuva, c(-1, 0, 1, 12), labels = c("0", "1", "2+")))
       else d %>% mutate(classe = cut(.data[[paste0(v, "_p")]], cortes_p, labels = rot_p, include.lowest = TRUE))
  bind_rows(d %>% mutate(periodo = "1991-2025"), d) %>%
    group_by(periodo, classe) %>%
    summarise(n_municipio_anos = n(), P_evento = mean(y > 0), media_eventos = mean(y), .groups = "drop") %>%
    mutate(descritor = dic_desc[[v]], alvo = a, classe = as.character(classe), .before = 1)
}))
salvar_tab(dose, "D05_dose_resposta_descritiva",
           "Proporção de município-anos com evento por classe do descritor climático, por período e alvo")
forca <- dose %>% group_by(alvo, descritor, periodo) %>%
  summarise(classe_baixa = first(classe), P_baixa = first(P_evento),
            classe_alta = last(classe), P_alta = last(P_evento),
            razao_alta_baixa = ifelse(P_baixa > 0, P_alta / P_baixa, NA), .groups = "drop")
salvar_tab(forca, "D06_forca_dose_resposta",
           "Razão P(evento) classe mais alta / mais baixa; vento = controle negativo")
p_dose <- dose %>% filter(periodo != "1991-2025",
                          !(alvo == "Só chuvas intensas" & periodo == "1991-2012")) %>%
  mutate(alvo = factor(alvo, levels = names(alvos)),
         classe = factor(classe, levels = c(rot_p, "0", "1", "2+")),
         descritor = factor(descritor, levels = dic_desc)) %>%
  ggplot(aes(classe, P_evento, colour = alvo, group = interaction(alvo, periodo), linetype = periodo)) +
  geom_line(linewidth = .6) + geom_point(size = 1.4) +
  facet_wrap(~ descritor, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = cores_alvo, name = NULL) +
  scale_linetype_manual(values = c("1991-2012" = 2, "2013-2025" = 1), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proporção de município-anos com ao menos um evento, por classe do descritor",
       subtitle = "Percentil local (município, entre anos); vento = controle negativo. Em 1991-2012 total e 'sem chuvas intensas' coincidem (linha laranja tracejada)",
       x = "Classe do descritor", y = "P(evento no município-ano)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), legend.box = "vertical")
salvar_fig(p_dose, "D2_dose_resposta_controle_negativo", 14, 6, paste(FONTE_ATLAS, FONTE_ERA5, sep = "; "),
           "Dose-resposta por descritor e período", "D05_dose_resposta_descritiva")

######@> D07 - Indício de coleta: registro por município e reconhecimento...
## Quantos municípios registram algo por ano e a fração de eventos
## 'reconhecidos' (todas as tipologias; a base não separa por grupo).
reg <- an %>% group_by(ano) %>%
  summarise(municipios_com_registro_hid = sum(hid_total > 0),
            eventos_todos_grupos = sum(atlas_n_eventos),
            reconhecidos_todos_grupos = sum(atlas_n_reconhecidos),
            registros_simples_todos_grupos = sum(atlas_n_registros_simples)) %>%
  mutate(pct_reconhecidos = 100 * reconhecidos_todos_grupos / pmax(1, eventos_todos_grupos))
salvar_tab(reg, "D07_indicadores_de_registro",
           "Municípios com registro hidrológico por ano; reconhecidos x registros simples (todos os grupos)")

######@> D08 - Perfil municipal + uso da terra (Etapas 12 e 13, só bruto)...
ult_mapb <- max(an$ano[!is.na(an$mapb_urb_area_ha)])
perfil <- an %>% group_by(geocodigo, municipio) %>%
  summarise(hid_total = sum(hid_total), hid_sem_chuva = sum(hid_sem_chuva),
            hid_so_chuva = sum(hid_so_chuva), prec_anual_media = mean(prec_anual),
            vento_medio = mean(vento_medio), .groups = "drop") %>%
  left_join(b$municipios %>% transmute(geocodigo, area_km2 = area_km2_shape,
                                       latitude = lat_centroide, longitude = lon_centroide),
            by = "geocodigo") %>%
  left_join(an %>% filter(ano == ult_mapb) %>%
              transmute(geocodigo, urbano_pct = mapb_cob_n2_area_urbanizada_pct,
                        floresta_pct = mapb_cob_n1_floresta_pct,
                        agropecuaria_pct = mapb_cob_n1_agropecuaria_pct,
                        risco_ha = mapb_risco_total_ha), by = "geocodigo") %>%
  left_join(an %>% filter(ano %in% c(1991, ult_mapb)) %>% group_by(geocodigo) %>%
              summarise(ganho_urbano_pp = diff(mapb_cob_n2_area_urbanizada_pct[order(ano)])),
            by = "geocodigo") %>%
  mutate(dens_total = 1000 * hid_total / area_km2,
         dens_sem_chuva = 1000 * hid_sem_chuva / area_km2,
         dens_so_chuva = 1000 * hid_so_chuva / area_km2,
         pct_chuva_intensa = 100 * hid_so_chuva / pmax(1, hid_total))
tend_mun <- an %>% group_by(geocodigo) %>%
  group_modify(~ tibble(tau_hid_total = mann_kendall(.x$hid_total)$tau,
                        tau_hid_sem_chuva = mann_kendall(.x$hid_sem_chuva)$tau)) %>% ungroup()
perfil <- perfil %>% left_join(tend_mun, by = "geocodigo")
salvar_tab(perfil, "D08_perfil_municipal_uso_terra",
           paste0("Perfil municipal: eventos, densidade, clima médio, MapBiomas em ", ult_mapb, ", ganho urbano 1991-", ult_mapb))

desc_mun <- c(urbano_pct = "Área urbanizada (%)", ganho_urbano_pp = "Ganho de área urbanizada (p.p.)",
              floresta_pct = "Floresta (%)", agropecuaria_pct = "Agropecuária (%)",
              risco_ha = "Área urbana em risco (ha)", area_km2 = "Área do município (km²)",
              prec_anual_media = "Precipitação anual média (mm)",
              vento_medio = "Vento médio (m/s)", latitude = "Latitude do centroide")
resp_mun <- c("Eventos - total" = "hid_total", "Eventos - sem chuvas intensas" = "hid_sem_chuva",
              "Eventos - só chuvas intensas" = "hid_so_chuva", "Eventos por mil km² - total" = "dens_total",
              "% chuvas intensas no total" = "pct_chuva_intensa")
assoc <- map_dfr(names(resp_mun), function(r) map_dfr(names(desc_mun), function(x)
  spearman_seguro(perfil[[desc_mun_col <- x]], perfil[[resp_mun[[r]]]]) %>%
    mutate(resposta = r, descritor = desc_mun[[x]], coluna = x, .before = 1))) %>%
  left_join(map_dfr(names(desc_mun), ~ tibble(coluna = .x,
            rho_descritor_x_latitude = suppressWarnings(cor(perfil[[.x]], perfil$latitude,
                                                            method = "spearman", use = "complete.obs")))),
            by = "coluna")
salvar_tab(assoc, "D09_associacao_municipal_bruta",
           "Spearman bruto entre municípios (n = 35); coluna extra mostra quanto cada descritor acompanha a latitude. Correlações parciais do script original foram cortadas (usam regressão)")
p_assoc <- assoc %>% filter(resposta %in% names(resp_mun)[1:3], coluna != "latitude") %>%
  mutate(resposta = factor(resposta, levels = names(resp_mun)[1:3])) %>%
  ggplot(aes(rho, reorder(descritor, rho), colour = resposta)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2.4, position = position_dodge(width = .5)) +
  scale_colour_manual(values = setNames(cores_alvo, names(resp_mun)[1:3]), name = NULL) +
  labs(title = "Associação entre municípios: eventos x descritores (Spearman, n = 35)",
       subtitle = "Correlação bruta, sem controle; latitude e área acompanham vários descritores (ver D09)",
       x = "rho de Spearman", y = NULL)
salvar_fig(p_assoc, "D3_associacao_municipal", 10, 5.5,
           paste(FONTE_ATLAS, FONTE_ERA5, FONTE_MAPB, sep = "; "),
           "Associação municipal bruta", "D09_associacao_municipal_bruta")

######@> D10 - I de Moran (4 vizinhos mais próximos, 999 permutações)...
set.seed(SEMENTE)
coord <- perfil %>% select(longitude, latitude) %>% as.matrix()
lw <- nb2listw(knn2nb(knearneigh(coord, k = 4)), style = "W")
ind_moran <- c(dens_total = "Eventos/mil km² - total", dens_sem_chuva = "Eventos/mil km² - sem chuvas intensas",
               dens_so_chuva = "Eventos/mil km² - só chuvas intensas", pct_chuva_intensa = "% chuvas intensas",
               tau_hid_total = "Tendência municipal (tau) - total", tau_hid_sem_chuva = "Tendência municipal (tau) - sem chuvas intensas",
               urbano_pct = "Área urbanizada (%)", prec_anual_media = "Precipitação anual média")
moran <- map_dfr(names(ind_moran), function(v) {
  x <- perfil[[v]]
  if(any(is.na(x)) || sd(x) == 0) return(tibble(indicador = ind_moran[[v]], coluna = v, I = NA, p_valor = NA))
  m <- moran.mc(x, lw, nsim = 999)
  tibble(indicador = ind_moran[[v]], coluna = v, I = unname(m$statistic), p_valor = m$p.value)
}) %>% mutate(leitura_descritiva = case_when(is.na(p_valor) ~ "indefinido",
                                             p_valor < alfa & I > 0 ~ "valores parecidos entre vizinhos",
                                             p_valor < alfa & I < 0 ~ "vizinhos contrastantes",
                                             TRUE ~ "sem padrão espacial detectável"),
             vizinhanca = "4 vizinhos mais próximos (centroides), pesos padronizados por linha",
             permutacoes = 999, semente = SEMENTE)
salvar_tab(moran, "D10_moran_municipal",
           "I de Moran (descritivo) para densidade de eventos, composição, tendências e dois descritores")

######@> D11 - Mapas...
shp <- st_read(file.path(dir_dados, "shp", "municipio_abrangencia_PMCAP-SC.shp"), quiet = TRUE) %>%
  st_make_valid() %>% st_transform(4326)
atr <- st_drop_geometry(shp)
## Mesma regra do script original: o shapefile tem três colunas de código
## preenchidas em partes diferentes (codigo_ibg, GEOCODIGO, GEOCODIGO_).
col_cod <- "coalesce(codigo_ibg, GEOCODIGO, GEOCODIGO_)"
shp$geocodigo <- coalesce(as.character(atr$codigo_ibg), as.character(atr$GEOCODIGO),
                          as.character(atr$GEOCODIGO_))
shp <- shp %>% group_by(geocodigo) %>% summarise(.groups = "drop") %>%
  left_join(b$municipios %>% select(geocodigo, municipio), by = "geocodigo") %>%
  left_join(perfil %>% select(-municipio), by = "geocodigo")
salvar_tab(tibble(n_feicoes_originais = nrow(atr), n_poligonos = nrow(shp),
                  municipios_sem_poligono = paste(setdiff(b$municipios$municipio, shp$municipio), collapse = "; "), coluna_codigo_no_shapefile = col_cod,
                  sem_atlas = sum(is.na(shp$hid_total)), crs = st_crs(shp)$input),
           "D11_shapefile_conferencia", "Conferência da junção shapefile x base")
mapa <- function(var, titulo, cor_alta) {
  ggplot(shp) + geom_sf(aes(fill = .data[[var]]), colour = "white", linewidth = .15) +
    scale_fill_gradient(low = "grey95", high = cor_alta, name = NULL, na.value = "grey35") +
    labs(title = titulo) + theme_void(base_size = 9) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 9),
          legend.key.width = unit(1.2, "cm"))
}
p_mapa <- mapa("dens_sem_chuva", "Eventos/mil km² - sem chuvas intensas", "#D55E00") |
  mapa("dens_so_chuva", "Eventos/mil km² - só chuvas intensas", "#009E73") |
  mapa("pct_chuva_intensa", "% de chuvas intensas no total", "#0072B2") |
  mapa("urbano_pct", paste0("Área urbanizada, % (", ult_mapb, ")"), "#CC79A7")
p_mapa <- p_mapa + plot_annotation(title = "Distribuição espacial, 1991-2025 (cinza escuro nos três primeiros mapas = sem Atlas: Camboriú e Paulo Lopes)")
salvar_fig(p_mapa, "D4_mapas_densidade_composicao", 14, 7,
           paste(FONTE_ATLAS, FONTE_MAPB, "shapefile municipio_abrangencia_PMCAP-SC", sep = "; "),
           "Mapas municipais", "D08_perfil_municipal_uso_terra; D10_moran_municipal")

fechar_registro()
