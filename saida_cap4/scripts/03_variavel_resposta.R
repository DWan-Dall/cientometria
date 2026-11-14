########################################################################
## 03_variavel_resposta.R - (C, parte 1) variável-resposta: desastres
## hidrológicos (Atlas), em três versões: total, sem "Chuvas Intensas" e
## só "Chuvas Intensas". Unidade: município-ano, 35 municípios, 1991-2025.
## Reaproveita as Etapas 02, 03 e 07 de script_AED_Hidrologico_ver02.R.
## Sem modelagem.
########################################################################
SCRIPT <- "03_variavel_resposta.R"
source(file.path(Sys.getenv("CAP4_DIR", "."), "scripts", "00_comum.R"))
b <- ler_bases()
an <- montar_analise(b)

######@> C01 - Totais e definição dos três alvos...
totais <- map_dfr(names(alvos), function(a) {
  v <- an[[alvos[[a]]]]
  tibble(alvo = a, eventos = sum(v), municipios = n_distinct(an$geocodigo),
         municipio_anos = length(v), anos = n_distinct(an$ano),
         ano_primeiro_evento = min(an$ano[v > 0]),
         municipios_com_evento = n_distinct(an$geocodigo[v > 0]))
}) %>% mutate(pct_do_total = 100 * eventos / eventos[1])
salvar_tab(totais, "C01_totais_variavel_resposta",
           "Totais dos três alvos; confere 732 = 443 + 289")

######@> C02 - Composição por tipologia (total do período)...
comp_total <- map_dfr(names(tip_hidro), function(t) {
  v <- an[[tip_hidro[[t]]]]
  tibble(tipologia = t, eventos = sum(v), municipio_anos_com_evento = sum(v > 0),
         primeiro_ano = if(any(v > 0)) min(an$ano[v > 0]) else NA,
         ultimo_ano = if(any(v > 0)) max(an$ano[v > 0]) else NA)
}) %>% mutate(pct = 100 * eventos / sum(eventos))
salvar_tab(comp_total, "C02_composicao_tipologica_total",
           "Eventos por tipologia do grupo hidrológico, 1991-2025")

######@> C03 - Composição por bloco de 5 anos (Etapa 02 original)...
bloco5 <- function(ano) cut(ano, c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                            labels = c("1991-95","1996-00","2001-05","2006-10",
                                       "2011-15","2016-20","2021-25"))
comp_bloco <- an %>% mutate(bloco = bloco5(ano)) %>% group_by(bloco) %>%
  summarise(across(all_of(unname(tip_hidro)), sum)) %>%
  rename_with(~ names(tip_hidro)[match(.x, tip_hidro)], all_of(unname(tip_hidro))) %>%
  mutate(total = rowSums(across(-bloco)),
         pct_chuvas_intensas = 100 * `Chuvas intensas` / total,
         pct_enxurradas = 100 * Enxurradas / total)
salvar_tab(comp_bloco, "C03_composicao_tipologica_por_bloco",
           "Composição tipológica por bloco de 5 anos")

p1 <- comp_bloco %>% select(bloco, all_of(names(tip_hidro))) %>%
  pivot_longer(-bloco, names_to = "tipologia", values_to = "n") %>%
  mutate(tipologia = factor(tipologia, levels = rev(names(tip_hidro)))) %>%
  ggplot(aes(bloco, n, fill = tipologia)) +
  geom_col(width = .75, colour = "white", linewidth = .3) +
  scale_fill_manual(values = cores_tip, name = NULL, breaks = names(tip_hidro)) +
  labs(title = "Composição do grupo hidrológico por bloco de 5 anos",
       subtitle = "35 municípios; 'Chuvas intensas' aparece só a partir de 2013",
       x = NULL, y = "Nº de eventos registrados")
salvar_fig(p1, "C1_composicao_tipologica_blocos", 9, 5.5, FONTE_ATLAS,
           "Barras empilhadas por tipologia e bloco", "C03_composicao_tipologica_por_bloco")

######@> C04 - Estatísticas descritivas no município-ano (Etapa 03 original)...
serie <- an %>% group_by(ano) %>% summarise(across(all_of(unname(alvos)), sum))
desc_resp <- map_dfr(names(alvos), function(a) {
  v <- an[[alvos[[a]]]]
  descrever(v) %>%
    mutate(alvo = a, unidade = "município-ano",
           razao_var_media = var(v) / mean(v),
           .before = 1)
}) %>% bind_rows(map_dfr(names(alvos), function(a) {
  v <- serie[[alvos[[a]]]]
  descrever(v) %>% mutate(alvo = a, unidade = "ano (soma dos 35 municípios)",
                          razao_var_media = var(v) / mean(v), .before = 1)
}))
salvar_tab(desc_resp, "C04_descritivas_resposta",
           "Estatísticas descritivas dos três alvos no município-ano e na série anual agregada; razão variância/média (sobredispersão, só descritiva)")

######@> C05 - Distribuição de frequências das contagens...
freq <- map_dfr(names(alvos), function(a) {
  tibble(alvo = a, eventos_no_municipio_ano = an[[alvos[[a]]]])
}) %>% mutate(classe = ifelse(eventos_no_municipio_ano >= 5, "5+",
                              as.character(eventos_no_municipio_ano))) %>%
  count(alvo, classe, name = "municipio_anos") %>%
  group_by(alvo) %>% mutate(pct = 100 * municipio_anos / sum(municipio_anos)) %>% ungroup()
salvar_tab(freq, "C05_frequencia_contagens",
           "Distribuição de frequências: quantos município-anos têm 0, 1, 2, ... eventos")
p2 <- freq %>% mutate(alvo = factor(alvo, levels = names(alvos)),
                      classe = factor(classe, levels = c(0:4, "5+"))) %>%
  ggplot(aes(classe, pct, fill = alvo)) + geom_col(width = .7) +
  geom_text(aes(label = sprintf("%.1f", pct)), vjust = -.3, size = 3) +
  facet_wrap(~ alvo) + scale_fill_manual(values = cores_alvo, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(title = "Distribuição das contagens no município-ano",
       subtitle = "1.225 município-anos (35 municípios x 35 anos); predominância de zeros",
       x = "Eventos no município-ano", y = "% dos município-anos")
salvar_fig(p2, "C2_distribuicao_contagens", 11, 4.8, FONTE_ATLAS,
           "Histograma das contagens por alvo", "C05_frequencia_contagens")

######@> C06 - Ausentes da resposta...
## Na janela, 'zero' = ano sem registro; NA = fora da cobertura. Os dois
## municípios sem Atlas ficam fora (não são zeros).
aus <- b$anual %>% filter(ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
  group_by(tem_atlas) %>%
  summarise(municipios = n_distinct(geocodigo), municipio_anos = n(),
            na_hidrologico = sum(is.na(atlas_n_eventos_hidrologico)),
            pct_na = 100 * mean(is.na(atlas_n_eventos_hidrologico)),
            zeros = sum(atlas_n_eventos_hidrologico == 0, na.rm = TRUE),
            municipios_lista = paste(sort(unique(municipio)), collapse = "; ")) %>%
  mutate(municipios_lista = ifelse(tem_atlas, "(35 municípios)", municipios_lista))
salvar_tab(aus, "C06_ausentes_resposta",
           "Ausentes da variável-resposta na janela 1991-2025, por presença de Atlas")

######@> C07 - Perfil municipal (Etapa 07 original)...
ranking <- an %>% group_by(geocodigo, municipio) %>%
  summarise(hid_total = sum(hid_total), hid_sem_chuva = sum(hid_sem_chuva),
            hid_so_chuva = sum(hid_so_chuva), anos_com_evento = sum(hid_total > 0),
            max_num_ano = max(hid_total), .groups = "drop") %>%
  left_join(b$municipios %>% select(geocodigo, area_km2_shape, lat_centroide), by = "geocodigo") %>%
  mutate(eventos_por_mil_km2 = 1000 * hid_total / area_km2_shape,
         pct_chuvas_intensas = 100 * hid_so_chuva / pmax(1, hid_total)) %>%
  arrange(desc(hid_total))
salvar_tab(ranking, "C07_perfil_municipal_resposta",
           "Eventos por município (três alvos), anos com evento, densidade por área")
p3 <- ranking %>% select(municipio, hid_sem_chuva, hid_so_chuva, hid_total) %>%
  pivot_longer(c(hid_sem_chuva, hid_so_chuva)) %>%
  mutate(name = factor(names(alvos)[match(name, alvos)], levels = names(alvos)[3:2])) %>%
  ggplot(aes(value, reorder(municipio, hid_total), fill = name)) +
  geom_col(width = .7, colour = "white", linewidth = .2) +
  scale_fill_manual(values = cores_alvo, name = NULL) +
  labs(title = "Eventos hidrológicos por município, 1991-2025",
       subtitle = "Barra inteira = hidrológico total", x = "Nº de eventos", y = NULL) +
  theme(axis.text.y = element_text(size = 8))
salvar_fig(p3, "C3_eventos_por_municipio", 8, 8, FONTE_ATLAS,
           "Ranking municipal", "C07_perfil_municipal_resposta")

######@> C08 - Danos associados ao grupo hidrológico (contexto)...
danos <- an %>% summarise(
  mortos = sum(atlas_dh_mortos_hidrologico, na.rm = TRUE),
  afetados = sum(atlas_dh_afetados_total_hidrologico, na.rm = TRUE),
  impacto_economico_total = sum(atlas_impacto_economico_total_hidrologico, na.rm = TRUE),
  na_impacto = sum(is.na(atlas_impacto_economico_total_hidrologico)))
salvar_tab(danos, "C08_danos_hidrologicos_contexto",
           "Danos humanos e econômicos do grupo hidrológico (unidade monetária conforme Atlas; não convertida)")

fechar_registro()
