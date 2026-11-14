########################################################################
## 00_comum.R - funções e definições comuns aos scripts do capítulo 4
##
## Origem: funções auxiliares e definição dos alvos copiadas de
## script_AED_Hidrologico_ver02.R (tema gráfico, Mann-Kendall, Sen,
## Spearman seguro, alvos hidrológicos, meses extremos de chuva).
## NÃO há ajuste de modelo em nenhum script desta pasta.
##
## Uso: cada script define SCRIPT <- "<nome>.R" e faz source() deste arquivo.
## Todo CSV salvo leva duas colunas finais: 'script' e 'linha_script'
## (a linha do script em que salvar_tab() foi chamada para aquela tabela).
########################################################################

invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(ggplot2); library(patchwork); library(scales)
})
options(stringsAsFactors = FALSE, scipen = 999, dplyr.summarise.inform = FALSE)
select <- dplyr::select; filter <- dplyr::filter
SEMENTE <- 42
set.seed(SEMENTE)

######@> Diretórios (relativos à pasta saida_cap4)...
dir_base    <- Sys.getenv("CAP4_DIR", ".")
dir_dados   <- file.path(dir_base, "dados_entrada")
dir_tab     <- file.path(dir_base, "tabelas")
dir_fig     <- file.path(dir_base, "figuras")
dir_scripts <- file.path(dir_base, "scripts")
for(d in c(dir_tab, dir_fig)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

######@> Parâmetros (os mesmos de script_AED_Hidrologico_ver02.R)...
ano_ini_atlas  <- 1991
ano_fim_atlas  <- 2025
ano_corte      <- 2013   # primeiro ano com "Chuvas Intensas" no Atlas
limiar_extremo <- 0.95   # percentil local (município x mês do ano)
alfa           <- 0.05

alvos <- c("Hidrológico total"               = "hid_total",
           "Hidrológico sem chuvas intensas" = "hid_sem_chuva",
           "Só chuvas intensas"              = "hid_so_chuva")
cores_alvo <- c("Hidrológico total"               = "#0072B2",
                "Hidrológico sem chuvas intensas" = "#D55E00",
                "Só chuvas intensas"              = "#009E73")
tip_hidro <- c("Enxurradas" = "atlas_n_eventos_tip_enxurradas",
               "Inundações" = "atlas_n_eventos_tip_inundacoes",
               "Alagamentos" = "atlas_n_eventos_tip_alagamentos",
               "Movimento de massa" = "atlas_n_eventos_tip_movimento_de_massa",
               "Chuvas intensas" = "atlas_n_eventos_tip_chuvas_intensas")
cores_tip <- c("Enxurradas" = "#D55E00", "Inundações" = "#0072B2",
               "Alagamentos" = "#56B4E9", "Movimento de massa" = "#E69F00",
               "Chuvas intensas" = "#009E73")

FONTE_ATLAS <- "Atlas Digital de Desastres no Brasil (MIDR/UFSC)"
FONTE_ERA5  <- "ERA5 mensal, Copernicus C3S/ECMWF"
FONTE_MAPB  <- "MapBiomas (coleção não registrada na base; ver LEIA-ME)"

######@> Tema gráfico (o da AED original)...
tema_aed <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
          strip.background = element_rect(fill = "grey92", colour = "grey70"),
          strip.text = element_text(face = "bold", size = base - 1),
          plot.title = element_text(face = "bold", size = base + 2),
          plot.subtitle = element_text(colour = "grey30", size = base - 1),
          plot.caption = element_text(colour = "grey35", size = base - 3, hjust = 0),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = base - 1))
}
theme_set(tema_aed())

######@> Localiza a linha do script em que a tabela/figura é salva...
linha_de <- function(nome) {
  txt <- readLines(file.path(dir_scripts, SCRIPT), warn = FALSE)
  l <- grep(paste0("\"", nome, "\""), txt, fixed = TRUE)
  if(length(l) == 0) NA_integer_ else l[1]
}

registro_saidas <- list()

salvar_tab <- function(dados, arquivo, descricao = "") {
  linha <- linha_de(arquivo)
  out <- as.data.frame(dados) %>% mutate(script = SCRIPT, linha_script = linha)
  write_csv(out, file.path(dir_tab, paste0(arquivo, ".csv")), na = "")
  registro_saidas[[length(registro_saidas) + 1]] <<-
    tibble(tipo = "tabela", arquivo = paste0(arquivo, ".csv"),
           script = SCRIPT, linha_script = linha, descricao = descricao)
  invisible(dados)
}

salvar_fig <- function(plot, arquivo, largura = 10, altura = 6, fonte,
                       descricao = "", tabela = "") {
  linha <- linha_de(arquivo)
  plot <- plot + plot_annotation(
    caption = paste(strwrap(paste0("Fonte: elaboração própria a partir de ", fonte,
                     ". Script ", SCRIPT, ", linha ", linha, "."), width = 12 * largura + 20),
                    collapse = "\n"),
    theme = theme(plot.caption = element_text(colour = "grey35", size = 8,
                                              hjust = 0)))
  ggsave(file.path(dir_fig, paste0(arquivo, ".png")), plot,
         width = largura, height = altura, dpi = 300, bg = "white")
  registro_saidas[[length(registro_saidas) + 1]] <<-
    tibble(tipo = "figura", arquivo = paste0(arquivo, ".png"),
           script = SCRIPT, linha_script = linha,
           descricao = paste0(descricao,
                              ifelse(tabela == "", "", paste0(" [dados: ", tabela, "]"))))
  invisible(NULL)
}

fechar_registro <- function() {
  reg <- bind_rows(registro_saidas)
  write_csv(reg, file.path(dir_tab, paste0("_registro_", sub("\\.R$", "", SCRIPT), ".csv")))
  message("Saídas de ", SCRIPT, ": ", nrow(reg))
}

######@> Mann-Kendall (com correção de empates) - DESCRITIVO...
mann_kendall <- function(x) {
  x <- as.numeric(x[!is.na(x)]); n <- length(x)
  if(n < 4) return(tibble(S = NA_real_, tau = NA_real_, z = NA_real_, p_valor = NA_real_))
  dif <- outer(x, x, "-"); S <- sum(sign(dif[lower.tri(dif)]))
  emp <- as.numeric(table(x))
  var_S <- (n * (n - 1) * (2 * n + 5) - sum(emp * (emp - 1) * (2 * emp + 5))) / 18
  if(var_S <= 0) return(tibble(S = S, tau = NA_real_, z = NA_real_, p_valor = NA_real_))
  z <- if(S > 0) (S - 1) / sqrt(var_S) else if(S < 0) (S + 1) / sqrt(var_S) else 0
  tibble(S = S, tau = S / (n * (n - 1) / 2), z = z, p_valor = 2 * pnorm(-abs(z)))
}

######@> Declividade de Sen (mediana das inclinações), por década...
declividade_sen <- function(y, x) {
  ok <- !is.na(y) & !is.na(x); y <- y[ok]; x <- x[ok]; n <- length(y)
  if(n < 4) return(NA_real_)
  i <- which(lower.tri(matrix(0, n, n)), arr.ind = TRUE)
  dx <- x[i[, 1]] - x[i[, 2]]; dy <- y[i[, 1]] - y[i[, 2]]
  10 * median(dy[dx != 0] / dx[dx != 0])
}

spearman_seguro <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if(length(x) < 4 || sd(x) == 0 || sd(y) == 0)
    return(tibble(rho = NA_real_, p_valor = NA_real_, n = length(x)))
  t <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  tibble(rho = unname(t$estimate), p_valor = t$p.value, n = length(x))
}

######@> Estatísticas descritivas padronizadas...
descrever <- function(x) {
  n_tot <- length(x); x2 <- x[!is.na(x)]
  tibble(n = n_tot, n_ausentes = n_tot - length(x2),
         pct_ausentes = 100 * (n_tot - length(x2)) / n_tot,
         media = mean(x2), dp = sd(x2), cv_pct = 100 * sd(x2) / abs(mean(x2)),
         minimo = min(x2), p05 = quantile(x2, .05, names = FALSE),
         q1 = quantile(x2, .25, names = FALSE), mediana = median(x2),
         q3 = quantile(x2, .75, names = FALSE), p95 = quantile(x2, .95, names = FALSE),
         maximo = max(x2),
         assimetria = mean((x2 - mean(x2))^3) / sd(x2)^3,
         prop_zeros = mean(x2 == 0))
}

######@> Leitura das bases...
ler_bases <- function() {
  anual  <- read_csv(file.path(dir_dados, "municipio_ano.csv"), show_col_types = FALSE,
                     guess_max = 5000)
  mensal <- read_csv(file.path(dir_dados, "municipio_mes.csv"), show_col_types = FALSE,
                     guess_max = 40000)
  municipios <- read_csv(file.path(dir_dados, "municipios.csv"), show_col_types = FALSE)
  anual$geocodigo <- as.character(anual$geocodigo)
  mensal$geocodigo <- as.character(mensal$geocodigo)
  municipios$geocodigo <- as.character(municipios$geocodigo)
  list(anual = anual, mensal = mensal, municipios = municipios)
}

######@> Base de análise: município-ano 1991-2025 com Atlas (35 municípios)...
## Mesmas definições de script_AED_Hidrologico_ver02.R, Etapa 01.
montar_analise <- function(b) {
  anual <- b$anual %>%
    filter(tem_atlas, ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
    mutate(hid_so_chuva  = atlas_n_eventos_tip_chuvas_intensas,
           hid_sem_chuva = atlas_n_eventos_tip_alagamentos +
             atlas_n_eventos_tip_enxurradas + atlas_n_eventos_tip_inundacoes +
             atlas_n_eventos_tip_movimento_de_massa,
           hid_total = atlas_n_eventos_hidrologico)
  stopifnot(all(anual$hid_total == anual$hid_sem_chuva + anual$hid_so_chuva))

  ## Meses extremos: percentil local (município x mês do ano) > 0,95
  extremos <- b$mensal %>%
    filter(tem_atlas, ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
    group_by(geocodigo, mes) %>%
    mutate(p_chuva = rank(era5_tp_mm_mes, na.last = "keep") /
             sum(!is.na(era5_tp_mm_mes))) %>%
    ungroup() %>%
    mutate(mes_extremo_chuva = p_chuva > limiar_extremo) %>%
    group_by(geocodigo, ano) %>%
    summarise(meses_ext_chuva = sum(mes_extremo_chuva),
              prec_anual = sum(era5_tp_mm_mes), .groups = "drop")

  ## Descritores anuais (Etapa 10 do script original)
  descritores <- b$mensal %>%
    arrange(geocodigo, ano, mes) %>%
    group_by(geocodigo) %>%
    mutate(tp_3m = era5_tp_mm_mes + lag(era5_tp_mm_mes, 1) + lag(era5_tp_mm_mes, 2)) %>%
    ungroup() %>%
    filter(ano >= ano_ini_atlas, ano <= ano_fim_atlas) %>%
    group_by(geocodigo, ano) %>%
    summarise(prec_max_mes = max(era5_tp_mm_mes, na.rm = TRUE),
              prec_3m_max = max(tp_3m, na.rm = TRUE),
              vento_medio = mean(era5_si10, na.rm = TRUE),
              temp_media = mean(era5_t2m_C, na.rm = TRUE),
              prec_conv_anual = sum(era5_cp_mm_dia * dias_no_mes, na.rm = TRUE),
              evap_anual = sum(era5_e_mm_dia * dias_no_mes, na.rm = TRUE),
              nuvens_pct = mean(era5_tcc_pct, na.rm = TRUE),
              .groups = "drop")

  anual %>%
    left_join(extremos, by = c("geocodigo", "ano")) %>%
    left_join(descritores, by = c("geocodigo", "ano")) %>%
    mutate(periodo = ifelse(ano < ano_corte,
                            paste0(ano_ini_atlas, "-", ano_corte - 1),
                            paste0(ano_corte, "-", ano_fim_atlas)))
}
