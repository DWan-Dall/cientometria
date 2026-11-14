# saida_cap4 — tabelas e figuras do Capítulo 4 (Análise exploratória de dados)

Esta pasta traz só tabelas, figuras e os scripts que as geram. Não há texto do capítulo e não há modelagem: nenhum script ajusta regressão, Poisson ou binomial negativa, aprendizado de máquina ou agrupamento. Mann-Kendall, Sen e I de Moran aparecem apenas como descrição.

Os originais ficaram intocados. Os dados de entrada são cópias baixadas do Google Drive, convertidas de parquet para CSV.

## Estrutura

| Pasta/arquivo | Conteúdo |
|---|---|
| `scripts/00_prepara_dados.py` | Converte os parquet do Drive em CSV (`dados_entrada/`). Não recalcula nada. |
| `scripts/00_comum.R` | Funções comuns, definição dos alvos e leitura das bases. |
| `scripts/01_metadados.R` | (A) Metadados das fontes e verificação das unidades do ERA5. |
| `scripts/02_series_temporais.R` | (B) Séries por base (Atlas anual, ERA5 anual e mensal, MapBiomas anual) e figura-síntese. |
| `scripts/03_variavel_resposta.R` | (C) Variável-resposta hidrológica: total, sem "Chuvas Intensas" e só "Chuvas Intensas". |
| `scripts/04_preditoras.R` | (C) Preditoras ERA5 e MapBiomas: descritivas, distribuição, ausentes e correlação entre elas. |
| `scripts/05_evidencias_hipoteses.R` | (D) Evidências para as hipóteses de coleta e de outros fatores. |
| `scripts/06_quadro_evidencias.R` | (D) Quadro-síntese de evidências e índice de saídas. |
| `rodar_tudo.sh` | Roda os scripts 01 a 06 em ordem. |
| `tabelas/*.csv` | 38 tabelas, mais `00_indice_saidas.csv` e `_registro_*.csv`. |
| `figuras/*.png` | 15 figuras, 300 dpi, em português, com fonte e linha do script no rodapé. |
| `rascunho_achados.md` | Lista de achados com a tabela de origem de cada um (não é redação final). |
| `dados_entrada/` | CSV convertidos e shapefile. Não versionar (dados da pesquisa). |

### Rastreabilidade dos números

Todo CSV tem duas colunas finais:
- `script`: o script que gerou a tabela;
- `linha_script`: a linha em que `salvar_tab()` grava aquela tabela.

O rodapé de cada figura traz o script e a linha. O arquivo `tabelas/00_indice_saidas.csv` lista as 53 saídas com descrição e com a tabela que alimenta cada figura.

## Como reproduzir

1. Baixe do Google Drive, sem alterar, os arquivos da pasta `Outputs/Consolidado/Parquet` da base:
   - `Consolidado_municipio_ano.parquet` e `Consolidado_municipios.parquet`;
   - `Consolidado_dicionario_{anual,mensal}.parquet` e `Consolidado_diagnostico_{cobertura,temporal}.parquet`;
   - os 37 arquivos `Municipal/Consolidado_<geocodigo>_<nome>.parquet` (versão mensal, sem `_anual`), em `mensal/`;
   - o shapefile `municipio_abrangencia_PMCAP-SC.{shp,shx,dbf,prj}`, em `shp/`.

   O `Consolidado_SC.RData` (16,6 MB) e o `Consolidado_municipio_mes.parquet` (19 MB) não foram usados porque passam do limite de download do conector. A base mensal foi remontada a partir dos 37 arquivos municipais e tem 38.443 linhas e 216 colunas.

2. Converta os parquet:

   ```bash
   python3 scripts/00_prepara_dados.py <pasta_baixada> dados_entrada
   ```

   O registro `dados_entrada/registro_entradas.csv` guarda o MD5 de cada arquivo de origem.

3. Gere as tabelas e figuras:

   ```bash
   ./rodar_tudo.sh
   ```

   O script usa `LC_ALL=C.UTF-8`; sem isso, os acentos dos CSV saem corrompidos.

### Versões usadas

- R 4.3.3, com dplyr 1.1.4, tidyr 1.3.1, purrr 1.0.2, readr 2.1.5, ggplot2 3.4.4, patchwork 1.2.0, scales 1.3.0, sf 1.0.15 (GDAL 3.8.4) e spdep 1.3.1.
- Python 3.13, com pandas 3.0.5 e pyarrow 26.0.1 (só no passo 00).

### Sementes

- `SEMENTE = 42` (`00_comum.R`), fixada com `set.seed()` no início de cada script.
- A semente é refixada antes do I de Moran (`moran.mc`, 999 permutações), que é o único passo aleatório. Os valores-p do Moran podem variar na terceira casa decimal se a semente mudar.

## Definições (iguais às de `script_AED_Hidrologico_ver02.R`)

- **Janela de análise:** 1991–2025, 35 municípios com Atlas. Camboriú e Paulo Lopes não têm Atlas; também não têm MapBiomas na base (ver pendências).
- **Hidrológico total:** coluna `atlas_n_eventos_hidrologico`. O script confere que ela é igual a "sem chuvas intensas" + "só chuvas intensas".
- **Sem chuvas intensas:** alagamentos + enxurradas + inundações + movimento de massa.
- **Mês de chuva extrema:** percentil local (município × mês do ano, 1991–2025) maior que 0,95.
- **Corte de período:** 2013, o primeiro ano com "Chuvas Intensas". O corte de 2012 da AED original também é reportado (D01).

## O que foi reaproveitado e o que foi cortado

De `script_AED_Hidrologico_ver02.R`:

| Etapa original | Situação |
|---|---|
| 01 Leitura, alvos, meses extremos | mantida (`00_comum.R`) |
| 02 Composição tipológica | mantida (C03) |
| 03 Distribuição das contagens | mantida (C04, C05) |
| 04 Viés de registro e quebra | mantida (D01, D04), com indicadores acrescentados |
| 05 Mann-Kendall e Sen | mantida como descrição (D02) |
| 06 e 10 Dose-resposta | mantidas como proporções condicionais descritivas (D05, D06) |
| 07 Ranking municipal | mantida (C07) |
| **08 Modelos de Poisson** | **CORTADA** (modelagem) |
| 09 Figuras | refeitas |
| 11 ACF/CCF anual | não reproduzida (fora do escopo pedido) |
| 12 Perfil municipal | mantida (D08) |
| 13 Associações municipais | só a correlação bruta de Spearman (D09). **CORTADAS** as correlações parciais por latitude e área, porque usam resíduos de `lm()`. No lugar, D09 mostra a correlação de cada descritor com a latitude. |
| 14 VIF | **CORTADO** (usa regressão `lm`). Substituído pela matriz de correlação de Spearman entre preditoras (C11 a C13). |
| 14 I de Moran e mapas | mantidos (D10, D4) |
| **15 Modelos de contagem (Poisson/NB) e agrupamento (k-means, silhueta)** | **CORTADA** |
| 16 Exportação XLSX | substituída por CSV individuais |

O `script_AED_Completo_ver00.R` (190 KB) **não** foi reaproveitado diretamente. As funções vieram dele por meio do `ver02`, que já é uma adaptação dele.
