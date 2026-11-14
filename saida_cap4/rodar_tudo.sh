#!/usr/bin/env bash
# Reproduz todas as tabelas e figuras do capítulo 4. Rodar dentro de saida_cap4/.
# Passo 0 (uma vez): python3 scripts/00_prepara_dados.py <pasta_parquet> dados_entrada
set -euo pipefail
export LC_ALL=C.UTF-8
rm -f tabelas/*.csv figuras/*.png
for s in 01_metadados 02_series_temporais 03_variavel_resposta 04_preditoras 05_evidencias_hipoteses 06_quadro_evidencias; do
  echo "== $s"; Rscript scripts/$s.R 2>&1 | grep -v -i "winding\|CPL_read_ogr" || true
done
