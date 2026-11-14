#!/usr/bin/env python3
"""00_prepara_dados.py - converte os parquet da base consolidada em CSV.

Por que existe: o R deste ambiente não tem o pacote 'arrow'; o Python lê
parquet com 'pyarrow'. Nada é recalculado aqui: só troca de formato.

Uso: python3 00_prepara_dados.py <pasta_com_os_parquet> <saida_cap4/dados_entrada>
A pasta de origem deve conter (cópias baixadas do Google Drive, intocadas):
  Consolidado_municipio_ano.parquet, Consolidado_municipios.parquet,
  Consolidado_dicionario_anual.parquet, Consolidado_dicionario_mensal.parquet,
  Consolidado_diagnostico_cobertura.parquet, Consolidado_diagnostico_temporal.parquet,
  mensal/Consolidado_<geocodigo>_<nome>.parquet (37 arquivos, base mensal por município)
  shp/municipio_abrangencia_PMCAP-SC.{shp,shx,dbf,prj}
"""
import sys, glob, os, shutil, hashlib
import pandas as pd

orig, dest = sys.argv[1], sys.argv[2]
os.makedirs(dest, exist_ok=True)
registro = []

def md5(p):
    return hashlib.md5(open(p, 'rb').read()).hexdigest()

for nome in ["municipio_ano", "municipios", "dicionario_anual",
             "dicionario_mensal", "diagnostico_cobertura", "diagnostico_temporal"]:
    p = os.path.join(orig, f"Consolidado_{nome}.parquet")
    df = pd.read_parquet(p)
    df.to_csv(os.path.join(dest, f"{nome}.csv"), index=False)
    registro.append((os.path.basename(p), md5(p), len(df), df.shape[1]))

fs = sorted(glob.glob(os.path.join(orig, "mensal", "Consolidado_42*.parquet")))
fs = [f for f in fs if not f.endswith("_anual.parquet")]
mensal = pd.concat([pd.read_parquet(f) for f in fs], ignore_index=True)
mensal.to_csv(os.path.join(dest, "municipio_mes.csv"), index=False)
for f in fs:
    registro.append((os.path.basename(f), md5(f), None, None))
registro.append(("municipio_mes (concatenado)", None, len(mensal), mensal.shape[1]))

os.makedirs(os.path.join(dest, "shp"), exist_ok=True)
for f in glob.glob(os.path.join(orig, "shp", "*")):
    shutil.copy(f, os.path.join(dest, "shp"))
    registro.append((os.path.basename(f), md5(f), None, None))

pd.DataFrame(registro, columns=["arquivo", "md5", "linhas", "colunas"]).to_csv(
    os.path.join(dest, "registro_entradas.csv"), index=False)
print(f"{len(fs)} arquivos mensais; base mensal {mensal.shape}; ok")
