# Rascunho de achados — Capítulo 4 (sem redação final)

Cada item traz o número, a tabela de origem (pasta `tabelas/`) e, quando for o caso, a ressalva. Os números estão arredondados; o valor exato está no CSV. As colunas `script` e `linha_script` de cada CSV indicam onde o número foi gerado.

> **Atenção:** todos os valores de precipitação usam a conversão já aplicada na base (tp × 1000 × dias do mês). A decisão sobre essa conversão está pendente com a autora (A04).

## A. Fontes de dados

1. As bases cobrem períodos diferentes:
   - Atlas: 1991–2025, 35 municípios;
   - ERA5: 1940 a jul/2026, 37 municípios;
   - MapBiomas (cobertura e urbanização): 1985–2024, 35 municípios;
   - MapBiomas (superfície de água): 1992–2025, só 25 municípios.

   Fonte: `A01_metadados_por_fonte`, `A02_metadados_por_variavel`.
2. Ausentes na janela 1991–2025:
   - Atlas e ERA5: 0%;
   - MapBiomas cobertura: 2,86%, porque 2025 ainda não existe;
   - áreas de risco: 6,29%, porque São João do Sul não tem valor;
   - superfície de água: 48%.

   Fonte: `A01`, `A02`.
3. ERA5 tem grade de 0,25°, com 1 a 8 pixels por município. Balneário Piçarras, Arroio do Silva e Passo de Torres têm um único pixel. Fonte: `A03_cobertura_municipal`.
4. Não há variável Climbra na base consolidada atual. O pipeline anterior (`consolidacao-bd-mestrado`) usava CLIMBRA; aqui ela aparece como substituída pelo ERA5, com o motivo a confirmar. Fonte: `A01` (linha "Climbra").
5. Unidades do ERA5: o produto baixado é `monthly_averaged_reanalysis`. A base aplica:
   - mm/dia = tp × 1000;
   - mm/mês = tp × 1000 × dias do mês.

   A precipitação anual média 1991–2025 sob cada hipótese:
   - ×1000×dias (H1): 1.724 mm;
   - ×1000 (H2): 57 mm;
   - ×1000×24×dias (H3): 41.373 mm.

   Só H1 é fisicamente plausível e é a que reproduz a coluna da base. **A decisão é da autora.** Fonte: `A04_unidades_era5_hipoteses`, `A05_unidades_era5_razoes_na_base`.

## B. Séries temporais

6. Eventos hidrológicos por ano (soma dos 35 municípios):
   - média de 12,1/ano em 1991–2012 e de 35,8/ano em 2013–2025;
   - pico de 81 eventos em 2022.

   Fonte: `B01_serie_anual_atlas_hidrologico`, `D01_quebra_2012_2013`.
7. Precipitação anual (média regional, ERA5):
   - 1.724 mm em 1991–2025, variando de 1.327 a 2.158 mm;
   - o maior valor da série 1940–2025 é 1983 (2.509 mm).

   Fonte: `B02_serie_anual_era5`.
8. Climatologia mensal 1991–2025:
   - mês mais chuvoso: janeiro (220 mm em média);
   - mês mais seco: julho (99 mm);
   - maior valor mensal regional: novembro (425 mm).

   Fonte: `B04_climatologia_mensal_era5`.
9. Sazonalidade dos eventos (base mensal do Atlas):
   - janeiro concentra 22,2% dos eventos de 1991–2012 e 19,3% dos de 2013–2025;
   - setembro cai de 14,7% para 3,6%;
   - junho passa de 0% para 7,3%.

   Fonte: `B05_sazonalidade_atlas_mensal`.
10. Área urbanizada (média municipal):
    - 6,8% em 1991 e 13,2% em 2024;
    - floresta cai de 36,6% para 33,3%.

    Fonte: `B06_serie_anual_mapbiomas`.

## C. Variável-resposta

11. São 732 eventos hidrológicos: 443 sem "Chuvas Intensas" (60,5%) e 289 de "Chuvas Intensas" (39,5%). "Chuvas Intensas" aparece pela primeira vez em 2013, em 34 dos 35 municípios. Fonte: `C01_totais_variavel_resposta`.
12. Tipologias no total do período:
    - Chuvas intensas: 289 (39,5%);
    - Enxurradas: 277 (37,8%);
    - Alagamentos: 73;
    - Inundações: 69;
    - Movimento de massa: 24.

    Fonte: `C02_composicao_tipologica_total`.
13. Peso das tipologias por bloco de 5 anos:
    - Enxurradas eram 96,9% dos eventos em 2006–10 e são 5,8% em 2021–25;
    - Chuvas intensas são 77,8% em 2021–25.

    Fonte: `C03_composicao_tipologica_por_bloco`.
14. No município-ano:
    - média de 0,60 evento (total);
    - 62,8% dos município-anos têm zero eventos (72,6% sem chuvas intensas; 86,3% só chuvas intensas);
    - máximo de 7 eventos.

    Fonte: `C04_descritivas_resposta`, `C05_frequencia_contagens`.
15. Razão variância/média (sobredispersão, só como descrição):
    - no município-ano: 1,67 (total), 1,29 (sem chuvas intensas), 2,24 (só chuvas intensas);
    - na série anual agregada: 19,5.

    Fonte: `C04`.
16. Ausentes da resposta: 0 nos 35 municípios. Camboriú e Paulo Lopes têm 100% de NA (sem Atlas, não são zeros). Fonte: `C06_ausentes_resposta`.
17. Por município:
    - mediana de 19 eventos, variando de 6 a 46;
    - Joinville (46), Florianópolis (38) e Balneário Camboriú (33) lideram;
    - Balneário Camboriú tem 721 eventos/mil km².

    Fonte: `C07_perfil_municipal_resposta`.

## C. Preditoras

18. Precipitação anual no município-ano: média de 1.718 mm, desvio de 342 mm, sem ausentes. Fonte: `C09_descritivas_preditoras`.
19. Variação entre municípios × entre anos:
    - uso da terra varia quase só entre municípios (área urbanizada 92,4%; floresta 99,2%);
    - a chuva anual varia metade no espaço (44,5%) e metade no tempo;
    - "meses extremos" tem 0% entre municípios, por construção do percentil local.

    Fonte: `C10_variacao_entre_dentro_municipios`.
20. Pares de preditoras redundantes (|rho| ≥ 0,7):
    - área urbanizada × não vegetada: 0,97;
    - acumulado de 3 meses × precipitação anual: 0,84;
    - vento × precipitação convectiva: −0,75;
    - floresta × nuvens: 0,76;
    - floresta × vento: −0,71.

    Fonte: `C11_correlacao_preditoras_municipio_ano`, `C12_pares_correlacao_alta`.

## D. Evidências para as hipóteses (não são conclusões)

O quadro completo, com leitura alternativa e pergunta em aberto para cada linha, está em `D00_quadro_evidencias_hipoteses`.

21. **Quebra de 2013:** o total sobe 2,96 vezes depois de 2013, mas "sem chuvas intensas" sobe só 1,13 vez. A chuva média fica estável (razão 0,97) e os meses extremos caem (0,82). Fonte: `D01`.
22. **Tendência descritiva:** o total cresce (Mann-Kendall tau 0,53 em 1991–2025). A precipitação anual (tau −0,05) e os meses extremos (tau 0,03) não mostram tendência. Fonte: `D02`.
23. **Correlação anual (Spearman):**
    - eventos × ano: 0,73;
    - eventos × meses extremos: 0,24;
    - "só chuvas intensas" × precipitação média: −0,09.

    Fonte: `D04`.
24. **Indícios de mudança na coleta:**
    - municípios com registro no ano: 9,4 (1991–2012) contra 19,2 (2013–2025);
    - % de eventos reconhecidos (todos os grupos): 81% em 2009–11 contra 20% em 2021–25.

    Ressalva: o indicador de reconhecimento mistura todos os grupos. Fonte: `D07`.
25. **Dose-resposta:** razão P(evento) entre a maior e a menor classe de chuva anual, no alvo "sem chuvas intensas":
    - 3,80 em 1991–2012;
    - 1,38 em 2013–2025.

    Fonte: `D06`, figura `D2`.
26. **Controle negativo (vento):** razão 2,43 em 1991–2012, perto da precipitação (3,80), e 1,07 depois de 2013. Ressalva: o vento covaria com a chuva convectiva (rho −0,75), então o controle não é limpo. Fonte: `D06`, `C11`.
27. **I de Moran (4 vizinhos):**
    - densidade total 0,37 (p = 0,001);
    - sem chuvas intensas 0,53;
    - só chuvas intensas 0,17;
    - precipitação média 0,50;
    - % urbanizada 0,30;
    - % chuvas intensas 0,10, sem padrão detectável.

    Fonte: `D10`, figura `D4`.
28. **MapBiomas × eventos entre municípios (bruto, n = 35):**
    - floresta 0,47, mas floresta × latitude = 0,78;
    - área urbanizada −0,08;
    - ganho urbano 0,21;
    - área urbana em risco 0,30;
    - área do município 0,41;
    - latitude 0,49 (mais eventos ao norte).

    Fonte: `D09`.
