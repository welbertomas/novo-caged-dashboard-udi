# novo-caged-dashboard-udi

Pipeline em R para baixar, atualizar e consolidar dados do Novo CAGED e gerar a planilha `output/DB_trabalho.xlsx`, usada no dashboard do emprego de Uberlândia (MG).

## O que os scripts permitem fazer

- **Extração inicial de microdados de Uberlândia** (`scripts/DB_extracao.R`): baixa os arquivos mensais no FTP do MTE, filtra o município alvo e salva a base histórica em `data_processed/CAGED_completo.rds`.
- **Extração inicial de saldo para todos os municípios** (`scripts/DB_extraisaldo.R`): baixa os arquivos mensais, agrega saldo por município e competência e salva em `data_processed/CAGED_painel_saldo.rds`.
- **Atualização mensal completa** (`scripts/DB_principal.R`):
  - baixa apenas o mês de referência definido em `scripts/config.R`;
  - atualiza os arquivos históricos processados;
  - recalcula estoque e variação mensal;
  - exporta/atualiza todas as abas da planilha `output/DB_trabalho.xlsx`.

## Fonte dos dados

- Ministério do Trabalho e Emprego (MTE)
- Programa de Disseminação das Estatísticas do Trabalho (PDET)
- FTP: `ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED`

Arquivos auxiliares esperados em `data_raw/`:
- IPC-CEPES (xlsx);
- estoque base (jan/2023) em `estoque.rds`;
- nomes e códigos dos municípios em `nomes_mun.rds`.

## Replicação do output `DB_trabalho.xlsx`

Para reproduzir a planilha final:

1. Ajuste `MES_ATUAL` em `scripts/config.R` (formato `AAAAMM`).
2. Execute `scripts/DB_principal.R` a partir da pasta `scripts`.
3. O arquivo final será gerado em `output/DB_trabalho.xlsx`.

> Observação: para primeira carga histórica, execute antes `scripts/DB_extracao.R` e `scripts/DB_extraisaldo.R`.

## Exemplo prático: gerar `DB_trabalho` e `boletim202602.pdf` quando sair 202602

Quando os microdados de **fevereiro/2026 (`202602`)** forem publicados no FTP do MTE:

1. Abra `scripts/config.R` e altere:

```r
MES_ATUAL <- "202602"
```

2. (Recomendado) confirme se o vetor `SM` tem o ano de referência necessário.
3. Execute o pipeline mensal a partir da pasta `scripts`:

```r
source("DB_principal.R")
```

4. Confira os arquivos gerados:
   - `output/DB_trabalho.xlsx`
   - `output/boletim_por_mes/boletim202602.pdf`

> Dica: se for necessário reprocessar o mesmo mês, basta manter `MES_ATUAL <- "202602"` e executar de novo; o pipeline remove o mês antes de anexar para evitar duplicidade.

## Erros comuns na atualização mensal (e como prevenir)

- **`MES_ATUAL` inválido** (fora de `AAAAMM` ou mês fora de `01..12`): o `DB_principal.R` agora valida isso no início e interrompe com mensagem objetiva.
  - Exemplo válido: `202601`.
- **Ano novo sem salário mínimo em `config.R`**: se o ano não existir no vetor `SM`, o pipeline usa o último ano disponível e emite `warning`.
  - Recomendação: atualizar `SM` no começo de cada ano para evitar aproximações.
- **Falha de FTP/download** (instabilidade de rede ou indisponibilidade no MTE): o pipeline falha rápido por arquivo, evitando atualização parcial silenciosa.
  - Recomendação: reexecutar o pipeline e, se persistir, testar conectividade ao FTP institucional.
- **Reprocessamento do mesmo mês**: os scripts removem o mês atual do histórico/painel antes de anexar novamente, reduzindo risco de duplicidade.

## Como excluir `_temp_ftp` após o processamento

O fluxo principal (`scripts/DB_principal.R`) já remove `_temp_ftp` automaticamente ao final (inclusive quando ocorre erro), via `on.exit`.

Se você quiser limpar manualmente:

1. **No R (recomendado):**

```r
source("scripts/DB_ftp_utils.R")
limpar_temp_ftp("_temp_ftp")
```

2. **No terminal (Linux/macOS):**

```bash
rm -rf _temp_ftp
```
