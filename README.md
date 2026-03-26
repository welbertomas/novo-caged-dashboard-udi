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

## Erros comuns na atualização mensal (e como prevenir)

- **`MES_ATUAL` inválido** (fora de `AAAAMM` ou mês fora de `01..12`): o `DB_principal.R` agora valida isso no início e interrompe com mensagem objetiva.
  - Exemplo válido: `202601`.
- **Ano novo sem salário mínimo em `config.R`**: se o ano não existir no vetor `SM`, o pipeline usa o último ano disponível e emite `warning`.
  - Recomendação: atualizar `SM` no começo de cada ano para evitar aproximações.
- **Falha de FTP/download** (instabilidade de rede ou indisponibilidade no MTE): o pipeline falha rápido por arquivo, evitando atualização parcial silenciosa.
  - Recomendação: reexecutar o pipeline e, se persistir, testar conectividade ao FTP institucional.
- **Reprocessamento do mesmo mês**: os scripts removem o mês atual do histórico/painel antes de anexar novamente, reduzindo risco de duplicidade.
