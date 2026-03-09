# ============================================================
# DB_principal.R  — orquestração do pipeline
# Nunca precisa ser editado. Altere apenas config.R.
# ============================================================

rm(list = ls())

# ── Configuração e pacotes ────────────────────────────────
source("config.R")

pkgs <- c("data.table", "archive", "curl", "readxl", "openxlsx", "haven")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

setwd(DIR_SCRIPT)

# ── Parâmetros derivados (não editar) ─────────────────────
ANO_ATUAL  <- substr(MES_ATUAL, 1, 4)
MES_NUMERO <- substr(MES_ATUAL, 5, 6)

DIR_TEMP <- file.path(DIR_RAIZ, "_temp_ftp")
if (!dir.exists(DIR_TEMP)) dir.create(DIR_TEMP, recursive = TRUE)

BASE_URL_FTP <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"

# ── Utilitários e variáveis ───────────────────────────────
source("DB_ftp_utils.R")
source("DB_variaveis.R")

# ── Banner ────────────────────────────────────────────────
cat("==============================================\n")
cat(sprintf("Mês de referência : %s\n", MES_ATUAL))
cat(sprintf("Ano               : %s\n", ANO_ATUAL))
cat(sprintf("Diretório         : %s\n", getwd()))
cat("==============================================\n\n")

# ============================================================
# DOWNLOAD ÚNICO DO MÊS ATUAL
# Os três arquivos são baixados uma vez e ficam em cache_ftp.
# Todos os scripts subsequentes leem dessa lista — sem novo
# acesso à rede ou ao disco.
# ============================================================
cat("── Baixando arquivos do mês", MES_ATUAL, "──\n")

cache_ftp <- baixar_mes_ftp(MES_ATUAL)

cat(sprintf(
  "   CAGEDEXC: %d obs. | CAGEDFOR: %d obs. | CAGEDMOV: %d obs.\n\n",
  nrow(cache_ftp$CAGEDEXC),
  nrow(cache_ftp$CAGEDFOR),
  nrow(cache_ftp$CAGEDMOV)
))

# ── Pipeline ──────────────────────────────────────────────
source("DB_novoarquivo.R")          # atualiza CAGED_completo.rds
source("DB_atualizaestoque.R")      # atualiza CAGED_painel.rds + estoqueatualizado.rds
source("DB_resultados.R")           # Tabelas 1–10  (Uberlândia, série histórica)
source("DB_resultados_ultimomes.R") # Tabelas 11–14 (todos os municípios, mês atual)

# ── Limpeza ───────────────────────────────────────────────
unlink(DIR_TEMP, recursive = TRUE)

cat("\n==============================================\n")
cat("Processamento concluído com sucesso!\n")
cat(sprintf("Planilha gerada: %s\n", ARQUIVO_TABELAS))
cat("==============================================\n")
