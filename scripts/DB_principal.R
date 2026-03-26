# ============================================================
# DB_principal.R  — orquestração do pipeline
# Nunca precisa ser editado. Altere apenas config.R.
# ============================================================

rm(list = ls())

# ── Configuração e pacotes ────────────────────────────────
if (file.exists("config.R")) {
  source("config.R")
} else if (file.exists(file.path("scripts", "config.R"))) {
  source(file.path("scripts", "config.R"))
} else {
  stop("Arquivo config.R não encontrado.")
}

pkgs <- c("data.table", "archive", "curl", "readxl", "openxlsx", "haven", "rmarkdown")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

setwd(DIR_SCRIPT)
if (!dir.exists(DIR_OUTPUT)) dir.create(DIR_OUTPUT, recursive = TRUE)

# ── Parâmetros derivados (não editar) ─────────────────────
ANO_ATUAL  <- substr(MES_ATUAL, 1, 4)
MES_NUMERO <- substr(MES_ATUAL, 5, 6)

DIR_TEMP <- file.path(DIR_RAIZ, "_temp_ftp")
if (!dir.exists(DIR_TEMP)) dir.create(DIR_TEMP, recursive = TRUE)

# Sempre remove _temp_ftp ao final (inclusive em caso de erro)
on.exit({
  if (dir.exists(DIR_TEMP)) unlink(DIR_TEMP, recursive = TRUE, force = TRUE)
}, add = TRUE)

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
source("processa_boletim.R")        # objetos do boletim (tabelas + gráficos)

# ── Renderização do boletim ───────────────────────────────
boletim_dir <- file.path(DIR_OUTPUT, "boletim por mes")
if (!dir.exists(boletim_dir)) dir.create(boletim_dir, recursive = TRUE)

arquivo_rmd <- file.path(boletim_dir, "boletim.Rmd")
arquivo_pdf <- paste0("boletim", MES_ATUAL, ".pdf")

rmarkdown::render(
  input = arquivo_rmd,
  output_file = arquivo_pdf,
  output_dir = boletim_dir,
  envir = globalenv(),
  quiet = TRUE
)

# ── Limpeza ───────────────────────────────────────────────
# Remove artefatos intermediários do LaTeX/knitr, mantendo apenas o PDF final
base_boletim <- file.path(boletim_dir, paste0("boletim", MES_ATUAL))
artefatos_boletim <- c(
  paste0(base_boletim, ".tex"),
  paste0(base_boletim, ".log"),
  paste0(base_boletim, ".aux"),
  paste0(base_boletim, ".out"),
  paste0(base_boletim, ".toc"),
  paste0(base_boletim, ".nav"),
  paste0(base_boletim, ".snm"),
  paste0(base_boletim, "_files")
)
unlink(artefatos_boletim[file.exists(artefatos_boletim)], recursive = TRUE, force = TRUE)

cat("\n==============================================\n")
cat("Processamento concluído com sucesso!\n")
cat(sprintf("Planilha gerada: %s\n", ARQUIVO_TABELAS))
cat(sprintf("Boletim gerado : %s\n", file.path(boletim_dir, arquivo_pdf)))
cat("==============================================\n")
