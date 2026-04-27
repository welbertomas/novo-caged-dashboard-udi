# ============================================================
# DB_principal.R - orquestracao do pipeline
# Nunca precisa ser editado. Altere apenas config.R.
# ============================================================

rm(list = ls())

if (file.exists("DB_bootstrap.R")) {
  source("DB_bootstrap.R")
} else if (file.exists(file.path("scripts", "DB_bootstrap.R"))) {
  source(file.path("scripts", "DB_bootstrap.R"))
} else {
  stop("Arquivo DB_bootstrap.R nao encontrado.")
}

if (file.exists("config.R")) {
  source("config.R")
} else if (file.exists(file.path("scripts", "config.R"))) {
  source(file.path("scripts", "config.R"))
} else {
  stop("Arquivo config.R nao encontrado.")
}

configurar_biblioteca_r(DIR_RAIZ)

pkgs <- c("data.table", "archive", "curl", "readxl", "openxlsx", "haven", "rmarkdown", "httr")
garantir_pacotes(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

setwd(DIR_SCRIPT)
if (!dir.exists(DIR_OUTPUT)) dir.create(DIR_OUTPUT, recursive = TRUE)

validar_mes_referencia <- function(anomes, campo = "MES_ATUAL") {
  if (!is.character(anomes) || length(anomes) != 1L || !grepl("^[0-9]{6}$", anomes)) {
    stop(sprintf("%s invalido. Use formato AAAAMM (ex.: 202601).", campo))
  }

  mes_num <- as.integer(substr(anomes, 5, 6))
  if (is.na(mes_num) || mes_num < 1L || mes_num > 12L) {
    stop(sprintf("%s invalido. O mes deve estar entre 01 e 12.", campo))
  }

  invisible(TRUE)
}

validar_mes_referencia(MES_ATUAL, "MES_ATUAL")

if (!exists("MESES_BOLETIM") || is.null(MESES_BOLETIM) || !length(MESES_BOLETIM)) {
  MESES_BOLETIM <- MES_ATUAL
}
MESES_BOLETIM <- unique(as.character(MESES_BOLETIM))
for (i in seq_along(MESES_BOLETIM)) {
  validar_mes_referencia(MESES_BOLETIM[[i]], sprintf("MESES_BOLETIM[%d]", i))
}

ANO_ATUAL <- substr(MES_ATUAL, 1, 4)
MES_NUMERO <- substr(MES_ATUAL, 5, 6)

DIR_TEMP <- file.path(DIR_RAIZ, "_temp_ftp")
if (!dir.exists(DIR_TEMP)) dir.create(DIR_TEMP, recursive = TRUE)

on.exit({
  limpar_temp_ftp(DIR_TEMP)
}, add = TRUE)

BASE_URL_FTP <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"

source("DB_ftp_utils.R")
source("DB_variaveis.R")

cat("==============================================\n")
cat(sprintf("Mes de referencia : %s\n", MES_ATUAL))
cat(sprintf("Ano               : %s\n", ANO_ATUAL))
cat(sprintf("Diretorio         : %s\n", getwd()))
cat("==============================================\n\n")
cat("== Baixando arquivos do mes", MES_ATUAL, "==\n")

cache_ftp <- baixar_mes_ftp(MES_ATUAL)

cat(sprintf(
  "   CAGEDEXC: %d obs. | CAGEDFOR: %d obs. | CAGEDMOV: %d obs.\n\n",
  nrow(cache_ftp$CAGEDEXC),
  nrow(cache_ftp$CAGEDFOR),
  nrow(cache_ftp$CAGEDMOV)
))

source("atualiza_ipc_cepes.R")
source("DB_novoarquivo.R")
source("DB_atualizaestoque.R")
source("DB_resultados.R")
source("DB_resultados_ultimomes.R")

boletim_dir <- file.path(DIR_OUTPUT, "boletim_por_mes")
if (!dir.exists(boletim_dir)) dir.create(boletim_dir, recursive = TRUE)

arquivo_rmd <- file.path(boletim_dir, "boletim.Rmd")
meses_capa <- c("jan", "fev", "mar", "abr", "mai", "jun",
                "jul", "ago", "set", "out", "nov", "dez")

configurar_pandoc <- function() {
  if (rmarkdown::pandoc_available("1.12.3")) {
    return(invisible(TRUE))
  }

  candidatos <- unique(c(
    Sys.getenv("RSTUDIO_PANDOC", unset = ""),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  ))
  candidatos <- candidatos[nzchar(candidatos) & dir.exists(candidatos)]

  for (candidato in candidatos) {
    Sys.setenv(RSTUDIO_PANDOC = candidato)
    if (rmarkdown::pandoc_available("1.12.3")) {
      return(invisible(TRUE))
    }
  }

  stop("Pandoc nao encontrado. Defina RSTUDIO_PANDOC para um diretorio com pandoc.exe.")
}

configurar_pandoc()

resolver_capa_boletim <- function(anomes) {
  mes_indice <- as.integer(substr(anomes, 5, 6))
  nome_capa <- paste0("capa_", meses_capa[mes_indice], substr(anomes, 3, 4), ".pdf")
  caminho_abs <- file.path(DIR_RAIZ, "boletim", nome_capa)

  if (!file.exists(caminho_abs)) {
    stop(sprintf("Capa do boletim nao encontrada para %s: %s", anomes, caminho_abs))
  }

  paste0("../../boletim/", nome_capa)
}

limpar_artefatos_boletim <- function(anomes) {
  base_boletim <- file.path(boletim_dir, paste0("boletim", anomes))
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
}

renderizar_boletim <- function(anomes) {
  assign("MES_ATUAL", anomes, envir = .GlobalEnv)
  assign("ANO_ATUAL", substr(anomes, 1, 4), envir = .GlobalEnv)
  assign("MES_NUMERO", substr(anomes, 5, 6), envir = .GlobalEnv)
  assign("CAPA_BOLETIM_LATEX", resolver_capa_boletim(anomes), envir = .GlobalEnv)

  source("processa_boletim.R")

  arquivo_pdf <- paste0("boletim", anomes, ".pdf")
  wd_anterior <- getwd()
  on.exit(setwd(wd_anterior), add = TRUE)
  setwd(boletim_dir)

  rmarkdown::render(
    input = "boletim.Rmd",
    output_file = arquivo_pdf,
    output_dir = ".",
    envir = globalenv(),
    quiet = TRUE
  )

  limpar_artefatos_boletim(anomes)
  cat(sprintf("OK Boletim gerado: %s\n", file.path(boletim_dir, arquivo_pdf)))
}

for (mes_boletim in MESES_BOLETIM) {
  cat(sprintf("\n== Renderizando boletim %s ==\n", mes_boletim))
  renderizar_boletim(mes_boletim)
}

unlink(DIR_TEMP[file.exists(DIR_TEMP)], recursive = TRUE, force = TRUE)

cat("\n==============================================\n")
cat("Processamento concluido com sucesso!\n")
cat(sprintf("Planilha gerada : %s\n", ARQUIVO_TABELAS))
cat(sprintf(
  "Boletins gerados: %s\n",
  paste(file.path(boletim_dir, paste0("boletim", MESES_BOLETIM, ".pdf")), collapse = " | ")
))
cat("==============================================\n")
