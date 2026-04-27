# ============================================================
# config.R  ← ÚNICO ARQUIVO EDITADO MENSALMENTE
# ============================================================

# ── Competência a processar (formato AAAAMM) ──────────────
MES_ATUAL <- "202602"

#MESES_BOLETIM <- c("202601", "202602")


# ── Caminhos ─────────────────────────────────────────────
.config_file <- NULL
for (i in sys.nframe():1) {
  if (!is.null(sys.frame(i)$ofile)) {
    .config_file <- normalizePath(sys.frame(i)$ofile, winslash = "/", mustWork = FALSE)
    break
  }
}
.config_dir <- if (!is.null(.config_file)) dirname(.config_file) else getwd()

DIR_RAIZ        <- normalizePath(file.path(.config_dir, ".."), winslash = "/", mustWork = FALSE)
DIR_RAW         <- file.path(DIR_RAIZ, "data_raw")
DIR_DATA        <- file.path(DIR_RAIZ, "data_processed")
DIR_SCRIPT      <- file.path(DIR_RAIZ, "scripts")
DIR_OUTPUT      <- file.path(DIR_RAIZ, "output")
ARQUIVO_TABELAS <- file.path(DIR_OUTPUT, "DB_trabalho.xlsx")
TABELAS_BOLETIM <- file.path(DIR_OUTPUT, "Boletim.xlsx")

# ── Município-alvo para microdados completos ──────────────
MUNICIPIO_ALVO <- 317020L   # Uberlândia (MG)

# ── Salários mínimos por ano ──────────────────────────────
SM <- c(
  "2020" = 1045, "2021" = 1100, "2022" = 1212,
  "2023" = 1320, "2024" = 1412, "2025" = 1518,
  "2026" = 1621
)
