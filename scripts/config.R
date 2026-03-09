# ============================================================
# config.R  ← ÚNICO ARQUIVO EDITADO MENSALMENTE
# ============================================================

# ── Competência a processar (formato AAAAMM) ──────────────
MES_ATUAL <- "202601"

# ── Caminhos ─────────────────────────────────────────────
config_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
                        error = function(e) NA_character_)
DIR_SCRIPT <- if (!is.na(config_file)) dirname(config_file) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DIR_RAIZ   <- normalizePath(file.path(DIR_SCRIPT, ".."), winslash = "/", mustWork = FALSE)
DIR_DATA   <- file.path(DIR_RAIZ, "data_processed")
DIR_OUTPUT <- file.path(DIR_RAIZ, "output")
ARQUIVO_TABELAS <- file.path(DIR_OUTPUT, "DB_trabalho.xlsx")

# ── Município-alvo para microdados completos ──────────────
MUNICIPIO_ALVO <- 317020L   # Uberlândia (MG)

# ── Salários mínimos por ano ──────────────────────────────
SM <- c(
  "2020" = 1045, "2021" = 1100, "2022" = 1212,
  "2023" = 1320, "2024" = 1412, "2025" = 1518,
  "2026" = 1621
)