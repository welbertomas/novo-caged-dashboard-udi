# ============================================================
# config.R  ← ÚNICO ARQUIVO EDITADO MENSALMENTE
# ============================================================

# ── Competência a processar (formato AAAAMM) ──────────────
MES_ATUAL <- "202601"

# ── Caminhos ─────────────────────────────────────────────
DIR_RAIZ       <- normalizePath("..", winslash = "/", mustWork = FALSE)
DIR_DATA       <- file.path(DIR_RAIZ, "data_processed")
DIR_SCRIPT     <- file.path(DIR_RAIZ, "scripts")
DIR_OUTPUT     <- file.path(DIR_RAIZ, "output")
ARQUIVO_TABELAS <- file.path(DIR_OUTPUT, "DB_trabalho.xlsx")

# ── Município-alvo para microdados completos ──────────────
MUNICIPIO_ALVO <- 317020L   # Uberlândia (MG)

# ── Salários mínimos por ano ──────────────────────────────
SM <- c(
  "2020" = 1045, "2021" = 1100, "2022" = 1212,
  "2023" = 1320, "2024" = 1412, "2025" = 1518,
  "2026" = 1621
)