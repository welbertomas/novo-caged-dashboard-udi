# ============================================================
# DB_atualizaestoque.R
# Colapsa os dados do cache_ftp por município, atualiza
# CAGED_painel.rds e recalcula estoqueatualizado.rds.
#
# PREMISSA: estoque.rds contém estoque ao final de jan/2023.
#
# Lê de:    cache_ftp (definido em DB_principal.R)
# Atualiza: CAGED_painel.rds, estoqueatualizado.rds
# ============================================================

cat("\n---- DB_atualizaestoque.R ----\n")

ARQUIVO_PAINEL  <- file.path(DIR_DATA, "CAGED_painel.rds")
ARQUIVO_BACKUP  <- file.path(DIR_DATA, "CAGED_painel_backup.rds")
ARQUIVO_ESTOQUE <- file.path(DIR_RAW, "estoque.rds")
ARQUIVO_SAIDA   <- file.path(DIR_DATA, "estoqueatualizado.rds")

COLS_PAINEL <- c("município", "competênciamov", "saldomovimentação")

# ── Carregar painel histórico ──────────────────────────────
if (!file.exists(ARQUIVO_PAINEL)) {
  stop("CAGED_painel.rds não encontrado. Execute DB_migracao.R primeiro.")
}
painel <- readRDS(ARQUIVO_PAINEL)
cat(sprintf("  Painel carregado: %d obs.\n", nrow(painel)))

# ── Remover mês atual caso já exista ──────────────────────
mes_int <- as.integer(MES_ATUAL)
n_antes <- nrow(painel)
painel  <- painel[competênciamov != mes_int]
if (nrow(painel) < n_antes) {
  warning(sprintf("Mês %s já existia no painel — removido para reprocessamento.", MES_ATUAL))
}

saveRDS(painel, ARQUIVO_BACKUP)

# ── Colapsar novo mês do cache (todos os municípios) ──────
novo_mes_raw <- data.table::rbindlist(
  lapply(cache_ftp, function(dt) dt[, .SD, .SDcols = intersect(COLS_PAINEL, names(dt))]),
  fill = TRUE, use.names = TRUE
)

novo_mes <- novo_mes_raw[,
  .(saldomovimentação = sum(saldomovimentação, na.rm = TRUE)),
  by = .(município, competênciamov)
]
cat(sprintf("  Municípios no novo mês: %d\n", nrow(novo_mes)))

# ── Atualizar painel ──────────────────────────────────────
painel_novo <- data.table::rbindlist(list(painel, novo_mes), fill = TRUE)
painel_novo <- painel_novo[,
  .(saldomovimentação = sum(saldomovimentação, na.rm = TRUE)),
  by = .(município, competênciamov)
]
data.table::setorder(painel_novo, município, competênciamov)
saveRDS(painel_novo, ARQUIVO_PAINEL)
cat(sprintf("✓ %s atualizado — %s adicionado.\n", ARQUIVO_PAINEL, MES_ATUAL))

# ============================================================
# CALCULAR ESTOQUE ATUALIZADO
# ============================================================

estoque_dt <- painel_novo[competênciamov >= 202301]
data.table::setorder(estoque_dt, município, competênciamov)

# ── Carregar estoque-base (jan/2023) ───────────────────────
if (!file.exists(ARQUIVO_ESTOQUE)) {
  stop("estoque.rds não encontrado. Mantenha o arquivo manualmente.")
}
estoque_base <- readRDS(ARQUIVO_ESTOQUE)
if (!all(c("município","estoque") %in% names(estoque_base))) {
  stop("estoque.rds precisa ter colunas 'município' e 'estoque'.")
}

estoque_dt <- merge(
  estoque_dt,
  estoque_base[, .(município, estoque_jan23 = estoque)],
  by = "município", all.x = TRUE
)

sem_base <- sum(is.na(estoque_dt$estoque_jan23))
if (sem_base > 0) {
  warning(sprintf("%d obs. sem estoque-base — resultados podem conter NA.", sem_base))
}

# Movimento de jan/2023 zerado: ponto de partida = estoque_jan23
estoque_dt[competênciamov == 202301, saldomovimentação := 0]

estoque_dt[, saldo_acumulado    := cumsum(saldomovimentação), by = município]
estoque_dt[, estoque_atualizado := data.table::fifelse(
  competênciamov == 202301, estoque_jan23, estoque_jan23 + saldo_acumulado
)]
estoque_dt[, estoque_anterior := data.table::shift(estoque_atualizado, 1L), by = município]
estoque_dt[, var_mes           := (saldomovimentação / estoque_anterior) * 100]

saida <- estoque_dt[, .(município, competênciamov, estoque_atualizado, var_mes)]
saveRDS(saida, ARQUIVO_SAIDA)
cat(sprintf("✓ %s salvo — %s adicionado.\n", ARQUIVO_SAIDA, MES_ATUAL))
