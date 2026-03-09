# ============================================================
# DB_novoarquivo.R
# Filtra Uberlândia nos dados do cache_ftp e anexa ao
# histórico CAGED_completo.rds.
#
# Lê de:    cache_ftp (definido em DB_principal.R)
# Atualiza: CAGED_completo.rds
# ============================================================

cat("\n---- DB_novoarquivo.R ----\n")

ARQUIVO_COMPLETO <- file.path(DIR_DATA, "CAGED_completo.rds")
ARQUIVO_BACKUP   <- file.path(DIR_DATA, "CAGED_completo_backup.rds")

# ── Carregar histórico local ───────────────────────────────
if (!file.exists(ARQUIVO_COMPLETO)) {
  stop("CAGED_completo.rds não encontrado. Execute DB_migracao.R primeiro.")
}
caged_hist <- readRDS(ARQUIVO_COMPLETO)
cat(sprintf("  Histórico carregado: %d obs.\n", nrow(caged_hist)))

# ── Backup de segurança ────────────────────────────────────
saveRDS(caged_hist, ARQUIVO_BACKUP)

# ── Remover mês atual caso já exista ──────────────────────
mes_int <- as.integer(MES_ATUAL)
n_antes <- nrow(caged_hist)
for (col in intersect(c("competênciamov","competênciadec","competênciaexc"),
                      names(caged_hist))) {
  caged_hist <- caged_hist[get(col) != mes_int | is.na(get(col))]
}
removidas <- n_antes - nrow(caged_hist)
if (removidas > 0) {
  warning(sprintf(
    "%d obs. do mês %s já existiam e foram removidas antes do novo import.",
    removidas, MES_ATUAL
  ))
}

# ── Filtrar Uberlândia do cache em memória ─────────────────
novos_dados <- data.table::rbindlist(
  lapply(cache_ftp, function(dt) dt[município == MUNICIPIO_ALVO]),
  fill = TRUE, use.names = TRUE
)

if (nrow(novos_dados) == 0) {
  stop("Nenhuma observação encontrada para Uberlândia nos arquivos do mês. Abortando.")
}
cat(sprintf("  Observações novas (Uberlândia): %d\n", nrow(novos_dados)))

# ── Combinar e salvar ──────────────────────────────────────
caged_atualizado <- data.table::rbindlist(
  list(caged_hist, novos_dados),
  fill = TRUE, use.names = TRUE
)
data.table::setorder(caged_atualizado, competênciamov)

saveRDS(caged_atualizado, ARQUIVO_COMPLETO)
cat(sprintf("✓ %s atualizado — %d obs. totais.\n",
            ARQUIVO_COMPLETO, nrow(caged_atualizado)))
