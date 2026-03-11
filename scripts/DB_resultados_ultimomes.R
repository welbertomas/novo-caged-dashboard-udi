# ============================================================
# DB_resultados_ultimomes.R
# Tabelas 11–14 — todos os municípios, mês atual (CAGEDMOV)
#
# Lê de:   cache_ftp$CAGEDMOV (memória), ipc (memória),
#          nomes_mun.rds, estoqueatualizado.rds
# Escreve: ARQUIVO_TABELAS (abas Tabela 11 a Tabela 14)
# ============================================================

cat("\n---- DB_resultados_ultimomes.R ----\n")

# ── Abrir workbook ────────────────────────────────────────
wb <- if (file.exists(ARQUIVO_TABELAS)) {
  openxlsx::loadWorkbook(ARQUIVO_TABELAS)
} else {
  stop("Workbook não encontrado. Execute DB_resultados.R primeiro.")
}

.escrever_aba <- function(wb, nome, dados) {
  if (nome %in% openxlsx::sheets(wb)) openxlsx::removeWorksheet(wb, nome)
  openxlsx::addWorksheet(wb, nome)
  openxlsx::writeDataTable(wb, nome, x=as.data.frame(dados),
                           tableStyle="TableStyleMedium9")
}


.limpar_num <- function(dados) {
  for (col in names(dados)) {
    if (is.numeric(dados[[col]])) {
      dados[[col]][!is.finite(dados[[col]])] <- NA_real_
    }
  }
  dados
}

# ── CAGEDMOV do mês — direto do cache em memória ─────────
dt_mov <- data.table::copy(cache_ftp$CAGEDMOV)

# Normalizar nomes de colunas
renomear <- c("salário"="salario","seção"="secao","graudeinstrução"="graudeinstrucao")
for (old in names(renomear)) {
  if (old %in% names(dt_mov)) setnames(dt_mov, old, renomear[old])
}

cat(sprintf("  CAGEDMOV: %d obs. / %d municípios\n",
            nrow(dt_mov), data.table::uniqueN(dt_mov$município)))

# ── Deflator (reutiliza objeto `ipc` criado em DB_resultados.R) ──
dt_mov <- merge(dt_mov, ipc, by="competênciamov", all.x=TRUE)
n_sem  <- sum(is.na(dt_mov$deflator))
if (n_sem > 0) message(sprintf("ATENÇÃO: %d obs. sem deflator IPC.", n_sem))

# ── Variáveis derivadas ───────────────────────────────────
dt_mov <- criar_variaveis(dt_mov, modo="ultimomes")

# ── Dados auxiliares ──────────────────────────────────────
nomes_mun <- readRDS(file.path(DIR_RAW, "nomes_mun.rds")) |> 
  data.table::as.data.table()
estoque_dt <- readRDS(file.path(DIR_DATA, "estoqueatualizado.rds"))

educ_ordem  <- c("Analfabeto","Fundamental Incompleto","Fundamental Completo",
                 "Médio Completo","Superior Completo","Pós-Graduação","Não Identificado")
setor_ordem <- c("Agropecuária","Indústria","Construção",
                 "Comércio","Serviços","Não Identificado")

# ── Helper: adicionar nome do município ───────────────────
.add_nome <- function(dt) {
  merge(dt, nomes_mun[, .(município, `Nome Município`=nome_mun)],
        by="município", all.x=TRUE)
}

# ====================================================================
# TABELAS
# ====================================================================

# Tabela 11: Saldo por Município
t11 <- dt_mov[, .(Admissões=sum(admissoes,na.rm=T),
                  Demissões=sum(demissoes,na.rm=T),
                  Saldo    =sum(saldomovimentação,na.rm=T)),
              by=.(município, Competência=competênciamov, `Mês/Ano`=data)]
t11 <- .add_nome(t11)
t11 <- merge(t11,
             estoque_dt[, .(município, competênciamov,
                            Estoque=estoque_atualizado, `Variação (%)`=var_mes)],
             by.x=c("município","Competência"), by.y=c("município","competênciamov"),
             all.x=TRUE)
data.table::setcolorder(t11, c("município","Nome Município","Competência","Mês/Ano",
                                "Admissões","Demissões","Saldo","Estoque","Variação (%)"))
data.table::setorder(t11, município)
.escrever_aba(wb, "Tabela 11", .limpar_num(t11))
cat("  ✓ Tabela 11\n")

# Tabela 12: Remuneração Média por Município
t12 <- dt_mov[, .(`Salário Admissão (R$)`=mean(remuneracao_adm,na.rm=T)),
              by=.(município, Competência=competênciamov, `Mês/Ano`=data)]
t12 <- .add_nome(t12)
data.table::setcolorder(t12, c("município","Nome Município","Competência",
                                "Mês/Ano","Salário Admissão (R$)"))
data.table::setorder(t12, município)
.escrever_aba(wb, "Tabela 12", .limpar_num(t12))
cat("  ✓ Tabela 12\n")

# Tabela 13: Remuneração por Escolaridade e Município
t13_long <- dt_mov[, .(Rem=mean(remuneracao_adm,na.rm=T)),
                   by=.(município, Competência=competênciamov, gdi_leg)]
t13_long[, gdi_leg := factor(gdi_leg, levels=educ_ordem)]
t13 <- data.table::dcast(t13_long, município + Competência ~ gdi_leg, value.var="Rem")
t13 <- .add_nome(t13)
data.table::setcolorder(t13, c("município","Nome Município","Competência",
                                intersect(educ_ordem, names(t13))))
data.table::setorder(t13, município)
.escrever_aba(wb, "Tabela 13", .limpar_num(t13))
cat("  ✓ Tabela 13\n")

# Tabela 14: Remuneração por Setor e Município
t14_long <- dt_mov[, .(Rem=mean(remuneracao_adm,na.rm=T)),
                   by=.(município, Competência=competênciamov, setores)]
t14_long[, setores := factor(setores, levels=setor_ordem)]
t14 <- data.table::dcast(t14_long, município + Competência ~ setores, value.var="Rem")
t14 <- .add_nome(t14)
data.table::setcolorder(t14, c("município","Nome Município","Competência",
                                intersect(setor_ordem, names(t14))))
data.table::setorder(t14, município)
.escrever_aba(wb, "Tabela 14", .limpar_num(t14))
cat("  ✓ Tabela 14\n")

openxlsx::saveWorkbook(wb, ARQUIVO_TABELAS, overwrite=TRUE)
cat(sprintf("\n✓ %s atualizado (Tabelas 11–14).\n", ARQUIVO_TABELAS))
