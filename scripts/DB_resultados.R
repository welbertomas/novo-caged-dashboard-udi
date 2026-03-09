# ============================================================
# DB_resultados.R
# Tabelas 1–10 — série histórica de Uberlândia (desde 202301)
#
# Lê:     CAGED_completo.rds, IPC-CEPES.xlsx, estoqueatualizado.rds
# Escreve: ARQUIVO_TABELAS (abas Tabela 1 a Tabela 10)
# ============================================================

cat("\n---- DB_resultados.R ----\n")

# ── Abrir/criar workbook ──────────────────────────────────
wb <- if (file.exists(ARQUIVO_TABELAS)) {
  openxlsx::loadWorkbook(ARQUIVO_TABELAS)
} else {
  openxlsx::createWorkbook()
}

.escrever_aba <- function(wb, nome, dados) {
  if (nome %in% openxlsx::sheets(wb)) openxlsx::removeWorksheet(wb, nome)
  openxlsx::addWorksheet(wb, nome)
  openxlsx::writeDataTable(wb, nome, data = as.data.frame(dados),
                           tableStyle = "TableStyleMedium9")
}

# ── IPC-CEPES ─────────────────────────────────────────────
arq_ipc <- Sys.glob("cepes_op_ipc_cepes_serie_historica_agregada_n_indice_1994_*.xlsx")
if (length(arq_ipc) == 0) stop("Arquivo IPC-CEPES não encontrado.")
arq_ipc <- arq_ipc[length(arq_ipc)]

ipc_raw <- as.data.table(
  readxl::read_excel(arq_ipc, sheet = "numeroindice", col_types = "numeric")
)
ipc_raw <- ipc_raw[!is.na(anomes)]
data.table::setorder(ipc_raw, anomes)
ipc_raw[, deflator := ipc_raw[anomes == max(anomes), numeroindice] / numeroindice]
setnames(ipc_raw, "anomes", "competênciamov")
ipc <- ipc_raw[, .(competênciamov, deflator)]
cat(sprintf("  IPC: %d períodos (último: %d)\n", nrow(ipc), max(ipc$competênciamov)))

# Expor `ipc` para DB_resultados_ultimomes.R (evita releitura do xlsx)
assign("ipc", ipc, envir = .GlobalEnv)

# ── Série histórica de Uberlândia ─────────────────────────
dt <- readRDS("CAGED_completo.rds")

# Normalizar nomes que podem chegar sem acento do Stata
renomear <- c("salário"="salario", "seção"="secao",
              "graudeinstrução"="graudeinstrucao")
for (old in names(renomear)) {
  if (old %in% names(dt)) setnames(dt, old, renomear[old])
}

# Remover colunas derivadas de rodadas anteriores
cols_derivadas <- c("admissoes","demissoes","mes","setores","class_porte","tamanho",
                    "indiceporte","faixa_etaria","genero","gdi_leg","divisao","div_leg",
                    "ano","data","saldo_mes","salariodef","remuneracao_udi_adm",
                    "remuneracao_udi_dem","deflator")
for (col in intersect(cols_derivadas, names(dt))) set(dt, j = col, value = NULL)

data.table::setorder(dt, competênciamov)

dt <- merge(dt, ipc, by = "competênciamov", all.x = TRUE)
n_sem_def <- sum(is.na(dt$deflator))
if (n_sem_def > 0)
  message(sprintf("ATENÇÃO: %d obs. sem deflator IPC.", n_sem_def))

dt <- criar_variaveis(dt, modo = "historico")
dt <- dt[competênciamov >= 202301]
cat(sprintf("  Série Uberlândia: %d obs.\n", nrow(dt)))

estoque_dt <- readRDS("estoqueatualizado.rds")

# ── Helpers de reshape ────────────────────────────────────
cols_setor <- c("Agropecuária","Indústria","Construção","Comércio","Serviços")
cols_porte <- c("MEI e Micro","Pequena","Média","Grande","Administração Pública")
porte_map  <- c(`1`="MEI e Micro",`2`="Pequena",`3`="Média",
                `4`="Grande",`5`="Administração Pública")

# ====================================================================
# TABELAS
# ====================================================================

# Tabela 1: Saldo Geral
t1 <- dt[, .(Admissões=sum(admissoes,na.rm=T), Demissões=sum(demissoes,na.rm=T),
             Saldo=sum(saldomovimentação,na.rm=T)),
         by=.(Competência=competênciamov, `Mês/Ano`=data)]
data.table::setorder(t1, Competência)
.escrever_aba(wb, "Tabela 1", t1)
cat("  ✓ Tabela 1\n")

# Tabela 2: Saldo por Setor
t2 <- data.table::dcast(
  dt[setores != "Não Identificado",
     .(Saldo=sum(saldomovimentação,na.rm=T)),
     by=.(Competência=competênciamov, `Mês/Ano`=data, setores)],
  Competência + `Mês/Ano` ~ setores, value.var="Saldo", fill=0L
)
data.table::setcolorder(t2, c("Competência","Mês/Ano", intersect(cols_setor, names(t2))))
data.table::setorder(t2, Competência)
.escrever_aba(wb, "Tabela 2", t2)
cat("  ✓ Tabela 2\n")

# Tabela 3: Saldo por Porte
t3_long <- dt[indiceporte != 6L,
  .(Saldo=sum(saldomovimentação,na.rm=T)),
  by=.(Competência=competênciamov, `Mês/Ano`=data, indiceporte)]
t3_long[, porte_leg := porte_map[as.character(indiceporte)]]
t3 <- data.table::dcast(t3_long, Competência + `Mês/Ano` ~ porte_leg,
                         value.var="Saldo", fill=0L)
data.table::setcolorder(t3, c("Competência","Mês/Ano", intersect(cols_porte, names(t3))))
data.table::setorder(t3, Competência)
.escrever_aba(wb, "Tabela 3", t3)
cat("  ✓ Tabela 3\n")

# Tabela 4: Remuneração Média
t4 <- dt[, .(`Salário Admissão (R$)`=mean(remuneracao_udi_adm,na.rm=T),
             `Salário Demissão (R$)`=mean(remuneracao_udi_dem,na.rm=T)),
         by=.(Competência=competênciamov, `Mês/Ano`=data)]
data.table::setorder(t4, Competência)
.escrever_aba(wb, "Tabela 4", t4)
cat("  ✓ Tabela 4\n")

# Tabela 5: Remuneração de Admissão por Setor
t5 <- data.table::dcast(
  dt[setores != "Não Identificado",
     .(Rem=mean(remuneracao_udi_adm,na.rm=T)),
     by=.(Competência=competênciamov, `Mês/Ano`=data, setores)],
  Competência + `Mês/Ano` ~ setores, value.var="Rem"
)
data.table::setcolorder(t5, c("Competência","Mês/Ano", intersect(cols_setor, names(t5))))
data.table::setorder(t5, Competência)
.escrever_aba(wb, "Tabela 5", t5)
cat("  ✓ Tabela 5\n")

# Tabela 6: Remuneração de Admissão por Porte
t6_long <- dt[indiceporte != 6L,
  .(Rem=mean(remuneracao_udi_adm,na.rm=T)),
  by=.(Competência=competênciamov, `Mês/Ano`=data, indiceporte)]
t6_long[, porte_leg := porte_map[as.character(indiceporte)]]
t6 <- data.table::dcast(t6_long, Competência + `Mês/Ano` ~ porte_leg, value.var="Rem")
data.table::setcolorder(t6, c("Competência","Mês/Ano", intersect(cols_porte, names(t6))))
data.table::setorder(t6, Competência)
.escrever_aba(wb, "Tabela 6", t6)
cat("  ✓ Tabela 6\n")

# Tabela 7: Remuneração de Admissão por Escolaridade
educ_ordem <- c("Analfabeto","Fundamental Incompleto","Fundamental Completo",
                "Médio Completo","Superior Completo","Pós-Graduação","Não Identificado")
t7_long <- dt[, .(Rem=mean(remuneracao_udi_adm,na.rm=T)),
              by=.(Competência=competênciamov, `Mês/Ano`=data, gdi_leg)]
t7_long[, gdi_leg := factor(gdi_leg, levels=educ_ordem)]
t7 <- data.table::dcast(t7_long, Competência + `Mês/Ano` ~ gdi_leg, value.var="Rem")
data.table::setcolorder(t7, c("Competência","Mês/Ano", intersect(educ_ordem, names(t7))))
data.table::setorder(t7, Competência)
.escrever_aba(wb, "Tabela 7", t7)
cat("  ✓ Tabela 7\n")

# Tabela 9: Desligamentos a Pedido
col_tipo <- intersect(c("tipomovimentação","tipomovimentacao"), names(dt))[1]
if (!is.na(col_tipo)) {
  t9 <- dt[get(col_tipo) == 40L,
    .(`Desligamentos a Pedido` = sum(saldomovimentação,na.rm=T) * -1L),
    by=.(Competência=competênciamov, `Mês/Ano`=data)]
  data.table::setorder(t9, Competência)
  .escrever_aba(wb, "Tabela 9", t9)
  cat("  ✓ Tabela 9\n")
} else {
  message("  ! Tabela 9 ignorada — coluna tipomovimentação não encontrada.")
}

# Tabela 10: Variação em Relação ao Estoque
t10 <- merge(
  dt[, .(Saldo=sum(saldomovimentação,na.rm=T)),
     by=.(município, Competência=competênciamov, `Mês/Ano`=data)],
  estoque_dt,
  by.x=c("município","Competência"), by.y=c("município","competênciamov"),
  all.x=TRUE
)
setnames(t10, c("estoque_atualizado","var_mes"),
              c("Estoque","Variação Mensal (%)"))
data.table::setorder(t10, município, Competência)
.escrever_aba(wb, "Tabela 10", t10)
cat("  ✓ Tabela 10\n")

openxlsx::saveWorkbook(wb, ARQUIVO_TABELAS, overwrite=TRUE)
cat(sprintf("\n✓ %s salvo (Tabelas 1–10).\n", ARQUIVO_TABELAS))
