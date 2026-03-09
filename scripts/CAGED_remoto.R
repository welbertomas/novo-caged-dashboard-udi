# ============================================================
#  NOVO CAGED — Importação de arquivos remotos via FTP
#  Equivalente ao script Stata, usando fonte remota (.7z)
#  Município-alvo: 317020
# ============================================================
#
#  Pacotes necessários:
#    install.packages(c("data.table", "archive", "curl", "dplyr"))
#
#  O pacote `archive` depende da biblioteca libarchive do sistema.
#  No Windows, ela já vem embutida no pacote CRAN.
#  O suporte a .7z está incluso nativamente.
# ============================================================

library(data.table)
library(archive)   # leitura de .7z sem extrair para disco
library(curl)      # download via FTP

# ============================================================
#  CONFIGURAÇÕES — edite aqui se necessário
# ============================================================
BASE_URL       <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"
ANOS           <- 2020:2026
MESES          <- sprintf("%02d", 1:12)
TIPOS          <- c("CAGEDEXC", "CAGEDFOR", "CAGEDMOV")
MUNICIPIO_ALVO <- 317020
DIR_TEMP       <- "E:/temp_caged"                 # pasta temporária
if (!dir.exists(DIR_TEMP)) dir.create(DIR_TEMP, recursive = TRUE)
ARQUIVO_SAIDA  <- "E:/Remoto NC/CAGED_completo.rds"   # RDS (rápido e fiel ao tipo)
ARQUIVO_CSV    <- "E:/Remoto NC/CAGED_completo.csv"    


# Colunas de interesse (nomes exatos do cabeçalho do arquivo)
COL_MUNICIPIO  <- "município"
COL_COMPET     <- "competênciamov"
COL_SALDO      <- "saldomovimentação"

# ============================================================
#  FUNÇÕES AUXILIARES
# ============================================================

#' Monta a URL do arquivo remoto
montar_url <- function(tipo, ano, mes) {
  anomes       <- paste0(ano, mes)
  nome_arquivo <- paste0(tipo, anomes, ".7z")
  url          <- paste0(BASE_URL, "/", ano, "/", anomes, "/", nome_arquivo)
  list(url = url, nome = nome_arquivo, anomes = anomes)
}

#' Baixa o .7z, lê o .txt de dentro e retorna um data.table filtrado.
#' Retorna NULL em caso de qualquer falha.
processar_arquivo <- function(tipo, ano, mes) {

  info         <- montar_url(tipo, ano, mes)
  destino_7z   <- file.path(DIR_TEMP, info$nome)

  # — 1. Download FTP ----------------------------------------
  tryCatch(
    curl::curl_download(info$url, destino_7z, quiet = TRUE),
    error = function(e) stop(paste("Download falhou:", conditionMessage(e)))
  )

  # — 2. Identificar o arquivo .txt dentro do .7z -------------
  conteudo  <- archive::archive(destino_7z)          # lista arquivos do zip
  txt_entry <- conteudo$path[grepl("\\.txt$", conteudo$path, ignore.case = TRUE)]

  if (length(txt_entry) == 0)
    stop("Nenhum .txt encontrado dentro do .7z")

  # — 3. Extrair o .txt para a pasta temporária e ler com fread ---
  archive::archive_extract(destino_7z, dir = DIR_TEMP)
  destino_txt <- file.path(DIR_TEMP, txt_entry[1])

  on.exit({
    if (file.exists(destino_7z))  file.remove(destino_7z)
    if (file.exists(destino_txt)) file.remove(destino_txt)
  })

  dt <- data.table::fread(
    destino_txt,
    sep          = ";",
    encoding     = "UTF-8",
    showProgress = FALSE,
    data.table   = TRUE
  )

  # — 4. Verificar colunas mínimas ----------------------------
  colunas_ok <- all(c(COL_MUNICIPIO, COL_COMPET, COL_SALDO) %in% names(dt))
  if (!colunas_ok) {
    # Tentar com nomes em minúsculas/sem acento (variação de versão)
    names(dt) <- tolower(iconv(names(dt), to = "ASCII//TRANSLIT"))
    colunas_ok <- all(c("municipio", "competenciamov", "saldomovimentacao") %in% names(dt))
    if (colunas_ok) {
      setnames(dt,
               c("municipio", "competenciamov", "saldomovimentacao"),
               c(COL_MUNICIPIO, COL_COMPET, COL_SALDO))
    } else {
      stop("Colunas esperadas não encontradas no arquivo")
    }
  }

  # — 5. Filtrar município e manter colunas -------------------
  dt <- dt[get(COL_MUNICIPIO) == MUNICIPIO_ALVO,
            .SD, .SDcols = c(COL_COMPET, COL_SALDO)]

  # — 6. Inverter saldo para CAGEDEXC -------------------------
  if (tipo == "CAGEDEXC") {
    dt[, (COL_SALDO) := get(COL_SALDO) * (-1L)]
  }

  dt
}

# ============================================================
#  LOOP PRINCIPAL
# ============================================================
dados_lista <- vector("list", length(ANOS) * length(MESES) * length(TIPOS))
idx         <- 1L
contador    <- 0L
erros       <- 0L
lista_erros <- character(0)

for (ano in ANOS) {
  for (mes in MESES) {
    for (tipo in TIPOS) {

      info <- montar_url(tipo, ano, mes)
      cat(sprintf("Processando: %-12s | Ano %d | Mês %s  ", tipo, ano, mes))

      resultado <- tryCatch(
        processar_arquivo(tipo, ano, mes),
        error = function(e) {
          message(sprintf("\n  ✗ Erro: %s\n    Motivo: %s", info$nome, conditionMessage(e)))
          NULL
        }
      )

      if (!is.null(resultado)) {
        cat(sprintf("✓  %d obs.\n", nrow(resultado)))
        dados_lista[[idx]] <- resultado
        contador           <- contador + 1L
      } else {
        erros       <- erros + 1L
        lista_erros <- c(lista_erros, info$nome)
      }

      idx <- idx + 1L
    }
  }
}

# ============================================================
#  CONSOLIDAR
# ============================================================
dados_lista    <- Filter(Negate(is.null), dados_lista)
dados_completos <- if (length(dados_lista) > 0) {
  rbindlist(dados_lista, fill = TRUE, use.names = TRUE)
} else {
  data.table()
}

# ============================================================
#  RELATÓRIO FINAL
# ============================================================
cat("\n")
cat("======================================================\n")
cat("                   RELATÓRIO FINAL\n")
cat("======================================================\n")
cat(sprintf("  Arquivos importados com sucesso : %d\n", contador))
cat(sprintf("  Arquivos com erro               : %d\n", erros))
cat(sprintf("  Total de observações carregadas : %d\n", nrow(dados_completos)))
cat("======================================================\n")

if (erros > 0) {
  cat("\n  Arquivos que falharam:\n")
  cat("  ----------------------\n")
  for (i in seq_along(lista_erros)) {
    cat(sprintf("  [%d] %s\n", i, lista_erros[i]))
  }
  cat("\n")
}

# ============================================================
#  SALVAR
# ============================================================
if (nrow(dados_completos) > 0) {

  saveRDS(dados_completos, ARQUIVO_SAIDA)
  cat(sprintf("  ✓ Salvo como RDS  : %s\n", ARQUIVO_SAIDA))

  data.table::fwrite(
    dados_completos,
    ARQUIVO_CSV,
    sep = ";",
    bom = TRUE,        # BOM UTF-8 para abrir corretamente no Excel
    dateTimeAs = "write.csv"
  )
  cat(sprintf("  ✓ Salvo como CSV  : %s\n", ARQUIVO_CSV))

} else {
  cat("  ✗ Nenhuma observação importada — arquivos não salvos.\n")
}

cat("======================================================\n")

library(dplyr)
saldo <- dados_completos |>
  group_by(`competênciamov`) |>
  summarise(saldo_total = sum(`saldomovimentação`, na.rm = TRUE)) |>
  arrange(`competênciamov`)
  
