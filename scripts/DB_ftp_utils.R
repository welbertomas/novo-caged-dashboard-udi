# ============================================================
# DB_ftp_utils.R
# Utilitários de download FTP — Novo CAGED
#
# Função principal exposta ao pipeline:
#   baixar_mes_ftp(anomes)  →  list(CAGEDEXC=dt, CAGEDFOR=dt, CAGEDMOV=dt)
#
# Depende de variáveis definidas em DB_principal.R:
#   BASE_URL_FTP, DIR_TEMP
# ============================================================

# ── Privadas ─────────────────────────────────────────────

.montar_url <- function(tipo, anomes) {
  ano  <- substr(anomes, 1, 4)
  nome <- paste0(tipo, anomes, ".7z")
  url  <- paste0(BASE_URL_FTP, "/", ano, "/", anomes, "/", nome)
  list(url = url, nome = nome)
}

# Nomes com acento → sem acento (para normalização de colunas)
.NOMES_ACENTO <- c(
  "município", "competênciamov", "saldomovimentação",
  "salário", "horascontratuais", "valorsaláriofixo",
  "competênciadec", "competênciaexc"
)
.NOMES_ASCII <- c(
  "municipio", "competenciamov", "saldomovimentacao",
  "salario", "horascontratuais", "valorsalariofixo",
  "competenciadec", "competenciaexc"
)

.normalizar_colunas <- function(dt) {
  nomes_lower <- tolower(iconv(names(dt), to = "ASCII//TRANSLIT"))
  idx <- match(nomes_lower, .NOMES_ASCII)
  novos <- ifelse(!is.na(idx), .NOMES_ACENTO[idx], names(dt))
  setnames(dt, names(dt), novos)
  dt
}

.converter_virgula <- function(dt) {
  cols <- intersect(c("salário", "horascontratuais", "valorsaláriofixo"), names(dt))
  for (col in cols) {
    if (is.character(dt[[col]])) {
      dt[, (col) := as.numeric(gsub(",", ".", get(col)))]
    }
  }
  dt
}

#' Baixa e lê um único arquivo .7z do FTP.
#' Usa cache RDS em DIR_TEMP para evitar re-download na mesma sessão.
#' Retorna NULL em caso de falha (erros são reportados como warning).
.baixar_um_arquivo <- function(tipo, anomes) {

  cache_rds <- file.path(DIR_TEMP, paste0(tipo, anomes, ".rds"))
  if (file.exists(cache_rds)) return(readRDS(cache_rds))

  info    <- .montar_url(tipo, anomes)
  dest_7z <- file.path(DIR_TEMP, info$nome)

  # Download
  tryCatch(
    curl::curl_download(info$url, dest_7z, quiet = TRUE),
    error = function(e) stop(paste("Download falhou:", conditionMessage(e)))
  )

  # Identificar .txt dentro do .7z
  conteudo  <- archive::archive(dest_7z)
  txt_entry <- conteudo$path[grepl("\\.txt$", conteudo$path, ignore.case = TRUE)]
  if (length(txt_entry) == 0) stop("Nenhum .txt encontrado no .7z")

  # Extrair e ler
  archive::archive_extract(dest_7z, dir = DIR_TEMP)
  dest_txt <- file.path(DIR_TEMP, txt_entry[1])

  on.exit({
    if (file.exists(dest_7z))  file.remove(dest_7z)
    if (file.exists(dest_txt)) file.remove(dest_txt)
  })

  dt <- data.table::fread(dest_txt, sep = ";", encoding = "UTF-8",
                           showProgress = FALSE)

  dt <- .normalizar_colunas(dt)
  dt <- .converter_virgula(dt)

  # Inverter saldo para exclusões
  if (tipo == "CAGEDEXC" && "saldomovimentação" %in% names(dt)) {
    dt[, saldomovimentação := saldomovimentação * -1L]
  }

  saveRDS(dt, cache_rds)
  dt
}

# ── Pública ───────────────────────────────────────────────

limpar_temp_ftp <- function(dir_temp = DIR_TEMP) {
  if (!dir.exists(dir_temp)) return(invisible(FALSE))
  unlink(dir_temp, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' Baixa CAGEDEXC, CAGEDFOR e CAGEDMOV de um mês e retorna
#' uma lista nomeada com os três data.tables completos
#' (sem filtro de município — cada script filtra o que precisa).
#'
#' @param anomes  String AAAAMM, ex: "202601"
#' @return list(CAGEDEXC = dt, CAGEDFOR = dt, CAGEDMOV = dt)
baixar_mes_ftp <- function(anomes) {
  tipos  <- c("CAGEDEXC", "CAGEDFOR", "CAGEDMOV")
  resultado <- vector("list", 3L)
  names(resultado) <- tipos

  for (tipo in tipos) {
    cat(sprintf("  %-10s %s ... ", tipo, anomes))
    dt <- tryCatch(
      .baixar_um_arquivo(tipo, anomes),
      error = function(e) {
        message(sprintf("ERRO — %s", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(dt)) {
      cat(sprintf("✓  %d obs.\n", nrow(dt)))
      resultado[[tipo]] <- dt
    } else {
      cat("✗\n")
      stop(sprintf("Falha crítica ao baixar %s%s. Abortando.", tipo, anomes))
    }
  }

  resultado
}
