# ============================================================
# DB_ftp_utils.R
# Utilitarios de download FTP - Novo CAGED
# ============================================================

.montar_url <- function(tipo, anomes) {
  ano <- substr(anomes, 1, 4)
  nome <- paste0(tipo, anomes, ".7z")
  url <- paste0(BASE_URL_FTP, "/", ano, "/", anomes, "/", nome)
  list(url = url, nome = nome)
}

.arquivo_valido <- function(caminho) {
  isTRUE(file.exists(caminho)) && isTRUE(file.info(caminho)$size > 0)
}

.baixar_via_curl_pkg <- function(url, dest, usar_epsv = TRUE) {
  handle <- curl::new_handle()
  if (!usar_epsv) {
    curl::handle_setopt(handle, ftp_use_epsv = FALSE)
  }
  curl::curl_download(url, dest, quiet = TRUE, handle = handle)
}

.baixar_via_download_file <- function(url, dest) {
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE, method = "libcurl")
}

.baixar_via_curl_exe <- function(url, dest) {
  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) {
    stop("curl.exe nao encontrado no PATH.")
  }

  saida <- suppressWarnings(system2(
    curl_bin,
    args = c(
      "--fail",
      "--silent",
      "--show-error",
      "--disable-epsv",
      "--output", dest,
      url
    ),
    stdout = TRUE,
    stderr = TRUE
  ))

  status <- attr(saida, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(saida, collapse = "\n"))
  }
}

.baixar_com_fallback <- function(url, dest) {
  if (.arquivo_valido(dest)) {
    return(dest)
  }

  tentativas <- list(
    "curl::curl_download" = function() .baixar_via_curl_pkg(url, dest, usar_epsv = TRUE),
    "curl::curl_download (sem EPSV)" = function() .baixar_via_curl_pkg(url, dest, usar_epsv = FALSE),
    "utils::download.file(libcurl)" = function() .baixar_via_download_file(url, dest),
    "curl.exe --disable-epsv" = function() .baixar_via_curl_exe(url, dest)
  )

  erros <- character()

  for (nome_tentativa in names(tentativas)) {
    if (file.exists(dest)) {
      unlink(dest, force = TRUE)
    }

    ok <- tryCatch({
      tentativas[[nome_tentativa]]()
      .arquivo_valido(dest)
    }, error = function(e) {
      erros <<- c(erros, sprintf("%s: %s", nome_tentativa, conditionMessage(e)))
      FALSE
    })

    if (ok) {
      return(dest)
    }
  }

  stop(
    paste(
      c(
        sprintf("Nao foi possivel baixar %s.", basename(dest)),
        "Tentativas:",
        paste0(" - ", erros),
        sprintf("URL: %s", url)
      ),
      collapse = "\n"
    )
  )
}

.NOMES_ACENTO <- c(
  "munic\u00edpio", "compet\u00eanciamov", "saldomovimenta\u00e7\u00e3o",
  "sal\u00e1rio", "horascontratuais", "valorsal\u00e1riofixo",
  "compet\u00eanciadec", "compet\u00eanciaexc"
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
  cols <- intersect(c("sal\u00e1rio", "horascontratuais", "valorsal\u00e1riofixo"), names(dt))
  for (col in cols) {
    if (is.character(dt[[col]])) {
      dt[, (col) := as.numeric(gsub(",", ".", get(col)))]
    }
  }
  dt
}

.baixar_um_arquivo <- function(tipo, anomes) {
  cache_rds <- file.path(DIR_TEMP, paste0(tipo, anomes, ".rds"))
  if (file.exists(cache_rds)) return(readRDS(cache_rds))

  info <- .montar_url(tipo, anomes)
  dest_7z <- file.path(DIR_TEMP, info$nome)

  tryCatch(
    .baixar_com_fallback(info$url, dest_7z),
    error = function(e) stop(paste("Download falhou:", conditionMessage(e)))
  )

  conteudo <- archive::archive(dest_7z)
  txt_entry <- conteudo$path[grepl("\\.txt$", conteudo$path, ignore.case = TRUE)]
  if (length(txt_entry) == 0) stop("Nenhum .txt encontrado no .7z")

  archive::archive_extract(dest_7z, dir = DIR_TEMP)
  dest_txt <- file.path(DIR_TEMP, txt_entry[1])

  on.exit({
    if (file.exists(dest_7z)) file.remove(dest_7z)
    if (file.exists(dest_txt)) file.remove(dest_txt)
  })

  dt <- data.table::fread(dest_txt, sep = ";", encoding = "UTF-8", showProgress = FALSE)
  dt <- .normalizar_colunas(dt)
  dt <- .converter_virgula(dt)

  if (tipo == "CAGEDEXC" && "saldomovimenta\u00e7\u00e3o" %in% names(dt)) {
    dt[, ("saldomovimenta\u00e7\u00e3o") := get("saldomovimenta\u00e7\u00e3o") * -1L]
  }

  saveRDS(dt, cache_rds)
  dt
}

limpar_temp_ftp <- function(dir_temp = DIR_TEMP) {
  if (!dir.exists(dir_temp)) return(invisible(FALSE))
  unlink(dir_temp, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

baixar_mes_ftp <- function(anomes) {
  tipos <- c("CAGEDEXC", "CAGEDFOR", "CAGEDMOV")
  resultado <- vector("list", 3L)
  names(resultado) <- tipos

  for (tipo in tipos) {
    cat(sprintf("  %-10s %s ... ", tipo, anomes))
    dt <- tryCatch(
      .baixar_um_arquivo(tipo, anomes),
      error = function(e) {
        message(sprintf("ERRO - %s", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(dt)) {
      cat(sprintf("OK  %d obs.\n", nrow(dt)))
      resultado[[tipo]] <- dt
    } else {
      cat("X\n")
      stop(sprintf("Falha critica ao baixar %s%s. Abortando.", tipo, anomes))
    }
  }

  resultado
}
