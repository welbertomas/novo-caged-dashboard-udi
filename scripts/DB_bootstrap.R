# ============================================================
# DB_bootstrap.R
# Utilitarios de bootstrap do ambiente R do projeto
# ============================================================

resolver_raiz_projeto <- function(dir_inicial = getwd()) {
  arquivo_origem <- NULL

  for (i in seq.int(sys.nframe(), 1L)) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(...) NULL)
    if (!is.null(ofile)) {
      arquivo_origem <- normalizePath(ofile, winslash = "/", mustWork = FALSE)
      break
    }
  }

  base_dir <- if (!is.null(arquivo_origem)) {
    dirname(arquivo_origem)
  } else {
    normalizePath(dir_inicial, winslash = "/", mustWork = FALSE)
  }

  if (basename(base_dir) == "scripts") dirname(base_dir) else base_dir
}

configurar_biblioteca_r <- function(dir_raiz = resolver_raiz_projeto()) {
  versao_menor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  versao_r <- paste(R.version$major, versao_menor, sep = ".")
  dir_lib <- file.path(dir_raiz, ".r_libs", versao_r)

  if (!dir.exists(dir_lib)) {
    dir.create(dir_lib, recursive = TRUE, showWarnings = FALSE)
  }

  .libPaths(unique(c(
    normalizePath(dir_lib, winslash = "/", mustWork = FALSE),
    .libPaths()
  )))

  invisible(.libPaths())
}

garantir_pacotes <- function(pkgs,
                             repos = "https://cloud.r-project.org") {
  faltantes <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(faltantes)) {
    install.packages(
      faltantes,
      lib = .libPaths()[1],
      repos = repos,
      dependencies = TRUE
    )
  }

  ainda_faltantes <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ainda_faltantes)) {
    stop(
      sprintf(
        "Pacotes ausentes apos a tentativa de instalacao: %s",
        paste(ainda_faltantes, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}
