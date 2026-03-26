# ============================================================
# DB_variaveis.R
# Função única de variáveis derivadas para os dois contextos
# do pipeline.
#
# criar_variaveis(dt, modo = "historico" | "ultimomes")
#
#   "historico"  — série completa de Uberlândia:
#                  admissões/demissões com lógica de exclusão,
#                  remuneração filtrada para todos os anos do SM,
#                  variáveis completas (divisão CNAE, faixa etária,
#                  gênero, etc.)
#
#   "ultimomes"  — CAGEDMOV de um único mês, todos os municípios:
#                  lógica simplificada de adm/dem,
#                  remuneração apenas para o ano atual.
#
# Depende de DB_principal.R: SM, ANO_ATUAL
# ============================================================

# ── Tabela de divisões CNAE (compartilhada) ───────────────
.DIVISAO_LEG <- c(
  `1`="Agricultura, Pecuária E Serviços Relacionados",
  `2`="Produção Florestal", `3`="Pesca E Aquicultura",
  `5`="Extração de Carvão Mineral",
  `6`="Extração De Petróleo E Gás Natural",
  `7`="Extração De Minerais Metálicos",
  `8`="Extração De Minerais Não-Metálicos",
  `9`="Atividades De Apoio À Extração De Minerais",
  `10`="Fabricação De Produtos Alimentícios",
  `11`="Fabricação De Bebidas", `12`="Fabricação De Produtos Do Fumo",
  `13`="Fabricação De Produtos Têxteis",
  `14`="Confecção De Artigos Do Vestuário E Acessórios",
  `15`="Couros, Artefatos De Couro E Calçados",
  `16`="Fabricação De Produtos De Madeira",
  `17`="Celulose, Papel E Produtos De Papel",
  `18`="Impressão E Reprodução De Gravações",
  `19`="Coque, Derivados Do Petróleo E Biocombustíveis",
  `20`="Fabricação De Produtos Químicos",
  `21`="Produtos Farmoquímicos E Farmacêuticos",
  `22`="Produtos De Borracha E De Material Plástico",
  `23`="Produtos De Minerais Não-Metálicos", `24`="Metalurgia",
  `25`="Produtos De Metal (Exceto Máquinas)",
  `26`="Equipamentos De Informática, Eletrônicos E Ópticos",
  `27`="Máquinas, Aparelhos E Materiais Elétricos",
  `28`="Fabricação De Máquinas E Equipamentos",
  `29`="Veículos Automotores, Reboques E Carrocerias",
  `30`="Outros Equipamentos De Transporte",
  `31`="Fabricação De Móveis", `32`="Fabricação De Produtos Diversos",
  `33`="Manutenção E Reparação De Máquinas E Equipamentos",
  `35`="Eletricidade, Gás E Outras Utilidades",
  `36`="Captação, Tratamento E Distribuição De Água",
  `37`="Esgoto E Atividades Relacionadas",
  `38`="Coleta E Tratamento De Resíduos",
  `39`="Descontaminação E Gestão De Resíduos",
  `41`="Construção De Edifícios", `42`="Obras De Infraestrutura",
  `43`="Serviços Especializados Para Construção",
  `45`="Comércio E Reparação De Veículos",
  `46`="Comércio Por Atacado", `47`="Comércio Varejista",
  `49`="Transporte Terrestre", `50`="Transporte Aquaviário",
  `51`="Transporte Aéreo",
  `52`="Armazenamento E Atividades Auxiliares Dos Transportes",
  `53`="Correio E Outras Atividades De Entrega",
  `55`="Alojamento", `56`="Alimentação",
  `58`="Edição E Edição Integrada À Impressão",
  `59`="Atividades Cinematográficas E De Televisão",
  `60`="Atividades De Rádio E De Televisão",
  `61`="Telecomunicações", `62`="Tecnologia Da Informação",
  `63`="Prestação De Serviços De Informação",
  `64`="Atividades De Serviços Financeiros",
  `65`="Seguros E Previdência Complementar",
  `66`="Atividades Auxiliares Dos Serviços Financeiros",
  `68`="Atividades Imobiliárias",
  `69`="Atividades Jurídicas, Contabilidade E Auditoria",
  `70`="Sedes De Empresas E Consultoria Em Gestão",
  `71`="Arquitetura, Engenharia E Análises Técnicas",
  `72`="Pesquisa E Desenvolvimento Científico",
  `73`="Publicidade E Pesquisa De Mercado",
  `74`="Outras Atividades Profissionais E Técnicas",
  `75`="Atividades Veterinárias",
  `77`="Aluguéis Não-Imobiliários",
  `78`="Seleção, Agenciamento E Locação De Mão-De-Obra",
  `79`="Agências De Viagens E Operadores Turísticos",
  `80`="Atividades De Vigilância E Segurança",
  `81`="Serviços Para Edifícios E Paisagismo",
  `82`="Serviços De Escritório E Apoio Administrativo",
  `84`="Administração Pública, Defesa E Seguridade Social",
  `85`="Educação", `86`="Atenção À Saúde Humana",
  `87`="Saúde Integrada Com Assistência Social",
  `88`="Serviços De Assistência Social Sem Alojamento",
  `90`="Atividades Artísticas, Criativas E De Espetáculos",
  `91`="Patrimônio Cultural E Ambiental",
  `92`="Exploração De Jogos De Azar E Apostas",
  `93`="Atividades Esportivas E De Recreação",
  `94`="Atividades De Organizações Associativas",
  `95`="Reparação De Equipamentos De Informática E Objetos Pessoais",
  `96`="Outras Atividades De Serviços Pessoais",
  `97`="Serviços Domésticos", `99`="Organismos Internacionais"
)

# ── Blocos reutilizáveis ──────────────────────────────────

.add_mes_data <- function(dt) {
  mes_nomes <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                 "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
  dt[, mes_num := competênciamov %% 100L]
  dt[, mes     := mes_nomes[mes_num]]
  dt[, ano     := competênciamov %/% 100L]
  dt[, data    := paste0(mes, "/", ano)]
  dt[, mes_num := NULL]
  dt
}

.add_setor_porte <- function(dt) {
  dt[, setores := data.table::fcase(
    secao == "A",                                         "Agropecuária",
    secao %in% c("B","C","D","E"),                        "Indústria",
    secao == "F",                                         "Construção",
    secao == "G",                                         "Comércio",
    secao %in% c("H","I","J","K","L","M","N","O",
                 "P","Q","R","S","T","U"),                "Serviços",
    secao == "Z",                                         "Não Identificado",
    default = NA_character_
  )]

  dt[, class_porte := data.table::fcase(
    secao %in% c("B","C","D","E","F"), 1L,
    secao == "O",                      2L,
    default = 0L
  )]

  dt[, tamanho := data.table::fcase(
    tamestabjan <= 4L  & class_porte == 1L,   "MEI e Micro",
    tamestabjan <= 3L  & class_porte == 0L,   "MEI e Micro",
    tamestabjan %in% 5:6 & class_porte == 1L, "Pequena",
    tamestabjan %in% 4:5 & class_porte == 0L, "Pequena",
    tamestabjan %in% 7:8 & class_porte == 1L, "Média",
    tamestabjan == 6L    & class_porte == 0L, "Média",
    tamestabjan %in% 9:10 & class_porte == 1L,"Grande",
    tamestabjan %in% 7:10 & class_porte == 0L,"Grande",
    class_porte == 2L,                         "Administração Pública",
    tamestabjan > 10L & class_porte != 2L,    "Ignorado",
    default = NA_character_
  )]

  dt[, indiceporte := data.table::fcase(
    tamanho == "MEI e Micro",           1L,
    tamanho == "Pequena",               2L,
    tamanho == "Média",                 3L,
    tamanho == "Grande",                4L,
    tamanho == "Administração Pública", 5L,
    tamanho == "Ignorado",              6L,
    default = NA_integer_
  )]
  dt
}

.add_escolaridade <- function(dt) {
  dt[, gdi_leg := data.table::fcase(
    graudeinstrucao == 1L,                "Analfabeto",
    graudeinstrucao %in% 2:4,            "Fundamental Incompleto",
    graudeinstrucao %in% 5:6,            "Fundamental Completo",
    graudeinstrucao %in% 7:8,            "Médio Completo",
    graudeinstrucao == 9L,               "Superior Completo",
    graudeinstrucao %in% c(10L,11L,80L), "Pós-Graduação",
    graudeinstrucao == 99L,              "Não Identificado",
    default = NA_character_
  )]
  dt
}

.add_salario_deflacionado <- function(dt) {
  dt[, salariodef := data.table::fifelse(
    salario > 0 & deflator > 0,
    salario * deflator,
    NA_real_
  )]
  dt
}

# ── Função principal ──────────────────────────────────────

#' @param dt    data.table com os microdados
#' @param modo  "historico" (padrão) ou "ultimomes"
criar_variaveis <- function(dt, modo = "historico") {
  .sm_ano <- function(ano_int) {
    chave <- as.character(ano_int)
    if (!is.na(SM[chave])) return(as.numeric(SM[chave]))
    anos_disp <- sort(as.integer(names(SM)))
    ano_ref <- max(anos_disp[anos_disp <= ano_int], na.rm = TRUE)
    if (!is.finite(ano_ref)) {
      stop(sprintf("Sem salário mínimo configurado para o ano %s.", ano_int))
    }
    warning(sprintf(
      "Salário mínimo de %s não encontrado em config.R; usando valor de %s.",
      ano_int, ano_ref
    ))
    as.numeric(SM[as.character(ano_ref)])
  }

  # ── Admissões e Demissões ─────────────────────────────
  if (modo == "historico") {
    # Lógica completa: considera o indicador de exclusão
    adm <- as.integer(dt$saldomovimentação == 1L)
    dem <- as.integer(dt$saldomovimentação == -1L)
    ind <- dt$indicadordeexclusão
    dt[, admissoes := data.table::fcase(
      is.na(ind),  adm, ind == 1L, -dem, default = 0L)]
    dt[, demissoes := data.table::fcase(
      is.na(ind),  dem, ind == 1L, -adm, default = 0L)]
  } else {
    # Lógica simplificada para CAGEDMOV puro
    dt[, admissoes := data.table::fifelse(saldomovimentação ==  1L,  1L, 0L)]
    dt[, demissoes := data.table::fifelse(saldomovimentação == -1L, -1L, 0L)]
  }

  # ── Blocos comuns ─────────────────────────────────────
  dt <- .add_mes_data(dt)
  dt <- .add_setor_porte(dt)
  dt <- .add_escolaridade(dt)
  dt[, saldo_mes := sum(saldomovimentação, na.rm = TRUE), by = competênciamov]

  if ("salario" %in% names(dt) && "deflator" %in% names(dt)) {
    dt <- .add_salario_deflacionado(dt)
  }

  # ── Blocos exclusivos do modo "historico" ─────────────
  if (modo == "historico") {

    dt[, faixa_etaria := data.table::fcase(
      idade <= 17L,       "17 ou menos",
      idade %in% 18:24,   "18-24",
      idade %in% 25:29,   "25-29",
      idade %in% 30:39,   "30-39",
      idade %in% 40:49,   "40-49",
      idade %in% 50:59,   "50-59",
      idade >= 60L,       "60 ou mais",
      default = NA_character_
    )]

    dt[, genero := data.table::fcase(
      sexo == 1L, "Homem",
      sexo == 3L, "Mulher",
      sexo == 9L, "Não identificado",
      default = NA_character_
    )]

    dt[, divisao := as.integer(subclasse %/% 100000L)]
    dt[, div_leg := .DIVISAO_LEG[as.character(divisao)]]

    # Remuneração com filtro de outliers para todos os anos
    dt[, remuneracao_udi_adm := NA_real_]
    dt[, remuneracao_udi_dem := NA_real_]
    anos_dt <- sort(unique(dt$ano))
    for (a in anos_dt) {
      sm <- .sm_ano(a)
      dt[ano == a & salario > sm * 0.3 & salario < sm * 150 &
         saldomovimentação == 1L &
         (is.na(indtrabintermitente) | indtrabintermitente != 1L),
         remuneracao_udi_adm := salariodef]
      dt[ano == a & salario > sm * 0.3 & salario < sm * 150 &
         saldomovimentação == -1L &
         (is.na(indtrabintermitente) | indtrabintermitente != 1L),
         remuneracao_udi_dem := salariodef]
    }

  } else {
    # Remuneração apenas para o ano atual
    sm <- .sm_ano(as.integer(ANO_ATUAL))
    dt[, remuneracao_adm := NA_real_]
    dt[ano == as.integer(ANO_ATUAL) &
       salario > sm * 0.3 & salario < sm * 150 &
       saldomovimentação == 1L &
       (is.na(indtrabintermitente) | indtrabintermitente != 1L),
       remuneracao_adm := salariodef]
  }

  dt
}
