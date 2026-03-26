# Gera tabelas e gráficos para boletim — versão simplificada

# ── Pacotes ──────────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, haven, knitr, rmarkdown, here,
               lubridate, scales, kableExtra, forcats, data.table)

# ── Configurações gerais ──────────────────────────────────────────────────────
Sys.setlocale("LC_ALL", if (.Platform$OS.type == "windows") "Portuguese_Brazil.UTF-8" else "pt_BR.UTF-8")
Sys.setenv(LANG = "pt_BR.UTF-8")
options(encoding = "UTF-8", scipen = 999)

# ── Parâmetros ────────────────────────────────────────────────────────────────
global_periodo_referencia <- MES_ATUAL
global_arquivo_tabelas    <- TABELAS_BOLETIM

data_ref       <- as.Date(paste0(global_periodo_referencia, "01"), "%Y%m%d")
periodo_12meses <- as.numeric(format(seq(data_ref, by = "-12 months", length.out = 2)[2], "%Y%m"))
ano_ref <- as.numeric(format(data_ref, "%Y"))

# ── Vetores de lookup (substituem case_when gigantes) ────────────────────────

meses_pt <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
              "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
mes_ref_num <- as.integer(format(data_ref, "%m"))
mes_ref_label <- meses_pt[mes_ref_num]
mes_ref_label_lower <- tolower(mes_ref_label)
data_inicio_serie <- seq(data_ref, by = "-12 months", length.out = 2)[2]
mes_inicio_serie <- meses_pt[as.integer(format(data_inicio_serie, "%m"))]
ano_inicio_serie <- as.integer(format(data_inicio_serie, "%Y"))
periodo_label_12m <- paste0("de ", tolower(mes_inicio_serie), "/", ano_inicio_serie,
                            " a ", mes_ref_label_lower, "/", ano_ref)
periodo_label_mes <- paste0(mes_ref_label_lower, " de ", ano_ref)
fonte_grafico <- paste0(
  "Fonte: Novo Caged/MTE. Elaboração: CEPES/IERI/UFU. ",
  "*Dados com ajustes declarados até ", mes_ref_label_lower, " de ", ano_ref, "."
)
fonte_tabela  <- fonte_grafico

setores_map <- c(
  A = "Agropecuária",
  B = "Indústria", C = "Indústria", D = "Indústria", E = "Indústria",
  F = "Construção", G = "Comércio",
  H="Serviços",I="Serviços",J="Serviços",K="Serviços",L="Serviços",
  M="Serviços",N="Serviços",O="Serviços",P="Serviços",Q="Serviços",
  R="Serviços",S="Serviços",T="Serviços",U="Serviços",
  Z = "Não Identificado"
)

# Níveis ordenados (definidos uma vez, reutilizados em todo o script)
lvl_setor     <- c("Agropecuária","Indústria","Construção","Comércio","Serviços")
lvl_porte     <- c("MEI e Micro","Pequena","Média","Grande","Administração Pública")
lvl_faixa     <- c("17 ou menos","18-24","25-29","30-39","40-49","50-59","60 ou mais")
lvl_gdi       <- c("Analfabeto","Fundamental Incompleto","Fundamental Completo",
                   "Médio Completo","Superior Completo","Pós-Graduação")
lvl_genero    <- c("Homem","Mulher")

div_leg_map <- c(
  "1"="Agricultura, Pecuária E Serviços Relacionados",
  "2"="Produção Florestal", "3"="Pesca E Aqüicultura",
  "5"="Extração de Carvão Mineral", "6"="Extração De Petróleo E Gás Natural",
  "7"="Extração De Minerais Metálicos", "8"="Extração De Minerais Não-Metálicos",
  "9"="Atividades De Apoio À Extração De Minerais",
  "10"="Fabricação De Produtos Alimentícios", "11"="Fabricação De Bebidas",
  "12"="Fabricação De Produtos Do Fumo", "13"="Fabricação De Produtos Têxteis",
  "14"="Confecção De Artigos Do Vestuário E Acessórios",
  "15"="Preparação De Couros E Fabricação De Artefatos De Couro, Artigos Para Viagem E Calçados",
  "16"="Fabricação De Produtos De Madeira",
  "17"="Fabricação De Celulose, Papel E Produtos De Papel",
  "18"="Impressão E Reprodução De Gravações",
  "19"="Fabricação De Coque, De Produtos Derivados Do Petróleo E De Biocombustíveis",
  "20"="Fabricação De Produtos Químicos",
  "21"="Fabricação De Produtos Farmoquímicos E Farmacêuticos",
  "22"="Fabricação De Produtos De Borracha E De Material Plástico",
  "23"="Fabricação De Produtos De Minerais Não-Metálicos",
  "24"="Metalurgia",
  "25"="Fabricação De Produtos De Metal, Exceto Máquinas E Equipamentos",
  "26"="Fabricação De Equipamentos De Informática, Produtos Eletrônicos E Ópticos",
  "27"="Fabricação De Máquinas, Aparelhos E Materiais Elétricos",
  "28"="Fabricação De Máquinas E Equipamentos",
  "29"="Fabricação De Veículos Automotores, Reboques E Carrocerias",
  "30"="Fabricação De Outros Equipamentos De Transporte, Exceto Veículos Automotores",
  "31"="Fabricação De Móveis", "32"="Fabricação De Produtos Diversos",
  "33"="Manutenção, Reparação E Instalação De Máquinas E Equipamentos",
  "35"="Eletricidade, Gás E Outras Utilidades",
  "36"="Captação, Tratamento E Distribuição De Água",
  "37"="Esgoto E Atividades Relacionadas",
  "38"="Coleta, Tratamento E Disposição De Resíduos; Recuperação De Materiais",
  "39"="Descontaminação E Outros Serviços De Gestão De Resíduos",
  "41"="Construção De Edifícios", "42"="Obras De Infra-Estrutura",
  "43"="Serviços Especializados Para Construção",
  "45"="Comércio E Reparação De Veículos Automotores E Motocicletas",
  "46"="Comércio Por Atacado, Exceto Veículos Automotores E Motocicletas",
  "47"="Comércio Varejista", "49"="Transporte Terrestre",
  "50"="Transporte Aquaviário", "51"="Transporte Aéreo",
  "52"="Armazenamento E Atividades Auxiliares Dos Transportes",
  "53"="Correio E Outras Atividades De Entrega",
  "55"="Alojamento", "56"="Alimentação",
  "58"="Edição E Edição Integrada À Impressão",
  "59"="Atividades Cinematográficas, Produção De Vídeos E De Programas De Televisão; Gravação De Som E Edição De Música",
  "60"="Atividades De Rádio E De Televisão", "61"="Telecomunicações",
  "62"="Atividades Dos Serviços De Tecnologia Da Informação",
  "63"="Atividades De Prestação De Serviços De Informação",
  "64"="Atividades De Serviços Financeiros",
  "65"="Seguros, Resseguros, Previdência Complementar E Planos De Saúde",
  "66"="Atividades Auxiliares Dos Serviços Financeiros, Seguros, Previdência Complementar E Planos De Saúde",
  "68"="Atividades Imobiliárias",
  "69"="Atividades Jurídicas, De Contabilidade E De Auditoria",
  "70"="Atividades De Sedes De Empresas E De Consultoria Em Gestão Empresarial",
  "71"="Serviços De Arquitetura E Engenharia; Testes E Análises Técnicas",
  "72"="Pesquisa E Desenvolvimento Científico",
  "73"="Publicidade E Pesquisa De Mercado",
  "74"="Outras Atividades Profissionais, Científicas E Técnicas",
  "75"="Atividades Veterinárias",
  "77"="Aluguéis Não-Imobiliários E Gestão De Ativos Intangíveis Não-Financeiros",
  "78"="Seleção, Agenciamento E Locação De Mão-De-Obra",
  "79"="Agências De Viagens, Operadores Turísticos E Serviços De Reservas",
  "80"="Atividades De Vigilância, Segurança E Investigação",
  "81"="Serviços Para Edifícios E Atividades Paisagísticas",
  "82"="Serviços De Escritório, De Apoio Administrativo E Outros Serviços Prestados Principalmente Às Empresas",
  "84"="Administração Pública, Defesa E Seguridade Social", "85"="Educação",
  "86"="Atividades De Atenção À Saúde Humana",
  "87"="Atividades De Atenção À Saúde Humana Integradas Com Assistência Social, Prestadas Em Residências Coletivas E Particulares",
  "88"="Serviços De Assistência Social Sem Alojamento",
  "90"="Atividades Artísticas, Criativas E De Espetáculos",
  "91"="Atividades Ligadas Ao Patrimônio Cultural E Ambiental",
  "92"="Atividades De Exploração De Jogos De Azar E Apostas",
  "93"="Atividades Esportivas E De Recreação E Lazer",
  "94"="Atividades De Organizações Associativas",
  "95"="Reparação E Manutenção De Equipamentos De Informática E Comunicação E De Objetos Pessoais E Domésticos",
  "96"="Outras Atividades De Serviços Pessoais", "97"="Serviços Domésticos",
  "99"="Organismos Internacionais E Outras Instituições Extraterritoriais"
)

# ── Leitura dos dados ─────────────────────────────────────────────────────────
df_caged <- readRDS(file.path(DIR_DATA, "CAGED_completo.rds"))

arq_ipc <- Sys.glob(file.path(DIR_RAW, "cepes_op_ipc_cepes_serie_historica_agregada_n_indice_1994_*.xlsx"))
if (!length(arq_ipc)) stop("Arquivo IPC-CEPES não encontrado.")
arq_ipc <- tail(arq_ipc, 1)

ipc <- as.data.table(readxl::read_excel(arq_ipc, sheet = "numeroindice", col_types = "numeric")) |>
  (\(dt) {
    dt <- dt[!is.na(anomes)]
    setorder(dt, anomes)
    dt[, deflator := dt[anomes == max(anomes), numeroindice] / numeroindice]
    setnames(dt, "anomes", "competênciamov")
    dt[, .(competênciamov, deflator)]
  })()

cat(sprintf("  IPC: %d períodos (último: %d)\n", nrow(ipc), max(ipc$competênciamov)))

# ── Processamento principal ───────────────────────────────────────────────────
df_processed <- df_caged |>
  inner_join(ipc, by = "competênciamov") |>
  filter(competênciamov >= periodo_12meses) |>
  mutate(
    # Admissões / demissões
    admissoes = if_else(saldomovimentação == 1,  saldomovimentação, 0L),
    demissoes = if_else(saldomovimentação == -1, saldomovimentação, 0L),
    
    # Data
    ano     = competênciamov %/% 100,
    mês_num = competênciamov %%  100,
    mês     = meses_pt[mês_num],
    data    = paste0(mês, "/", ano),
    
    # Setor
    setores = setores_map[seção],
    
    # Porte
    class_porte = case_when(seção %in% c("B","C","D","E","F") ~ 1L,
                            seção == "O" ~ 2L, TRUE ~ 0L),
    
    tamanho = case_when(
      class_porte == 2 ~ "Administração Pública",
      tamestabjan > 10 ~ "Ignorado",
      class_porte == 1 & tamestabjan <= 4 ~ "MEI e Micro",
      class_porte == 0 & tamestabjan <= 3 ~ "MEI e Micro",
      class_porte == 1 & tamestabjan <= 6 ~ "Pequena",
      class_porte == 0 & tamestabjan <= 5 ~ "Pequena",
      class_porte == 1 & tamestabjan <= 8 ~ "Média",
      class_porte == 0 & tamestabjan == 6 ~ "Média",
      TRUE ~ "Grande"
    ),
    tamanho = factor(tamanho, levels = c(lvl_porte, "Ignorado")),
    
    # Faixa etária
    faixa_etaria = cut(idade,
                       breaks = c(-Inf, 17, 24, 29, 39, 49, 59, Inf),
                       labels = lvl_faixa,
                       right  = TRUE),
    
    # Gênero
    genero = factor(case_when(sexo == 1 ~ "Homem", sexo == 3 ~ "Mulher",
                              TRUE ~ "Não identificado"),
                    levels = c(lvl_genero, "Não identificado")),
    
    # Escolaridade
    gdi_leg = factor(case_when(
      graudeinstrução == 1              ~ "Analfabeto",
      graudeinstrução %in% 2:4         ~ "Fundamental Incompleto",
      graudeinstrução %in% 5:6         ~ "Fundamental Completo",
      graudeinstrução %in% 7:8         ~ "Médio Completo",
      graudeinstrução == 9             ~ "Superior Completo",
      graudeinstrução %in% c(10,11,80) ~ "Pós-Graduação",
      TRUE ~ "Não Identificado"
    ), levels = c(lvl_gdi, "Não Identificado")),
    
    # Divisão CNAE
    divisao = floor(subclasse / 100000),
    div_leg = div_leg_map[as.character(divisao)],
    
    # Salário deflacionado
    salariodef = if_else(salário > 0 & deflator > 0, salário * deflator, NA_real_),
    
    # Remuneração filtrada (admissão e demissão)
    remuneracao_udi_adm = if_else(
      saldomovimentação == 1 & indtrabintermitente != 1 &
        salário > SM[as.character(ano)] * 0.3 &
        salário < SM[as.character(ano)] * 150,
      salariodef, NA_real_
    ),
    remuneracao_udi_dem = if_else(
      saldomovimentação == -1 & indtrabintermitente != 1 &
        salário > SM[as.character(ano)] * 0.3 &
        salário < SM[as.character(ano)] * 150,
      salariodef, NA_real_
    )
  ) |>
  arrange(competênciamov) |>
  mutate(data = factor(data, levels = unique(data))) |>
  group_by(competênciamov, data) |>
  mutate(saldo_mes = sum(saldomovimentação, na.rm = TRUE)) |>
  ungroup()

# ── Funções auxiliares ────────────────────────────────────────────────────────
fmt_num <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x) sprintf("%.2f%%", x)

# Pivot largo com saldo por grupo, com linha de acumulado
saldo_wide <- function(df, grp_var, levels = NULL) {
  df |>
    group_by(competênciamov, data, across(all_of(grp_var))) |>
    summarise(saldo = sum(saldomovimentação, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = all_of(grp_var), values_from = saldo, values_fill = 0L) |>
    arrange(competênciamov) |>
    (\(d) {
      cols <- if (!is.null(levels)) levels else setdiff(names(d), c("competênciamov","data"))
      d <- d[, c("competênciamov","data", intersect(cols, names(d)))]
      acum <- d |>
        filter(competênciamov >= (ano_ref * 100 + 1),
               competênciamov <= global_periodo_referencia) |>
        summarise(across(all_of(intersect(cols, names(d))), sum, na.rm = TRUE)) |>
        mutate(data = paste0("Acum. ", ano_ref), competênciamov = NA_real_)
      bind_rows(d, acum)
    })()
}

# Remuneração média larga com linha de variação acumulada
remuneracao_wide <- function(df, grp_var, ano_ref) {
  jan <- ano_ref * 100 + 1
  dez <- global_periodo_referencia
  df |>
    group_by(competênciamov, data, across(all_of(grp_var))) |>
    summarise(rem = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = all_of(grp_var), values_from = rem) |>
    arrange(competênciamov) |>
    (\(d) {
      cols <- setdiff(names(d), c("competênciamov","data"))
      acum <- d |>
        filter(competênciamov %in% c(jan, dez)) |>
        summarise(across(all_of(cols), ~(last(.x)/first(.x) - 1)*100)) |>
        mutate(data = paste0("Acum. ", ano_ref, " (%)"), competênciamov = NA_real_) |>
        mutate(across(all_of(cols), fmt_pct))
      d |> mutate(across(all_of(cols), ~fmt_num(round(.x)))) |>
        bind_rows(acum)
    })()
}

# Métricas de Tabela 4 (saldo/adm/dem por grupo)
tabela4_grupo <- function(df, grp_var, levels = NULL,
                          tot_adm, tot_dem_abs) {
  df |>
    group_by(across(all_of(grp_var))) |>
    summarise(
      Admissões     = sum(admissoes, na.rm = TRUE),
      Desligamentos = sum(demissoes, na.rm = TRUE),
      Saldo         = sum(saldomovimentação, na.rm = TRUE),
      .groups = "drop"
    ) |>
    (\(d) if (!is.null(levels))
      mutate(d, across(all_of(grp_var), ~factor(.x, levels = levels))) |> arrange(across(all_of(grp_var)))
     else d
    )() |>
    rename(Variáveis = all_of(grp_var)) |>
    select(Variáveis, Admissões, Desligamentos, Saldo)
}

# ── Dados agregados ───────────────────────────────────────────────────────────

# Tabela 1 / Gráfico 1 — Saldo geral
df_geral_saldo <- df_processed |>
  group_by(competênciamov, data) |>
  summarise(admissoes = sum(admissoes), demissoes = sum(demissoes),
            saldomovimentação = sum(saldomovimentação),
            saldo_mes = first(saldo_mes), .groups = "drop") |>
  arrange(competênciamov)

df_ano_ref <- df_geral_saldo |>
  filter(competênciamov >= (ano_ref * 100 + 1),
         competênciamov <= global_periodo_referencia)

# Tabela 2 — Saldo por setor
df_saldo_setor <- df_processed |>
  filter(setores != "Não Identificado") |>
  saldo_wide("setores", lvl_setor)

# Tabela 3 — Saldo por porte
df_saldo_porte <- df_processed |>
  filter(!tamanho %in% c("Ignorado")) |>
  saldo_wide("tamanho", lvl_porte)

# Base do mês de referência (usada em várias tabelas e gráficos)
df_base_mes <- df_processed |>
  filter(competênciamov == global_periodo_referencia)

tot_adm     <- sum(df_base_mes$admissoes)
tot_dem_abs <- sum(abs(df_base_mes$demissoes))

# Tabela 4 — Sintética do mês
df_t4_setores   <- tabela4_grupo(filter(df_base_mes, setores != "Não Identificado"),
                                 "setores", lvl_setor, tot_adm, tot_dem_abs)
df_t4_porte     <- tabela4_grupo(filter(df_base_mes, !tamanho %in% c("Ignorado","Administração Pública")),
                                 "tamanho", lvl_porte[1:4], tot_adm, tot_dem_abs)
df_t4_faixa     <- tabela4_grupo(df_base_mes, "faixa_etaria", lvl_faixa, tot_adm, tot_dem_abs)
df_t4_genero    <- tabela4_grupo(filter(df_base_mes, genero != "Não identificado"),
                                 "genero", lvl_genero, tot_adm, tot_dem_abs)
df_t4_escol     <- tabela4_grupo(filter(df_base_mes, gdi_leg != "Não Identificado"),
                                 "gdi_leg", lvl_gdi, tot_adm, tot_dem_abs)

# Tabelas 5-7 — Remuneração
df_remuneracao_geral  <- df_processed |>
  group_by(competênciamov, data) |>
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE),
            remuneracao_udi_dem = mean(remuneracao_udi_dem, na.rm = TRUE),
            .groups = "drop") |>
  arrange(competênciamov)

df_tabela6 <- df_processed |> filter(setores != "Não Identificado") |>
  remuneracao_wide("setores", ano_ref)

df_tabela7 <- df_processed |>
  filter(!tamanho %in% c("Ignorado","Administração Pública")) |>
  remuneracao_wide("tamanho", ano_ref)

# Dados para Gráficos 2,3,4,6,7,8 (mesmo mês base, agrupamentos diferentes)
df_g2 <- df_base_mes |> filter(!tamanho %in% c("Administração Pública","Ignorado"),
                               setores != "Não Identificado") |>
  group_by(setores, tamanho) |>
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop") |>
  mutate(setores = factor(setores, lvl_setor), tamanho = factor(tamanho, lvl_porte[1:4]))

df_g3 <- df_base_mes |>
  group_by(faixa_etaria) |>
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

df_g4 <- df_base_mes |>
  filter(genero != "Não identificado", gdi_leg != "Não Identificado") |>
  group_by(genero, gdi_leg) |>
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

df_g6 <- df_base_mes |>
  filter(!tamanho %in% c("Administração Pública","Ignorado"), setores != "Não Identificado") |>
  group_by(setores, tamanho) |>
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = "drop") |>
  mutate(setores = factor(setores, lvl_setor), tamanho = factor(tamanho, lvl_porte[1:4]))

df_g7 <- df_base_mes |>
  group_by(faixa_etaria) |>
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = "drop")

df_g8 <- df_base_mes |>
  filter(genero != "Não identificado", gdi_leg != "Não Identificado") |>
  group_by(genero, gdi_leg) |>
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = "drop")

# ── Temas e paletas de cores ──────────────────────────────────────────────────
bar_colors_saldo <- c("TRUE" = "steelblue", "FALSE" = "darkred")

tema_base <- theme_minimal() %+replace% theme(
  plot.title   = element_text(size = 12, face = "bold", hjust = 0.5,
                              lineheight = 1.2, margin = margin(t=15, b=10, unit="pt")),
  plot.caption = element_text(size = 9, hjust = 0, margin = margin(t=25, unit="pt")),
  plot.title.position = "plot"
)

# ── Gráficos ──────────────────────────────────────────────────────────────────

# Gráfico 1 — Saldo mensal
grafico1 <- ggplot(df_geral_saldo, aes(x = data, y = saldo_mes, fill = saldo_mes > 0)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = bar_colors_saldo, guide = "none") +
  geom_text(aes(label = fmt_num(saldo_mes)),
            vjust = ifelse(df_geral_saldo$saldo_mes > 0, -0.5, 1.2), size = 3.5) +
  scale_y_continuous(labels = fmt_num, limits = c(-3500, 2000),
                     breaks = seq(-3500, 2000, 1000)) +
  labs(title = paste0("Gráfico 1 – Uberlândia/MG: Saldo do emprego formal,\n", periodo_label_12m, ", com ajustes*"),
       x = NULL, y = "Saldo", caption = fonte_grafico) +
  tema_base +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        panel.grid.major.x = element_blank())

# Gráfico 2 — Saldo por setor e porte (barras horizontais facetadas)
grafico2 <- ggplot(df_g2, aes(x = fct_rev(tamanho), y = saldomovimentação, fill = saldomovimentação > 0)) +
  geom_col() +
  scale_fill_manual(values = bar_colors_saldo, guide = "none") +
  geom_text(aes(label = fmt_num(saldomovimentação)),
            hjust = ifelse(df_g2$saldomovimentação > 0, -0.2, 1.2), size = 3.5) +
  facet_wrap(~setores, scales = "free_y", ncol = 1, strip.position = "left") +
  coord_flip() +
  labs(title = paste0("Gráfico 2 – Uberlândia/MG: Saldo por setor de atividade e porte da empresa\nem ", periodo_label_mes, ", com ajustes*"),
       x = NULL, y = "Saldo", caption = fonte_grafico) +
  tema_base +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "gray95"),
        panel.grid.major.y = element_blank())

# Gráfico 3 — Saldo por faixa etária
grafico3 <- ggplot(df_g3, aes(x = fct_rev(faixa_etaria), y = saldomovimentação, fill = saldomovimentação > 0)) +
  geom_col() +
  scale_fill_manual(values = bar_colors_saldo, guide = "none") +
  geom_text(aes(label = fmt_num(saldomovimentação)),
            hjust = ifelse(df_g3$saldomovimentação > 0, -0.2, 1.2), size = 3.5) +
  coord_flip() +
  labs(title = paste0("Gráfico 3 – Uberlândia/MG: Saldo por faixa etária do empregado\nem ", periodo_label_mes, ", com ajustes*"),
       x = NULL, y = "Saldo", caption = fonte_grafico) +
  tema_base + theme(panel.grid.major.y = element_blank())

# Gráfico 4 — Saldo por gênero e escolaridade
grafico4 <- ggplot(df_g4, aes(x = fct_rev(gdi_leg), y = saldomovimentação, fill = saldomovimentação > 0)) +
  geom_col(position = position_dodge(0.9)) +
  scale_fill_manual(values = bar_colors_saldo, guide = "none") +
  geom_text(aes(label = fmt_num(saldomovimentação)),
            hjust = ifelse(df_g4$saldomovimentação > 0, -0.2, 1.2), size = 3.5) +
  facet_wrap(~genero, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(limits = c(-550, 600), breaks = seq(-550, 600, 200)) +
  labs(title = paste0("Gráfico 4 – Uberlândia/MG: Saldo por gênero e grau de instrução do empregado\nem ", periodo_label_mes, ", com ajustes*"),
       x = NULL, y = "Saldo", caption = fonte_grafico) +
  tema_base +
  theme(strip.text = element_text(size=10, face="bold"),
        strip.background = element_rect(fill="gray95"),
        panel.grid.major.y = element_blank())

# Gráfico 5 — Salário médio real de admissão ao longo do tempo
grafico5 <- ggplot(df_remuneracao_geral, aes(x = data, y = remuneracao_udi_adm)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = fmt_num(round(remuneracao_udi_adm))), vjust = -0.5, size = 3) +
  labs(title = paste0("Gráfico 5 – Uberlândia/MG: Salário médio real de admissão\n", periodo_label_12m, " (em R$), com ajustes*"),
       x = NULL, y = "Salário de admissão", caption = fonte_grafico) +
  tema_base +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        axis.text.y = element_blank(), panel.grid.major.x = element_blank())

# Gráficos 6-8 — padrão similar (salário por categoria)
make_grafico_salario <- function(df, x_var, fill_var = NULL,
                                 title, x_lim = c(0, 3500)) {
  p <- ggplot(df, aes(x = fct_rev(!!sym(x_var)), y = remuneracao_udi_adm)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = fmt_num(round(remuneracao_udi_adm))),
              hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_y_continuous(limits = x_lim, labels = NULL) +
    labs(title = title, x = NULL, y = NULL, caption = fonte_grafico) +
    tema_base +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.y = element_blank())
  if (!is.null(fill_var))
    p <- p + facet_wrap(as.formula(paste("~", fill_var)), scales = "free_y") +
      theme(strip.text = element_text(size=10, face="bold"),
            strip.background = element_rect(fill="gray95"))
  p
}

grafico6 <- make_grafico_salario(
  df_g6, "tamanho", "setores",
  paste0("Gráfico 6 – Uberlândia/MG: Salário médio real de admissão por grupamento\nde atividade econômica e porte da empresa, ", periodo_label_mes, " (em R$), com ajustes*")
)

grafico7 <- make_grafico_salario(
  df_g7, "faixa_etaria",
  title = paste0("Gráfico 7 – Uberlândia/MG: Salário médio real de admissão\npor faixa etária, ", periodo_label_mes, " (em R$), com ajustes*"),
  x_lim = c(0, 3200)
)

grafico8 <- make_grafico_salario(
  df_g8, "gdi_leg", "genero",
  paste0("Gráfico 8 – Uberlândia/MG: Salário médio real de admissão\npor gênero e grau de instrução do empregado, ", periodo_label_mes, " (em R$), com ajustes*"),
  x_lim = c(0, 10000)
)

# ── Tabelas ───────────────────────────────────────────────────────────────────

# Tabela 1
tabela1 <- df_geral_saldo |>
  select(`Mês/Ano` = data, Admissões = admissoes, Desligamentos = demissoes,
         Saldo = saldomovimentação) |>
  mutate(across(where(is.numeric), fmt_num)) |>
  add_row(`Mês/Ano` = paste0("Saldo acumulado no ano ", ano_ref),
          Admissões    = fmt_num(sum(df_ano_ref$admissoes)),
          Desligamentos = fmt_num(sum(df_ano_ref$demissoes)),
          Saldo        = fmt_num(sum(df_ano_ref$saldomovimentação)))

# Tabela 2
tabela2 <- df_saldo_setor |>
  select(`Mês / Ano` = data, all_of(lvl_setor)) |>
  mutate(across(all_of(lvl_setor), fmt_num))

# Tabela 3
tabela3 <- df_saldo_porte |>
  select(`Mês / Ano` = data, all_of(lvl_porte)) |>
  mutate(across(all_of(lvl_porte), fmt_num))

# Tabela 4 — combina seções com cabeçalhos
header_row <- function(txt) tibble(Variáveis = txt, Admissões = NA_real_,
                                   Desligamentos = NA_real_, Saldo = NA_real_)

tabela4a <- bind_rows(
  header_row("Grupamento de atividade econômica"), df_t4_setores,
  header_row(""), header_row("Porte da empresa"),  df_t4_porte
) |>
  mutate(across(c(Admissões, Desligamentos, Saldo),
                ~ifelse(is.na(.), "", fmt_num(.))))

tabela4b <- bind_rows(
  header_row("Faixa Etária"), df_t4_faixa,
  header_row(""), header_row("Gênero"), df_t4_genero
) |>
  mutate(across(c(Admissões, Desligamentos, Saldo),
                ~ifelse(is.na(.), "", fmt_num(.))))

tabela4c <- bind_rows(
  header_row("Grau de instrução"), df_t4_escol,
  header_row(""),
  df_base_mes |>
    summarise(Admissões = sum(admissoes),
              Desligamentos = -tot_dem_abs,
              Saldo = sum(saldomovimentação)) |>
    mutate(Variáveis = "Total mês de referência") |>
    select(Variáveis, Admissões, Desligamentos, Saldo)
) |>
  mutate(across(c(Admissões, Desligamentos, Saldo),
                ~ifelse(is.na(.), "", fmt_num(.))))

# Tabela 5 — Remuneração geral com variações mensais
tabela5 <- df_remuneracao_geral |>
  mutate(var_adm = (remuneracao_udi_adm / lag(remuneracao_udi_adm) - 1) * 100,
         var_dem = (remuneracao_udi_dem / lag(remuneracao_udi_dem) - 1) * 100) |>
  select(`Ano / Mês` = data,
         `Salário de Admissão` = remuneracao_udi_adm, var_adm,
         `Salário de Demissão` = remuneracao_udi_dem, var_dem) |>
  mutate(`Salário de Admissão` = fmt_num(round(`Salário de Admissão`)),
         `Salário de Demissão` = fmt_num(round(`Salário de Demissão`)),
         var_adm = fmt_pct(var_adm), var_dem = fmt_pct(var_dem)) |>
  (\(d) {
    jan_val <- df_remuneracao_geral |> filter(competênciamov == ano_ref*100+1)
    dez_val <- df_remuneracao_geral |> filter(competênciamov == global_periodo_referencia)
    add_row(d, `Ano / Mês` = paste0("Acum. ", ano_ref, " (%)"), `Salário de Admissão` = "",
            var_adm = fmt_pct((dez_val$remuneracao_udi_adm/jan_val$remuneracao_udi_adm - 1)*100),
            `Salário de Demissão` = "",
            var_dem = fmt_pct((dez_val$remuneracao_udi_dem/jan_val$remuneracao_udi_dem - 1)*100))
  })()

# Tabelas 6 e 7 — já tratadas pela função remuneracao_wide()
tabela6 <- df_tabela6
tabela7 <- df_tabela7
