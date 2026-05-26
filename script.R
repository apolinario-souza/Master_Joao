setwd("C:/Users/lemeu/Master_Joao")

library(ez)
library(emmeans)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

library(writexl)

# Carregar dados
dados <- read_excel("ER_1sub.xlsx")

variavel <- "ER_1sub"

# ============================================================
# REMOÇÃO DE OUTLIERS POR GRUPO DENTRO DE CADA BLOCO
# Inclui Bloco_1 até Bloco_11, sendo Bloco_11 = R_1
# Critério: 1.5 x IQR
# ============================================================

remover_outliers <- function(dados, colunas_blocos, grupo = "Grupo") {
  
  dados %>%
    pivot_longer(
      cols = all_of(colunas_blocos),
      names_to = "Bloco",
      values_to = "Valor"
    ) %>%
    group_by(.data[[grupo]], Bloco) %>%
    mutate(
      Q1 = quantile(Valor, 0.25, na.rm = TRUE),
      Q3 = quantile(Valor, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Limite_inf = Q1 - 1.5 * IQR,
      Limite_sup = Q3 + 1.5 * IQR,
      Outlier = Valor < Limite_inf | Valor > Limite_sup
    ) %>%
    ungroup() %>%
    filter(!Outlier | is.na(Outlier)) %>%
    select(-Q1, -Q3, -IQR, -Limite_inf, -Limite_sup, -Outlier) %>%
    pivot_wider(
      names_from = Bloco,
      values_from = Valor
    )
}

colunas_blocos <- paste0("Bloco_", 1:11)

dados <- remover_outliers(
  dados = dados,
  colunas_blocos = colunas_blocos,
  grupo = "Grupo"
)

# ============================================================
# ANOVA: somente participantes completos nos blocos 1 a 10
# ============================================================

blocos <- dados[, paste0("Bloco_", 1:10)]
participantes_completos <- complete.cases(blocos)

dados_completos <- dados[participantes_completos, ]

dados_long <- data.frame(
  Participante = rep(dados_completos$Participante, each = 10),
  Grupo = rep(dados_completos$Grupo, each = 10),
  Bloco = rep(paste0("Bloco_", 1:10), times = nrow(dados_completos)),
  Valor = as.vector(t(dados_completos[, paste0("Bloco_", 1:10)]))
)

dados_long$Participante <- factor(dados_long$Participante)
dados_long$Grupo <- factor(dados_long$Grupo)
dados_long$Bloco <- factor(
  dados_long$Bloco,
  levels = paste0("Bloco_", 1:10)
)

anova_mista <- ezANOVA(
  data = dados_long,
  dv = Valor,
  wid = Participante,
  within = Bloco,
  between = Grupo,
  detailed = TRUE,
  type = 3
)

print(anova_mista)

# ============================================================
# TESTE T DO R_1: usando todos com valor no Bloco_11 após outliers
# ============================================================

dados_r1 <- dados[!is.na(dados$Bloco_11), ]

t_test_r1 <- t.test(
  Bloco_11 ~ Grupo,
  data = dados_r1,
  var.equal = FALSE
)

print(t_test_r1)

# ============================================================
# ESTATÍSTICAS PARA O GRÁFICO
# ============================================================

estatisticas_blocos <- aggregate(
  Valor ~ Grupo + Bloco,
  data = dados_long,
  FUN = function(x) c(
    media = mean(x),
    erro = sd(x) / sqrt(length(x))
  )
)

estatisticas_blocos <- data.frame(
  Grupo = estatisticas_blocos$Grupo,
  Bloco = estatisticas_blocos$Bloco,
  Media = estatisticas_blocos$Valor[, "media"],
  Erro = estatisticas_blocos$Valor[, "erro"],
  Tipo = "Bloco",
  X_num = as.numeric(estatisticas_blocos$Bloco)
)

estatisticas_r1_plot <- dados_r1 %>%
  group_by(Grupo) %>%
  summarise(
    Media = mean(Bloco_11, na.rm = TRUE),
    Erro = sd(Bloco_11, na.rm = TRUE) / sqrt(n()),
    Bloco = "Bloco_11",
    Tipo = "Retenção",
    X_num = 11,
    .groups = "drop"
  )

estatisticas_completas <- bind_rows(
  estatisticas_blocos,
  estatisticas_r1_plot
)

estatisticas_completas$Grupo <- factor(
  estatisticas_completas$Grupo,
  levels = c("GAG", "GCG"),
  labels = c("GAG ▼", "GCG ▲")
)

# ============================================================
# DADOS INDIVIDUAIS PARA O GRÁFICO
# ============================================================

dados_individuais_aq <- dados_completos %>%
  select(Participante, Grupo, all_of(paste0("Bloco_", 1:10))) %>%
  pivot_longer(
    cols = all_of(paste0("Bloco_", 1:10)),
    names_to = "Bloco",
    values_to = "Valor"
  ) %>%
  mutate(
    Bloco_Num = as.numeric(gsub("Bloco_", "", Bloco))
  )

dados_individuais_r1 <- dados_r1 %>%
  select(Participante, Grupo, Bloco_11) %>%
  mutate(
    Bloco = "Bloco_11",
    Bloco_Num = 11,
    Valor = Bloco_11
  ) %>%
  select(Participante, Grupo, Bloco, Bloco_Num, Valor)

dados_individuais <- bind_rows(
  dados_individuais_aq,
  dados_individuais_r1
)

dados_individuais$Grupo <- factor(
  dados_individuais$Grupo,
  levels = c("GAG", "GCG"),
  labels = c("GAG ▼", "GCG ▲")
)

# ============================================================
# GRÁFICO
# ============================================================

ggplot() +
  geom_jitter(
    data = dados_individuais,
    aes(x = Bloco_Num, y = Valor, color = Grupo),
    alpha = 0.3,
    size = 1.2,
    width = 0.2,
    height = 0
  ) +
  geom_line(
    data = subset(estatisticas_completas, Tipo == "Bloco"),
    aes(x = X_num, y = Media, color = Grupo, group = Grupo),
    size = 1.2
  ) +
  geom_point(
    data = estatisticas_completas,
    aes(x = X_num, y = Media, color = Grupo, group = Grupo),
    size = 3
  ) +
  geom_errorbar(
    data = estatisticas_completas,
    aes(
      x = X_num,
      y = Media,
      ymin = Media - Erro,
      ymax = Media + Erro,
      color = Grupo
    ),
    width = 0.2,
    size = 0.8
  ) +
  geom_vline(
    xintercept = 10.5,
    linetype = "dashed",
    color = "gray50",
    size = 0.8
  ) +
  annotate(
    "text",
    x = 10.5,
    y = max(estatisticas_completas$Media + estatisticas_completas$Erro, na.rm = TRUE),
    label = "↳ 24 horas",
    angle = 90,
    vjust = -0.5,
    size = 3.5,
    color = "gray50"
  ) +
  scale_x_continuous(
    breaks = 1:11,
    labels = c(paste0("B_", 1:10), "R_1")
  ) +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
    legend.position = "bottom",
    panel.grid = element_blank()
  )




write_xlsx(
  dados,
  paste0(variavel, "_sem_outliers.xlsx")
)
# Para salvar o gráfico:
ggsave(paste0(variavel, ".png"), width = 10, height = 6, dpi = 300)