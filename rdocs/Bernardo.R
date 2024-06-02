banco<-read.csv("D:\\banco_final.csv")
library(hms)
library(lubridate)
library(tidyverse)
#Análise 1

décadas<- banco%>%
  mutate(Ano=(year(date_aired)))
decadas<- décadas%>%
  select(Ano,format)
decadas<- decadas%>%
  mutate(frequencia=n())

decadas<- decadas%>%
  mutate(decada = case_when(
    between(Ano, 1960, 1969) ~ "Década de 60",
    between(Ano, 1970, 1979) ~ "Década de 70",
    between(Ano, 1980, 1989) ~ "Década de 80",
    between(Ano, 1990, 1999) ~ "Década de 90",
    between(Ano, 2000, 2009) ~ "Década de 2000",
    between(Ano, 2010, 2019) ~ "Década de 2010",
    TRUE ~ "Década de 2020"
  ))

decadas<- decadas%>%
  rename(Formato=format)
decadas <- decadas %>%
  mutate(Formato = case_when(
    Formato %>% str_detect("CrossOver") ~ "CrossOver",
    Formato %>% str_detect("Movie") ~ "Filme",
    Formato %>% str_detect("Serie") ~ "Série"
  ))


decadas%>%
  group_by(decada,Formato)%>%
  summarise(freq=n())

trans_decadas <- decadas %>%
  mutate(decada = case_when(
    decada %>% str_detect("Década de 60") ~ "60",
    decada %>% str_detect("Década de 70") ~ "70",
    decada %>% str_detect("Década de 80") ~ "80",
    decada %>% str_detect("Década de 90") ~ "90",
    decada %>% str_detect("Década de 2000") ~ "2000",
    decada %>% str_detect("Década de 2010") ~ "2010",
    decada %>% str_detect("Década de 2020") ~ "2020"
  )) %>%
  group_by(decada, Formato) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(trans_decadas$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_decadas$freq, " (", porcentagens, ")"))

trans_decadas$decada<-factor(trans_decadas$decada,levels = c("60","70","80","90","2000","2010","2020"))
levels(trans_decadas$decada)

ggplot(trans_decadas) +
  aes(x = decada, y = freq, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Frequência") +
  theme_estat()
ggsave("linhadecadas.pdf", width = 158, height = 93, units = "mm")

#Análise 2
notas<-banco%>%
  select(series_name,season,imdb)

notas<-notas[notas$season == "1" |  notas$season== "2"|notas$season== "3"|notas$season =="4", ]

ggplot(notas) +
  aes(x = season,imdb , y =imdb ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporadas", y = "Notas imdb") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

print_quadro_resumo <- function(data, var_name, title="Medidas resumo
da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5)
                                ,2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    32
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
  }
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
33
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}


notas%>%
  group_by(season)%>%
  print_quadro_resumo(var_name = "imdb")

#Análise 3

terreno<-count(banco,setting_terrain)
terreno <- terreno %>%
  arrange(desc(n))%>%
  head(3)

armadilha<-banco%>%
  select(trap_work_first,setting_terrain)
armadilha<-armadilha%>%
  filter(trap_work_first!="")
armadilha <- armadilha %>%
  mutate(trap_work_first = case_when(
    trap_work_first %>% str_detect("True") ~ "Funcionou",
    trap_work_first %>% str_detect("False") ~ "Não funcionou"
  )) %>%
  group_by(setting_terrain,trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
  )
armadilha<- armadilha %>%
  filter(setting_terrain %in% terreno$setting_terrain)
armadilha<- armadilha%>%
  rename(Armadilha=trap_work_first)
armadilha <- armadilha %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Forest") ~ "Floresta",
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect("Urban") ~ "Urbano"
  ))

porcentagens <- str_c(armadilha$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(armadilha$freq, " (", porcentagens, ")"))

ggplot(armadilha) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = Armadilha, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Tipos de Terreno", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freqterreno.pdf", width = 158, height = 93, units = "mm")

quiquadrado<-banco%>%
  select(trap_work_first,setting_terrain)%>%
  filter(trap_work_first!="")%>%
  filter(setting_terrain %in% terreno$setting_terrain)
tabelaq<-table(quiquadrado$trap_work_first, quiquadrado$setting_terrain)
resultado_quiquadrado <- chisq.test(tabelaq)
print(resultado_quiquadrado)
quiquadrado1<- resultado_quiquadrado$statistic
n <- sum(tabelaq)
r <- nrow(tabelaq)
c <- ncol(tabelaq)

coef_contingencia <- sqrt(quiquadrado1 / (n * min(r - 1, c - 1)))
print(coef_contingencia)

df <- (r - 1) * (c - 1)
coef_contingencia_ajustado <- coef_contingencia / sqrt(1 - coef_contingencia^2 / df)
print(coef_contingencia_ajustado)

#Analise 4

engajamento<-banco%>%
  select(imdb,engagement)

ggplot(engajamento) +
  aes(x = imdb, y = engagement) +
  geom_point(alpha=0.5,colour = "#A11D21", size = 3) +
  labs(
    x = "Notas Imdb",
    y = "Engajamento"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

cor(engajamento$imdb,engajamento$engagement,method = "pearson")

engajamento %>%
  print_quadro_resumo(var_name = "engagement")

engajamento %>%
  print_quadro_resumo(var_name = "imdb")
var(engajamento$engagement)

#Analise 5

capturados<-banco%>%
  select(engagement,caught_fred,caught_daphnie,caught_velma,caught_shaggy,caught_scooby,caught_other,caught_not)

c <- capturados %>%
  mutate(row = row_number()) %>%
  pivot_longer(
    cols = starts_with("caught"),
    names_to = "character",
    values_to = "caught"
  ) %>%
  filter(caught == "True") %>%
  mutate(character = case_when(
    character == "caught_fred" ~ "Fred",
    character == "caught_daphnie" ~ "Daphnie",
    character == "caught_velma" ~ "Velma",
    character == "caught_shaggy" ~ "Salsicha",
    character == "caught_scooby" ~ "Scooby",
    character == "caught_other" ~ "Outros",
    character == "caught_not" ~ "Nenhum"
    ))

scooby_doo_data_summarized <- c %>%
  group_by(row) %>%
  summarise(caught_by = paste(character, collapse = ", "))

c <- c %>%
  mutate(row = row_number()) %>%
  left_join(scooby_doo_data_summarized, by = "row") %>%
  select(-row)



ggplot(c) +
  aes(x = reorder(character, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagens", y = "Engajamento") +
  theme_estat()
ggsave("capturados.pdf", width = 158, height = 93, units = "mm")

c %>%
  group_by(character)%>%
  print_quadro_resumo(var_name = "engagement")
