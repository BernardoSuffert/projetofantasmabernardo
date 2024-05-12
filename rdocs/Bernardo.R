banco<-read.csv("D:\\banco_final.csv")
library(hms)
library(lubridate)
library(tidyverse)
décadas<- banco%>%
  mutate(Ano=(year(date_aired)))
decadas<- décadas%>%
  select(Ano,format)
decadas<- decadas%>%
  mutate(frequencia=n())

decadas<- decadas%>%
  mutate(decada = case_when(
    between(Ano, 1961, 1970) ~ "Década de 60",
    between(Ano, 1971, 1980) ~ "Década de 70",
    between(Ano, 1981, 1990) ~ "Década de 80",
    between(Ano, 1991, 2000) ~ "Década de 90",
    between(Ano, 2001, 2010) ~ "Década de 2000",
    between(Ano, 2011, 2020) ~ "Década de 2010",
    TRUE ~ "Década de 2020"
  ))



decadas%>%
  group_by(decada,format)%>%
  summarise(freq=n())

trans_decadas <- decadas %>%
  mutate(decada = case_when(
    decada %>% str_detect("Década de 60") ~ "de 60",
    decada %>% str_detect("Década de 70") ~ "de 70",
    decada %>% str_detect("Década de 80") ~ "de 80",
    decada %>% str_detect("Década de 90") ~ " de 90",
    decada %>% str_detect("Década de 2000") ~ " de 2000",
    decada %>% str_detect("Década de 2010") ~ " de 2010",
    decada %>% str_detect("Década de 2020") ~ " de 2020"
  )) %>%
  group_by(decada, format) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(trans_decadas$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_decadas$freq, " (", porcentagens, ")"))

ggplot(trans_decadas) +
  aes(
    x = fct_reorder(decada, freq, .desc = T), y = freq,
    fill = format, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Décadas", y = "Frequência") +
  estat_theme()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

ggplot(trans_decadas) +
  aes(
    x = fct_reorder(decada, freq_relativa, .desc = T), y = freq_relativa,
    fill = format
  ) +
  geom_col(position = "fill") +
  labs(x = "Décadas", y = "Frequência Relativa") +
  theme_estat()
ggsave("colunas-bi-freqrelativa.pdf", width = 158, height = 93, units = "mm")

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

print_quadro_resumo <- function(data, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(imdb),2),
              `Desvio Padrão` = round(sd(imdb),2),
              `Variância` = round(var(imdb),2),
              `Mínimo` = round(min(imdb),2),
              `1º Quartil` = round(quantile(imdb, probs = .25),2),
              `Mediana` = round(quantile(imdb, probs = .5),2),
              `3º Quartil` = round(quantile(imdb, probs = .75),2),
              `Máximo` = round(max(imdb),2)) %>%
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
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
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
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

notas%>%
  group_by(season)%>%
  print_quadro_resumo()

#Análise 3

terreno<-count(banco,setting_terrain)
terreno <- terreno %>%
  arrange(desc(n))

ggplot(terreno) +
  aes(x = fct_reorder(setting_terrain, n, .desc=T), y = n) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) + 
  labs(x = "Terreno", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

terreno<-terreno%>%
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
  rename(Armadilha_funcionou_de_primeira=trap_work_first)

ggplot(armadilha) +
  aes(
    x = fct_reorder(setting_terrain, freq_relativa, .desc = F), 
    y = freq_relativa * 100,  
    fill = Armadilha_funcionou_de_primeira
  ) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = paste0(freq_relativa, "%")),
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3, 
    fontface = "bold"
  ) +
  labs(x = "Tipos de Terreno", y = "Porcentagem (%)") +
  scale_y_continuous(labels = scales::percent_format()) +       
  theme_estat()
ggsave("barras-bi-porcentagem.pdf", width = 158, height = 93, units = "mm")

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

