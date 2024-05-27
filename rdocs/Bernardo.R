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
  aes(x = decada, y = freq, group = format, colour = format) +
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
  rename(Armadilha_funcionou_de_primeira=trap_work_first)
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
    fill = Armadilha_funcionou_de_primeira, label = legendas
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

#Analise 4

engajamento<-banco%>%
  select(imdb,engagement)

ggplot(engajamento) +
  aes(x = imdb, y = engagement) +
  geom_point(colour = "#A11D21", size = 3) +
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
  select(engagement,caught_fred,caught_daphnie,caught_velma,caught_shaggy,caught_scooby)

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
    character == "caught_shaggy" ~ "Shaggy",
    character == "caught_scooby" ~ "Scooby"
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