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
