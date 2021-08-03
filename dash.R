# -----------------------------------------------------------------------------------------------------

# Analise Exploratória dos censo 1970 até 2010 município Entre Rios

# -----------------------------------------------------------------------------------------------------

# Carrega Pacotes
library(tidyverse)
library(forcats)

# -----------------------------------------------------------------------------------------------------

# Importa dados censo Entre Rios

# -----------------------------------------------------------------------------------------------------

# Carrega dados
entre_rios <- read_csv("dados/entre_rios/populacao_entre_rios_idade_sexo_area_1970_2010.csv")

# Indices de 'a' ou 'de'
a_ou_de <- str_detect(entre_rios$idade, " a ")

# banco com idade_categorica
idade_cat <- entre_rios %>% 
                filter(a_ou_de) 

# Recuper valores distintos ordem do fator
idades_cat <- idade_cat %>% select(idade) %>%
              distinct() %>%
              mutate(idade = str_replace(idade, "anos", "")) %>% 
              mutate(idade = str_trim(idade)) %>%
              pull()

# Altera idades categorizadas
idade_cat <- idade_cat %>% 
                mutate(idade = str_replace(idade, "anos", "")) %>%
                mutate(idade = str_trim(idade),
                       ano = as.factor(ano),
                       idade = factor(idade))

# Reordena niveis
idade_cat <- idade_cat %>% 
                mutate(idade = fct_relevel(idade, idades_cat)) 

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população total por ano

# -----------------------------------------------------------------------------------------------------

# Declara total por ano
total_ano <- idade_cat %>% 
                group_by(ano) %>% 
                summarize(total = sum(pop, na.rm = T)) %>% 
                mutate(ano = as.numeric(levels(ano)))

# Grafico serie historica de populacao por ano e sexo
ggplot(total_ano, aes(x = ano, y =total))+
  geom_line(size = 1.2)+
  labs(title = "População 1970-2010",
       x = "Ano",
       y = "População")

# -----------------------------------------------------------------------------------------------------

# Grafico serie historica total da populacao por area e por ano

# -----------------------------------------------------------------------------------------------------

# Declara dados do gráfico
ano_area <- idade_cat %>%
              group_by(ano, area) %>% 
              summarize(total = sum(pop, na.rm = T))
             

# Grafico serie historica de populacao por ano e area
ggplot(ano_area, aes(x =ano, y =total, color = area, group = area))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "População 1970-2010 por Área",
       x = "Ano",
       y = "População",
       color = "Área")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população por ano e sexo

# -----------------------------------------------------------------------------------------------------

# Declara df Sexo
ano_sexo <- idade_cat %>% 
        group_by(ano, sexo) %>% 
        summarize(total = sum(pop, na.rm = T))

# Grafico serie historica de populacao por ano e sexo
ggplot(ano_sexo, aes(x =ano, y =total, color = sexo, group = sexo))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "População 1970-2010 por Sexo",
       x = "Ano",
       y = "População",
       color = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população por idade e ano

# -----------------------------------------------------------------------------------------------------

# Agregado por idade ano
idade_ano <- idade_cat %>% 
  group_by(idade, ano) %>% 
  summarise(total = sum(pop, na.rm = T))

# Grafico de linhas com 2 variaveis
ggplot(idade_ano, aes(x = as.factor(idade), y = total, color = ano, group = ano))+
  geom_line(size = 1.2)+
  labs(title = "População 1970 - 2010 por Idade",
       x = "Idade",
       y = "População", 
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 2 variaveis
ggplot(idade_ano, aes(x = ano, y = total, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  labs(title = "População 1970 - 2010 por Ano e Idade",
       x = "Ano",
       y = "População", 
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, sexo e ano na área rural

# -----------------------------------------------------------------------------------------------------

# Seleciona area Rural
rural <- idade_cat %>%  filter(area == "Rural")

# Grafico de linhas com 3 variaveis
ggplot(rural, aes(x = idade, y = pop, color = ano, group = ano))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "População 1970 - 2010 por Idade e Sexo na Área Rural",
       x = "Idade",
       y = "População",
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, sexo e ano na área urbana

# -----------------------------------------------------------------------------------------------------

# Seleciona area Rural
urbana <- idade_cat %>%  filter(area == "Urbana")

# Grafico de linhas com 3 variaveis
ggplot(urbana, aes(x = idade, y = pop, color = ano, group = ano))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "População 1970 - 2010 por Idade e Sexo na Área Urbana",
       x = "Idade",
       y = "População",
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 3 variaveis
ggplot(urbana, aes(x = ano, y = pop, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "População 1970 - 2010 por Idade e Sexo na Área Urbana",
       x = "Ano",
       y = "População",
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, ano, area e genero

# -----------------------------------------------------------------------------------------------------

# Grafico de linhas com 4 variaveis
ggplot(idade_cat, aes(x = idade, y = pop, color = ano, group = ano))+
  geom_line(size = 1.2)+
  facet_grid(sexo~area)+
  labs(title = "População 1970 - 2010 por Idade, Área, Sexo",
       x = "Idade",
       y = "População", 
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 4 variaveis
ggplot(idade_cat, aes(x = ano, y = pop, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~area)+
  labs(title = "População 1970 - 2010 por Idade, Área, Sexo",
       x = "Ano",
       y = "População", 
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide populacional 1970

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970
pop_1970 <- idade_cat %>% 
              filter(ano == 1970) %>% 
              group_by(idade, sexo) %>% 
              summarise(pop = sum(pop, na.rm = T)) %>% 
              ungroup() %>% 
              mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_1970, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_1970, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_1970, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  labs(title = "População Entre Rios 1970 por Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide populacional 1980

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970
pop_1980 <- idade_cat %>% 
  filter(ano == 1980) %>% 
  group_by(idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_1980, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_1980, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_1980, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  labs(title = "População Entre Rios 1980 por Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")

# -----------------------------------------------------------------------------------------------------


# Grafico de piramide populacional 1990

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970
pop_1990 <- idade_cat %>% 
  filter(ano == 1990) %>% 
  group_by(idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_1990, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_1990, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_1990, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  labs(title = "População Entre Rios 1990 por Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide populacional 2000

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970
pop_2000 <- idade_cat %>% 
  filter(ano == 2000) %>% 
  group_by(idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_2000, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_2000, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_2000, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  labs(title = "População Entre Rios 2000 por Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide populacional 2010

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970
pop_2010 <- idade_cat %>% 
  filter(ano == 2010) %>% 
  group_by(idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_2010, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_2010, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_2010, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  labs(title = "População Entre Rios 2010 por Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")

# -----------------------------------------------------------------------------------------------------


# Grafico de piramide todas as populacoes

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970 - 2010
pop_total <- idade_cat %>% 
  group_by(ano, idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_total, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_total, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_total, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "População Entre Rios por Idade Ano e Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")+
  facet_wrap(ano~.)

# Declara pop 1980 - 2010
pop_total <- idade_cat %>% 
  filter(!ano == 1970) %>% 
  group_by(ano, idade, sexo) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_total, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_total, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_total, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-2500, 0, 500), seq(0, 2500, 500)),
                     labels = paste0(as.character(c(seq(2500, 0, -500), seq(0, 2500, 500))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "População Entre Rios por Idade Ano e Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")+
  facet_wrap(ano~.)













# -----------------------------------------------------------------------------------------------------

# Nascimentos 2003 - 2019

# -----------------------------------------------------------------------------------------------------

# Leitura dos dados de nascimentos
nascimentos <- read_csv("dados/entre_rios/nasci_entre_rios_sexo_idade_2003_2019.csv")

# -----------------------------------------------------------------------------------------------------

# Limpeza

# -----------------------------------------------------------------------------------------------------

# banco com idade_categorica
nascimentos_cat <- nascimentos %>% 
                      mutate(idade = str_replace_all(idade, "Menos de 15 anos", "0 a 15 anos"))

# Indice com a palavras anos
anos_nasc <- str_detect(nascimentos_cat$idade, "anos")

# Indices de 'a'
a_nasc <- str_detect(nascimentos_cat$idade, " a ")

# Filtra categorias relevantes
nascimentos_cat <- nascimentos_cat %>% 
                    filter(a_nasc & anos_nasc) 

# Recuper valores distintos ordem do fator
nascimento_cat <- nascimentos_cat %>% select(idade) %>%
                    distinct() %>%
                    mutate(idade = str_replace(idade, "anos", "")) %>% 
                    mutate(idade = str_trim(idade)) %>%
                    pull()

# Altera idades categorizadas
nascimentos_cat <- nascimentos_cat %>% 
                mutate(idade = str_replace(idade, "anos", "")) %>%
                mutate(idade = str_trim(idade),
                       ano = as.factor(ano),
                       idade = factor(idade))

# Reordena niveis
nascimentos_cat <- nascimentos_cat %>% 
                    mutate(idade = fct_relevel(idade, nascimento_cat))

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica de nascimentos total por ano

# -----------------------------------------------------------------------------------------------------

# Declara total por ano
nascimentos_ano <- nascimentos_cat %>% 
  group_by(ano) %>% 
  summarize(total = sum(nascimento, na.rm = T)) 

# Grafico serie historica de populacao por ano e sexo
ggplot(nascimentos_ano, aes(x = as.numeric(levels(ano)), y = total))+
  geom_line(size = 1.2)+
  labs(title = "Nascimentos 2003 - 2019",
       x = "Ano",
       y = "Nascimentos")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica de nascimentos por ano e sexo

# -----------------------------------------------------------------------------------------------------

# Declara total por ano
nascimentos_sexo <- nascimentos_cat %>% 
  group_by(ano, sexo) %>% 
  summarize(total = sum(nascimento, na.rm = T)) 

# Grafico serie historica de populacao por ano e sexo
ggplot(nascimentos_sexo, aes(x = ano, y =total, color = sexo, group = sexo))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "Nascimentos 2003-2019 por Sexo",
       x = "Ano",
       y = "Nascimentos",
       color = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da óbitos por idade e ano

# -----------------------------------------------------------------------------------------------------

# Gera funcao Categorias de idades
calc_idades_nascimentos <- function(data){
  aux <- data %>% 
    as.character() %>%
    str_sub(-2,-1) %>% 
    str_trim() %>% 
    as.numeric()
  return(aux)
}

# Agregado por idade ano
nascimentos_idade_ano <- nascimentos_cat %>% 
  group_by(ano, idade) %>% 
  summarise(total = sum(nascimento, na.rm = T)) %>% 
  ungroup()

# Grafico de linhas com 2 variaveis
ggplot(nascimentos_idade_ano, aes(x = ano, y = total, color = idade, group = idade))+
  geom_line(size = 1.2)+
  labs(title = "Nascimentos 2003 - 2019 por Ano e Idade da Mãe",
       x = "Ano",
       y = "Nascimentos", 
       color = "Idade da Mãe")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide todas os obitos

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970 - 2010
nascimentos_total <- nascimentos_cat %>% 
                      mutate(total = ifelse(sexo == "Homem", -1*nascimento, nascimento))

# Desenha grafico
ggplot(nascimentos_total, aes(x = idade, y = total, fill = sexo)) + 
  geom_bar(data = subset(nascimentos_total, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(nascimentos_total, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-100, 0, 50), seq(0, 100, 50)),
                     labels = paste0(as.character(c(seq(100, 0, -50), seq(0, 100, 50))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "Nascimentos Entre Rios por Idade da Mãe Ano e Sexo",
       x = "Idade da Mãe",
       y = "Nascimentos", 
       fill = "Sexo")+
  facet_wrap(ano~.)

# -----------------------------------------------------------------------------------------------------

















# -----------------------------------------------------------------------------------------------------

# Obitos 2003 - 2019

# -----------------------------------------------------------------------------------------------------

# Leitura dos dados de obitos
obitos <- read_csv("dados/entre_rios/obitos_entre_rios_sexo_idade_2003_2019.csv")

# -----------------------------------------------------------------------------------------------------

# Limpeza

# -----------------------------------------------------------------------------------------------------

# banco com idade_categorica
obitos_cat <- obitos %>% 
  mutate(idade = str_replace_all(idade, "Menos de 1 ano", "0 a 1 anos"))


# Indice com a palavras anos
anos <- str_detect(obitos_cat$idade, "anos")

# Indices de 'a'
a <- str_detect(obitos_cat$idade, " a ")

# Filtra categorias relevantes
obitos_cat <- obitos_cat %>% 
                filter(a & anos) %>% 
                filter(idade != "1 a 14 anos") %>% 
                filter(idade != "15 a 84 anos")
  
# -----------------------------------------------------------------------------------------------------

# Recuper valores distintos ordem do fator
obito_cat <- obitos_cat %>% select(idade) %>%
                distinct() %>%
                mutate(idade = str_replace(idade, "anos", "")) %>% 
                mutate(idade = str_trim(idade)) %>%
                pull()

# Altera idades categorizadas
obitos_cat <- obitos_cat %>% 
                mutate(idade = str_replace(idade, "anos", "")) %>%
                mutate(idade = str_trim(idade),
                       ano = as.factor(ano),
                       idade = factor(idade))

# Reordena niveis
obitos_cat <- obitos_cat %>% 
                mutate(idade = fct_relevel(idade, obito_cat)) 

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da óbitos total por ano

# -----------------------------------------------------------------------------------------------------

# Declara total por ano
obito_ano <- obitos_cat %>% 
              group_by(ano) %>% 
              summarize(total = sum(total, na.rm = T)) 

# Grafico serie historica de populacao por ano e sexo
ggplot(obito_ano, aes(x = as.numeric(levels(ano)), y = total))+
  geom_line(size = 1.2)+
  labs(title = "Óbitos 2003 - 2019",
       x = "Ano",
       y = "Óbitos")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica de óbitos por ano e sexo

# -----------------------------------------------------------------------------------------------------

# Declara df Sexo
obitos_sexo <- obitos_cat %>% 
  group_by(ano, sexo) %>% 
  summarize(total = sum(total, na.rm = T))

# Grafico serie historica de populacao por ano e sexo
ggplot(obitos_sexo, aes(x = ano, y =total, color = sexo, group = sexo))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "Óbitos 2003-2019 por Sexo",
       x = "Ano",
       y = "Óbitos",
       color = "Sexo")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da óbitos por idade e ano

# -----------------------------------------------------------------------------------------------------

# Gera funcao Categorias de idades
calc_idades_obito <- function(data){
  aux <- data %>% 
    as.character() %>%
    str_sub(-2,-1) %>% 
    str_trim() %>% 
    as.numeric()
    return(aux)
}

# Agregado por idade ano
obitos_idade_ano <- obitos_cat %>% 
                      group_by(idade, ano) %>% 
                      summarise(total = sum(total, na.rm = T)) %>% 
                      mutate(cat_idade = calc_idades_obito(idade)) %>% 
                      mutate(cat_novo = case_when(cat_idade < 20 ~ "0 - 19",
                                                  (cat_idade >= 20 & cat_idade < 40) ~ "20 - 39",
                                                  (cat_idade >= 40 & cat_idade < 60) ~ "40 - 59",
                                                  (cat_idade >= 60 & cat_idade < 80) ~ "60 - 79",
                                                  (cat_idade >= 80)  ~ "80 - 100",
                                                )) %>% 
                      ungroup() %>% 
                      select(ano, cat_novo, total) %>% 
                      group_by(ano, cat_novo) %>% 
                      summarise(total = sum(total, na.rm = T)) %>% 
                      ungroup()

# Grafico de linhas com 2 variaveis
ggplot(obitos_idade_ano, aes(x = ano, y = total, color = as.factor(cat_novo), group = as.factor(cat_novo)))+
  geom_line(size = 1.2)+
  labs(title = "Óbitos 2003 - 2019 por Ano e Grupos de Idade",
       x = "Ano",
       y = "Óbitos", 
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico de piramide todas os obitos

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970 - 2010
obitos_total <- obitos_cat %>% 
  mutate(total = ifelse(sexo == "Homem", -1*total, total))

# Desenha grafico
ggplot(obitos_total, aes(x = idade, y = total, fill = sexo)) + 
  geom_bar(data = subset(obitos_total, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(obitos_total, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-10, 0, 5), seq(0, 10, 5)),
                     labels = paste0(as.character(c(seq(10, 0, -5), seq(0, 10, 5))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "Óbitos Entre Rios por Idade Ano e Sexo",
       x = "Idade",
       y = "Óbitos", 
       fill = "Sexo")+
  facet_wrap(ano~.)

# ------------------------------------------------------------------------------------------------------

# Grafico piramid obitos categorias menores

# ------------------------------------------------------------------------------------------------------

# Agregado por idade ano
obitos_idade_ano_sexo <- obitos_cat %>% 
  group_by(idade, ano, sexo) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  mutate(cat_idade = calc_idades_obito(idade)) %>% 
  mutate(cat_novo = case_when(cat_idade < 20 ~ "0 - 19",
                              (cat_idade >= 20 & cat_idade < 40) ~ "20 - 39",
                              (cat_idade >= 40 & cat_idade < 60) ~ "40 - 59",
                              (cat_idade >= 60 & cat_idade < 80) ~ "60 - 79",
                              (cat_idade >= 80)  ~ "80 - 100",
  )) %>% 
  ungroup() %>% 
  select(ano, cat_novo, sexo, total) %>% 
  group_by(ano, cat_novo, sexo) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  ungroup()

# Declara pop 1970 - 2010
obitos_total_sexo <- obitos_idade_ano_sexo %>% 
  mutate(total = ifelse(sexo == "Homem", -1*total, total))

# Desenha grafico
ggplot(obitos_total_sexo, aes(x = cat_novo, y = total, fill = sexo)) + 
  geom_bar(data = subset(obitos_total_sexo, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(obitos_total_sexo, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-50, 0, 25), seq(0, 50, 25)),
                     labels = paste0(as.character(c(seq(50, 0, -25), seq(0, 50, 25))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "Óbitos Entre Rios por Idade Ano e Sexo",
       x = "Idade",
       y = "Óbitos", 
       fill = "Sexo")+
  facet_wrap(ano~.)












# ------------------------------------------------------------------------------------------------------

# Carrega dados de Imigração

# ------------------------------------------------------------------------------------------------------

# Leitura dos dados de obitos
imigracao <- read_csv("dados/entre_rios/imigracao_entre_rios_area_idade_sexo_2001_2010.csv")

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população total por ano

# -----------------------------------------------------------------------------------------------------

# Declara total por ano
total_ano <- imigracao %>% 
  group_by(ano) %>% 
  summarize(total = sum(total, na.rm = T))

# Grafico serie historica de populacao por ano e sexo
ggplot(total_ano, aes(x = ano, y =total))+
  geom_line(size = 1.2)+
  labs(title = "Imigração Entre Rios 2001 - 2010",
       x = "Ano",
       y = "População")+
  scale_x_continuous(breaks = total_ano$ano, labels = total_ano$ano)

# -----------------------------------------------------------------------------------------------------

# Grafico serie historica total da populacao por area e por ano

# -----------------------------------------------------------------------------------------------------

# Declara dados do gráfico
ano_area <- imigracao %>%
  group_by(ano, area) %>% 
  summarize(total = sum(total, na.rm = T))


# Grafico serie historica de populacao por ano e area
ggplot(ano_area, aes(x =ano, y =total, color = area, group = area))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "Imigração por Área 2001 - 2010",
       x = "Ano",
       y = "População",
       color = "Área")+
  scale_x_continuous(breaks = ano_area$ano, labels = ano_area$ano)

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população por ano e sexo

# -----------------------------------------------------------------------------------------------------

# Declara df Sexo
ano_sexo <- imigracao %>% 
  group_by(ano, sexo) %>% 
  summarize(total = sum(total, na.rm = T))

# Grafico serie historica de populacao por ano e sexo
ggplot(ano_sexo, aes(x =ano, y =total, color = sexo, group = sexo))+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  labs(title = "Imigração por Sexo 2001 - 2010",
       x = "Ano",
       y = "População",
       color = "Sexo")+
  scale_x_continuous(breaks = ano_sexo$ano, labels = ano_sexo$ano)

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica da população por idade e ano

# -----------------------------------------------------------------------------------------------------

# Agregado por idade ano
idade_ano <- imigracao %>% 
  group_by(idade, ano) %>% 
  summarise(total = sum(total, na.rm = T))

# Grafico de linhas com 2 variaveis
ggplot(idade_ano, aes(x = as.factor(idade), y = total, color = as.factor(ano), group = as.factor(ano)))+
  geom_line(size = 1.2)+
  labs(title = "Imigração por Idade 2001 - 2010",
       x = "Idade",
       y = "População", 
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 2 variaveis
ggplot(idade_ano, aes(x = ano, y = total, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  labs(title = "Imigração por Ano e Idade 2001 - 2010",
       x = "Ano",
       y = "População", 
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_continuous(breaks = idade_ano$ano, labels = idade_ano$ano)


# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, sexo e ano na área rural

# -----------------------------------------------------------------------------------------------------

# Seleciona area Rural
rural <- imigracao %>%  filter(area == "Rural")

# Grafico de linhas com 3 variaveis
ggplot(rural, aes(x = idade, y = total, color = as.factor(ano), group = as.factor(ano)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "Imigração por Idade 2001 - 2010 na Área Rural",
       x = "Idade",
       y = "População",
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, sexo e ano na área urbana

# -----------------------------------------------------------------------------------------------------

# Seleciona area Rural
urbana <- imigracao %>%  filter(area == "Urbana")

# Grafico de linhas com 3 variaveis
ggplot(urbana, aes(x = idade, y = total, color = as.factor(ano), group = as.factor(ano)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "Imigração por Idade 2001 - 2010 na Área Urbana",
       x = "Idade",
       y = "População",
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 3 variaveis
ggplot(urbana, aes(x = ano, y = total, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~.)+
  labs(title = "Imigração por Idade 2001 - 2010 na Área Urbana",
       x = "Ano",
       y = "População",
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------

# Grafico da serie historica por idade, ano, area e genero

# -----------------------------------------------------------------------------------------------------

# Grafico de linhas com 4 variaveis
ggplot(imigracao, aes(x = idade, y = total, color = as.factor(ano), group = as.factor(ano)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~area)+
  labs(title = "Imigração 2001 - 2010 por Idade, Área, Sexo",
       x = "Idade",
       y = "População", 
       color = "Ano")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Grafico de linhas com 4 variaveis
ggplot(imigracao, aes(x = ano, y = total, color = as.factor(idade), group = as.factor(idade)))+
  geom_line(size = 1.2)+
  facet_grid(sexo~area)+
  labs(title = "Imigração 2001 - 2010 por Idade, Área, Sexo",
       x = "Ano",
       y = "População", 
       color = "Idade")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# -----------------------------------------------------------------------------------------------------


# Grafico de piramide todas as populacoes

# -----------------------------------------------------------------------------------------------------

# Declara pop 1970 - 2010
pop_total <- imigracao %>% 
  group_by(ano, idade, sexo) %>% 
  summarise(pop = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = ifelse(sexo == "Homem", -1*pop, pop))

# Desenha grafico
ggplot(pop_total, aes(x = idade, y = pop, fill = sexo)) + 
  geom_bar(data = subset(pop_total, sexo == "Mulher"), stat = "identity") + 
  geom_bar(data = subset(pop_total, sexo == "Homem"), stat = "identity") + 
  scale_y_continuous(breaks = c(seq(-40, 0, 20), seq(0, 40, 20)),
                     labels = paste0(as.character(c(seq(40, 0, -20), seq(0, 40, 20))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw()+
  labs(title = "Imigração 2001 - 2010 por Idade Ano, e Sexo",
       x = "Idade",
       y = "População", 
       fill = "Sexo")+
  facet_wrap(ano~.)

# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------










