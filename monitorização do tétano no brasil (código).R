# Downloading packages ====

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("read.dbc")
install.packages("foreign")
install.packages("lubridate")
install.packages("ggtext")
install.packages("viridis")
install.packages("ggpubr")
install.packages("compareGroups")
install.packages("forestplot")
install.packages("gtsummary")
install.packages("pacman")
remotes::install_github("danicat/read.dbc", force = TRUE)
install.packages("base")

# Load packages=====
library(pacman)
pacman::p_load(tidyverse,
               dplyr,
               base,
               ggplot2,
               lubridate,
               foreign,
               ggtext,
               viridis,
               ggpubr,
               compareGroups,
               geobr,
               forestplot,
               gtsummary,
               read.dbc)

# Downloading dbcs====
# Define URLs and file names
urls <- c(
  "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"
)

# Download and read DBC files in a loop
for (url in urls) {
  download.file(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/", ifelse(as.numeric(url) <= 20, "finais", "PRELIM"), "/TETABR", url, ".dbc"), paste0("TETABR", url, ".dbc"), mode = "wb")
}

# Create a list of data frames and assign to individual variables
tetano_list <- lapply(paste0("TETABR", urls, ".dbc"), read.dbc)
for (i in 1:length(urls)) assign(paste0("tetano", urls[i]), tetano_list[[i]])



# Combine dataframes ====
# This section combines multiple dataframes containing data on tetanus cases. Each dataframe represents data from a specific year. 
# The dataframes are combined using the rbind() function, which stacks them vertically.
# The resulting dataframes are named tettot1 and tettot2.
tettot1 <- rbind(tetano07,tetano08,tetano09,tetano10,tetano11,tetano12,
                 tetano13,tetano14,tetano15,tetano16,tetano17)

tettot3 <- rbind(tetano18, tetano19, tetano20, tetano21, tetano22)

# Selecting Columns:
# After combining the dataframes, the code selects only the first 57 columns from tettot1 using the index [1:57].
# This step presumably selects specific columns of interest for further analysis.
# The resulting subset is stored back into tettot1.

tettot1 <- tettot1[, c(1:57)]

# Combining Dataframes (Again):
# The next step combines tettot1 and tettot2 using rbind() to create a single dataframe named tettot.
# This step merges the data from different years into one consolidated dataframe for analysis.

tettot2 <- rbind(tettot1, tettot3)

# Criar gráfico Odds Ratio ====

#Alterar labels
# Modifying labels and preparing the data for the plot
john <- tettot2 %>%
  filter(CLASSI_FIN == 1) |>
  mutate(Desfecho = case_when(
    EVOLUCAO == 1 ~ "Cura",
    EVOLUCAO == 2 ~ "Óbito",
    EVOLUCAO == 3 ~ "Óbito",
    EVOLUCAO == 9 ~ NA_character_,
    TRUE ~ NA_character_),
    Vacinacao = case_when(
      NU_DOSE == 1 ~ "1",
      NU_DOSE == 2 ~ "1",
      NU_DOSE == 3 ~ "1",
      NU_DOSE == 4 ~ "1",
      NU_DOSE == 5 ~ "1",
      NU_DOSE == 6 ~ "0",
      NU_DOSE == 9 ~ NA_character_,
      TRUE ~ NA_character_)) |>
  mutate(across(starts_with("CS_"),
                ~ replace(., .==9, NA_character_))) |>
  mutate(across(starts_with("CS_"), as.character)) |>
  mutate(across(starts_with("CS_"), ~ replace(., .=="1", "1"))) |>
  mutate(across(starts_with("CS_"), ~ replace(., .=="2", "0")))

colnames(john)

#Selecionando as variáveis qque vão entrar no gráfico em um df
# Selecting variables for the plot in a dataframe
tabodds <- createTable(compareGroups(Desfecho ~ CS_TRISMO+CS_RISO+CS_OPISTOT+
                                       CS_NUCA+CS_ABDOMIN+CS_MEMBROS+CS_CRISES+CS_SIN_OUT,
                                     data = john, ref = "1",na.action = NULL),show.ratio = TRUE,hide.no=0, show.p.overall=FALSE)
#criando df
# Creating a dataframe
lista <- as.data.frame(tabodds$descr)


lista$OR

#criar o gráfico propriamente dito
# Creating the actual plot
lista$ORm <- gsub(" \\[.*", "",lista$OR)
lista$ORmin <- gsub("\\;.*", "",lista$OR)
lista$ORmin <- gsub(".*\\[", "",lista$ORmin)
lista$ORmax <- gsub(".*\\;", "",lista$ORmax)
lista$ORmax <- gsub("]", "",lista$ORmax)
# lista$Variable <- rownames(lista)
lista$Variable <- c("Trismo", "Riso sardônico", "Opistótono", "Rigidez nucal", "Rigidez abdominal",
                    "Rigidez nos membros","Crise convulsiva", "Outros sintomas")

lista <- lista %>% mutate_at(c('ORm' ,'ORmin', 'ORmax'), as.numeric)


Variable <- c(c("Desfecho por Tétano","", "Variável"), lista$Variable, c(NA,"Sumário"))
Positive <- c(c("","Cura", "N = 2265"), lista$Cura,c(NA,NA))
Negative <- c(c("","Óbito", "N = 1496"), lista$Óbito,c(NA,NA))
OR <- c(c("","","OR"),lista$OR,c(NA,NA))
p.value <- c(c("","","p-valor"), lista$p.ratio, c(NA,""))
summary <- c(c(TRUE,TRUE,TRUE),rep(NA,9),c(TRUE)) ### aqui estava rep(NA,10)
mean <- c(c(NA,NA,NA),lista$ORm,c(NA),c(mean(lista$ORm)))
lower <- c(c(NA,NA,NA),lista$ORmin,c(NA),c(mean(lista$ORmin)))
upper <- c(c(NA,NA,NA),lista$ORmax,c(NA),c(mean(lista$ORmax)))

base <- data.frame(Variable, Positive, Negative, OR, p.value, summary, mean, lower, upper)

base2 <- base |>
  mutate(p.value = case_when(
    p.value == "0.000" ~ "<0.001",
    TRUE ~ p.value))

base3 <- base2[c(1:3,11,5,8,10,7,9,6,4),]
rownames(base3) <- base3[,1]

rownames(base3) <- c("Desfecho por Tétano","Lab","Variável",           
                     "Outros sintomas","Riso sardônico","Rigidez abdominal",  
                     "Crise convulsiva","Rigidez nucal","Rigidez nos membros",
                     "Opistótono","Trismo")

base3 <- base3[c("Desfecho por Tétano","Lab","Variável",
                 "Crise convulsiva",
                 "Trismo","Opistótono","Rigidez nos membros","Riso sardônico",
                 "Rigidez abdominal","Outros sintomas"),]




base3 |>
  forestplot(labeltext = c(Variable, Positive, Negative, OR, `p.value`),
             is.summary = summary,
             clip = c(0.05, 14),
             boxsize = 0.3,
             hrzl_lines = list("2" = gpar(lty = 1),
                               "4" = gpar(lty = 1),
                               "11" = gpar(lty = 1)),
             xlog = TRUE,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.8)),
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue")) |>
  fp_decorate_graph(graph.pos = 4)


# Criar pirâmide etária para casos de tétano ====

# Filtrar apenas ano e mês das datas de notificação dos dataframes, criando uma nova coluna
# Filtering only year and month from the notification dates of the dataframes, creating a new column
tettot2$ano <- as.numeric(format(tettot2$DT_NOTIFIC, "%Y"))
tettot2$mês <- as.numeric(format(tettot2$DT_NOTIFIC, "%m"))

# Criar variável idade
# Create age variable
tettot2$idade <- tettot2$ano - as.numeric(as.character(tettot2$ANO_NASC))

# Criar pirâmide etária dos casos confirmados de tétano
# Create age pyramid for confirmed cases of tetanus
tettot3 <- tettot2 %>%
  filter(CLASSI_FIN == 1) |>
  # Transformar fatores de etiologia em labels
  # Convert etiology factors into labels
  mutate(faixa = case_when(
    (between(idade, 0, 4))~"0 a 4 anos",
    (between(idade, 5, 9))~"5 a 9 anos",
    (between(idade, 10, 14))~"10 a 14 anos",
    (between(idade, 15, 19))~"15 a 19 anos",
    (between(idade, 20, 24))~"20 a 24 anos",
    (between(idade, 25, 29))~"25 a 29 anos",
    (between(idade, 30, 34))~"30 a 34 anos",
    (between(idade, 35, 39))~"35 a 39 anos",
    (between(idade, 40, 44))~"40 a 44 anos",
    (between(idade, 45, 49))~"45 a 49 anos",
    (between(idade, 50, 54))~"50 a 54 anos",
    (between(idade, 55, 59))~"55 a 59 anos",
    (between(idade, 60, 64))~"60 a 64 anos",
    (between(idade, 65, 69))~"65 a 69 anos",
    (between(idade, 70, 74))~"70 a 74 anos",
    (between(idade, 75, 79))~"75 a 79 anos",
    (between(idade, 80, 84))~"80 a 84 anos",
    (between(idade, 85, 89))~"85 a 89 anos",
    (between(idade, 90, 94))~"90 a 94 anos",
    (between(idade, 95, 99))~"95 a 99 anos",
    (idade > 100)~"Acima dos 100 anos",
    TRUE ~ NA)) |>
  # Contar casos confirmados por ano por faixa
  # Count confirmed cases per year per age group
  group_by(CS_SEXO,faixa) |>
  summarise(count = n()) |>
  # Transformar no fator a ser ordenado
  # Transform into the factor to be sorted
  mutate(faixa = factor(faixa, levels = faixa))

# Colocando na ordem correta
# Putting in the correct order
Geral <- data.frame(Faixa = c("0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos",
                              "25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos",
                              "45 a 49 anos","50 a 54 anos","55 a 59 anos","60 a 64 anos",
                              "65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos",
                              "85 a 89 anos","90 a 94 anos","95 a 99 anos","Acima dos 100 anos"),
                    Count = rep("0"))

# Criar um dataframe só para dados do sexo M e do sexo F, para depois juntá-los
# Create a dataframe for male and female data only, to join them later
M <- tettot3 |>
  filter(CS_SEXO == "M")
M <- left_join(Geral, M, by=c("Faixa"="faixa")) |>
  mutate(CS_SEXO = case_when(
    is.na(CS_SEXO) ~ "M",
    TRUE ~ CS_SEXO),
    count = case_when(
      is.na(count) ~ 0,
      TRUE ~ count))

F <- tettot3 |>
  filter(CS_SEXO == "F")
F <- left_join(Geral, F, by=c("Faixa"="faixa"))

join <- cbind(M,F)

colnames(join) <- c("Faixa","C1","Male","MaleC","Faixa_1","C2","Female","FemaleC")

join <- join |>
  mutate(Faixa = factor(Faixa, levels = c("0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos",
                                          "25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos",
                                          "45 a 49 anos","50 a 54 anos","55 a 59 anos","60 a 64 anos",
                                          "65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos",
                                          "85 a 89 anos","90 a 94 anos","95 a 99 anos","Acima dos 100 anos")))

# Crie o gráfico de pirâmide etária
# Create the age pyramid graph
piramide_etaria <- join |>
  ggplot(aes(x = Faixa)) +
  geom_bar(aes(y = MaleC, fill = "Homens"), stat = "identity") +
  geom_bar(aes(y = -FemaleC, fill = "Mulheres"), stat = "identity") +
  scale_fill_manual(values = c("Homens" = "blue", "Mulheres" = "pink")) +
  labs(title = "Pirâmide Etária",
       x = "Idade",
       y = "Faixa Etária",
       fill = "Gênero") +
  theme_minimal() +
  coord_flip() +
  theme(plot.title.position = "plot",
        legend.direction = "horizontal",
        legend.position = "bottom")

# Exibir o gráfico
# Display the graph
print(piramide_etaria)

# Tabela contendo caracterização dos casos de tétano ====

tettot2 |>
  mutate(Desfecho = case_when(
    EVOLUCAO == 1 ~ "Cura",
    EVOLUCAO == 2 ~ "Óbito",
    EVOLUCAO == 3 ~ "Óbito",
    EVOLUCAO == 9 ~ "Não disponível",
    TRUE ~ "Não disponível")) |>
  mutate(Sexo_biologico = case_when(
    CS_SEXO == "M" ~ "Masculino",
    CS_SEXO == "F" ~ "Feminino",
    TRUE ~ "Não disponível")) |>
  mutate(Profilaxia = case_when(
    TP_PROFILA == 1 ~ "Soro antitetânico",
    TP_PROFILA == 2 ~ "Imunoglobulina",
    TP_PROFILA == 4 ~ "Antibiótico",
    TP_PROFILA == 3 ~ "Vacina",
    TP_PROFILA == 5 ~ "Nenhum",
    TP_PROFILA == 9 ~ "Ignorado",
    TRUE ~ "Não disponível")) |>
  mutate(Numero_de_doses = case_when(
    NU_DOSE == 1 ~ "1 dose",
    NU_DOSE == 2 ~ "2 doses",
    NU_DOSE == 3 ~ "3 doses",
    NU_DOSE == 5 ~ "3 doses+1 reforço",
    NU_DOSE == 6 ~ "3 doses+2 reforços",
    NU_DOSE == 9 ~ "Ignorado")) |>
  
  ##filter(!is.na(EVOLUCAO2))|>
  select(Desfecho, Sexo_biologico, Profilaxia, Numero_de_doses)|>
  tbl_summary(by = Desfecho, missing = "no")
