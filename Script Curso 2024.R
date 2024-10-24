#Curso 2023####


#TÓPICO 1 - INTRODUÇÃO AO R#### 

#1ºtítulo####
##2ºtítulo####
###3ºtítulo####


#Atalho para abrir o diretório de trabalho
#ctrl + shift + h

#Atalho para salvar o script ctrl+s


##Criar data frame####

#Criação dos vetores####
Id <-  c(1:4)
Nomes <- c("Ana", "Gilberto", "Rodrigo", "Marcela")
Peso <- c(75.6, 99, 62.8, 102)
Idades <- c(25, 18, 44, 23)
Escolaridade <- c("Graduação", "Mestrado", "Primário", "Graduação")
Exerc_Recomend <- c("Natação", "Pilates", "Musculação", "Corrida")
Comidas_preferidas <- c("Chocolate", "Sorvete", "Milho", "Pão")

#Criação do data frame
Ficha_Pacientes <- data.frame(Id, Nomes, Peso, 
                              Idades, Escolaridade, 
                              Exerc_Recomend, Comidas_preferidas)
#Função para ver o data frame
View(Ficha_Pacientes)

#Identificar o diretório de trabalho####
#Função para identificar o caminho em que o arquivo de script será salvo
getwd()
#[1] "C:/Users/nayar/OneDrive/2. ANO CORRENTE/EVENTOS 2024/CURSO R 2024 BÁSICO"

#Função manual para selecionar o diretório de trabalho
setwd("C:/Users/nayar/OneDrive/2. ANO CORRENTE/EVENTOS 2024/CURSO R 2024 BÁSICO")

#salvar#####
#Função para salvar o data frame
save(Ficha_Pacientes, file = "Ficha_Pacientes.RData")


#TÓPICO 2 - Análise exploratória e organização dos dados####
#Análise descritiva####

#ver a base####
#Função para análise das classes de cada variável
str(Ficha_Pacientes)
# 'data.frame':	4 obs. of  7 variables:
#   $ Id                : int  1 2 3 4
# $ Nomes             : chr  "Ana" "Gilberto" "Rodrigo" "Marcela"
# $ Peso              : num  75.6 99 62.8 102
# $ Idades            : num  25 18 44 23
# $ Escolaridade      : chr  "Graduação" "Mestrado" "Primário" "Graduação"
# $ Exerc_Recomend    : chr  "Natação" "Pilates" "Musculação" "Corrida"
# $ Comidas_preferidas: chr  "Chocolate" "Sorvete" "Milho" "Pão"

#A base de dados deverá ter um nome de coluna e linhas 
#Para saber esses nomes use as funções rownames e colnames
rownames(Ficha_Pacientes)
#[1] "1" "2" "3" "4"
colnames(Ficha_Pacientes)#Usando a função names também 
#Conseguimos ver todas as colunas
# [1] "Id"                 "Nomes"              "Peso"               "Idades"            
# [5] "Escolaridade"       "Exerc_Recomend"     "Comidas_preferidas"


print(Ficha_Pacientes)
# Id    Nomes  Peso Idades Escolaridade Exerc_Recomend Comidas_preferidas
# 1  1      Ana  75.6     25    Graduação        Natação          Chocolate
# 2  2 Gilberto  99.0     18     Mestrado        Pilates            Sorvete
# 3  3  Rodrigo  62.8     44     Primário     Musculação              Milho
# 4  4  Marcela 102.0     23    Graduação        Corrida                Pão

#Análise das médias
print(mean(Ficha_Pacientes$Idades))
#[1] 27.5
summary(Ficha_Pacientes$Idades)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   21.75   24.00   27.50   29.75   44.00 
summary(Ficha_Pacientes$Exerc_Recomend)
# Length     Class      Mode 
#  4 character character 
table(Ficha_Pacientes$Escolaridade)
# Graduação  Mestrado  Primário 
# 2          1         1 

#Seleção####
#Selação dos dados indicando 1º a linha e 2º a coluna
Ficha_Pacientes[2,5]
#[1] "Mestrado"
Ficha_Pacientes[2,5, drop=F]
# Escolaridade
# 2     Mestrado
Ficha_Pacientes[2,c("Peso", "Idades", "Nomes")]
# Peso Idades    Nomes
# 2   99     18 Gilberto
 
#Inserção####
#Inserir novos dados em toda a base 
Ficha_Pacientes$Sexo <-  c("F", "M", "M", "F")
Ficha_Pacientes$Quant_filhos <-  c(4:7)

#Uma outra forma de inserção de dados 
#é utilizando uma função muito importante no rbase 
# a função cbind

Ficha_Pacientes <- cbind(Ficha_Pacientes,
                         Prim_emprego = 
                        c("sim", "nao", "nao", "sim"))

#Filtros####
#Subset:
#Quero uma base de dados com apenas 5 variáveis
subset(Ficha_Pacientes, select = c("Id", "Nomes", "Peso", 
                                   "Idades", "Sexo"))

Base_menor <-  subset(Ficha_Pacientes, select = 
                        c("Id", "Nomes", "Peso", 
                          "Idades", "Sexo"))
#Mas eu queria apenas pessoas do sexo masculino
Base_menor <-  subset(Ficha_Pacientes,
Sexo == "M",
select = c("Id", "Nomes", "Peso", "Idades", "Sexo"))

#Mas eu tbém queria pessoas com idades maiores ou
#igual a 20 anos
Base_menor <-  subset(Ficha_Pacientes,
Idades >= 20,
select = c("Id", "Nomes", "Peso", "Idades", "Sexo"))

#Filtro usando o filter 
# seleciona apenas colunas numéricas
Filter(is.numeric, Ficha_Pacientes)
# Id  Peso Idades
# 1  1  75.6     25
# 2  2  99.0     18
# 3  3  62.8     44
# 4  4 102.0     23

# seleciona apenas colunas de texto
Filter(is.character, Ficha_Pacientes)
#        Nomes Escolaridade Exerc_Recomend Comidas_preferidas
# 1      Ana    Graduação        Natação          Chocolate
# 2 Gilberto     Mestrado        Pilates            Sorvete
# 3  Rodrigo     Primário     Musculação              Milho
# 4  Marcela    Graduação        Corrida                Pão

#Separando dados das colunas####
library(stringr)

#Inserindo uma variável com 2 informações
Ficha_Pacientes$Nome_Mãe <- 
c('Joana Silva', 'Maria Aparecida', 'Silvia souza',
  'Luzia Santana')

#Separando
str_split_fixed(Ficha_Pacientes$Nome_Mãe, " ", 2)


# Dividindo a coluna 'Nome_Mãe' em duas novas colunas: 
#'Primeiro_Nome' e 'Sobrenome'
Ficha_Pacientes[c("Primeiro_Nome", "Sobrenome")] <- 
  str_split_fixed(Ficha_Pacientes$Nome_Mãe, " ", 2)



#Alteração#####
#Altarendo dados de linhas ou colunas específicas
#Vamos alterar a idade da Marcela de 23 para 53
Ficha_Pacientes$Idades[4] <- 53 
#Seguindo essa mesma lógica podemos alterar o nome das variáveis
#Ou então criar uma cópia com um nome diferente

#Alterando conjunto maiores de dados
#recategorizar usando o pacote memisc, função recode
library(memisc)

#Quero transformar a idade, fazer com que ela deixe de 
#Ser numérica e se torne categorica (faixas de idade)
Ficha_Pacientes$Idade_Faixas <- recode(Ficha_Pacientes$Idades,
"Jovens" <-  c(18:25), "Idoso" <- 44)

#tipos de variáveis####
#as.factor####
#Podemos alterar qualquer variável, mas é importante 
#lembrar que tipo de variável estamos alterando
#e entender como essa variável estará após as alterações

Ficha_Pacientes$Escolaridade <- 
  as.factor(Ficha_Pacientes$Escolaridade)

#OLhar a estrutura dos dados
str(Ficha_Pacientes)

#Verificar os níveis
Ficha_Pacientes$Escolaridade
#Levels: Graduação Mestrado Primário

#Se eu não concordar com a ordem atribuída pelo R,
#Se essa ordem não fizer sentido para as análises estatísticas
#então eu posso alterar a ordem usando a função levels
Ficha_Pacientes$Escolaridade2 <-  
  factor(Ficha_Pacientes$Escolaridade,
levels = c("Primário", "Graduação", "Mestrado"))

#as.numeric/as.character####
#Em geral o ambiente de programação entende quais são 
#os tipos das variáveis, porém pode ser que ele entenda errado
#ou que em razão da abertura da base de dados seja necessário
#fazer alguns ajustes, dizendo para o ambiente o que é numérico 
#e o que é caracter, nesses casos usamos as funções as. =como


  as.character

  as.numeric

#TÓPICO 3 - Salvamentos e abertura da base de dados####
#salvamento####
  
#Vamos salvar a base que criamos em csv E xlsx 

#CSV salvamento####
write.csv2(Ficha_Pacientes, file = "Ficha2.csv", 
           quote = F, #Dividir por ponto e vírgulas
           row.names = F,#remover a primeira coluna com id do sistema
           fileEncoding = "latin1")#função pra definir a linguagem 

#CSV abertura####
  
#Precisaremos do pacote readr
library(readr)
#abertura
Ficha2 <- read_delim("Ficha2.csv", delim = ";", 
                     locale = locale(encoding = "Latin1"))
View(Ficha2)

#Xlsx Salvamento#### 

#Precisaremos do pacote 
library(writexl)

#salvando
writexl::write_xlsx(Ficha_Pacientes, path = "Ficha3.xlsx")

#Xlsx Abertura####
#Pacote:
library(readxl)

Ficha3 <- read_excel("Ficha3.xlsx")

View(Ficha3) 

#Abertura de dados reais####
#Abertura em SAV####

#Pacote:
library(haven)

#Como o nome da base é bastate grande, vamos chamar por um 
#nome mais resumido
Brasil2014 <- read_sav("863896541Brazil LAPOP AmericasBarometer 2014 Espanol v3.0_W.sav")

#Após realizar as alterações na base
#se quiser salvar no mesmo formato, use:
write_sav(data = Brasil2014, path = "Brasil2014.sav")

#Abertura em DTA####
#Use o mesmo pacote haven
Brasil2014dta <-  
  read_dta("636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")

#Após realizar as alterações na base
#se quiser salvar no mesmo formato, use:
write_dta(data = Brasil2014dta, path = "Brasil2014dta.dta")

################################

#TÓPICO 4 - Análise descritiva####

#Pacote para abertura da base de dados
library(haven)

#Abertura da base de dados
Brasil2014dta <-  
  read_dta("636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")

Brasil2014dta <- X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W

Brasil2014dta <- haven::zap_labels(Brasil2014dta)

#Para olhar a base
View(Brasil2014dta)

#Seleção das variáveis####
colnames(Brasil2014dta)
Brasil2014dta$q11n

#Variáveis utilizadas####
# ls3 = Satisfação com a vida  (SatVida) - inverter a escala 
# q2 = Idade  
# q1 = Sexo
# q10new = Renda familiar (RendFam)
# q10d = Percepção sobre a renda (PercRend) - Inverter a escala
# q11n = Estado civil (EstCiv)
# q12c = Quantidade de moradores no mesmo domicílio (QuantMor)
# ocup4a = Ocupação (Ocupacao)
# q12= Quantos filhos tem (Filhos)


#Após definir todas as variáveis que iremos querer basta 
#utilizar a função subset que aprendemos na seção anterior

Bra2014Menor <-  subset(Brasil2014dta, select = 
                        c("ls3", "q2", "q1", 
                          "q10new", "q10d",
                          "q11n", "q12c", "ocup4a",
                          "q12"))

#Salve a base de dados
save(Bra2014Menor, file = "Bra2014Menor.RData")
#Agora vejam como está a base de dados

#Toda a base é codificada numéricamente, 
#Para algumas variáveis analisar dessa forma não faz sentido, 
#Por isso iremos ter que recodificar e reorganizar

#Para isso, vamos entender como estão organizado os dados 
#através da função table ou summary que são funções
#que nos auxiliam a olhar a organização das variáveis

str(Bra2014Menor)

#Para alterar as variáveis iremos utilizar a função
#Recode que percente ao pacote:
library(memisc)
#Além disso, precisaremos utilizar o as.factor para 
#alterar algumas variáveis, pois essa função 
#somente funciona para variáveis que são fatores.

table(Bra2014Menor$ls3)
# 1   2   3   4 
#825 480 131  62 

#Essa variável ls3 deve ser recodificada e sua 
#escala alterada. 

#transformar uma variável que está como caracter 
#em fator
Bra2014Menor$ls3 <-  as.factor(Bra2014Menor$ls3)

#Recodificar
Bra2014Menor$SatVida <- recode(Bra2014Menor$ls3, 
                                  "Muito Satisfeito" <- 1,
                                  "Pouco Satisfeito" <- 2,
                                  "Pouco Insatisfeito" <- 3,
                                  "Muito insatisfeito" <- 4)
table(Bra2014Menor$SatVida)
#Vamos olhar como estão os levels, e se for preciso 
#alterar
levels(Bra2014Menor$SatVida)
#[1] "Muito satisfeito"   "Pouco satisfeito"   
#"Pouco insatisfeito" "Muito insatisfeito"

#Queremos que seja o contrário disso
Bra2014Menor$SatVida <-  
  factor(Bra2014Menor$SatVida, 
         levels = c("Muito insatisfeito", "Pouco Insatisfeito",
                    "Pouco Satisfeito", "Muito Satisfeito"))

#Olhamos de novo, só pra garantir e ok! 
levels(Bra2014Menor$SatVida)
table(Bra2014Menor$SatVida)

summary(Bra2014Menor$q2)#qual é a nossa preocupação 
#quando trabalhamos com idade?
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16.00   26.00   38.00   39.63   51.00   89.00 

#A idade é numérica, então está ok, só vamos alterar seu nome
Bra2014Menor$Idade <- Bra2014Menor$q2
#Vejam que na verdade não mudamos o nome da variável, 
#Só criamos uma cópia dela com outro nome
#Se for preciso, depois conseguimos trabalhar com a original

table(Bra2014Menor$q1)
# 1   2 
#749 751 

#Vamos alterar o nome para sexo e categorizar, podemos manter a ordem.

Bra2014Menor$q1 <-  as.factor(Bra2014Menor$q1)

Bra2014Menor$Sexo <-  recode(Bra2014Menor$q1, 
                             "Homem" <- 1, 
                             "Mulher" <- 2)

#A renda está em faixas, ou a gente faz toda a recategorização
#Ou mantem dessa forma lembrando que os números não representam a 
#quantidade de salários mínimos ou algo assim, mas as faixas de renda 
table(Bra2014Menor$q10new)
# 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
# 1  71  63 129  68  83  63  86  76 146  80 142 185 113  56  33  49 

#Vamos manter dessa forma e alterar apenas o nome
Bra2014Menor$RendFam <- Bra2014Menor$q10new

#Aqui, sobre a percepção sobre a renda familiar, vamos
#recategorizar juntando categorias
table(Bra2014Menor$q10d)
# 1   2   3   4 
# 120 645 545 187 

Bra2014Menor$PercRend <- as.factor(Bra2014Menor$q10d)

Bra2014Menor$PercRend <- recode(Bra2014Menor$PercRend, 
                                "Satisfeito" <- c(1,2),
                            "Insatisfeito" <- c(3,4))
table(Bra2014Menor$PercRend)
# Satisfeito Insatisfeito 
# 765          732 

levels(Bra2014Menor$PercRend)
#[1] "Satisfeito"   "Insatisfeito"


Bra2014Menor$PercRend <-  
  factor(Bra2014Menor$PercRend, 
         levels = c( "Insatisfeito", "Satisfeito"))

#O estado civil tem algumas categorias que podem ser agregadas
#porque para as nossas análises não é necessário tanta especificidade
table(Bra2014Menor$q11n)
#   1   2   3   4   5   6   7 
# 471 611 246  45  40  76  11 

Bra2014Menor$q11n <-  as.factor(Bra2014Menor$q11n)

Bra2014Menor$EstCiv <- recode(Bra2014Menor$q11n, 
                              "Solteiro" <- 1, 
                              "Casado" <-  c(2,3,7),
                              "Separado" <- c(4,5),
                              "Viúvo" <- 6) 
table(Bra2014Menor$EstCiv)
# Solteiro   Casado Separado    Viúvo 
#   471      868       85       76 

#Quantidade de pessoas na casa é numérica, então vamos manter assim
#Vamos apenas mudar o nome
summary(Bra2014Menor$q12c)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   3.827   5.000  15.000 

Bra2014Menor$QuantMor <- Bra2014Menor$q12c

#A ocupação também podemos juntar algumas categorias
table(Bra2014Menor$ocup4a)
# 1   2   3   4   5   6   7 
# 812  63  81 125 200 180  39 

Bra2014Menor$ocup4a <-  as.factor(Bra2014Menor$ocup4a)

Bra2014Menor$Ocupacao <- recode(Bra2014Menor$ocup4a,
      "Emprego_Rem" <-  1,
      "Emprego_N_Rem" <-  5,
      "Estudante" <-  4,
      "Aposentado" <- 6,
      "Nao_Empregado" <- c(2,3,7))
table(Bra2014Menor$Ocupacao)

#Quantidade de filhos é numérica, 
#Sendo assim somente mudaremos o nome
summary(Bra2014Menor$q12)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   2.000   1.888   3.000  16.000       2 

Bra2014Menor$Filhos <-  Bra2014Menor$q12

#Após observar a organização de cada uma das variáveis
#vamos retornar organizando cada uma delas 

#Medidas de tendência central####

#Média e mediana####

#Funções já utilizadas
summary(Bra2014Menor$Filhos)
table(Bra2014Menor$Filhos)
table(Bra2014Menor$Ocupacao)

#Funçoes específicas para média e mediana
mean(Bra2014Menor$Filhos, na.rm = T)
median(Bra2014Menor$Filhos, na.rm = T)

#Para o cálculo da moda:http://www.dma.uem.br/kit/outros-arquivos/moda.pdf
#Explicações do help
moda <- function(x) {
  modal <- unique(x) 
  modal[which.max(tabulate(match(x, modal)))]
}


# A função moda recebe um vetor x como argumento.
# A função unique(x) retorna os valores únicos
# presentes em x. 
# Isso significa que ela cria uma lista sem duplicatas, contendo cada valor que aparece em x apenas uma vez.
# A função match(x, modal) retorna um vetor de 
# posições indicando onde os valores de x aparecem 
# na lista de valores únicos (modal).
# A função tabulate() conta quantas vezes cada valor 
# de modal aparece em x, resultando em um vetor de 
# frequências.
# which.max() retorna o índice do valor que aparece
# com mais frequência (a moda).
# O vetor modal[] é usado para selecionar o valor 
# correspondente à moda.

# Agora podemos rodar a função moda:

moda(Bra2014Menor$Filhos)
#[1] 0



#Frequência
#Pacote
library(descr)
freq(Bra2014Menor$SatVida)

freq(Bra2014Menor$SatVida)

#Tratar como valor perdido uma determinada categoria
freq(Bra2014Menor$SatVida, user.missing = "Muito satisfeito")

#Table - Análise cruzada####

table(Bra2014Menor$SatVida, Bra2014Menor$Sexo)
#                    Homem Mulher
# Muito insatisfeito   433    392
# Pouco insatisfeito   236    244
# Pouco satisfeito      51     80
# Muito satisfeito      28     34


#Vamos salvar o table que criamos dentro de um 
#objeto e depois vamos chamar ele usando 
#Uma outra função.
Obj <- table(Bra2014Menor$SatVida, Bra2014Menor$Sexo)

#Vamos utilizar a função: prop.table()
#Para apresentar os valores percentuais, contudo
#Teremos que multiplicar por 100, por isso salvamos 
#tudo dentro de um novo objeto
Obj2 <- prop.table(Obj, margin = 1)
#Apresenta o percentual na linha 
Obj2*100

Obj3 <- prop.table(Obj, margin = 2 )
#Apresenta o percentual na coluna
Obj3 * 100


#Escolher quantas casas após a vírgula queremos utilizar
options(digits = 1)

###
plot(Bra2014Menor$Idade)
plot(Bra2014Menor$Ocupacao)
#
hist(Bra2014Menor$Idade)
boxplot(Bra2014Menor$Idade)
#
boxplot(Idade ~ Sexo, data= Bra2014Menor)
#
boxplot(Idade ~ SatVida, data= Bra2014Menor)

#TÓPICO 5 - ####
#BASE DE DADOS UTILIZADA: BRA2014MENOR
#ESSA BASE É FRUTO DE UM SUBSET DA BASE 
#DO LAPOP 2014

#Falar sobre salvamento de imagem ! 

#Apresentação gráfica####

# A função theme(legend.position = ...) 
#no pacote ggplot2 do R permite que você 
#especifique a posição da legenda em relação ao gráfico. 
#Aqui estão algumas opções para a posição da legenda:
#   
#   "none": Sem legenda.
# "left": À esquerda do gráfico.
# "right": À direita do gráfico.
# "top": Acima do gráfico.
# "bottom": Abaixo do gráfico.
# "inside": Dentro do gráfico, em uma das quatro posições laterais.
# "inside_horizontal": Dentro do gráfico, acima ou abaixo.
# "inside_vertical": Dentro do gráfico, à esquerda ou à direita.

#Boxplot####

#Nós vimos que a versão mais simples do boxplot já 
#Vem com a mediana, mas podemos inserir também  
#o ponto da média

#No R base nós fazemo isso
#através da criação de um objeto contendo a média
#Que depois será inserido junto ao gráfico
Média <- mean(Bra2014Menor$Idade)

boxplot(Bra2014Menor$Idade)
points(Média, pch=16,col="red")



#PCH significa Plot character, ou plotagem de caracter
#é a função que define qual será o caracter plotado 
#existem 25 modelos, você pode escrever pch no help 
#Para encontrar todos os tipos
#Col é a cor que o seu ponto terá

#Agora vamos etender como isso funciona no ggplot 
#Documentação do pacote 33

#Pacote para criação de gráficos 
library(ggplot2)

#Antes de enteder como criar um boxplot 
#É preciso entender que qualquer projeção gráfica utilizando 
#o ggplot é criada através de uma série de camadas específicas 
#Cada camada irá inserir um detalhe ao gráfico 

#Além disso, existe um mapeamento geral que compõe cada gráfico
#Vamos ver como isso funciona

#1º versão:
ggplot(Bra2014Menor, aes(y= Idade)) + 
  geom_boxplot()



#2ª versão:
ggplot(Bra2014Menor, aes(y= Idade)) + 
  geom_boxplot() + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem")


#todos os gráficos possuem em seu mapeamento o eixo y e o eixo x
#No caso do boxplot o ideal é que em um dos eixos
#exista uma variável qualitativa, quando temos a 
#intenção de comparar categorias entre si, inserimos uma variável
#específica, quando não temos a inteção de trabalhar com variáveis 
#cruzadas temos a intenção de fazer uma análise cruzada
#podemos não inserir nada, porém isso fará diferença quando quisermos
#realizar certas análises, como a média
#Por isso, inserimos algum outra variável no eixo faltante
#Pode ser uma contagem com a função count ou então uma única
#informação

#3ª versão: Inserção da média

Bra2014Menor$V <- c("pessoa")

#*Falar sobre as cores
 ggplot(Bra2014Menor, aes(y= Idade, x = V)) + 
geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ #Cores do gráfico
   theme_classic() + #Temas 
   labs(title = "Gráfico de idades", y="Idade", x="Contagem")

 #Versão 4
 ggplot(Bra2014Menor, aes(y= Idade, x = V)) + 
  geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
               shape=16, size=4, color="#87C4EB") 

#Versão 5
#Fonte####
Fonte <-  theme(text = element_text(family = "serif", size = 13),
title = element_text(color = "#8b0000"),
axis.line = element_line(color = "#8b0000"), 
axis.text = element_text(colour = "#8b0000", size = rel(0.5)),
plot.background = element_rect(fill = "grey90", colour = "black", 
                                            linewidth = 1)) 
#rel() is used to specify sizes relative


#Análise cruzada
 #Versão 1
 ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
  geom_boxplot() 
  


#Versão 2
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo,
                                     fill= Sexo)) + 
  geom_boxplot()
#

#Versão 3
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo,
                                     color= Sexo)) + 
  geom_boxplot()



#Versão 4
 ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3, 
               fill= c("blue", "red"), alpha = 0.2)

 #Versão 5
 #Salvar em um objeto
 Gráfico <- ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
   geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=3, 
                fill= c("blue", "red"), alpha = 0.2)
 Gráfico
 
 #Versão 6
 Gráfico1 <-   Gráfico + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
                 shape=16, size=2, color="red")
 
   
   
#Versão 7
Gráfico1 + Fonte


#Salvamento de imagem####
jpeg("Grafico2.jpeg")
Média <- mean(Bra2014Menor$Idade)
boxplot(Bra2014Menor$Idade)
points(Média, pch=16,col="red")
dev.off() 




#Histograma####
#Versão1
Bra2014Menor$Id <- c(1:1500)

Gráfico1 <- ggplot(Bra2014Menor, aes(x= Idade))+
  geom_histogram()

Gráfico1

#Versão2
Gráfico2 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram()

Gráfico2

#Versão3
Gráfico3 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge") #Para colocar lado a lado as barras

Gráfico3

#Versão4
Gráfico4 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) #Para colocar lado a lado as barras
#tamanho das barras
Gráfico4

#Versão5 - cor
# https://colorbrewer2.org/#type=qualitative&scheme=Pastel2&n=3

Gráfico5 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")

Gráfico5

#Versão6 

Gráfico6 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")+
  scale_y_continuous(limits = c(0, 103))+
  scale_x_continuous(limits = c(0, 103)) +
  labs(title = "Idade por sexo", y="Contagem", x= "Idade") +
  theme_classic()
  

Gráfico6

#versão7
Gráfico6 + Fonte

#Gráficco de barras geom_bar####
library(scales) #Pacote para criação de percentual


#Versão1
Gráfico1 <- ggplot(Bra2014Menor, aes(x= PercRend)) +
  geom_bar() 

Gráfico1

#Para retirar os NA
library(tidyr)
Bra2014Menor <- drop_na(Bra2014Menor, PercRend)

#Se estiver em notação científica, mostrar como faz para mudar
Gráfico1

#Versão2
#Percentual####
#Ao invés de eixo número, podemos utilizar um eixo percentual
#Para entender mais:http://rstudio-pubs-static.s3.amazonaws.com/15833_7ab6dc7be6b64b53a576093a364dacf9.html

Gráfico2 <- ggplot(Bra2014Menor, aes(x= PercRend, 
  y= (..count..)/sum(..count..))) +
  geom_bar()

Gráfico2

#Versão3
#Podemos mostrar o eixo em percentual, para que não seja 
#Preciso mentalmente ficar multiplicando por 100
#Vamos também colocar uma cor, de acordo com o Percentual de renda

Gráfico3 <- ggplot(Bra2014Menor, aes(x= PercRend, 
 y= (..count..)/sum(..count..), fill= PercRend)) +
  geom_bar() +
  scale_y_continuous(labels = percent)  +
  theme_classic()

Gráfico3

#Versão4
#Inserção dos labs
Gráfico4 <-  Gráfico3 +
  labs(title = "Percepção sobre a renda",
       y= "Percentual",
       x = "Percepção")

#Versão5
#Além do título e rótulos dos eixos
#podemos inserir os rótulos dos dados

Gráfico5 <- Gráfico4 +
geom_text(aes(y = ((..count..)/sum(..count..)),
              label = percent((..count..)/sum(..count..))),
          stat = "count", vjust = -0.25, color="black",size=4)

Gráfico5

#Versão6
#Na fase final de organização podemos cuidar 
#das cores e demais detalhes

Gráfico6 <- Gráfico5 +  
  scale_fill_brewer(palette="BuPu", type = "seq")

Gráfico6

#Versão7
#Juntar a fonte
Gráfico7 <- 
Gráfico6 + Fonte +
theme(legend.position="none")

#Local: "bottom", "top", "left", ou "right" 

#Função facet_wrap
#Sequência de paineis * Dar a tese como exemplo

Gráfico7 + facet_wrap( ~ Bra2014Menor$Sexo)


#Gráfico de barras sobrepostas####
#Versão1
Gráfico1 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..))) +
  geom_bar()

Gráfico1

#Para retirar os NA
library(tidyr)
Bra2014Menor <- drop_na(Bra2014Menor, SatVida)

#Versão2 Criação do gráfico de barras sobrepostas
Gráfico2 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..),
                                     fill=Sexo)) +
  geom_bar()
Gráfico2
#Versão3 Percentual e organização do eixo y
library(scales) #Pacote para criação de percentual

Gráfico3 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..),
                                     fill=Sexo)) +
  geom_bar() +
  scale_y_continuous(labels = percent, limits = c(0, 0.6),
  breaks = pretty(c(0,0.6), n = 5)) 


Gráfico3

#Versão4 Cor e título

Gráfico4 <-  Gráfico3 + scale_fill_brewer(palette = "Spectral") +
labs(title = "Satisfação em relação a vida",
     y="Percentual", x="Satisfação") +
  theme_light()

Gráfico4

#Versão 5 Inserção da fonte

Gráfico4 + Fonte

#Gráfico de pizza&seção####


# PACOTE
library(ggplot2)
library(descr)
#Criar uma pequena base com 
freq(Bra2014Menor$Sexo, plot = F)

#            Frequência Percentual
# Homem         749      49.93
# Mulher        751      50.07
# Total        1500     100.00

#Criação de uma base de dados pequena
data <- data.frame(
  category = c("Homem", "Mulher"),
  count= c(746, 749), 
  percentual= c(49.9, 50.0)
)

# Computar os valores
data$fraction <- data$count / sum(data$count)

# Computador os valores do eixo y
data$ymax <- cumsum(data$fraction)

# Cálculo do eixo y 
data$ymin <- c(0, head(data$ymax, n=-1))

# Cálculo da posição do label
data$labelPosition <- (data$ymax + data$ymin) / 2

# Cálculo do label
#1
data$label <- paste0(data$category, "\n 50% ", data$count)
#2
data$label <- paste0(data$category, " 50% ", data$count)

#3QUANDO CRIAR A BASE DE DADOS DIRETO COM O PERCENTUAL
data$label <- paste0(data$percentual, " % ", data$category)

# gráfico
#Versão1
obj1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, 
                        xmax=4, xmin=3, fill=category))+
  geom_rect() #Cria um gráfico reto

obj1
#Versão 2

obj2 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") #Cria um gráfico redondo

obj2

#Versão 3
obj3 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() #Retira todas as marcas de eixos

obj3

#Versão 4
obj4 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() + #Retira todas as marcas de eixos
  theme(legend.position = "bottom") + #Altera o lugar da legenda
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3, 
              fill= "white", color= "black")

obj4

#Versão 5 Gráfico de seção
obj5 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() + #Retira todas as marcas de eixos
  theme(legend.position = "bottom") + #Altera o lugar da legenda
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3, 
              fill= "white", color= "black")+ 
  xlim(c(2, 4)) 

obj5

#Versão 6 organização do gráfico

obj6 <- obj5 + scale_fill_brewer(palette="Pastel2")+
  labs (fill = "Sexo", title = "       
                     Sexo dos entrevistados") + 
  theme(legend.position = "none")


  #Para apagar somente o título do gráfico
# theme(legend.title = element_blank())

  obj6

#Gráfico de dispersão#####
library(tidyverse)
Bra2014Menor$Filhos <- as.numeric(Bra2014Menor$Filhos)

#Versão 1
ggplot(Bra2014Menor, aes(x=Idade, y= Filhos)) +
  geom_point()

#Versão 2 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos))+
  geom_point(size= 3, alpha= 0.7) #Tamanho dos pontos e transparência

#Versão 3
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+ #Cor dos pontos
  geom_point(size= 3, alpha= 0.7) 

#Versão 4 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+ 
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm") #Inserção de uma reta linear model

#Versão 5 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm", color = "black", se = T) #Organização da reta 

#Salvando a versão 5 para organizar a formatação
gráfico5 <-  ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                                      color= Idade))+
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm", color = "black", se = T)

#Versão 6
gráfico5 + 
  Fonte +
  theme(legend.position = "none") +
  labs(title = "               Idade por quantidade de filhos") 

#Gráfico de linhas####
library(tidyverse)

#Com a nossa base atual é difícil construir 
#Um gráfico de linhas, então vamos montar uma base pequen
library(descr)


#Criação da base
Base_Filhos <- data.frame(
  Anos = c(2010, 2012, 2014, 2015,2017),
  Med_Filhos_Bra = c(8, 6, 4, 7, 3),
  Med_Filhos_PR = c(5, 4, 8, 6, 2),
  Med_filhos_MG = c(6, 3, 2, 5, 7))

#Salvar a base 
save(Base_Filhos, file = "Base_Filhos.RData")

#Construir o gráfico
#Versão 1 
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line()

#Versão 2 
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(arrow = arrow()) #Inserir uma flecha

#Versão 3
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(arrow = arrow(
    angle = 15, ends = "both", type = "closed"
  ))#inserir flecha de tam 15 dos dois lados e fechada (preta)

#Versão 4
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(color= "blue", linetype = 2) #Cores e tipo de linha

#Versão 5
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(color= "blue", linetype = 2) +
  geom_point(color="black", size=2)#inserção de pontos marcando

#Versão 6
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_step()

#Tipos de linhas 
#http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software

################
#Versão 7
library(tidyverse)
#Vamos usar o tidyverse para criar uma base de dados novas
#gather ou seja juntar, as informações da média (numérica),
#Med_Est (categorica com o nome dos estudos), tudo isso pelos 
#anos 
BaseNewF <- Base_Filhos %>% gather(Med_Est, Media, -Anos)


ggplot(BaseNewF, aes (x = Anos, y=Media,
                      fill= Med_Est))+ #Dividindo por grupos
  geom_line()

#Versão 8 
ggplot(BaseNewF, aes (x = Anos, y=Media,
                      linetype =Med_Est, #Organizando o tipo da linha por grupo
                      color= Med_Est))+
  geom_line(size = 1)

#Versão 9 
Vers9 <- ggplot(BaseNewF, aes (x = Anos, y=Media,
                               colour = Med_Est))+
  geom_line(size = 1) +
  geom_point( alpha=0.5, color = "black", size = 2)
scale_color_brewer(palette="Set1") #Inserção de cor

Vers9

#Versão 10 dividindo em gráfico pequenininhos

Vers9 +
  facet_wrap(~ Med_Est, nrow = 3, ncol = 1) + #Função facet_wrap
  theme(legend.position = "none") + Fonte


#Ribbon####
#Organizar pra ensinar no futuro
ggplot(BaseNewF, aes (x = Anos, y=Media,
                      fill= Med_Est))+
  geom_ribbon(aes(ymax = Media,
                  ymin = 0),
              alpha = 0.3)

#Tópico 6####
#Processamento dos dados####

##### para gerar o output como porcentagem
library(tidyverse)



#################################



#Para realizar o processamento precisamos 
#dos seguintes pacotes: expss e openxlsx
install.packages("expss")
library(expss)

install.packages("openxlsx")
library(openxlsx)


#Por gentileze, todos usando o default  de salvamento em UTF-8

options(OutDec = ",", digit = 0)

#Como em nossos relatórios utilizamos valores percentuais
#com o símbolo do %, então teremos que criar um objeto 
#chamado "add_percent" que irá adicionar percentual a nossa
#planilha.
#Então olhem no sumário, cliquem e add percent, 
#selecinem até o final e rodem. Depois voltem aqui
#para retormarmos o passo a passo.

#Vamos inicialmente importar a base de dados que coletamos
#Chamando ela de base, utilizando o pacote rio e a função 
#import

install.packages("rio")
library(rio)

base = rio::import("BANCOTREI.xlsx", which = 1)

head(base)


#Queremos analisar o que ?
#-Cidade, com base no sexo e rendimentos..

#Agora transforme todos os labels em caracteres, 
#usando essas linhas:

#install.packages("Hmisc")#Para quem já ativou o tidyverse não precisa
#library(Hmisc)

var.labels = as.character(names(base))

for(i in seq_along(base)){
  Hmisc::label(base[, i]) <- var.labels[i]
}

#RU####
#PROCESSAMENTO DE RESPOSTA ÚNICA

#Agora é preciso que você escolha quais variáveis 
#irão compor a sua lista de variáveis de respostas únicas, 
#anotando o número das colunas, que pode ser verificado na
#na própria base de dados
vars_ps = list(base[,c(3:4)], base[,c(5)]) #falar da 5


#Agora escolhemos o que desejamos cruzar com as variáveis 
#escolhidas anteriormente
vars_cruz = with(base, list(total(), SEXO))

#Feito isso, criamos a base, que no caso, vamos chamar de 
#RU, se referindo a resposta única
ru = base %>% 
  calculate(cro_cpct(cell_vars = vars_ps, 
                     col_vars = vars_cruz)) %>%  
  tab_sort_desc %>%  
  set_caption("RU") %>% 
  add_percent(digit = 0)


#criamos uma pasta de trabalho
wb = createWorkbook() 
#Adicionamos nesse documento criado a aba RU
sh = addWorksheet(wb, "RU")
#Gravamos todas tabelas em um documento
xl_write(ru, wb, sh)


#valor absoluto + percentual####

#Achado do arthur para análise dos valores absolutos também
# Absoluto = base %>% 
#   tab_cells(vars_ps) %>%
#   tab_cols(total(),vars_cruz) %>%
#   tab_stat_cases(label = "N", total_label = "") %>%
#   tab_stat_cpct(label="%", total_statistic = "w_cpct", 
#                 total_label = "") %>% 
#   tab_pivot(stat_position = "outside_rows") %>%  
#   set_caption("Absoluto")

#Argumentos que podem ser utilizados para organização 
#dos valores percentuais:
#“outside_rows”, “inside_rows”, 
#“outside_columns”, “inside_columns”


Absoluto = base %>%
  tab_cells(vars_ps) %>%
  tab_cols(total(),vars_cruz) %>%
  tab_stat_cases(label = "N", total_label = "") %>%
  tab_pivot(stat_position = "outside_rows") %>%
  set_caption("Absoluto")

sh2 = addWorksheet(wb, "Absoluto")
xl_write(Absoluto, wb, sh2)

#PESO####
base$Peso <- as.numeric(base$Peso)


ANALIPESO = base %>% 
  calculate(cro_cpct(cell_vars = vars_ps, 
                     col_vars = vars_cruz, 
                     total_statistic =  "w_cases",
                     weight = base$Peso)) %>%  
  tab_sort_desc %>%  
  set_caption("PESO")%>% 
  add_percent(digits = 0)


sh1 = addWorksheet(wb, "PESO")
xl_write(ANALIPESO, wb, sh1)

#RM####
#Agora a criação da aba com o processamento de respostas múltiplas
#Utilizamos a função mrset_p para analisar mais de uma coluna
rm = base %>% 
  calculate(cross_cpct(base, cell_vars = list(mrset_p("Q1"),
                                              mrset_p("Q2")),
                                  col_vars = vars_cruz)) %>%  
  tab_sort_desc %>%  set_caption("RM")%>% 
  add_percent(digits = 0)

sh2 = addWorksheet(wb, "RM")
xl_write(rm, wb, sh2)
#MÉDIA####
vmedias = list(base[,c(12)])

medias_cruz = with(base, list(total(), SEXO))

MEDIA = base %>% 
  calculate(cro_mean(cell_vars = vmedias, 
                     col_vars = medias_cruz)) %>%  
  tab_sort_desc %>%  set_caption("MEDIA")

sh3 = addWorksheet(wb, "MEDIA")
xl_write(MEDIA, wb, sh3)


#criar arquivo xls
saveWorkbook(wb, "Output4.xlsx", overwrite = TRUE)


############################################################




#Tópico 7#####
#Vamos utilizar a base de candidatos eleitos
#e não eleitos para o senado 2022
#Testes inferenciais bivariados####
library(readxl)
BASE_SEN_2022 <- read_excel("BASE SEN 2022.xlsx", 
 col_types = c("numeric", "text", "text", 
       "text", "text", "numeric", "numeric", 
      "text", "numeric", "text", "text", 
      "numeric", "numeric", "text", "numeric", 
     "text", "numeric", "text", "numeric", 
       "text", "numeric", "text", "numeric", 
        "numeric", "text", "text"))


##Teste de qui-quadrado####
#Diferença de média entre eleitos e não eleitos por cor e dps sexo

Teste1 <- table(BASE_SEN_2022$DS_SIT_TOT_TURNO, 
                BASE_SEN_2022$DS_COR_RACA)

chisq.test(Teste1)

# 
# Pearson's Chi-squared test
# 
# data:  Teste1
# X-squared = 15.701, df = 5, p-value = 0.00775

Teste2 <- table(BASE_SEN_2022$DS_SIT_TOT_TURNO, 
                BASE_SEN_2022$DS_GENERO)

chisq.test(Teste2)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  Teste2
# X-squared = 1.0059, df = 1, p-value = 0.3159




##Lambda####
#Para rodar o teste de lambda 

#Antes de rodar o teste, vamos atribuir os levels
BASE_SEN_2022$DS_COR_RACA <- 
  factor(BASE_SEN_2022$DS_COR_RACA, 
         levels = c("PRETA",  "PARDA", "INDÍGENA", 
                    "NÃO INFORMADO",  "AMARELA", "BRANCA"))

levels(BASE_SEN_2022$DS_COR_RACA)


BASE_SEN_2022$DS_GENERO <- 
  factor(BASE_SEN_2022$DS_GENERO, 
         levels = c("FEMININO",  "MASCULINO"))

levels(BASE_SEN_2022$DS_GENERO)

install.packages("rapportools")
library(rapportools)

lambda.test(Teste1)

# $row
# [1] 0
# 
# $col
# [1] 0.07407407

lambda.test(Teste2)

# $row
# [1] 0
# 
# $col
# [1] 0


##Gamma####

#Teste de Gamma (Y)
install.packages("vcdExtra")
library(vcdExtra)

#Vamos criar uma variável chamada
#Satisfação com a vida, somente para rodar o teste
#Vamos criá-la, a partir do estdo civil

 table(BASE_SEN_2022$DS_ESTADO_CIVIL)
# CASADO(A)             DIVORCIADO(A)   SEPARADO(A) JUDICIALMENTE 
# 133                        36                         4 
# SOLTEIRO(A)                  VIÚVO(A) 
# 27                         5 

#1= será o totalmente insastisfeito e 5=totalmente satisfeito
library(memisc)
 
table(BASE_SEN_2022$DS_ESTADO_CIVIL)

BASE_SEN_2022$SatVida <- recode(BASE_SEN_2022$DS_ESTADO_CIVIL, 
                                "Totalmente insatisfeito" <- "VIÚVO(A)",
                                "Insatisfeito" <- "SEPARADO(A) JUDICIALMENTE",
                                "Meio termo" <- "DIVORCIADO(A)",
                                "Satisfeito" <-  "SOLTEIRO(A)",
                                "Totalmente satisfeito" <- "CASADO(A)")
table(BASE_SEN_2022$SatVida)
BASE_SEN_2022$SatVida <-  as.factor(BASE_SEN_2022$SatVida)
BASE_SEN_2022$DS_GRAU_INSTRUCAO <-  as.factor(BASE_SEN_2022$DS_GRAU_INSTRUCAO)

tab2 <- table(BASE_SEN_2022$SatVida, 
              BASE_SEN_2022$DS_GRAU_INSTRUCAO)
GKgamma(tab2)
# gamma        : -0.371 
# std. error   : 0.041 
# CI           : -0.451 -0.291 

chisq.test(tab2, simulate.p.value = T) #Apenas para simular o valor de p
# Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
# 
# data:  tab2
# X-squared = 26.001, df = NA, p-value = 0.3208


##Kendal####
install.packages("Kendall")
library(Kendall)

Kendall(BASE_SEN_2022$SatVida, 
        BASE_SEN_2022$DS_GRAU_INSTRUCAO)

#tau = -0.00451, 2-sided pvalue =0.94604

#Teste T####
#não pareado –APROPRIADO PARA AMOSTRAS INDEPENDENTES ####
options(scipen = 999, digits = 1)

table(BASE_SEN_2022$CD_GENERO)

HOMEM <- subset(BASE_SEN_2022, CD_GENERO == 2)
summary(HOMEM$TOTAL_VOTOS)

MULHER <- subset(BASE_SEN_2022, CD_GENERO == 4)
summary(MULHER$TOTAL_VOTOS)


t.test(MULHER$TOTAL_VOTOS,HOMEM$TOTAL_VOTOS)
# 
# Welch Two Sample t-test
# 
# data:  MULHER$TOTAL_VOTOS and HOMEM$TOTAL_VOTOS
# t = -2, df = 203, p-value = 0.02
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -506729  -35829
# sample estimates:
#   mean of x mean of y 
# 280697    551975 

#pareado –APROPRIADO PARA AMOSTRAS DEPENDENTES ####

#Criação da base de dados para a realização do teste t
BaseHipSen <- data.frame(
  "Regiões" = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste",
                 "Sul", "Argentina"),
  "Can_2020" = c(5, 10, 7, 6, 11, 8), 
  "Can_2050" = c(25, 15, 4, 12, 30, 17)
)
View(BaseHipSen)

#Realização do teste 
t.test(BaseHipSen$Can_2020, BaseHipSen$Can_2050, paired = T)

#saída do teste

# Paired t-test
# 
# data:  BaseHipSen$Can_2020 and BaseHipSen$Can_2050
# t = -2.5908, df = 5, p-value = 0.04879
# alternative hypothesis: true mean 
#difference is not equal to 0
# 95 percent confidence interval:
#   -18.59377288  -0.07289378
# sample estimates:
#   mean difference 
# -9.333333 


#teste-t de Student para uma amostra####

#Primeiro vamos conferir qual é a média na Argentina
#depois excluir esse dado da amostra. 
#Em 2050 é 17 e em 2020 é 8. 

#Agora criamos uma base sem esses dados
BaseMenor <-  BaseHipSen[-6,]

#Rodamos o teste:
t.test(BaseMenor$Can_2050, mu=17) #"μ" (lê-se "mi") é a letra grega "mu"
# é o valor da média hipotética 
#(ou valor médio) que você deseja testar

# One Sample t-test
# 
# data:  BaseMenor$Can_2050
# t = 0.043093, df = 4, p-value = 0.9677
# alternative hypothesis: true mean is not equal to 17
# 95 percent confidence interval:
#   4.314184 30.085816
# sample estimates:
#   mean of x 
# 17.2 


#Correlação####

#Correlação de Pearson####
# Aqui, simulamos um conjunto de dados com variáveis 
# relacionadas ao nível de educação e à renda anual, 
# temas centrais nas análises socioeconômicas.

#Criar uma base de dados

set.seed(123)

educ <- sample(8:20, 100, replace = TRUE)
renda <- educ *5000 + rnorm(100, mean= 30000, sd=10000)

dados_educ_renda <- data.frame(educ, renda)

# Visualizar as primeiras linhas da base de dados
head(dados_educ_renda)

cor.test(dados_educ_renda$educ, dados_educ_renda$renda, 
         method = "pearson")

options(scipen = 999)

# Calcular a correlação de Pearson entre educação e renda
cor(dados_educ_renda$educ, dados_educ_renda$renda, method = "pearson")

#Apresentação gráfica####

# Gráfico de Dispersão (Scatter Plot) com Linha de Tendência
# O gráfico de dispersão é uma das maneiras mais comuns de 
# visualizar a correlação entre duas variáveis. 
# Podemos adicionar uma linha de regressão para destacar a 
# relação entre educação e renda.

# Carregar pacotes necessários
library(ggplot2)

# Criar o gráfico de dispersão com linha de tendência
ggplot(dados_educ_renda, aes(x = educ, y = renda)) +
  geom_point(color = "blue") +  # Adiciona pontos
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  # Adiciona linha de regressão linear
  labs(title = "Relação entre Educação e Renda",
       x = "Anos de Escolaridade",
       y = "Renda Anual (R$)") +
  theme_minimal()

# Esse exemplo simula a relação entre o nível educacional 
# (anos de escolaridade) e a renda anual. Em pesquisas sociais, 
# esperamos encontrar uma correlação positiva, pois mais anos de educação 
# geralmente estão associados a maiores rendimentos.

#Boxplot: Mostra a variação da renda em cada nível educacional, 
#incluindo valores atípicos.
# Gráfico de Boxplot
ggplot(dados_educ_renda, aes(x = factor(educ), y = renda)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribuição da Renda por Anos de Escolaridade",
       x = "Anos de Escolaridade",
       y = "Renda Anual (R$)") +
  theme_minimal()

# Correlação de Spearman####
# Primeiro, vamos carregar o conjunto de dados 
# e calcular a correlação de Pearson entre duas 
# variáveis: mpg (milhas por galão) e 
# wt (peso do carro).

# Carregar conjunto de dados
data(mtcars)

# Visualizar as primeiras linhas dos dados
head(mtcars)

# Calcular correlação de Pearson
cor(mtcars$mpg, mtcars$wt)

cor.test(mtcars$mpg, mtcars$wt)

# Neste caso, estamos calculando a correlação 
# entre o consumo de combustível e o peso do carro. 
# Espera-se uma correlação negativa, ou seja, 
# quanto maior o peso, menor a 
# eficiência do carro em termos de consumo.

# Se suspeitarmos que a relação não é linear, 
# podemos utilizar a correlação de Spearman,
# que não exige linearidade entre as variáveis.

# Calcular correlação de Spearman
cor.test(mtcars$mpg, mtcars$wt, method = "spearman")



# Matriz de correlação
# Podemos calcular a correlação entre várias 
# variáveis de uma vez, gerando uma matriz de 
# correlação.

# Matriz de correlação entre algumas variáveis 
#do conjunto de dados
cor(mtcars[, c("mpg", "wt", "hp", "qsec")])

# Para facilitar a interpretação,  é possível visualizar a correlação 
# utilizando um gráfico de calor.

# Instalar e carregar a biblioteca necessária para visualização
install.packages("corrplot")
library(corrplot)

# Criar a matriz de correlação
matriz_cor <- cor(mtcars)

# Plotar o gráfico de correlação
corrplot(matriz_cor, method = "color",
         type = "upper", tl.col = "black")
# Esse gráfico mostra a força e a direção 
# da correlação entre as variáveis. 
# As cores indicam a intensidade da correlação,
# com azul representando correlação positiva e 
# vermelho correlação negativa.

#Correlação de Kendall####
# 
# (Confiança Social e Participação Religiosa)
# Vamos agora criar um exemplo onde avaliamos a correlação 
# entre a confiança nas instituições sociais e a frequência à igreja,
# medidos em escalas ordinais.

# Criar uma base de dados fictícia
set.seed(456)

dados_conf_religiao <- data.frame(
  confiança_instituições = sample(1:5, 100, replace = TRUE),  
  # 1 = Nenhuma confiança, 5 = Muita confiança
  freq_igreja = sample(1:5, 100, replace = TRUE)  
  # 1 = Nunca, 5 = Todo fim de semana
)

# Visualizar as primeiras linhas da base de dados
head(dados_conf_religiao)

# Calcular a correlação de Kendall entre confiança nas
#instituições e frequência à igreja
cor.test(dados_conf_religiao$confiança_instituições,
         dados_conf_religiao$freq_igreja, method = "kendall")

cor(dados_conf_religiao$confiança_instituições, 
    dados_conf_religiao$freq_igreja, method = "kendall")
#[1] -0.05724389

# Neste exemplo fictício, estamos analisando a correlação
# entre a confiança nas instituições sociais e a frequência
# à igreja. Esses dados são frequentemente utilizados para 
# investigar a relação 
# entre crenças religiosas e confiança na sociedade.

help("cor.test")

#correlação- aproximação da regressão#####


options(scipen =999)
plot(BASE_SEN_2022$TOTAL_RECEITA, BASE_SEN_2022$TOTAL_VOTOS)

cor.test(~TOTAL_VOTOS + TOTAL_RECEITA, data = BASE_SEN_2022,
         method = "pearson",
         continuity = FALSE,
         conf.level = 0.95)

#Regressão####
Model <-  lm(TOTAL_VOTOS ~ TOTAL_RECEITA, data = BASE_SEN_2022)

colnames(BASE_SEN_2022)

summary(Model)

BASE_SEN_2022$DS_GRAU_INSTRUCAO <-  (BASE_SEN_2022$DS_GRAU_INSTRUCAO)

Model2 <-  lm(TOTAL_VOTOS ~ TOTAL_RECEITA + NR_IDADE_DATA_POSSE +
                CD_GENERO + CD_GRAU_INSTRUCAO + CD_COR_RACA +
                CD_ESTADO_CIVIL, data = BASE_SEN_2022)
summary(Model2)

Model3 <-  lm(TOTAL_VOTOS ~ TOTAL_RECEITA + NR_IDADE_DATA_POSSE +
                CD_GENERO, data = BASE_SEN_2022)
summary(Model3)


table(BASE_SEN_2022$CD_SIT_TOT_TURNO)
BASE_SEN_2022$Sit_Votacao <- recode(BASE_SEN_2022$CD_SIT_TOT_TURNO, 
                                    0 <- 4, 1 <- 1)


voto1 <- glm(bozo ~ educ, data = ESEB2018, family = binomial(link = logit))

voto1 <- glm(Sit_Votacao~ TOTAL_RECEITA, data = BASE_SEN_2022, family = binomial(
  link = logit))

summary(voto1)


#Regressão####
##Linear simples####

#Primeiro definir o diretório de trabalho
getwd()
setwd("C:/Users/nayar/OneDrive/8. AMBIENTE DE PROGRAMAÇÃO R/1. CURSO 2023/DISCIPLINA LUCAS")

#Pacotes utilizados
library(readr)
library(ggplot2)
library(sjPlot)
library(memisc)
library(ggfortify)
library(olsrr)
library(coefplot)
library(ggpmisc)

install.packages("")


#Abertura da base de dados
library(readr)
sen2018 <- read_delim("sen2018.csv", delim = ";", 
                      escape_double = FALSE, locale = locale(encoding = "latin1"), 
                      trim_ws = TRUE)

#sen2018 <- read.csv2("sen2018.csv", fileEncoding = "latin1")

#Visualização da base de dados####
View(sen2018)   


colnames(sen2018)
# [1] "SQ_CANDIDATO"             "SIGLA_UF"                 "NOME_CANDIDATO"          
# [4] "NUMERO_PARTIDO"           "SIGLA_PARTIDO"            "IDADE_DATA_POSSE"        
# [7] "DESCRICAO_SEXO"           "DESCRICAO_GRAU_INSTRUCAO" "DESCRICAO_ESTADO_CIVIL"  
# [10] "DESCRICAO_COR_RACA"       "DESCRICAO_OCUPACAO"       "DESC_SIT_TOT_TURNO"      
# [13] "SITUACAO_REELEICAO"       "DES_SITUACAO_CANDIDATURA" "votos"                   
# [16] "TOTAL_RECEITAS"          

table(sen2018$DES_SITUACAO_CANDIDATURA)
# APTO 
# 311 

table(sen2018$DESC_SIT_TOT_TURNO)
# ELEITO NÃO ELEITO 
# 54           257 

#O total de votos está relacionado com ter sido eleito? 
#O que será que influencia o total de votos?
#Será que a receita pode ser uma variável preditiva 

#Icialmente temos que deverificar se há relação entre 
# total de votos (que variável é essa?)
# e receita (e essa, como podemos chamá-la?)


#Gráfico dispersão####
#Análise gráfica entre receitas e votos
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point()
#Imagem 1 


#Notem que os valores das receitas são tão altos 
#que aparecem em notação científica, queremos alterar isso?
#Se sim, podemos fazer o seguinte:
options(scipen = 999)
#Imagem 2 

#Organização do gráfico
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receitas", y= "Votos", title = "Receitas por votos") +
  theme_classic()
#Imagem 3

Gra1 <- ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receita", y= "Votos", title = "Receitas por votos") +
  theme_classic()


#Imagem 4 - aprimorando
#inserção da equação da reta 

#pessoal, vejam que agora eu inseri mais algumas linhas 
#no gráfico, essas linhas novas, vão estar marcadas com um "#"
#São para a inserção da equação da reta do modelo 1 de regressão 
# no gráfico, a função utilizada é a " stat_poly_eq", do pacote 
# ggpmisc, então para conseguir rodar esse gráfico, instalem 
#e ativem o pacote. 

library(ggpmisc)

options(scipen = 999)
Gra1 <- ggplot(data = sen2018, 
               aes(y = votos, x= TOTAL_RECEITAS,
                   size = TOTAL_RECEITAS, 
                   color =votos)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T, color = "red")+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label..,#
                                 sep = "*plain(\",\")~~")),#
               label.x = 0.05, label.y = 400,#
               parse = TRUE, coef.digits = 2)+#
  labs(x ="Receita", y= "Votos", 
       title = "Receitas por votos")+ theme_classic() 


formatacao <- theme(text = element_text(family = "serif", size = 14),
      title = element_text(color = "black"),
      axis.line = element_line(color = "black")) +
  theme(legend.position="none") 

# “left”,“top”, “right”, “bottom” and none

Gra2 <- Gra1 + formatacao

#Olhar o gráfico 
Gra2


#Certo, essa já uma visualização gráfica mais próxima 
#do real, no entanto, vejam que as escolas de valores 
#são bastante distintas?

#Então para melhorar a visualização e tornar estatísticamente
#mais proporcional os dados, podemos realizar uma transformação logarítmica
#utilizando a função log


sen2018$log.votos <- log(sen2018$votos)
sen2018$log.receitas <- log(sen2018$TOTAL_RECEITAS)

ggplot(data = sen2018, aes(y = log.votos, x= log.receitas )) +
  geom_point()

#Construção do modelo

Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#Apresentação do Erro
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#  NA/NaN/Inf in 'y'

#Análise da base para identificar o erro

#Correção do erro
sen2018[is.na(sen2018) | sen2018== "Inf"] =NA

sen2018[is.na(sen2018) | sen2018== "-Inf"] =NA

sen2018[is.na(sen2018) | sen2018== "NAN"] =NA

#Modelo após a correção
Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#OU
Model1.1 <- lm(log.votos ~ log.receitas, data = sen2018)

summary(Model1.1)
#Interpretação no slide

# Call:
#   lm(formula = log.votos ~ log.receitas, data = sen2018)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.9274 -0.8677 -0.0294  0.9192  3.7049 
# 
# Coefficients:
#   Estimate Std. Error t value            Pr(>|t|)    
# (Intercept)   7.43396    0.34037   21.84 <0.0000000000000002 ***
#   log.receitas  0.30823    0.02228   13.84 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.366 on 300 degrees of freedom
# (9 observations deleted due to missingness)
# Multiple R-squared:  0.3896,	Adjusted R-squared:  0.3876 
# F-statistic: 191.5 on 1 and 300 DF,  p-value: < 0.00000000000000022

#Construção da tabela que poderá ser apresentada junto ao texto
#Usando a função tab_model do pacote sjPlot
library(sjPlot)
tab_model(Model1.1, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

#Análise de resíduos####


resid <- (cbind(sen2018$log.votos, predict(Model1.1), 
                residuals(Model1.1)))

view(resid)

sen2018[252, c(3, 15, 16)]
#84940        R$2.250.000

#Análise de resíduos através de gráficos#### 
#Função autoplot

library("ggfortify")

obj1 <- autoplot(Model1,
                 which = 1:4,
                 nrow = 2,
                 ncol = 2) 

obj1 + theme_classic() 

#Gráficos:
#1º resíduos pelos valores ajustados, 
#linearidade, aproximadamente horizontal
#O primeiro gráfico exibido é útil para testarmos a
#independência entre valores preditos e resíduos

#2º Distribuição normal, deve estar em cima da linha
#O Normal QQ plot nos ajuda a verificar essa 
#exigência ao exibir no eixo horizontal a distribuição
#esperada em uma distribuição normal e no
#vertical os resíduos padronizados

#3º Homocedasticidade - não pode ter padrão triangular
#4º Mostra pra gente se existem outliers


#Linear multipla#### 

#Quais outras variáveis podem ser interessantes para análise 
#da quantidade de votos?
#Idade na época da posse?
#Sexo?
#Grau de instrução?
#Raça?


#Então incialmente vamos rapidamente olhar e organizar essas variáveis 
#Para pode inserir no teste

summary(sen2018$IDADE_DATA_POSSE)
sen2018$Idade <- sen2018$IDADE_DATA_POSSE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 27.0    47.0    55.0    55.1    63.0    83.0 

table(sen2018$DESCRICAO_SEXO)
sen2018$Sexo <- sen2018$DESCRICAO_SEXO
# FEMININO MASCULINO 
# 56       255 

table(sen2018$DESCRICAO_GRAU_INSTRUCAO)

sen2018$Instrução <- sen2018$DESCRICAO_GRAU_INSTRUCAO
sen2018$Instrução <- as.factor(sen2018$Instrução)
sen2018$Instrução <- recode(sen2018$Instrução, 
                            "Baixa" <-  c("ENSINO FUNDAMENTAL INCOMPLETO",
                              "ENSINO FUNDAMENTAL COMPLETO"),
                            "Média" <-  c("ENSINO M\xc9DIO COMPLETO",
                                          "ENSINO M\xc9DIO INCOMPLETO"),
                            "Superior" <-  c("SUPERIOR COMPLETO",
                                             "SUPERIOR INCOMPLETO"))

table(sen2018$DESCRICAO_COR_RACA)
sen2018$Raca <- sen2018$DESCRICAO_COR_RACA
sen2018$Raca <-  as.character(sen2018$DESCRICAO_COR_RACA)
# IND\xcdGENA     AMARELA      BRANCA       PARDA       PRETA 
#           2           1         210          66          32 


#Modelo de regressão 

Model2 <- lm(log.votos ~ log.receitas + Idade + 
               Sexo + Instrução + Raca, data = sen2018)

summary(Model2)

tab_model(Model2, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "numeric_stars")

#Condionantes para o teste de regressão####
#VIF: Variance Inflation Factors

library(olsrr) # Pacote para VIF



ols_vif_tol(Model2)

# Variables  Tolerance       VIF
# 1      log.receitas 0.93141736  1.073633
# 2             Idade 0.90301139  1.107406
# 3     SexoMASCULINO 0.96812327  1.032926
# 4 InstruçãoSuperior 0.99366053  1.006380
# 5       RacaAMARELA 0.50128312  1.994881
# 6        RacaBRANCA 0.01698194 58.886088
# 7         RacaPARDA 0.02266780 44.115445
# 8         RacaPRETA 0.03725117 26.844794

#Condition Index####

ols_eigen_cindex(Model2)

#Coefplot 

#O Coefplot é um tipo de gráfico que pode ser utilizado
#para apresentar graficamente os resultados de um teste 
#de regressão #pacote coefplot 
library("coefplot")


obj1 <- coefplot(Model2, title = "Análise dos votos a senado em 2018",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

obj2 <- obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 
obj2 + formatacao 


#GUARDAR

table(BASE_SEN_2022$DS_GENERO)


Sen_2022_menor <-  subset(BASE_SEN_2022, DS_GENERO =="MASCULINO")

table(Sen_2022_menor$DS_COR_RACA)
table(Sen_2022_menor$CD_COR_RACA)

library(memisc)
Sen_2022_menor$DS_GRAU_INSTRUCAO <-  as.factor(Sen_2022_menor$DS_GRAU_INSTRUCAO)

Bra2014Menor$PercRend <- recode(Bra2014Menor$PercRend, 
                                "Satisfeito" <- c(1,2),
                                "Insatisfeito" <- c(3,4))

Sen_2022_menor$Escol <- recode(Sen_2022_menor$CD_GRAU_INSTRUCAO,
                               "Baixa_Esc" <- c(2,3,4,5,6,7), 
                               "Alta_Esc" <- c(8))

table(Sen_2022_menor$Escol)
