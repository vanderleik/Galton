# Portfólio de Data Science

# Projeto nr. 01 - Altura dos filhos versus altura dos pais

# Dados utilizados por Francis Galton na análise da altura dos filhos em comparação à altura dos pais
# A famosa comparação de Galton das alturas de 928 filhos adultos com as de seus 205 pares de pais (pai e mãe).
# Quando os pais são mais altos que a média, seus filhos tendem a ser mais baixos (ou seja, mais próximos da média)
# e quando os pais são mais baixos que a média, seus filhos tendem a ser mais altos. 
# Galton chamou isso de "regressão à mediocridade".

# Variáveis:
# parent = altura do pai/mãe em polegadas (amplitude 64 - 73)
# child  = altura da criança em polegadas (amplitude 61.7 - 73.7)

# Fonte:
# Galton, F., "Regression Towards Mediocrity in Hereditary Stature," Journal of the Anthropological Institute of Great Britain and Ireland, 15, 246-263, 1886.
# Os dados estão disponíveis no pacote UsingR.

# Diretório de trabalho
setwd("C:/FCD/PortfolioR/Galton")
getwd()

# Pacotes carregados para a análise a seguir.

library(tidyverse)
library(UsingR)
library(ggpubr)

# Como os dados estão estão em polegadas e no Brasil trabalhamos com centímetros, o primeiro passo é converter os dados.
# Uma polegada equivale a aproximadamente 2,54cm.

galton <- 2.54 * galton

View(galton)

# Estatísticas descritivas

# Optei por dividir as estatísticas descritivas em dois grupos:

# 1 - Medidas de tendência central: média, mediana, moda, valores mínimo e máximo e amplitude;
# 2 - Medidas de dispersão: desvio padrão, variância e coeficiente de varição.

# 1 - Medidas de tendência central: média, mediana, moda, valores mínimo e máximo e amplitude;

summary(galton$child)
summary(galton$parent)

range(galton$child)
diff(range(galton$child)) # amplitude

range(galton$parent)
diff(range(galton$parent)) # amplitude

# Criando função para obter a moda
obter_moda <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# Obtendo a moda
moda_child <- obter_moda(galton$child)
moda_child
moda_parent <- obter_moda(galton$parent)
moda_parent

# Contagem de vezes que a moda aparece no dataset
filter(galton, child == 175.768) %>%
  count()
filter(galton, parent == 173.99) %>%
  count()

# A amplitude da altura dos filhos é maior do que a amplitude da altura dos pais. Enquanto a altura dos pais varia entre 162,6cm e 185,4cm, a dos filhos varia entre 156,7cm 187,2cm.
# A altura média dos filhos é de 172,9cm enquanto a altura média dos pais é ligeiramente maior, 173,5Ocm.
# Analisando a mediana, vemos que a mediana dos pais é quase um centímetro maior que a dos filhos enquanto a moda dos filhos é quase dois centímetros maior. Porém, a altura de 175,768cm (moda) aparece 167 vezes para os filhos enquanto a altura de 173,99cm (moda) aparece 219 vezes para os pais.

# 2 - Medidas de dispersão: desvio padrão, variância e coeficiente de varição.

devPadPais = sd(galton$parent)
devPadFilhos = sd(galton$child)
varPais = var(galton$parent)
varFilhos = var(galton$child)
mediaPais = mean(galton$parent)
cvPais = (devPadPais/mediaPais)*100
mediaFilhos = mean(galton$child)
cvFilhos = (devPadFilhos/mediaFilhos)*100

# Criando vetores e colocando dados
estatisticas = c("Desvio Padrão", "Variância", "Coeficiente de Variação")
child = c(round(devPadFilhos, digits = 2), round(varFilhos, digits = 2), round(cvFilhos, digits = 2))
parent = c(round(devPadPais, digits = 2), round(varPais, digits = 2), round(cvPais, digits = 2))


# passo o nome dos vetores (sem precisar usar o "c")
dispersao = data.frame(estatisticas, child, parent)
dispersao

# Podemos também estimar uma medida da variância conjunta dos dados, através da covariância, e uma medida do grau de associação linear entre as variáveis, com o coeficiente de correlação.
# Covariância
covar = cov(galton$parent, galton$child)
covar

# Correlação
correl = cor(galton$parent, galton$child)
correl

# Analisando os resultados das medidas de dispersão podemos ver que a altura dos filhos tem uma variabilidade maior do que a dos pais em torno da média. O mesmo pode ser visto pelo coeficiente de variação, que é uma medida padronizada de dispersão.
# A covariância indica haver algum grau de variação conjunta positiva entre as varáveis, mas a medida de correlação, de 0,46, dá uma ideia de uma associação não muito forte entre elas.

# Boxplot
# Quero comparar os dois boxplots lado a lado, por isso uso a função 'par'
par(mfrow = c(1, 2), oma = c(4, 1, 1, 1))
boxplot(galton$child, main = "Boxplot para a altura dos filhos", ylab = "Child (cm)")
boxplot(galton$parent, main = "Boxplot para a altura dos pais", ylab = "Parent (cm)")

# Não conseguimos identificar dados discrepantes na variável altura dos filhos, mas no caso da altura dos pais parece haver valores extremos tanto na borda superior quanto inferior.
# Não vamos nos preocupar com esses outliers por enquanto, acredito que eles não irão afetar o resultado do modelo que irei estimar na sequência.

# Histograma
par(mfrow = c(1, 2), oma = c(4, 1, 1, 1))
hist(galton$child, main = "Histograma para a altura dos filhos", xlab = "Altura (cm)")
hist(galton$parent, main = "Histograma para a altura dos pais", xlab = "Altura (cm)")

# O histograma mostra novamente os dados extremos da altura dos pais. Ele mostra também como os dados são mais dispersos no caso da distribuição da altura dos filhoes, comparativamente à distribuição da altura dos pais, como se pode observar pelas medidas de dispersão.

# Quero analisar os dados em forma de categoria, para poder definir uma estatura relativa para cada variável.

table(galton$child)
table(galton$parent)

# Calculando a proporção de cada categoria
model_table1 <- table(galton$child)
model_table2 <- table(galton$parent)
prop.table(model_table1) #retorna a proporção % de filhos em cada categoria de altura
prop.table(model_table2) #retorna a proporção % de pais em cada categoria de altura

# Graficamente talvez seja melhor para entender a diferença entre as séries
par(mfrow = c(1, 2), oma = c(4, 1, 1, 1))
barplot(prop.table(table(galton$child)), xlab = "child")
barplot(prop.table(table(galton$parent)), xlab = "parent")

# Vou criar duas novas colunas (variáveis categóricas), onde eu classifico os indivíduos em baixo, mediano, alto e muito alto, usando como critério os quartis definidos anteriormente nas estatísticas descritivas, que se mostraram mais adequados. Porém, eu poderia também usar a "prop.table" pra isso, mas não quero fazer isso nesse momento.

group_child <- function(child){
  if (child >= 156 & child <= 168){
    return('baixo')
  }else if(child > 168 & child <= 173){
    return('mediano')
  }else if (child > 173 & child <= 178){
    return('alto')
  }else if (child > 178){
    return('muito alto')
  }
}

group_parent <- function(parent){
  if (parent >= 162 & parent <= 171){
    return('baixo')
  }else if(parent > 171 & parent <= 174){
    return('mediano')
  }else if (parent > 174 & parent <= 176){
    return('alto')
  }else if (parent > 176){
    return('muito alto')
  }
}


# ajusta essas transformações no dataset
galton$child_group <- sapply(galton$child, group_child)
galton$child_group <- as.factor(galton$child_group)

galton$parent_group <- sapply(galton$parent, group_parent)
galton$parent_group <- as.factor(galton$parent_group)

View(galton)

# Nos gráficos gerados a partir da categorização é possível observar que a medida que a altura dos pais aumenta, a altura dos filhos também aumenta, mas não na mesma proporção. Isso corrobora com a tese de Galton sobre a regressão à mediocridade, ou seja, a tendência dos filhos terem altura mais próximas da média.

ggplot(data = galton) +
  geom_point(mapping = aes(x = parent, y = child, color = child_group)) +
  labs(title="Gráfico de dispersão da altura dos filhos e dos pais",  
       y="Altura dos filhos",x="Altura dos pais", caption="") 

ggplot(data = galton) +
  geom_point(mapping = aes(x = parent, y = child)) +
  facet_wrap(~child_group) +
  labs(title="Gráfico de dispersão da altura dos filhos e dos pais",  
       y="Altura dos filhos",x="Altura dos pais", caption="") 

# O gráfico de disperãoa abaixo mostra que à medida em que a média da altura dos pais aumenta, a média da altura dos filhos almenta também, indicando a possibilidade de termos uma correlação positiva entre essas duas variáveis.

ggscatter(galton, x = "parent", y = "child", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Pais", ylab = "Filhos", title = "Gráfico de dispersão da altura dos pais e filhos")

# Entrando na reta final, Vou dividir a amostra em dois grupos, sendo 70% para treinamento e 30% para teste. A partir daí eu treino o modelo e verifico a sua acurácia ou capacidade de prever a altura dos filhos, tendo como base apenas a altura dos pais (uma espécie de transferência de altura, ou hereditariedade).
linhas <- sample(1:nrow(galton), 0.7 * nrow(galton))
dados_treino <- galton[linhas,]
dados_teste <- galton[-linhas,]

View(dados_treino)
View(dados_teste)

# Eu gosto de analisar o resultado de um modelo estimado, mesmo que seja com dados de treino. Isso me ajuda a verificar se o modelo está atendendo àquilo que se espera dele, a priori.
# No caso deste estudo, o que eu espero encontrar é uma relação positiva e significativa entre a variável dependente, child, e a variável explicativa, parent.
# Isso significa que o coeficiente estimado "parent" tem que ter sinal positivo e tem que ser estatisticamente significativo.
# Para treinar o modelo de regressão eu uso a função "lm".

modelo1 <- lm(child ~ parent, data = dados_treino)
summary(modelo1)

# Uma vez que o modelo tenha sido treinado, podemos ver que o sinal dos coeficientes estimados estão de acordo com o que se esperava. Ou seja, o intercepto é positivo, até mesmo porque não faria sentido ele ser negativo. Ele representa a soma de todas as variáveis que de alguma forma influenciam a altura dos filhos de forma significativa, mas que não estamos considerando neste modelo.
# Em termos numéricos, sem considerar a altura dos pais, os filhos terão pelo menos 57cm de altura. É estranho pensar assim, mas podemos pensar também que os pais não são capazes de influenciar pelo menos 57cm da altura dos filhos!
# O coeficiente angular, determina a capacidade de transferência da altura dos pais para os filhos. Para cada centímetro a mais de altura dos pais, os filhos terão 0,67cm a mais de altura. Note que não é proporcional, mas é positivo. Já tínhamos visto acima, pelas outras estatísticas, que haveria uma relação positiva entre a altura dos pais e dos filhos, mas que essa relação não seria muito forte. Isso significa que existem outros fatores que afetam a altura dos filhos, e que não é apenas a hereditariedade.
# Do ponto de vista da significância estatística, os dois coeficientes estimados são estatisticamente significativos, ao nível de 1% de significância estatística. A probabilidade de se cometer um erro do tipo 1 é praticamente nulo.
# Olhando agora o R quadrado, vemos que o nosso modelo é capaz de explicar apenas 22,32% da altura dos filhos. Como dito anteriormente, há outras variáveis que não foram consideradas neste estudo e que afetam de forma significativa a variável dependente.
# Em termos práticos, a capacidade preditiva do modelo é bem fraca!


# Vamos prosseguir com a análise, apenas a título de exercício, tendo em vista a qualidade dos resultados apresentados.
previsoes <- predict(modelo1, dados_teste)
summary(previsoes)
summary(galton$child)

# Vamos dar uma olhada rápida no resumo estatístico dos resultados previstos comparativamente com os dados de treino. O modelo super estima os valores mínimos e subestima os valores máximos da série de dados.
# Tanto a média quanto a mediana são previstos de forma quase precisa, o que é bem interessante.

# Visualizando os valores previstos e observados
resultados <- cbind(previsoes, dados_teste$child) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
min(resultados)
max(resultados)

View(resultados)
