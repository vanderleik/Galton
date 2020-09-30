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

# Estou usando o tidyverse para trabalhar com os dados, porque ele engloba uma série de pacotes para análise e manipulação de dados.
library(tidyverse)
library(UsingR)
library(ggpubr)

# Como os dados estão estão em polegadas e no Brasil trabalhamos com centímetros, o primeiro passo foi converter os dados.
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

# A altura dos pais varia entre 162,6cm e 185,4cm, enquanto os filhos tem como amplitude os valores mínimo de 156,7cm e máximo de 187,2cm.
# O interessante é que os filhos tem uma altura mínima menor e uma altura máxima maior do que os pais, e no entanto, a média e a mediana da altura dos filhos é menor do que a dos pais.

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

range(galton$child)
diff(range(galton$child))

range(galton$parent)
diff(range(galton$parent))

# Analisando os resultados das medidas de dispersão podemos ver que a altura dos filhos tem uma variabilidade maior do que a dos pais.
# Lembrando que a amplitude das medidas de altura dos pais é menor do que a dos filhos. Enquanto os filhos tem altura mínima e máxima variando entre 156,718 e 187,198, ou uma amplitude de 30,48cm, enquanto os pais tem altura mínima e máxima variando entre 162,56 e 185,42, ou uma amplitude de 22,86cm. 

# Boxplot
boxplot(galton$child, main = "Boxplot para a altura dos filhos", ylab = "Child (cm)")
boxplot(galton$parent, main = "Boxplot para a altura dos pais", ylab = "Parent (cm)")

# Não conseguimos identificar dados discrepantes nos dados de altura dos filhos, mas no caso da altura dos pais parece haver dois dados discrepantes, que são os dados extremos.
# Porém, esses outliers não precisam de nenhum tratamento, pois não afetam o comportamento investigado neste estudo.

# Histograma
hist(galton$child, main = "Histograma para a altura dos filhos", xlab = "Altura (cm)")
hist(galton$parent, main = "Histograma para a altura dos pais", xlab = "altura (cm)")

# O histograma mostra novamente os dados extremos da altura dos pais, mas como há uma distribuição mais concentrada, esses dados extremos não afetam o resultado da análise.
# Por outro lado, o histograma dos filhos mostra como de fato os dados são mais dispersos do que os a distribuição das alturas dos pais, como se pode observar pelas medidas de dispersão.

# Listando as categorias das variáveis

table(galton$child)
table(galton$parent)

# Calculando a proporção de cada categoria
model_table1 <- table(galton$child)
model_table2 <- table(galton$parent)
prop.table(model_table1) #retorna a proporção % de filhos em cada categoria de altura
prop.table(model_table2) #retorna a proporção % de pais em cada categoria de altura

# Graficamente a diferença fica clara
barplot(prop.table(table(galton$child)), xlab = "child")
barplot(prop.table(table(galton$parent)), xlab = "parent")

# Gráfico de dispersão com linha de regressão
ggscatter(galton, x = "parent", y = "child", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Pais", ylab = "Filhos", title = "Gráfico de dispersão da altura dos pais e filhos")

# Vou criar duas novas colunas (variáveis categóricas), onde eu classifico os indivíduos em baixo, mediano, alto e muito alto.
# O critério para definir essas alturas é relativa a cada uma das variáveis, child e parent.
# Para fazer essa divisão dos dados eu utilizo os quartis obtidos no summary logo acima.

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

head(galton)
# Agora podemos observar que alguns indivíduos baixos são filhos de pais considerados baixos, mas alguns outros são filhos de pais medianos, altos ou muito altos. O mesmo acontece com as demais categorias definidas.

ggplot(data = galton) +
  geom_point(mapping = aes(x = parent, y = child, color = child_group)) +
  labs(title="Gráfico de dispersão da altura dos filhos e dos pais",  
       y="Altura dos filhos",x="Altura dos pais", caption="") 

ggplot(data = galton) +
  geom_point(mapping = aes(x = parent, y = child)) +
  facet_wrap(~child_group) +
  labs(title="Gráfico de dispersão da altura dos filhos e dos pais",  
       y="Altura dos filhos",x="Altura dos pais", caption="") 

# O gráfico de disperão mostra claramente que à medida em que a média da altura dos pais aumenta, a média da altura dos filhos almenta também, indicando a possibilidade de termos uma correlação positiva entre essas duas variáveis.

# Covariância
covar = cov(galton$parent, galton$child)
covar

# Correlação
correl = cor(galton$parent, galton$child)
correl

# Construindo a Matriz de Correlação
cor(galton[c("child", "parent")])


# A covariância indica haver algum grau de cariação conjunta entre as varáveis, mas a medida de correlação, de 0,46, dá uma ideia de correlação não muito forte entre elas.

# Vou dividir a amostra em dois grupos. 70% para treinamento e 30% para previsão
linhas <- sample(1:nrow(galton), 0.7 * nrow(galton))
dados_treino <- galton[linhas,]
dados_teste <- galton[-linhas,]

View(dados_treino)
View(dados_teste)

# Para treinar o modelo de regressão eu uso a função "lm".

modelo1 <- lm(child ~ parent, data = dados_treino)
summary(modelo1)

# Os coeficientes estimados são estatisticamente significativos, tanto o intercepto quando o coeficiente angular, o que significa que há uma relação entre essas variáveis.
# Em média, para cada centímetro a mais de altura dos pais, 0,63cm são transferidos aos filhos, na forma de herança genética.
# Dito de outra forma, um pai de 180cm de altura, por exemplo, esperamos que tenha um filho com 177,10cm de altura.
# Porém, há uma parcela expressiva da altura dos filhos que não pode ser explicada pela altura dos pais. Ou seja, quando nasce um filho, sabemos que pelo menos 63cm da sua altura não é explicada pela altura dos seus pais.

# Agora fazemos previsões com o modelo usando dados de teste
previsoes <- predict(modelo1, dados_teste)
summary(previsoes)
summary(galton$child)

# Obtendo os resíduos
res <- residuals(modelo1)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

plot(modelo1)

# Visualizando os valores previstos e observados
resultados <- cbind(previsoes, dados_teste$child) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
min(resultados)
max(resultados)
head(resultados)
View(resultados)

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(galton$child) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

