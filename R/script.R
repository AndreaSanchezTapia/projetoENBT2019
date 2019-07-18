sal <- read.csv("data/salarios.csv")

# explore os dados com as funções head e summary
# criando objetos para auxiliar a construção do gráfico
# criando modelos lineares

mh <- lm(salario ~ experiencia, data = sal[sal$sexo == "H", ])
mm <- lm(salario ~ experiencia, data = sal[sal$sexo == "M", ])
coefh <- coef(mh)
coefm <- coef(mm)

# definindo os limites dos eixos
limy <- c(min(sal$salario), max(sal$salario))
limx <- c(min(sal$experiencia), max(sal$experiencia))

## definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"

# define parametros graficos
par(mfrow = c(1, 2), las = 1,bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas


# plot dos valores de salario dos homens

plot(salario ~ experiencia, data = sal[sal$sexo == "H", ],
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby, xlab = labx)

# linha do previsto pelo modelo
## a + b*x
abline(a = coefh[1], b = coefh[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot do salario das mulheres
plot(salario ~ experiencia, data = sal[sal$sexo == "M", ],
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "", xlab = labx)
mtext("B", 3, adj = 0, font = 2)

# linha do previsto pelo modelo
## a + b*x
abline(a = coefm[1], b = coefm[2],
       col = 'navy', lwd = 2)


# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("figs/figura01.png",
    res = 300, width = 2400, height = 1200)

# define parametros graficos
par(mfrow = c(1, 2), las = 1, bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas

# plot dos valores de salario dos homens
plot(salario ~ experiencia, data = sal[sal$sexo == "H", ],
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby, xlab = labx)

# linha do previsto pelo modelo

## a + b*x
abline(a = coefh[1], b = coefh[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot do salario das mulheres
plot(salario ~ experiencia, data = sal[sal$sexo == "M", ],
        col = "navy",
        ylim = limy, xlim = limx,
        ylab = "", xlab = labx)
mtext("B", 3, adj = 0, font = 2)

# linha do previsto pelo modelo
## a + b*x
abline(a = coefm[1], b = coefm[2],
       col = 'navy', lwd = 2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()



plot(salario ~ experiencia, data = sal[sal$sexo == "H", ],
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby, xlab = labx)

# linha do previsto pelo modelo
## a + b*x
abline(a = coefh[1], b = coefh[2],
       col = 'tomato', lwd = 2)

## usando points para adicionar os pontos do salario das mulheres
points(salario ~ experiencia, data = sal[sal$sexo == "M", ],
       col = "navy")

# linha do previsto pelo modelo das mulheres
## a + b*x
abline(a = coefm[1], b = coefm[2],
       col = 'navy', lwd = 2)

# incluindo a legenda
legend("topleft", legend = c("homens", "mulheres"),
       col = c("tomato", "navy"),
       lty = 1, bty = 'n')

cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")


par(mfrow = c(2, 2), bty = 'l', las = 1)
boxplot(Sepal.Length ~ Species, data = iris,
        xlab = "", col = cores)
boxplot(Sepal.Width ~ Species,
        data = iris, col = cores)
boxplot(Petal.Length ~ Species,
        data = iris, xlab = "", col = cores)
boxplot(Petal.Width ~ Species,
        data = iris, col = cores)
par(mfrow = c(1, 1))

