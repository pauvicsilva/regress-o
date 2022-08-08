# regressao

library(RColorBrewer)
library(GGally)
library(scatterplot3d)
library(corrgram)
# library(stringr)

dados=read.csv2(file.choose(), header=T)
dados$Market = as.numeric(dados$Market)

# dados$Market=dados$Market/100
# dados$Participacao=dados$Participacao/100
# transformas das em uma matriz numerica

mm=dados[,8:43]
m = as.matrix.data.frame(mm)


# d =data.frame(Market,Industry,C,MotorGraders,H,FF)
# m = as.matrix.data.frame(d)

# criar um data,frame com a matriz de correlação

corre= data.frame(cor(m))
View(corre)

# definir alpha, que seria correlações mínima a serem consideradas no modelo.
# e plotar data.frame matriz de correlação, somente com as variaveis que
# atendem a apha.
#
# EX: alpha = 0.10 => variaveis com corelação > 0.10 ou < -0.10.

alpha= 0.20
mult=which(corre$Market>(alpha) | corre$Market<(-alpha))
d=data.frame(corre[mult,mult] )
View(d)

df=mm[,mult]

# pode pular a parte 3D !!!!!!!!!!!!!!!!!!!

############### Função par 3d  ###############
# colocar 10 cores em "cores"
# usar padonização 0 a 1. e dividir por 10

tab=dados$Market
div=10
colorir = function(tab,div){
  pad=c( tab - min(tab) )/( max(tab) - min(tab) )
  
  id = 1+as.integer(pad*div)
  cores = c("#C0B4B4", "#C0B4B4", "#00FFFB","#00B2FF","#BBFF00","#00FF11","#FFFF00","#FF9D00","#FF0044","#FF0000", "#252020")
  #  "#0DFF00", = cores[...,3, ...]
  legendas = c()
  for (i in 1:length(id)){
    legendas[i] =  cores[id[i]]
    paste(id[i],cores[id[i]])
    
  }
  return(legendas)
}
legendas=colorir(dados$Market,10)
i=1:63
paste(id[i],cores[id[i]])

####################################
x = dados$M_BHL_Class_A 
y = dados$M_750J_850J
z = dados$Motor.Graders

scatterplot3d(x, y, z, pch = 19, type="h", color =legendas) 
###################################################
# pular até aqui !!!!!!!!!!!!!!!!


pairs(df)
ggpairs(df, lower = list(continuous = "smooth"))

# ggpairs(df, columns = 1:4, ggplot2::aes(colour=legendas))

corrgram(d)
df=data.frame(x,y,z,d$Market)
corrgram(df, lower.panel = panel.pts, upper.panel= panel.conf, diag.panel = panel.density)
#
# df=data.frame(m[,c(mult)])

###################################

df = mm[,mult]

# ajuste=lm(df$Market ~ df$TO + df$A + df$H + df$Motor.Graders + df$M_750J_850J + df$M_BHL_Class_A + df$M_LDR_A )
# df$M_750J_850J + df$M_BHL_Class_A + df$TO

ajuste=lm(df$Market ~  df$M_620G_622G + df$MotorGraders )

ajuste

summary(ajuste)
plot(ajuste)


summary( lm(df$Market ~ df$H ))

cor.test(df$Market, df$M_750J_850J )
plot(df$Government, df$Market)

############ RESIDUOS ####################

windows()
plot(1:length(df$Market),residuals(ajuste),xlab="Ordem",ylab="Resíduos")
abline(h=0)
windows()
plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)
windows()
plot(df$Market,residuals(ajuste),xlab="Market",ylab="Resíduos")
abline(h=0)
