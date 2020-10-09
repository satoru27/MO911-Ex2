# Leitura de dados
mydata = read.csv("ex1.csv")
mydata$type <- as.factor(mydata$type)

# separação dos dados entre pacientes com diabetes e sem diabetes
data_yes = mydata$bp[mydata$type == "Yes"]
data_no = mydata$bp[mydata$type == "No"]

# intervalo de confiança usando o teste t
t.test(data_yes)$conf.int
t.test(data_no)$conf.int

# intervalo de confiança usando o teste de wilcox
wilcox.test(data_yes,conf.int=T)$conf.int
wilcox.test(data_no,conf.int=T)$conf.int

# requisitos do bootstrap
library(boot)
auxf <- function(dado,indice){
  return(mean(dado[indice]))
}

# calculo do CI para pacientes diabeticos por meio do bootstrap
bb_yes = boot(data_yes,R=5000, statistic=auxf)
boot.ci(bb_yes,type="bca")



# calculo do CI para pacientes nao diabeticos por meio do bootstrap
bb_no = boot(data_no,R=5000, statistic=auxf)
boot.ci(bb_no,type="bca")



# Interceção dos intervalos de confiança
ci_boot_yes = boot.ci(bb_yes,type="bca")$bca[1,4:5]
ci_boot_no = boot.ci(bb_no,type="bca")$bca[1,4:5]
ci_boot_yes
ci_boot_no


# Tamanho de efeito
# Calcule o Cohen D com o pooled standard deviation para a diferença
# entre a pressão sangüínea de pacientes com e sem diabetes.
#
# diferença das médias dividido pelo desvio padrão "dos dois conjuntos"
# a media ponderada dos desvios padrão dos dois conjuntos. Essa abordagem
# é chamada de pooled standard variation.


mean_yes = mean(data_yes)
sd_yes = sd(data_yes)

mean_no = mean(data_no)
sd_no = sd(data_no)

# para o calculo do pooled standard deviation é utilizado a formula em
# # https://www.statisticshowto.com/pooled-standard-deviation/

pooled_sd = sqrt( ((length(data_yes)-1)*sd_yes^2 + (length(data_no)-1)*sd_no^2)/
                    (length(data_yes) + length(data_no) - 2) )

cohen_d = (mean_yes - mean_no) / pooled_sd
cohen_d


# Intervalo de confiança para o tamanho de efeito
install.packages("effsize")
library(effsize)
cohen_d_new = cohen.d(data_yes, data_no, pooled=TRUE) 
# fazendo pooled para comparar com o resultado anterior, ambos foram iguais

cohen_d_new$conf.int

# Isso resume tudo que é importante: se o intervalo de confiança cruza o 0
# então tradicionalmente a diferença não é significativa.
# (0.1503741, 0.7456945) -> o intervalo não cruza o 0 então ele é
# estatisticamente significativo
