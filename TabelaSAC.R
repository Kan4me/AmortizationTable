##Tabela SAC 

################
# Desenvolver uma função que consiga construir uma planilha de amortização
# conforme o Sistema de Amortização Constante (SAC). Após montar, calcule 
#a planilha para o caso de um financiamento no valor de R$ 775000 que deverá 
# ser pago em 48 meses à taxa anual de 21%

#ATT: comandos para exportar a tabela gerada em XLSX, seja em uma pasta
#local ou no RStudio Cloud, conforme o PATH de interesse.

install.packages("xlsx")
library("xlsx")

# Montando a função geral

SAC <- function(P, n, i){
  
  ie <- ((1+i)^(1/12))-1 #Juros equivalente mensal
  
  A <- P/n #Valor da amortização
  
  c <- n+2 #automatização nomeação das linhas
  
  vA <- matrix(c(0,rep(A, n)), ncol = 1 , nrow = n+1) 
  vJ <- matrix(0, ncol = 1, nrow = n+1)
  vSD <- matrix(0, ncol = 1, nrow = n+1)
  vR <- matrix(0, ncol = 1, nrow = n+1)
  vP <- matrix(0, ncol = 1, nrow = n+2)
  
  vSD[1] <- P #Saldo devedor no tempo 0 
  vJ[1] <- 0 #Juros no tempo 0
  vR[1] <- 0 #Valor da prestação no tempo 0
  
  
  #O for abaixo começa na posição 2 porque a linha 2 da matriz é o mês 1
  
  for (t in 2:n){
    
    vSD[t] <- vSD[t-1] - vA[t] #Saldo devedor
    
    vJ[t] <- vSD[t-1] * ie #Juros do período
    
    vR[t] <- vA[t] + vJ[t] #Valor da prestação
    
  }
  
    #Valores do último mês do financiamento
  
  vSD[n+1] <- round(vSD[n], 2) - round(vA[n+1], 2) #Saldo devedor do último 
                                                  #mês do financiamento
  
  vJ[n+1] <- vSD[n] * ie #Juros do período
  
  vR[n+1] <- vA[n+1] + vJ[n+1] #Valor da prestação  do último mês do financiamento
  
  
  svR <- sum(vR) #Soma das prestações
  svA <- sum(vA) #Soma da amotização
  svJ <- sum(vJ) #Soma dos juros
  
  soma <- cbind(svR, svA, svJ, 0) #O 0 no final desse comando corresponde
  #a quitação do financiamento 
  
  resultado <- cbind(vR, vA, vJ, vSD)
  resultado <- rbind(resultado, soma)
  
  
  #Automatização nomeação das linhas para qualquer n
  
  for (w in 0:c){
    
    if (w < c){
      
        vP[w] <- w-1
        
    } else {
      
      vP[w] <- "Total"
      
    }

  }
  
  
  colnames(resultado) <- c('Prestações', 'Amortização',
                           'Juros', 'Saldo Devedor')
  
  row.names(resultado) <- vP
  
  #write.xlsx(matrix or df,'/PATH/NameFile.xlsx')
  write.xlsx(resultado,'/cloud/project/TabelaPRICE.xlsx')
  
  return(resultado)
  
}

#Os termos da função geral são: 
#P: valor de interesse financiado
#n: número de meses de pagamento do financiamento
#i: taxa de juros anual efetiva


#Função SAC(P, n, i)

SAC(775000, 48, 0.21)

SAC(1720000, 50, 0.142857)

