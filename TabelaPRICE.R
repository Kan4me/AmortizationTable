##Tabela PRICE 

################
# Desenvolver uma função que consiga construir uma planilha de amortização
# conforme o Sistema Francês de Amortização (PRICE). Após montar, calcule 
#a planilha para o caso de um financiamento no valor de R$ 1000 que deverá 
# ser pago em 4 meses à taxa anual de 42,576%

#ATT: comandos para exportar a tabela gerada em XLSX, seja em uma pasta
#local ou no RStudio Cloud, conforme o PATH de interesse.

install.packages("xlsx")
library("xlsx")

# Montando a função geral

PRICE <- function(P, n, i, type_i){
  if (type_i == month) {
  ie = i
  } else if (type_i == year) {
  ie <- ((1+i)^(1/12))-1 #Juros equivalente mensal
  } else {
  print("Tipo de taxa de juros inválida. Tente novamente")
  break
  }

  PMT <- P*((((1+ie)^n)*ie)/(((1+ie)^n)-1)) #Valor da prestação
  
  c <- n+2 #automatização nomeação das linhas
  
  vPMT <- matrix(c(0,rep(PMT, n)), ncol = 1 , nrow = n+1) 
  vJ <- matrix(0, ncol = 1, nrow = n+1)
  vSD <- matrix(0, ncol = 1, nrow = n+1)
  vAmort <- matrix(0, ncol = 1, nrow = n+1)
  vMes <- matrix(0, ncol = 1, nrow = n+2)
  
  vSD[1] <- P #Saldo devedor no tempo 0 
  
  
  #O for abaixo começa na posição 2 porque a linha 2 da matriz é o mês 1
  
  for (t in 2:n){
    
    vJ[t] <- vSD[t-1] * ie #Juros do período
    
    vAmort[t] <- vPMT[t] - vJ[t] #Valor da amortização
    
    vSD[t] <- vSD[t-1] - vAmort[t] #Saldo devedor
    
  }

  
  #Valores do último mês do financiamento
  
  vJ[n+1] <- vSD[n] * ie #Juros do período
  
  vAmort[n+1] <- vPMT[n+1] - vJ[n+1] #Valor da amortização  do último mês do financiamento
  
  vSD[n+1] <- round(vSD[n], 2) - round(vAmort[n+1], 2) #Saldo devedor do último 
  #mês do financiamento  
  
  svPMT <- sum(vPMT) #Soma das prestações
  svAmort <- sum(vAmort) #Soma da amotização
  svJ <- sum(vJ) #Soma dos juros
  
  soma <- cbind(svPMT, svAmort, svJ, 0) #O 0 no final desse comando corresponde
  #a quitação do financiamento 
  
  resultado <- cbind(vPMT, vAmort, vJ, vSD)
  resultado <- rbind(resultado, soma)
  
  
  #Automatização nomeação das linhas para qualquer n
  
  for (w in 0:c){
    
    if (w < c){
      
      vMes[w] <- w-1
      
    } else {
      
      vMes[w] <- "Total"
      
    }
    
  }
  
  
  colnames(resultado) <- c('Prestações', 'Amortização',
                           'Juros', 'Saldo Devedor')
  
  row.names(resultado) <- vMes
  
  #write.xlsx(matrix or df,'/PATH/NameFile.xlsx')
  write.xlsx(resultado,'/[PATH_OF_INTEREST]/TabelaPRICE.xlsx')
  
  return(resultado)
  
}

#Os termos da função geral são: 
#P: valor de interesse financiado
#n: número de meses de pagamento do financiamento
#i: taxa de juros anual efetiva


#Função PRICE(P, n, i)

PRICE(1000, 4, 0.42576, year)

PRICE(775000, 48, 0.21, year)
