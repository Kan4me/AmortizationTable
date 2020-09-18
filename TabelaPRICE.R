# Montando a função geral

PRICE <- function(P, n, i){
  
  ie <- ((1+i)^(1/12))-1 #Juros equivalente mensal
  
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
  
  return(resultado)
  
}

#Os termos da função geral são: 
#P: valor de interesse financiado
#n: número de meses de pagamento do financiamento
#i: taxa de juros anual efetiva


#Função PRICE(P, n, i)

PRICE(1000, 4, 0.42576)

PRICE(775000, 48, 0.21)