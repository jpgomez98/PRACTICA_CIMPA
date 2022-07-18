
Cantones = unique(basecanton$Canton)[1]
Eval.pd = NULL
Eval.tot = NULL
p1 = list()
p2 = list()
df1 = list()
df2 = list()
Predicciones = matrix(NA, ncol = 2, nrow = 3*length(Cantones))
Index = seq(1,3*length(Cantones), 3)

LI <- list()
LS <- list()
df3 <- list()

for (i in 1:length(Cantones)) {
  
  X_trainc = X_train %>% filter(Canton == Cantones[i])
  X_trainc = as.matrix(X_trainc[,-1])
  y_trainc = y_train %>% filter(Canton == Cantones[i])
  y_trainc = as.matrix(y_trainc[,-1])
  
  X_testc = X_test %>% filter(Canton == Cantones[i])
  X_testc = as.matrix(X_testc[,-1])
  y_testc = y_test %>% filter(Canton == Cantones[i])
  y_testc = as.matrix(y_testc[,-1])
  
  X_all = basecanton2 %>% filter(Canton == Cantones[i])
  X_all = as.matrix(X_all[,-c(1,23)])
  
  base = as.data.frame(basecanton %>% filter(Canton == Cantones[i]) %>% dplyr::select(RR))
  
  
  
  model %>% compile(loss = "mse", 
                    optimizer = optimizer_adam(lr = 0.0007),
                    metric = "mean_absolute_error")
  trained_model <- model %>% fit(
    x = X_trainc, 
    y = y_trainc, 
    batch_size = 18, 
    epochs = 100, 
    validation_split = 0.1,
    shuffle = F) 
  
  predice = function(x) {
    
    y_values = (model %>% predict(x))
    result = (y_values*(max(base$RR) - min(base$RR))+min(base$RR))
    return (as.numeric(result))
  
    }
  
  # predicciones a 3 meses
  
  Predicciones[Index[i]:(Index[i]),1] = cbind(Cantones[i], predice(X_testc))
  
  # base para las predicciones
  df1[[i]] = as.data.frame(cbind(predice(X_testc), y_testc, Fecha[233:235],Mes[233:235]))
  colnames(df1[[i]]) = c("y_pred", "RR", "Fecha","Mes")
  df1[[i]]$RR = as.numeric(df1[[i]]$RR)
  df1[[i]]$y_pred = as.numeric(df1[[i]]$y_pred)
  
  
  
  p1[[i]] = ggplot(df1[[i]], aes(x = Fecha, y = RR, group = 1)) + geom_line(colour = "blue") + 
    geom_line( aes(x = Fecha, y = y_pred, colour = "red"))+   
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.text.x = element_text(angle = 45), legend.position = "none" )+
    labs (x = "Fecha", y = "Riesgo Relativo") +
    ggtitle(paste("Predicciones 2021 del cantón", Cantones[i], sep = " "))
  
  Eval.pd[i] = as.numeric(metricas(df1[[i]]))
  #### VALORES APROXIMADOS ####
  ## Generar valores ajustados
  
  df2[[i]] = as.data.frame(cbind(predice(X_all), base$RR, Fecha,Mes))
  colnames(df2[[i]]) = c("y_pred", "RR", "Fecha","Mes")
  df2[[i]]$RR = as.numeric(df2[[i]]$RR)
  df2[[i]]$y_pred = as.numeric(df2[[i]]$y_pred)
  

  
}



  everyother1 <- function(x) x[(seq_along(Fecha) + 5)%%12 == 6]
  
  # aqui hago las predicciones del 2000 hasta el 2020 con intervalos de bootstrap
  

  for (j in 1:12) {
    x <- df2[[i]][Mes==j,1] # tomo el mes j con las predicciones para cada mes (es decir que tomo el mes j para los años que hayan reportados)
    x <- as.numeric(x)
    n <- length(x) # voy a hacer un remuestreo para la cantidad de observaciones que tenga de ese mes j
    Tn <- mean(x) # saco la media para estos datos
    
    B <- 1000
    Tboot_b <- NULL
    for(b in 1:B) {  
      xb <- sample(x, size = n, replace = TRUE)  
      Tboot_b[b] <- mean(xb)
    }
    # calculo de estadisticos de bootstrap
    Tboot <- mean(Tboot_b)
    Vboot <- var(Tboot_b)
    sdboot <- sqrt(Vboot)
    
    # ahora el IC de bootstrap estudentizado
    B <- 1000
    Tboot_b <- NULL
    Tboot_bm <- NULL
    sdboot_b <- NULL
    
    for (b in 1:B) {  
      xb <- sample(x, size = n, replace = TRUE)  
      Tboot_b[b] <- mean(xb) # la media
      for (m in 1:B) {    
        xbm <- sample(xb, size = n, replace = TRUE)    
        Tboot_bm[m] <- mean(xbm)  
      }  
      sdboot_b[b] <- sd(Tboot_bm)
    }
    
    z_star <- (Tboot_b - Tn) / sdboot_b
    
    # se crean los limites inferiores y superiores
    LI[[j]] = c( as.numeric(df2[[i]][Mes==j,1]) - quantile(z_star, 1 - 0.05 / 2) * sdboot)
    LS[[j]] = c( as.numeric(df2[[i]][Mes==j,1]) - quantile(z_star, 0.05 / 2) * sdboot)
  }
  
  
  enero <- as.matrix(cbind(df2[[i]][Mes==1,],LI[[1]],LS[[1]]))
  febrero <- as.matrix(cbind(df2[[i]][Mes==2,],LI[[2]],LS[[2]]))
  marzo <- as.matrix(cbind(df2[[i]][Mes==3,],LI[[3]],LS[[3]]))
  abril <- as.matrix(cbind(df2[[i]][Mes==4,],LI[[4]],LS[[4]]))
  mayo <- as.matrix(cbind(df2[[i]][Mes==5,],LI[[5]],LS[[5]]))
  junio <- as.matrix(cbind(df2[[i]][Mes==6,],LI[[6]],LS[[6]]))
  julio <- as.matrix(cbind(df2[[i]][Mes==7,],LI[[7]],LS[[7]]))
  agosto <- as.matrix(cbind(df2[[i]][Mes==8,],LI[[8]],LS[[8]]))
  sept <- as.matrix(cbind(df2[[i]][Mes==9,],LI[[9]],LS[[9]]))
  octu <- as.matrix(cbind(df2[[i]][Mes==10,],LI[[10]],LS[[10]]))
  nov <- as.matrix(cbind(df2[[i]][Mes==11,],LI[[11]],LS[[11]]))
  dici <- as.matrix(cbind(df2[[i]][Mes==12,],LI[[12]],LS[[12]]))
  
  df3[[i]] <- as.data.frame(rbind(enero,febrero,marzo,abril,mayo,junio,julio,agosto,sept,octu,nov,dici))
  colnames(df3[[i]]) <- c("y_pred","RR","Fecha","Mes","LI","LS")
  df3[[i]]$y_pred <- as.numeric(df3[[i]]$y_pred)
  df3[[i]]$RR <- as.numeric(df3[[i]]$RR)
  df3[[i]]$LI <- as.numeric(df3[[i]]$LI)
  df3[[i]]$LS <- as.numeric(df3[[i]]$LS)
  
  p2[[i]] = ggplot(df3[[i]], aes(x = Fecha, y = RR, group = 1)) + geom_line(colour = "blue") + 
    geom_line(aes(x = Fecha, y = y_pred, colour = "red"))+   
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.text.x = element_text(angle = 45), legend.position = "none" )+
    scale_x_discrete(breaks = everyother1) + labs (x = "Fecha", y = "Riesgo Relativo") +
    geom_ribbon(aes(ymin=LI,ymax=LS), alpha=0.2, fill = "red") +
    ggtitle(paste(Cantones[i]))
  
  
  Eval.tot[i] = as.numeric(metricas(df2[[i]]))
  
  
  k_clear_session()




