
library(modeest)
library(igraph)


setwd("C:/Users/JR012563/Desktop/Curso IEC/Proyecto/")

###################################################
#Variables
N=50                #Número de individuos
Rep=1800            #Número de días
m=0                 #Iniciador

###################################################
# Bases de datos donde se guarda  la info
MonPerPTot=matrix(0,nrow=Rep,ncol=N)        #Probabilidad total de compra
MonPerP=matrix(0,nrow=Rep,ncol=N)           #Probabilidad de vecinos
MonPerC=matrix(0,nrow=Rep,ncol=N)           #Probabilidad de consumo


###################################################
#Bases de datos de dinero
Cuenta=matrix(1000, nrow=N, ncol=1)         #Cuenta de banco de cada individuo
Quincena=matrix(0, nrow=Rep, ncol=1)        #Programa para poder ingresar cada quincena cierta cantidad
dia=0
for(i in 1:Rep){
    dia=dia+1
    if(dia==15){
        Quincena[i,1]=500   
        dia=0
    }
    
}


###################################################
# Comportamientos de consumo
Estrato=matrix(0, nrow=N, ncol=1)
PConsumo=matrix(0, nrow=N, ncol=1)
for(i in 1:N){
    Estrato[i,1]=floor(runif(1,1,5))
    Est=Estrato[i,1]
    if(Est==1){                                 #Compradores de mucha frecuencia
        PConsumo[i,1]=runif(1,0.75,1)
    }
    else if(Est==2){                            #Compradores de media frecuencia
        PConsumo[i,1]=runif(1,0.5,0.75)
    }
    else if(Est==3){
        PConsumo[i,1]=runif(1,0.25,0.5)         #Compradores de baja frecuencia
    }
    else{
        PConsumo[i,1]=runif(1,0.0,0.25)         #Compradores de casi nula frecuencia
    }
    
}


################################################# 
#Creación de Red de Consumo
gC <- sample_smallworld(1, N, 5, p=0.00, loops = FALSE, multiple = FALSE)
#plot(gC)
MC <- as_adjacency_matrix(gC, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
MC <- as.matrix(MC)


################################################# 
#Creación de Red de Amistades
gA <- sample_smallworld(1, N, 5, p=0.05, loops = FALSE, multiple = FALSE)
#plot(gA)
MA <- as_adjacency_matrix(gA, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
MA <- as.matrix(MA)




##################################################
##################################################
#Simulación
##################################################
##################################################


for(r in 1:Rep){
    ##################################################
    #####     Calculo Probabilidad de  Consumo según Vecindad
    ##################################################
    m=m+1
    if(m==30){
        for(i in 1:N){
            prod=MA[,i]*Estrato[,1]
            x <- prod[prod !=0] 
            avEstrato[i,1]= which.max(table(x))
            
            Estrato=avEstrato
            MonPerP[r,i]=avEstrato[i,1]
        }
        
        for(i in 1:N){
            Est=avEstrato[i,1]
            if(Est==1){
                PConsumo[i,1]=runif(1,0.75,1)
            }
            else if(Est==2){
                PConsumo[i,1]=runif(1,0.5,0.75)
            }
            else if(Est==3){
                PConsumo[i,1]=runif(1,0.25,0.5)
            }
            else{
                PConsumo[i,1]=runif(1,0.0,0.25)
            }
        }
        
        m=0
    }
    
    else{
        
        for(i in 1:N){
            Est=Estrato[i,1]
            if(Est==1){
                avEstrato[i,1]=floor(runif(1,1,3))
            }
            else if(Est==2){
                avEstrato[i,1]=floor(runif(1,2,4))
            }
            else if(Est==3){
                avEstrato[i,1]=floor(runif(1,3,5))
            }
            else{
                avEstrato[i,1]=floor(runif(1,3,5))
            }
            
            Estrato=avEstrato
            MonPerP[r,i]=avEstrato[i,1]
        }
        
        for(i in 1:N){
            Est=avEstrato[i,1]
            if(Est==1){
                PConsumo[i,1]=runif(1,0.75,1)
            }
            else if(Est==2){
                PConsumo[i,1]=runif(1,0.5,0.75)
            }
            else if(Est==3){
                PConsumo[i,1]=runif(1,0.25,0.5)
            }
            else{
                PConsumo[i,1]=runif(1,0.0,0.25)
            }
        }
        
        
    }
    
    ##################################################
    #####     Calculo Proba Consumo por Dinero disponible en su cuenta
    ##################################################  
    PLiq=matrix(0, nrow=N, ncol=1)
    
    for(i in 1:N){
        if(Cuenta[i,1]<= 500 && Cuenta[i,1]>0){
            PLiq[i,1]=Cuenta[i,1]/500
        }
        else if(Cuenta[i,1]==0){
            PLiq[i,1]=0
        }
        else{
            PLiq[i,1]=1
        }
        
    }
    
    
    ##################################################
    #####     Probabilidad Total de Compra
    ##################################################
    P=matrix(0,nrow=N,ncol=1)
    P=0.5*PConsumo+0.5*PLiq
    
    for(i in 1:N){
        MonPerPTot[r,i]=P[i,1]
        
    }
    
    
    ##################################################
    #####     Dinámica de Consumo
    ##################################################
    for(i in 1:N){
        for(j in 1:N){
            
            
            if(MC[i,j]==1){   #Si están conectados comercialmente
                
                if(runif(1,0,1)<P[j,1]){ #si van a comprar o no según su Probabilidad de consumir
                    
                    if(Cuenta[j,1]<= 0){    #Si no tienen dinero, no pueden comprar
                        Cuenta[j,1]=Cuenta[j,1]
                    }
                    else{
                        Cuenta[i,1]=Cuenta[i,1]+1 #vendedor
                        Cuenta[j,1]=Cuenta[j,1]-1 #comprador
                    }   
                    
                }
                
                
                
                
            }
            
        }
        MonPerC[r,i]=Cuenta[i,1]
    }
    
    
    
    
    ################################### Redistribución de enlaces de compra-venta
    gC <- rewire(gC, each_edge(p = 0.1, loops = FALSE))
    MC <- as_adjacency_matrix(gC, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
    MC <- as.matrix(MC)
    
    Pago=Quincena[i,1]
    Cuenta=Cuenta+Pago
    #jpeg(paste0("Plot",r,".jpg"))
    #plot(hist(Cuenta))
    #dev.off()
    
    PConsumo=avPC
   
    
}



jpeg(paste0("Compleja_Plot",r,".jpg"))
plot(hist(Cuenta))
dev.off()

plot(MonPerP[,1])

##################################################
##################################################
#Análisis de series de tiempo de consumo
##################################################
##################################################

TS_C=matrix(0,ncol=1,nrow=N)
for(i in 1:N){
#approx_entropy(MonPerC[,30], edim = 2, r = 0.2*sd(ts), elag = 1)
dfa.analysis = dfa(time.series = MonPerC[,i], npoints = 10, window.size.range=c(10,100), do.plot=FALSE)
TS_C[i,1] = estimate(dfa.analysis,do.plot=TRUE)

}