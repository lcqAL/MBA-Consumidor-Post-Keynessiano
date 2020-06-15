

setwd("C:/Users/JR012563/Desktop/Curso IEC/Proyecto/")

###################################################
#Variables
N=100				#Número de individuos
Rep=1800			#Número de días


###################################################
#Bases de datos de dinero
Cuenta=matrix(1000, nrow=N, ncol=1)				#Cuenta de banco de cada individuo
MonPerC=matrix(0,nrow=Rep,ncol=N)				#Probabilidad de consumo
Quincena=matrix(0, nrow=Rep, ncol=1)			#Programa para poder ingresar cada quincena cierta cantidad
dia=0
for(i in 1:Rep){
	dia=dia+1
	if(dia==15){
	 Quincena[i,1]=500	
	 dia=0
	}
	
}


################################################# 
#Creación de Red de Consumo
g <- sample_smallworld(1, N, 5, p=0.00, loops = FALSE, multiple = FALSE)
#plot(g)
M <- as_adjacency_matrix(g, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
M <- as.matrix(M)

##################################################
##################################################
#Simulación
##################################################
##################################################

for(r in 1:Rep){
    
    for(i in 1:N){
        for(j in 1:N){
            

            if(M[i,j]==1){

            	if(Cuenta[j,1]<= 0){
            		Cuenta[j,1]=Cuenta[j,1]
            	}
            	else{
                    Cuenta[i,1]=Cuenta[i,1]+1 #vendedor
                    Cuenta[j,1]=Cuenta[j,1]-1 #comprador
            	}	


            }
            
        }
        MonPerC[r,i]=Cuenta[i,1]
    }
    
 #Reenlace de la red de consumo
    g <- rewire(g, each_edge(p = 0.1, loops = FALSE))
    M <- as_adjacency_matrix(g, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
    M <- as.matrix(M)

    Pago=Quincena[i,1]
    Cuenta=Cuenta+Pago
    #jpeg(paste0("Plot",r,".jpg"))
    #plot(hist(Cuenta))
    #dev.off()
}




##################################################
##################################################
#Análisis de series de tiempo de consumo
##################################################
##################################################

TS_S=matrix(0,ncol=1,nrow=N)
for(i in 1:N){
#approx_entropy(MonPerC[,30], edim = 2, r = 0.2*sd(ts), elag = 1)
dfa.analysis = dfa(time.series = MonPerC[,i], npoints = 10, window.size.range=c(10,100), do.plot=FALSE)
TS_S[i,1] = estimate(dfa.analysis,do.plot=TRUE)

}

    jpeg(paste0("Plot",r,".jpg"))
    plot(hist(Cuenta))
    dev.off()
