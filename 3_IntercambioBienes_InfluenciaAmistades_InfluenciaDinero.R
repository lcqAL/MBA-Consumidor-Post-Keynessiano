#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

setwd("C:/Users/JR012563/Desktop/Curso IEC/Proyecto/Simple/Entorno/")


L=7
N=L*L
Rep=5000
p=0.5

#g <- make_lattice(length = L, dim = 2, circular = FALSE)
g <- sample_smallworld(1, N, 5, p=0.00, loops = FALSE, multiple = FALSE)
M <- as_adjacency_matrix(g, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
M <- as.matrix(M)
#plot(g)
#plot(degree.distribution(g))


#el <- matrix( c(1,2,2,3,3,4,4,5,5,1), nc = 2, byrow = TRUE)
#g <- graph_from_edgelist(el, directed = FALSE)
#M <- as_adjacency_matrix(g, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
#M <- as.matrix(M)



Cuenta=matrix(100, nrow=N, ncol=1)              #Cuenta de banco de cada individuo
MonPerC=matrix(0,nrow=Rep,ncol=N)               #Probabilidad de consumo
MonPerP=matrix(0,nrow=Rep,ncol=N)               #Probabilidad de consumo

###################################################
# Comportamientos de consumo
Estrato=    matrix(0, nrow=N, ncol=1)
avEstrato=  matrix(0, nrow=N, ncol=1)
PConsumo=   matrix(0, nrow=N, ncol=1)
ColorCons=  matrix(0, nrow=N, ncol=1)
for(i in 1:N){
    Estrato[i,1]=floor(runif(1,1,5))
    Est=Estrato[i,1]
    if(Est==1){                                 #Compradores de mucha frecuencia
        PConsumo[i,1]=runif(1,0.75,1)
        ColorCons[i,1]="blue"
    }
    else if(Est==2){                            #Compradores de media frecuencia
        PConsumo[i,1]=runif(1,0.5,0.75)
        ColorCons[i,1]="gray50"
    }
    else if(Est==3){
        PConsumo[i,1]=runif(1,0.25,0.5)         #Compradores de baja frecuencia
        ColorCons[i,1]="tomato"
    }
    else{
        PConsumo[i,1]=runif(1,0.0,0.25)         #Compradores de casi nula frecuencia
        ColorCons[i,1]="gold"
    }
    
}


################################################# 
#Creación de Red de Amistades
#gA <- sample_smallworld(1, N, 5, p=0.05, loops = FALSE, multiple = FALSE)
gA <- make_lattice(length = L, dim = 2, circular = FALSE)

V(gA)$color <- ColorCons
l <- layout_with_fr(gA) 
plot(gA, vertex.label=NA, vertex.size=7, edge.curved=0.1, layout=l)

MA <- as_adjacency_matrix(gA, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
MA <- as.matrix(MA)




for(r in 1:Rep){
    
    
    
    for(i in 1:N){
        prod=MA[i,]*Estrato[,1]
        x <- prod[prod !=0]
        #table(x)
        a <- length(mlv(x, method = "mfv"))
        #View(x)
        
        if(a==1){
            avEstrato[i,1]=mlv(x, method = "mfv")
        }else{
            avEstrato[i,1]=floor( runif( 1, (min(mlv(x, method = "mfv"))) , (1+max(mlv(x, method = "mfv"))) ) )
        }
        
        
        MonPerP[r,i]=avEstrato[i,1]
    }
    Estrato=avEstrato
    
    for(i in 1:N){
        Est=avEstrato[i,1]
        if(Est==1){
            PConsumo[i,1]=runif(1,0.75,1)               #Seguro Consume
            ColorCons[i,1]="blue"
        }
        else if(Est==2){
            PConsumo[i,1]=runif(1,0.5,0.75) 
            ColorCons[i,1]="gray50"            
        }
        else if(Est==3){
            PConsumo[i,1]=runif(1,0.25,0.5)
            ColorCons[i,1]="tomato"
        }
        else{
            PConsumo[i,1]=runif(1,0.0,0.25)             #Seguro No consume
            ColorCons[i,1]="gold"
        }
    }
    
    
    
    for(j in 1:N){
        for(i in 1:N){
            
            
            if(M[i,j]==1){
                
                if(runif(1,0,1)<PConsumo[i,1]){
                    
                    
                    #print(paste0("rep__",r,"___i_",i,"_j_",j,"_"))
                    if(Cuenta[i,1]<= 0){
                        Cuenta[i,1]=Cuenta[i,1]
                    }
                    else{
                        Cuenta[i,1]=Cuenta[i,1]-10 #vendedor
                        Cuenta[j,1]=Cuenta[j,1]+10 #comprador
                        #print(paste0("Cuenta:i_",Cuenta[i,1]))
                        #print(paste0("Cuenta:j_",Cuenta[j,1]))
                    }   
                    
                }
                
            }
            
        }
        #MonPerC[r,i]=Cuenta[i,1]
    }
    
    
    for(i in 1:N){
        MonPerC[r,i]=Cuenta[i,1]
    }
    #print( MonPerC[r,] )
    
    
    
    
    
    g <- rewire(g, each_edge(p = 1, loops = FALSE))
    M <- as_adjacency_matrix(g, type = "both", attr = NULL, edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
    M <- as.matrix(M)
    #plot(g)
    
    
    #jpeg(paste0("Grafo",r,".jpg"))
    #V(gA)$color <- ColorCons
    #l <- layout_with_fr(gA) 
    #plot(gA, vertex.label=NA, vertex.size=7, edge.curved=0.1, layout=l)
    #dev.off()
}

    #jpeg(paste0("Grafo",r,".jpg"))
    V(gA)$color <- ColorCons
    l <- layout_with_fr(gA) 
    plot(gA, vertex.label=NA, vertex.size=7, edge.curved=0.1, layout=l)
    #dev.off()

#View(Cuenta)
#View(MonPerC)

#jpeg(paste0("Plot",r,".jpg"))
plot(hist(Cuenta))
#dev.off()

plot(MonPerC[,2], type="l")
plot(MonPerC[,5], type="l")


##################################################
##################################################
#Análisis de series de tiempo de consumo
##################################################
##################################################


TS_S=matrix(0,ncol=1,nrow=N)
for(i in 1:N){
    #approx_entropy(MonPerC[,30], edim = 2, r = 0.2*sd(ts), elag = 1)
    dfa.analysis = dfa(time.series = MonPerC[,i], npoints = 10, window.size.range=c(10,100), do.plot=FALSE)
    TS_S[i,1] = estimate(dfa.analysis,do.plot=FALSE)
    
}

View(TS_S)
