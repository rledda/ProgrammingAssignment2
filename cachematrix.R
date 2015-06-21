## La funzione 'makeCacheMatrix' consente l'archiviazione nella cache di una matrice e della sua matrice inversa 
## mediante la creazione di una 'lista' di 4 funzioni (set, get, set.inv, get.inv)
## sfruttando l'operatore '<<', il quale consente di assegnare un valore definito nell'ambiente di lavoro ad un oggetto definito in un ambiente distinto.

makeCacheMatrix <- function(m = matrix()) { # 'm' e' la matrice iniziale di input
        
        m.inv <- NULL # in 'm.inv' viene archiviata la matrice inversa di (inizializzata con valore NULL)
        # 'set' e' una funzione che consente di gestire 2 matrici, una come argomento iniziale ed una nella memoria cache
        # ad es. makeCacheMatrix(m) - qui opero con la prima matrice 'm'
        # ad es. makeCacheMatrix$set(m2) - qui opero con la seconda matrice 'm2'
        set <- function(m2) { 
                m <<- m2 # assegno a 'm' il valore della matrice nuova 'm2'
                m.inv <<- NULL # inizializzo m.inv con valore NULL
        }
        
        get <- function() m # assegno a 'get' la matrice 'm' di input
        set.inv <- function(inv) m.inv <<- inv # imposto la matrice inversa
        get.inv <- function() m.inv # restituisco la matrice inversa, da calcolarsi con la funzione 'cacheSolve'
        # costruisco una lista che contiene 4 funzioni
        # o <- makeCacheMatrix(m)
        # o$set(m2) # per cambiare la matrice
        # o$get # per recuperare la  matrice impostata 'm'
        # o$set.inv # per impostare la matrice inversa di 'm2', calcolata con la funzione 'cacheSolve'
        # o$get.inv # per recuperare la matrice inversa di 'm', calcolata con la funzione 'cacheSolve'
        list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## La funzione cacheSolve calcola la matrice inversa della matrice archiviata nella 'lista' creata con la funzione 'makeCacheMatrix'
## 'cacheSolve' verifica se la matrice inversa e' gia' stata calcolata;
# se cio' e' vero, recupera la matrice inversa dalla cache, 
## altrimenti calcola la matrice inversa con la funzione 'solve' ed la carica nella cache

cacheSolve <- function(o, ...) { #l'argomento 'o' costituisce l'output della funzione makeCacheMatrix
        m <- o$get.inv() # recupero la matrice inversa archiviata in 'o'
        # verifico se la matrice inversa non e' stata ancora calcolata - vedi comando "m.inv <- NULL" nella funzione 'makeCacheMatrix'
        if(!is.null(m)) { # se la matrice inversa e' stata calcolata, cioe' se o$get.inv() non e' NULL
                message("Recupero in corso dei dati dalla cache....") #recupero la matrice inversa dalla cache
                return(m) # restituisco la matrice inversa
        }
        matr <- o$get() # se la matrice inversa non e' stata calcolata, recupero la matrice da o$get()
        m <- solve(matr) # calcolo la matrice inversa 
        o$set.inv(m) # assegno la matrice inversa a o$set.inv
        m # restituisco la matrice inversa
}
