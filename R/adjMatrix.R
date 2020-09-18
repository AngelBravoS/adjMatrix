#' A Function for calculate an adjacency matrix from a dataframe
#'
#' This function takes the first two columns of a distance dataframe or matrix and return it's adjacency matrix. You can also select a logical or numerical output.
#' @param a three columns dataframe and a logical output selector
#' @keywords adjacency matrix
#' @export
#' @examples
#'> initialVertex <- c("A","A","C","C")
#'> finalVertex <- c("B","C","D","E")
#'> vertexDistance <- c(2,3,1,4)
#'> data <- data.frame(initialVertex, finalVertex, vertexDistance)
#'> data
#' initialVertex finalVertex vertexDistance
#' 1             A           B              2
#' 2             A           C              3
#' 3             C           D              1
#' 4             C           E              4
#'> matriz <- adjMatrix(data)
#'> matriz
#'         A     B     C     D     E
#'[1,]  TRUE  TRUE FALSE FALSE FALSE
#'[2,]  TRUE FALSE  TRUE FALSE FALSE
#'[3,] FALSE FALSE  TRUE  TRUE FALSE
#'[4,] FALSE FALSE  TRUE FALSE  TRUE
#'
#'> matriz <- adjMatrix(datos, F)
#'> matriz
#'     A B C D E
#'[1,] 1 1 0 0 0
#'[2,] 1 0 1 0 0
#'[3,] 0 0 1 1 0
#'[4,] 0 0 1 0 1
adjMatrix = function(dataframe, boolean=TRUE){
	library(forcats)
	library(miscTools)

	data <- data.frame(initialVertex=dataframe[,1], finalVertex=dataframe[,2], vertexDistance=dataframe[,3])
	factors <- lvls_union(list(data$initialVertex, data$finalVertex))
	factors <- sort(factors)
	m <- nrow(data)
	n <- length(factors)

	adjacencyMatrix <- matrix(ncol = n, nrow = 0)
	colnames(adjacencyMatrix) <- factors

	for (i in 1:m){
		adjacencyMatrix <- rbind(adjacencyMatrix, logical(length = n))
		for (j in 1:n){
			if( (data[i,]$initialVertex == factors[j]) | (data[i,]$finalVertex == factors[j])){
				adjacencyMatrix[i,j] = TRUE
			}
		}
	}
	if(boolean==FALSE){
		adjacencyMatrix <- 1*adjacencyMatrix
	}
	return(adjacencyMatrix)
}
