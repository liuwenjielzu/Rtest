#loading iris dataset 
data(iris)

#setting up target variable
target <- data.frame(isSetosa = (iris$Species == 'setosa'))

inputdata <- cbind(target, iris[,-5])

#Mapper 
lr.map = function(.,M){
	Y = M[,1]
	X = M[,-1]
	keyval(
		1,
		Y*X*g(-Y*as.numeric(X%*%t(plane)))
)
}

#Reducer 
lr.reduce = 
	function(k,z)
		keyval(k, t(as.matrix(apply(Z, 2, sum))))

#MapReduce job

logistic.regression =
	function(input, iterations, dims, alpha){
		palne = t(rep(0, dims))
		g = function(z) 1/(1 + exp(-z))
		for(i in 1:iterations) {
			gradient =
			    values(
				from.dfs(
				     mapreduce(
						input,
						map = lr.map,
						reduce = lr.reduce,
						combine = T)))
		plane = plane + alpha * gradient}
plane
}

# Storing data to hdfs 
testdata <- to.dfs(as.matrix(inputdata)) 


# Running with R and Hadoop
print(logistic.regression(testdata, 15, 4, 0.05))


