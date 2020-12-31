data = matrix(sample(c(-1,1), 10000, replace = TRUE), ncol = 10)

ninputs = nrow(data)

# we create two different databases, and we will apply the same algorithm to both
targets.sum = ifelse(rowSums(data) >=0,1,0)
targets.prod = ifelse(apply(data, 1, prod)>=0,1,0)

inputs = cbind( data[,1:10], rep(-1,ninputs))

## We separate our database into train and test datasets, 80/20. 
split.perc = 0.8
smp_size <- floor(split.perc * ninputs)
train <- inputs[1:smp_size,]
test <- inputs[(smp_size+1):ninputs,]
  
epsilon = 0.01


set.seed(100)


wts = runif(11)*99 ## start with a deliberately poor set of initial weights.


array.error.train = c() # we store the training errors
array.error.test = c() # we store the test errors
for (iteration in 1:400) {
  
  order = sample(smp_size);
  error.train = 0;
  error.test = 0;
  for (i in (ninputs-smp_size)){
    x = inputs[i+smp_size,]
    t = targets.prod[i+smp_size]
    y = sum ( x * wts )
    y = y > 0;
    error.test = error.test + (0.5*(t - y)^2); # we just calculate error
    
  }
  for (i in order) { 
    x = train[i,]
    t = targets.prod[i]
    y = sum ( x * wts )
    y = y > 0;
    error.train = error.train + (0.5*(t - y)^2);
    dw = epsilon * ( t - y ) * x
    wts = wts + dw; # actual learning
  }
  
  # title =sprintf('Iteration %d error %.3f\n', iteration, error)
  # plotdata()
  # title(main=title)
  # Sys.sleep(0.1)
  array.error.train = c(array.error.train , error.train)
  array.error.test = c(array.error.test , error.test)
  
}


plot(1:length(array.error.train), array.error.train, type="l",col="red")
lines(1:length(array.error.test), array.error.test, type="o",col="green")

