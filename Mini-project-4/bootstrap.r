##########  Function to calculate the Bootstrapped Distribuition  ##########

new.boot = function(data,func.name,R)
{
  t = numeric(R)
  n = length(data)
  for(i in 1:R)
  {
    indices = sample(1:n,n,replace=T)
    t[i] = func.name(data,indices)
  }
  t0 = func.name(data,1:n)
  weights = numeric(n)
  for(i in 1:n)
  {
    weights[i] = 1/n
  }
  print(paste0("ORDINARY NONPARAMETRIC BOOTSTRAP"))
  print(paste0("Original: ",(t0)))
  print(paste0("Bias: ",format(round((mean(t)-t0),4))))
  print(paste0("Std. error: ",format(round((sd(t)),4))))
  return(list(t0=t0,t=t,R=R,data=data,statistic=func.name,weights=weights))
}

##########  Function to find the Confidence Intervels  ##########

new.boot.ci = function(x)
{
  conf = 0.95
  alpha = 1 - conf
  N = c(x[[1]]-(mean(x[[2]])-x[[1]])-qnorm(1-(alpha/2))*sd(x[[2]]), 
        x[[1]]-(mean(x[[2]])-x[[1]])-qnorm((alpha/2))*sd(x[[2]]))
  l = ((x[[3]]+1)*(alpha/2))
  u = ((x[[3]]+1)*(1-(alpha/2)))
  P = sort(x[[2]])[c(l,u)]
  B = c(2*x[[1]]-sort(x[[2]])[c(u)],2*x[[1]]-sort(x[[2]])[c(l)])
  print(paste0("Normal approximation: (",format(round(N[1],4)),"  ",format(round(N[2],4)),")"))
  print(paste0("Basic approximation: (",B[1],"  ",B[2],")"))
  print(paste0("Percentile approximation: (",P[1],"  ",P[2],")"))
  return(list(R=x[[3]],t0=x[[1]],Normal=(N),Basic=(B),Percentile=(P)))
}

########## Function to calculate the 90-th percentile of a samlpe data ##########

ninty.npar = function(x,indices)
{
  result = quantile(x[indices],prob=0.90,type=1)
  return(result)
}

# To compute the 90-th percentile from the draws
# First we load the given data and store it into variable
cpu = scan(file="cputime.txt")

# we call the new.boot function that gives the bootstrap draws with 
#           bias and standard deviation of the draws
ninty.npar.boot = new.boot(cpu, ninty.npar, 999)

# to get the confidence intervals of the draws call the new.boot.ci function
#       with the result stored in 'ninty.npar.boot' variable
conf = new.boot.ci(ninty.npar.boot)