#Get sqaure matrix length
getNumRows=function(matrix){
  return(sqrt(length(matrix)))
}

#Check if all probabilities sums to 1 FUNCTION
validMarkov = function(a, P) {
  for (i in 1:a) {
    if (sum(P[i,]) != 1) {
      print("Error: Matrix invalid")
    } else {
      cat("Row ", i, " is valid\n")
    }
  }
}

#Function for getting P^n
P_n=function(P, n) {
  Pn = P;
  for (i in 2:n) {
    Pn = P%*%Pn;
  }
  return(Pn)
}


# Enter your markov transition matrix
P = matrix(c(0.2, 0.25,0.4,0.5,0.5,0.3,0.3,0.25,0.3), ncol = 3)

#number of rows you have
a = getNumRows(P)

#Check if all probabilities sums to 1
validMarkov(a, P)

#Get P^n
n = 1
P_n(P,n)

#bar-a: Have to enter own bar-a
bar_a = c(0,1,0)

bar_a%*%P_n(P,n)
