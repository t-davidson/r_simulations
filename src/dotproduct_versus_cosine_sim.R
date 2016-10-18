###Testing difference in performance between cosine similarity and dot product

cosine_similarity<-function(x,y){
  sim = (x%*%y)/(sqrt(x%*%x)*sqrt(y%*%y))
  return(sim)
}

add_if_nonzero<-function(vector){
  for (i in 1:length(vector)) {
    if (vector[i] > 0) 
      #vector[i] = vector[i]+sample(0:10, 1)
      vector[i] = vector[i]+10
  }
  return(vector)
}

simulate_vectors<-function(N = 100){

  a = rep(0, times = N)
  b = rep(0, times = N)
  c = rep(0, times = N)
  #cosine_ab = rep(0, times = N)
  
  for (i in 1:N) {
    a[i] = sample(1:100, 1)
    b[i] = sample(1:100, 1)
    c[i] = sample(1:100, 1)
  }
  dot_ab = a%*%b
  dot_ac = a%*%c
  dot_cb = b%*%c
  cos_ab = cosine_similarity(a,b)
  cos_ac = cosine_similarity(a,c)
  cos_bc = cosine_similarity(b,c)
  return(c(dot_ab, dot_ac, dot_cb, cos_ab, cos_ac, cos_bc))
}

simulation<-function(N=1000){
  
  dot_ab = rep(0, N)
  cos_ab = rep(0, N)
  
for (i in 1:N) {
  scores = simulate_vectors()
  dot_ab[i] = scores[1]
  cos_ab[i] = scores[4]
  
}
  hist(dot_ab, xlab="Dot Product Scores", main="Distribution of dot product scores", col="blue")
  hist(cos_ab, xlab="Cosine Similarity Scores", main="Distribution of cosine similarity scores", col="red")
  plot(dot_ab, cos_ab, xlab="Dot Products", ylab="Cosine Similarity Scores")
  
}


simulate_vectors_add<-function(N = 100){
  
  a = rep(0, times = N)
  b = rep(0, times = N)
  c = rep(0, times = N)
  #cosine_ab = rep(0, times = N)
  
  for (i in 1:N) {
    a[i] = sample(1:100, 1)
    b[i] = sample(1:100, 1)
    c[i] = sample(1:100, 1)
  }
  add_a = add_if_nonzero(a)
  add_b = add_if_nonzero(b)
  add_c = add_if_nonzero(c)
  dot_ab = a%*%b
  dot_ac = a%*%c
  dot_cb = b%*%c
  add_ab = add_a%*%add_b
  dot_ab_a = add_a%*%b
  dots = c(dot_ab, dot_ac, dot_cb)
  cos_ab = cosine_similarity(a,b)
  cos_ac = cosine_similarity(a,c)
  cos_bc = cosine_similarity(b,c)
  coss = c(cos_ab, cos_ac, cos_bc)
  cos_add_ab = cosine_similarity(add_a, add_b)
  cos_ab_a = cosine_similarity(add_a, b)
  return(c(dot_ab, cos_ab, add_ab, cos_add_ab, dot_ab_a, cos_ab_a))
}

simulation2<-function(N=1000){
  
  dot_ab = rep(0, N)
  cos_ab = rep(0, N)
  dot_ab_add = rep(0, N)
  cos_ab_add = rep(0, N)
  
  dot_ab_add_a = rep(0, N)
  cos_ab_add_a = rep(0, N)
  
  for (i in 1:N) {
    scores = simulate_vectors_add()
    dot_ab[i] = scores[1]
    cos_ab[i] = scores[2]
    dot_ab_add[i] = scores[3]
    cos_ab_add[i] = scores[4]
    
    dot_ab_add_a[i] = scores[5]
    cos_ab_add_a[i] = scores[6]
    
  }
  plot(dot_ab, cos_ab, xlab="Dot Products", ylab="Cosine Similarity Scores")
  plot(dot_ab, dot_ab_add, xlab="Dot Products", ylab="Dot Products of Higher TF")
  plot(cos_ab, cos_ab_add, xlab="Cosine Similarity Scores", ylab="Cosine Similarity Scores of Higher TF")
  plot(dot_ab, cos_ab_add, xlab="Dot Products of Higher TF ", ylab="Cosine Similarity Scores of Higher TF")
  
  plot(dot_ab, dot_ab_add_a, xlab="Dot Product AB", ylab="Dot Product AB where A larger TF")
  plot(cos_ab, cos_ab_add_a, xlab="Cosine Sim AB", ylab="Cosine Sim AB where A larger TF")
  #cor(dot_ab, dot_ab_add)
  #cor(dot_ab, dot_ab_add_a)
  #cor(cos_ab, cos_ab_add)
  #cor(cos_ab, cos_ab_add_a)
  results = as.matrix(cbind(dot_ab, dot_ab_add, dot_ab_add_a, cos_ab, cos_ab_add, cos_ab_add_a))
  return(results)
}
