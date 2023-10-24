#Sieve of Eratosthenes

n<-100
prime<-function(n)
  sieve<-rep(T,n)
sieve[1]<-F
prime<-vector("numeric",0)

prime_marked <-n^.5

new_prime <- which(sieve)[1]

while(new_prime<=prime_marked){
  prime<-c(prime,new_prime)
  sieve[seq(new_prime,n,new_prime)]<-F
  new_prime<-which(sieve)[1]
}
prime<-c(prime,which(sieve))
print(prime)

#Prime factor
prime_factor<-function(x){
  smallest_factor<-prime(x)
  prime_factor<-vector("numeric",0)
  while(x!=1){
    if(x%%smallest_factor[1]==0){
      prime_factor<-c(prime_factor,smallest_factor[1])
      x<-x/smallest_factor[1]
    }
    else{
      smallest_factor<-smallest_factor[1]
    }
  }
  return(prime_factor)
}