vige<-function(text,klic){ 
  text=gsub(" ", "", text, fixed = TRUE)
  klic=gsub(" ", "", klic, fixed = TRUE)
  print(text)
  a=letters
  j=1
  temp=numeric()
  out=numeric()
  sifra=character(0)
  for (i in 1:nchar(text)){
    print(i)
    print(j)
    temp[i]= which(a==substring(klic,j,j))-1
    out[i]=which(a==substring(text,i,i))+temp[i]
    print(c('out',out))
    if (out[i]>26){
      out[i]=out[i]-26
    }
    if (j==nchar(klic)){
      j=1
    }else
    {j=j+1}
  }
  for (i in 1:nchar(text)){ 
    sifra[i]=a[out[i]]}
  sifra=paste(sifra,collapse='')
  print(sifra)
}
