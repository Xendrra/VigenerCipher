vige<-function(text,klic){ 
  a=letters
  j=1
  temp=numeric()
  out=numeric()
  for (i in 1:nchar(text)){
    temp[i]=which(a==substring(klic,j,j))
    out[i]=which(a==substring(text,j,j))#+temp[i]
    print(c('out',out[i]))
    if (out[i]>26){
      out[i]=out[i]-26
    }
    out[i]=a[i]
    if (j==nchar(klic)){
      j=1
      print(j)
    }else
    {j=j+1}
  }
  print(temp)
}
