VigenerCipher<-function(text,klic){ 
  text=utf8ToInt(text)
  for (i in 1:length(text)){
    if (text[i]==32){
      text[i]=0
      i=i-1
      print(text)
      print(i)
    }
  }
  text=text-96
  klic=utf8ToInt(klic)-97
  print(text)
  print(klic)
  k=1
  for (i in 1:length(text)){
      text[i]=text[i]+klic[k]
      if (k<length(klic)){
        k=k+1
      }else{ 
        k=1}
      if (text[i]>26){
        text[i]=text[i]-26
      }
  }
  print(text+96)
  text=intToUtf8(text+96)
    print(text)
}
  