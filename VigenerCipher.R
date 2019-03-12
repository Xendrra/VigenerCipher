VigenerCipher<-function(text,klic){ 
  #funkce VigenerCipher vraci sifrovany text
  #vstup1...text, retezec ktery ma byt sifrovan, pouze znaky anglicke alfabety
  #vstup2...klic, retezec pouzity k zasifrovani, pouze znaky anglicke alfabety
  #vystup...sifra, zakodovany vstupni text
  text=gsub(" ", "", text, fixed = TRUE)        #odstraneni mezer
  klic=gsub(" ", "", klic, fixed = TRUE)  
  text=tolower(text)                            #osetreni v pripade kapitalek na vstupu
  klic=tolower(klic)
  a=letters                                     #pomocny vektor s pismeny
  j=1 
  temp=numeric()                                #iniciace promennych
  out=numeric()
  sifra=character(0)
  for (i in 1:nchar(text)){
    temp[i]= which(a==substring(klic,j,j))-1    #docasna promenna, prevadi pismena na cisla posunu (a=0)
    out[i]=which(a==substring(text,i,i))+temp[i] #najde index kazdeho znaku v pomocnem vektoru a pricte zadany posun 
    if (out[i]>26){                             #pokud index prekroci index pismene z, zacina znovu od a
      out[i]=out[i]-26 
    }else if(out[i]>52){out[i]=out[i]-52}
    
    if (j==nchar(klic)){                        #pokud index j presahne delku klice, vraci se zpet k prvnimu znaku
      j=1
    }else                                       #jinak pokracuje na dalsi znak klice
    {j=j+1}
  }
  for (i in 1:nchar(text)){                     
    sifra[i]=a[out[i]]}                         #prevod vypocitanych cisel zpet na pismena
  sifra=paste(sifra,collapse='')                #spojeni jednotlivych znaku do jednoho retezce
  return(sifra)                                
}
