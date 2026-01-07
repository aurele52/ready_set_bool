str = "01|1|"
arg2 = -1
op = -1

f(arg1) { 
  rm str[0]
  if (!str) {
    return arg1
  }
  if (str[0] == number) {
    f(str[0])
    if (arg2) {
      arg1 = arg1 op arg2
      f(str[0])
    }
  }
  else {
    op = str[0]
    arg2 = arg1
    return
  }







}


print f(0)
