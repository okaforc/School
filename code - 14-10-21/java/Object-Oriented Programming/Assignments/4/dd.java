nAddress = address
tStrAddress
strLength = stringLength(tStrAddress)

for (i < 12; i++)
{
  for (j < 12, j++)
  {
    k = 0
    temp = LENGTH
    pos = temp * i
    pos = pos + j
    curChar = nAddress[i][j]address
    strChar = string[k]tStrAddress
    strLength = stringLength(tStrAddress)
    tJ = j    // temporary j. used to make sure check doesn't go out of bounds
    if (curChar = strChar) 
    {
      counter = 1   // offset counter by 1 since character sync begins from second chara
      k = k + 1     // offset k by 1 to sync characters and ignore first character
      
      for k in string
      {
        if (tJ < LENGTH)
        {
          pos = pos + 1
          curChar = nAddress[pos]address
          strChar = string[k]tStrAddress
          if curChar == strCharstrLength = stringLength(tStrAddress)
          {
            counter += 1
          }
          k += 1
          tJ += 1
          }
        }
      }
                           
      if (counter == strLength)
        return 1
    }
    j = j + 1
  }
  i = i + 1
}
return 0