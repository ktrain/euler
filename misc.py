import string

def load_names(filename):
  file = open(filename,'r')
  names = []
  for name in file:
    names += [name.strip().lower()]
  file.close()
  names.sort()
  return names

def p22(filename):
  names = load_names(filename)
  value = {'a':1, 'b':2, 'c':3, 'd':4, 'e':5, 'f':6, 'g':7, 'h':8, 'i':9,
  'j':10, 'k':11, 'l':12, 'm':13, 'n':14, 'o':15, 'p':16, 'q':17, 'r':18,
  's':19, 't':20, 'u':21, 'v':22, 'w':23, 'x':24, 'y':25, 'z':26}

  score = 0
  i = 1
  for name in names:
    name_score = 0
    for char in name:
      name_score += value[char]
    score += name_score * i
    i += 1
  return score


def p4():
  upper = 999
  lower = 100
  pals = []

  for i in range(lower, upper+1):
    for j in range(lower, upper+1):
      prod = i * j
      s = str(prod)
      if (s[0] == '9' and len(s) > 5 and s == s[::-1]):
        pals += [s]
  pals.sort()
  return pals


def p10(n):
  sieve = range(3, n+1, 2)
  for i in sieve:
    print i
    for j in sieve:
      if j % i == 0:
        del j
  return [2] + sieve
