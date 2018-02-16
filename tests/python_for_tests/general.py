from sys import stdout

# -----------------------------------------------

def pause():

  stdout.write('Press [return] to continue...')
  stdout.flush()
  try:
    raw_input()
  except:
    input()

# -----------------------------------------------
