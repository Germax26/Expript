import expript

while True:
    text = input('> ')

    result, error = expript.resolve('<stdin>', text)
    if error: print(str(error))
    else: print(result)