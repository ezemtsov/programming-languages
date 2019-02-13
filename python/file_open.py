fin = open(file="./simple_txt.txt",encoding='utf-8')

i = 1
for line in fin:
    word = line.strip()
    if i == 1:
        print(word)
        i += 1
