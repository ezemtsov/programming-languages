import pyhdb

connection = pyhdb.connect(
    host="b70.rgapvba0015",
    port=31015,
    user="EVGENY_Z",
    password="Zyama8-8")

cursor = connection.cursor()
cursor.execute("SELECT 2+2 FROM DUMMY")
print(cursor.fetchone())


connection.close()
