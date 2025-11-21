import mysql.connector
# Connect to the SalesOrdersExampleTest database
conn = mysql.connector.connect(
    host='localhost',        # Change 'localhost' to your DB host
    user='root',     # Replace with your DB user name
    password='gr33nG0BL!N69', # Replace with your DB password
    database='midterm'
)

cursor = conn.cursor()

procedures = [
	'GetTournamentLocation',
	'allplayers',
	'coachandteam',
	'unplayedtourn',
	'top10',
	'highscore',
	'tenaboveavg'
]

for proc in procedures:
	print(f"Results from {proc}:")
	cursor.callproc(proc)
	for result in cursor.stored_results():
		for row in result.fetchall():
			print(row)
	print("\n-----------------------\n")
	
cursor.close()
conn.close()
