import mysql.connector
import tkinter as tk
from tkinter import messagebox, ttk

# Database connection function
def connect_to_db():
    return mysql.connector.connect(
    	host='localhost',        # Change 'localhost' to your DB host
	user='root',     # Replace with your DB user name
	password='gr33nG0BL!N69',
        database="inclassdb"
    )

# Add employee to database
def add_employee(id, name, salary, department_id):
    try:
        conn = connect_to_db()
        cursor = conn.cursor()
        query = "CALL AddEmployee(%s, %s, %s, %s)"
        cursor.execute(query, (id, name, salary, department_id))
        conn.commit()
        cursor.close()
        conn.close()
        messagebox.showinfo("Success", "Employee added successfully!")
    except mysql.connector.Error as e:
        messagebox.showerror("Database Error", f"Error: {e}")

# View all employees from the database
def view_all_employees():
    try:
        conn = connect_to_db()
        cursor = conn.cursor()
        query = "CALL ViewAllEmployees()"
        cursor.execute(query)
        employees = cursor.fetchall()
        display_employees(employees)
        cursor.close()
        conn.close()
    except mysql.connector.Error as e:
        messagebox.showerror("Database Error", f"Error: {e}")

# View specific employee based on criteria
def view_specific_employee(emp_id, name, dept_id):
    try:
        conn = connect_to_db()
        cursor = conn.cursor()
        query = "CALL ViewSpecifcEmployee(%s, %s, %s)"
        params = []
        cursor.execute(query, (
		emp_id if emp_id else None, 
		name if name else None, 
		dept_id if dept_id else None
	))
        employees = cursor.fetchall()
        display_employees(employees)
        cursor.close()
        conn.close()
    except mysql.connector.Error as e:
        messagebox.showerror("Database Error", f"Error: {e}")

# Display employee data in the listbox
def display_employees(employees):
    listbox.delete(0, tk.END)  # Clear previous entries
    for employee in employees:
        listbox.insert(tk.END, f"ID: {employee[0]}, Name: {employee[1]}, Salary: {employee[2]}, DepartmentID: {employee[3]}")

# Switch frame function
def switch_frame(frame):
    frame.tkraise()

# GUI setup
root = tk.Tk()
root.title("Employee Manager")
root.geometry("600x600")

# Fonts and Styles
LABEL_FONT = ("Helvetica", 12)
ENTRY_FONT = ("Helvetica", 12)
BUTTON_FONT = ("Helvetica", 12)

# Create frames for different views
start_frame = ttk.Frame(root, padding="10 10 10 10")
add_employee_frame = ttk.Frame(root, padding="10 10 10 10")
view_employee_frame = ttk.Frame(root, padding="10 10 10 10")
view_options_frame = ttk.Frame(root, padding="10 10 10 10")

for frame in (start_frame, add_employee_frame, view_employee_frame, view_options_frame):
    frame.grid(row=0, column=0, sticky="nsew")

# Start Frame
start_label = ttk.Label(start_frame, text="Select an Option", font=LABEL_FONT)
start_label.grid(row=0, column=0, columnspan=2, pady=20)

add_employee_button = ttk.Button(start_frame, text="Add Employees", command=lambda: switch_frame(add_employee_frame))
add_employee_button.grid(row=1, column=0, padx=20, pady=20)

view_employee_button = ttk.Button(start_frame, text="View Employees", command=lambda: switch_frame(view_options_frame))
view_employee_button.grid(row=1, column=1, padx=20, pady=20)

# Add Employee Frame
id_label = ttk.Label(add_employee_frame, text="Employee ID", font=LABEL_FONT)
id_label.grid(row=0, column=0, sticky="W", pady=5)
id_entry = ttk.Entry(add_employee_frame, font=ENTRY_FONT)
id_entry.grid(row=0, column=1, pady=5)

name_label = ttk.Label(add_employee_frame, text="Name", font=LABEL_FONT)
name_label.grid(row=1, column=0, sticky="W", pady=5)
name_entry = ttk.Entry(add_employee_frame, font=ENTRY_FONT)
name_entry.grid(row=1, column=1, pady=5)

salary_label = ttk.Label(add_employee_frame, text="Salary", font=LABEL_FONT)
salary_label.grid(row=2, column=0, sticky="W", pady=5)
salary_entry = ttk.Entry(add_employee_frame, font=ENTRY_FONT)
salary_entry.grid(row=2, column=1, pady=5)

dept_label = ttk.Label(add_employee_frame, text="Department ID", font=LABEL_FONT)
dept_label.grid(row=3, column=0, sticky="W", pady=5)
dept_entry = ttk.Entry(add_employee_frame, font=ENTRY_FONT)
dept_entry.grid(row=3, column=1, pady=5)

add_button = ttk.Button(add_employee_frame, text="Add Employee", command=lambda: add_employee(
    id_entry.get(), name_entry.get(), salary_entry.get(), dept_entry.get()))
add_button.grid(row=4, column=0, columnspan=2, pady=20)

back_button1 = ttk.Button(add_employee_frame, text="Back", command=lambda: switch_frame(start_frame))
back_button1.grid(row=5, column=0, columnspan=2, pady=10)

# View Options Frame
view_all_button = ttk.Button(view_options_frame, text="View All Employees", command=view_all_employees)
view_all_button.grid(row=0, column=0, columnspan=2, pady=20)

view_specific_button = ttk.Button(view_options_frame, text="Select Employee", command=lambda: switch_frame(view_employee_frame))
view_specific_button.grid(row=1, column=0, columnspan=2, pady=20)

back_button2 = ttk.Button(view_options_frame, text="Back", command=lambda: switch_frame(start_frame))
back_button2.grid(row=2, column=0, columnspan=2, pady=10)

# View Specific Employee Frame
emp_id_label = ttk.Label(view_employee_frame, text="Employee ID", font=LABEL_FONT)
emp_id_label.grid(row=0, column=0, sticky="W", pady=5)
emp_id_entry = ttk.Entry(view_employee_frame, font=ENTRY_FONT)
emp_id_entry.grid(row=0, column=1, pady=5)

name_label2 = ttk.Label(view_employee_frame, text="Name", font=LABEL_FONT)
name_label2.grid(row=1, column=0, sticky="W", pady=5)
name_entry2 = ttk.Entry(view_employee_frame, font=ENTRY_FONT)
name_entry2.grid(row=1, column=1, pady=5)

dept_id_label = ttk.Label(view_employee_frame, text="Department ID", font=LABEL_FONT)
dept_id_label.grid(row=2, column=0, sticky="W", pady=5)
dept_id_entry = ttk.Entry(view_employee_frame, font=ENTRY_FONT)
dept_id_entry.grid(row=2, column=1, pady=5)

search_button = ttk.Button(view_employee_frame, text="Search Employee", command=lambda: view_specific_employee(
    emp_id_entry.get(), name_entry2.get(), dept_id_entry.get()))
search_button.grid(row=3, column=0, columnspan=2, pady=20)

back_button3 = ttk.Button(view_employee_frame, text="Back", command=lambda: switch_frame(view_options_frame))
back_button3.grid(row=4, column=0, columnspan=2, pady=10)

# Frame for displaying employees
listbox_frame = ttk.Frame(root, padding="10 10 10 10")
listbox_frame.grid(row=1, column=0, padx=20, pady=20, sticky="W")

listbox_label = ttk.Label(listbox_frame, text="Employee List", font=LABEL_FONT)
listbox_label.grid(row=0, column=0, sticky="W")

listbox = tk.Listbox(listbox_frame, font=ENTRY_FONT, width=70, height=10)
listbox.grid(row=1, column=0, sticky="W")

# Adding a scrollbar to the listbox
scrollbar = ttk.Scrollbar(listbox_frame, orient="vertical", command=listbox.yview)
listbox.config(yscrollcommand=scrollbar.set)
scrollbar.grid(row=1, column=1, sticky="NS")

# Start the GUI loop
switch_frame(start_frame)
root.mainloop()
