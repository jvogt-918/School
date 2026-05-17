USE inclassdb;

DELIMITER //
CREATE PROCEDURE AddEmployee(
	in p_EmpID int,
	in p_Name varchar(100),
    in p_Salary int,
    in p_DepartmentID int
)
BEGIN
	INSERT INTO Employees (EmpID, Name, Salary, DepartmentID) 
    VALUES (p_EmpID, p_Name, p_Salary, p_DepartmentID);
END //
DELIMITER ;

DELIMITER //
CREATE PROCEDURE ViewAllEmployees()
BEGIN
	SELECT * FROM employees;
END //
DELIMITER ;

DELIMITER //
CREATE PROCEDURE ViewSpecifcEmployee(
	in p_EmpID int,
	in p_Name varchar(100),
    in p_DepartmentID int
)
BEGIN
	SELECT EmpID, Name, Salary, DepartmentID 
    FROM employees 
    WHERE 
    (p_EmpID is NULL  or EmpID = p_EmpID) 
    and (p_Name is NULL or Name = p_Name)
    and (p_DepartmentID is NULL or DepartmentID = p_DepartmentID);
END //
DELIMITER ;