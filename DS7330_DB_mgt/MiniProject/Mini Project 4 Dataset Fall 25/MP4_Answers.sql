USE university;

SELECT ID, name, dept_name
FROM student
ORDER BY name ASC;

SELECT name, salary
FROM instructor
WHERE dept_name IN ('Comp. Sci.', 'Elec. Eng.')
ORDER BY salary DESC;

SELECT *
FROM course
WHERE course_id LIKE 'CS-1%';

SELECT MAX(enrollment) AS max_enrollment, MIN(enrollment) AS min_enrollment
FROM (
  SELECT course_id, sec_id, semester, year, COUNT(*) AS enrollment
  FROM takes
  GROUP BY course_id, sec_id, semester, year
  HAVING COUNT(*) > 0
) AS enrollments;

CREATE VIEW faculty AS
SELECT ID, name, dept_name
FROM instructor;

CREATE VIEW CSinstructors AS
SELECT *
FROM instructor
WHERE dept_name = 'Comp. Sci.';