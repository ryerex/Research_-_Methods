-- Q1: Who takes CS112
--
SELECT sno 
FROM take
WHERE cno = 'CS112';

-- Q2: What are the student numbers and names of students who take CS12
--
SELECT student.sno,sname, age
FROM student, take
WHERE student.sno = take.sno
	AND take.cno = 'CS112'
ORDER BY age DESC,sname;

--	Alternatively	
SELECT sno, sname
FROM student
WHERE 	(sno IN
		(SELECT sno 
		FROM take
		WHERE (cno = 'CS112')));
		
-- Q3: Who takes CS112 OR CS114
--
SELECT DISTINCT sno 
FROM take
WHERE cno = 'CS112'
	OR cno = 'CS114';
	
-- Q4: Who takes both CS112 AND CS114
-- Type 1 Version
SELECT x.sno
FROM take x, take y
WHERE (y.sno = x.sno)
	AND (y.cno = 'CS112')
	AND (x.cno = 'CS114');
		

-- Q5: Who does NOT take CS112
--
SELECT sno, sname
FROM student
WHERE NOT (sno IN
 	      (SELECT sno 
		  FROM take
		  WHERE (cno = 'CS112')));


-- Q6: Who takes a course which is NOT CS112
--
SELECT DISTINCT sno
FROM take
WHERE cno != 'CS112';

-- Q7: Who takes AT LEAST 2 courses
--
SELECT DISTINCT x.sno
FROM take x, take y
WHERE x.sno = y.sno
	AND x.cno != y.cno;

-- Q8: Who takes AT MOST 2 courses
--
SELECT sno
FROM student
WHERE NOT sno IN
	(SELECT DISTINCT x.sno
 	FROM take x, take y, take z
	WHERE x.sno = y.sno
		AND y.sno = z.sno
		AND x.cno != y.cno
		AND y.cno != z.cno
		AND x.cno != z.cno);


-- Q9: Who takes EXACTLY 2 courses
--
SELECT DISTINCT x.sno
FROM take x, take y
WHERE x.sno = y.sno
	AND x.cno != y.cno
	AND NOT x.sno IN 
	(SELECT DISTINCT x.sno
	FROM take x, take y, take z
	WHERE x.sno = y.sno
		AND y.sno = z.sno
		AND x.cno != y.cno
		AND y.cno != z.cno
		AND x.cno != z.cno);


-- Q10: Who Takes ONLY CS112
-- This selects those who take  a course that is NOT CS112
SELECT DISTINCT sno
FROM take
WHERE cno != 'CS112';
	
-- This selects those who DO take CS112
SELECT DISTINCT sno
FROM take
WHERE cno = 'CS112';
	
-- Combine them
SELECT DISTINCT sno
FROM take
WHERE cno = 'CS112'
	AND NOT sno IN
		(SELECT DISTINCT sno
		FROM take
		WHERE cno != 'CS112');
		

-- Q11: Who takes EITHER CS CS112 OR CS114
--
SELECT sno
FROM take
WHERE 	((cno = 'CS112')
	OR 	 (cno = 'CS114'))
	AND NOT (sno IN
		(SELECT x.sno
		FROM take x, take y
		WHERE 	(x.sno = y.sno)
			AND	(x.cno = 'CS112')
			AND (y.cno = 'CS114')));
	

-- Q12: Who are the YOUNGEST students
--
SELECT sno
FROM student
WHERE NOT (age IN
	(SELECT x.age
	FROM student x, student y
	WHERE x.age > y.age));

-- Alternatively
SELECT sno
FROM student
WHERE NOT (sno IN
	(SELECT x.sno
	FROM student x, student y
	WHERE x.age > y.age));
	
-- Q13: Who takes EVERY course
-- This returns any combination of student and course not taken
SELECT sno||' '||cno 
FROM student, courses
WHERE (sno||cno NOT IN
	  (SELECT sno||cno
	  FROM take));
		  
-- Full query
SELECT DISTINCT sno 
FROM student
WHERE (sno NOT IN
	  (SELECT sno
	  FROM student, courses
	  WHERE (sno||cno NOT IN
		(SELECT sno||cno
		FROM take))));
		
-- Q14: For each department that jas more than 3 
--		professors older than 50, what is the 
--		average salary of such professors?
--
SELECT dept, AVG(salary)
FROM professor
WHERE age > 50
GROUP BY dept
HAVING (COUNT(*) > 0);

-- Q15: What is the GPA of each student
--

-- Q16: What is the overall average salary of all
--		who are older than 50?
--

-- Q17: Whose salsry is greater than the overall
--		average?
--

-- Q18: Whose salary is greater than the average
--		salary within their department?
--

-- 	What are full names and ages of professors who
--	teach CS112
SELECT p.fname, p.lname, age
FROM professor p, teach t
WHERE (p.fname = t.fname)
	AND (p.lname = t.lname)
	AND (cno = 'CS112');


--	Who does not teach CS112
--
SELECT fname, lname
FROM professor
WHERE (fname||lname NOT IN
  	(SELECT fname||lname
	FROM teach
	WHERE (cno = 'CS112')));
	


--	Use of concatenation and substring()
SELECT lname ||', '||substring(fname,1,1)||'.'
FROM professor;
		  
