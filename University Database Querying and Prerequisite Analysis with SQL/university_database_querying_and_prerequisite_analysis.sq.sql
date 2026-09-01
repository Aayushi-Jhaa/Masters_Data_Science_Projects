-------------------------
-- University Database Querying and Prerequisite Analysis with SQL
-- Name: Aayushi Jha
-- zID: z5576935
-------------------------


-- Q1
DROP VIEW IF EXISTS Q1 CASCADE;
CREATE or REPLACE VIEW Q1(count) AS
SELECT COUNT(DISTINCT course_enrolments.student)
FROM course_enrolments
JOIN courses ON course_enrolments.course= courses.id
JOIN subjects ON courses.subject = subjects.id
WHERE course_enrolments.mark > 85
AND subjects.code LIKE 'COMP____'
;


-- Q2
DROP VIEW IF EXISTS Q2 CASCADE;
CREATE or REPLACE VIEW Q2(count) AS
SELECT COUNT(*)
FROM (
    SELECT course_enrolments.student
    FROM course_enrolments
    JOIN courses ON course_enrolments.course = courses.id
    JOIN subjects ON courses.subject = subjects.id
    WHERE subjects.code LIKE 'COMP____'
    AND course_enrolments.mark IS NOT NULL
    GROUP BY course_enrolments.student
    HAVING AVG(course_enrolments.mark) > 85
) AS Q2ANS
; 


-- Q3
DROP VIEW IF EXISTS Q3 CASCADE;
CREATE or REPLACE VIEW Q3(unswid,name) AS
SELECT people.unswid, people.name
FROM people 
JOIN (
    SELECT course_enrolments.student AS student, 
    AVG(course_enrolments.mark) AS average_mark, 
    COUNT(course_enrolments.course) AS comp_course_count
    FROM course_enrolments 
    JOIN courses ON course_enrolments.course = courses.id
    JOIN subjects ON courses.subject = subjects.id
    WHERE subjects.code LIKE 'COMP____' 
    AND course_enrolments.mark IS NOT NULL
    GROUP BY course_enrolments.student
    HAVING AVG(course_enrolments.mark) > 85 
    AND COUNT(course_enrolments.course) >= 6
) AS qualified_students
ON people.id = qualified_students.student
;


-- Q4
DROP VIEW IF EXISTS Q4 CASCADE;
CREATE or REPLACE VIEW Q4(unswid,name) AS
SELECT people.unswid, people.name
FROM people
JOIN (
    SELECT course_enrolments.student, subjects.code, subjects.uoc, MAX(course_enrolments.mark) AS max_mark
    FROM course_enrolments
    JOIN courses ON course_enrolments.course = courses.id
    JOIN subjects ON courses.subject = subjects.id
    WHERE subjects.code LIKE 'COMP____'
    AND course_enrolments.mark IS NOT NULL
    GROUP BY course_enrolments.student, subjects.code, subjects.uoc
) AS max_courses
ON people.id = max_courses.student
GROUP BY people.unswid, people.name
HAVING SUM(max_courses.max_mark * max_courses.uoc) :: NUMERIC / SUM(max_courses.uoc) :: NUMERIC > 85
AND COUNT(DISTINCT max_courses.code) > 5
;


-- Q5
DROP VIEW IF EXISTS Q5 CASCADE;
CREATE or REPLACE VIEW Q5(count) AS
SELECT COUNT(DISTINCT subjects.id) AS count
FROM subjects
JOIN courses ON subjects.id = courses.subject
JOIN semesters ON semesters.id = courses.semester
JOIN orgunits ON orgunits.id = subjects.offeredby
WHERE orgunits.longname = 'School of Computer Science and Engineering'
AND semesters.year = 2012 
;


-- Q6
DROP VIEW IF EXISTS Q6 CASCADE;
CREATE or REPLACE VIEW Q6(count) AS
SELECT COUNT(DISTINCT course_staff.staff) AS count
FROM course_staff
JOIN staff_roles ON course_staff.role = staff_roles.id
JOIN courses ON course_staff.course = courses.id
JOIN semesters ON courses.semester = semesters.id
JOIN affiliations ON course_staff.staff = affiliations.staff
JOIN orgunits ON affiliations.orgunit = orgunits.id
WHERE staff_roles.name = 'Course Lecturer'
AND semesters.year = 2012
AND orgunits.longname = 'School of Computer Science and Engineering'
;


-- Q7
DROP VIEW IF EXISTS Q7 CASCADE;
CREATE or REPLACE VIEW Q7(course_id,unswid) AS
SELECT courses.id AS course_id, people.unswid AS unswid
FROM courses 
JOIN subjects ON courses.subject = subjects.id
JOIN orgunits o1 ON subjects.offeredby = o1.id
JOIN semesters ON courses.semester = semesters.id
JOIN course_staff ON course_staff.course = courses.id
JOIN staff_roles ON course_staff.role = staff_roles.id
JOIN people ON people.id = course_staff.staff
JOIN affiliations ON affiliations.staff = course_staff.staff
JOIN orgunits o2 ON affiliations.orgunit = o2.id
WHERE semesters.year = 2012
AND o1.longname = 'School of Computer Science and Engineering'
AND staff_roles.name = 'Course Lecturer'
AND o2.longname = 'School of Computer Science and Engineering'
;


-- Q8
DROP VIEW IF EXISTS Q8 CASCADE;
CREATE or REPLACE VIEW Q8(course_id,unswid) AS
SELECT courses.id AS course_id, people.unswid AS unswid
FROM courses
JOIN subjects ON courses.subject = subjects.id
JOIN orgunits o1 ON subjects.offeredby = o1.id
JOIN semesters ON courses.semester = semesters.id
JOIN course_staff ON course_staff.course = courses.id
JOIN staff_roles ON course_staff.role = staff_roles.id
JOIN people ON people.id = course_staff.staff
JOIN affiliations ON affiliations.staff = course_staff.staff
JOIN orgunits o2 ON affiliations.orgunit = o2.id
WHERE semesters.year = 2012
AND o1.longname = 'School of Computer Science and Engineering'
AND staff_roles.name = 'Course Lecturer'
AND o2.longname = 'School of Computer Science and Engineering'
;


-- Q9
DROP FUNCTION IF EXISTS Q9 CASCADE;
CREATE or REPLACE FUNCTION Q9(subject1 integer, subject2 integer) returns text
AS $$
DECLARE
    Subject1_code TEXT;        
    Prerequiste TEXT;  
BEGIN

    -- Get the course code for subject1
    SELECT code INTO Subject1_code FROM subjects WHERE id = subject1;

    -- Get the prerequisite string for subject2
    SELECT _prereq INTO Prerequiste FROM subjects WHERE id = subject2;
    
    IF POSITION(Subject1_code IN Prerequiste) > 0 THEN
        RETURN format('%s is a direct prerequisite of %s.', subject1, subject2);
    ELSE
        RETURN format('%s is not a direct prerequisite of %s.', subject1, subject2);
    END IF;

END
$$ language plpgsql;


-- Q10
DROP FUNCTION IF EXISTS Q10 CASCADE;
CREATE or REPLACE FUNCTION Q10(subject1 integer, subject2 integer) returns text
AS $$
DECLARE 
    Subject1_code TEXT;
    Subject2_Prerequisite TEXT;
    Prerequisite_list INTEGER[];
    Temporary TEXT [];
    Course TEXT;
    Course_Id INTEGER;
    Status  BOOLEAN := TRUE;
    Course_Id_Temporary TEXT;
    Course_Id_Temporary_Id INTEGER;
    Course_Id_Temporary_code TEXT;

BEGIN 

    -- Get the course code for subject1
    SELECT code INTO Subject1_code FROM subjects WHERE Id= subject1;

    -- Get the prerequisite string for subject2
    SELECT _prereq INTO Subject2_Prerequisite FROM subjects WHERE Id = subject2;

    -- If subject2 has prerequisites, extract them and store the corresponding IDs in Prerequisite_list
    IF Subject2_Prerequisite IS NOT NULL THEN 
        Temporary := array(SELECT unnest (regexp_matches(Subject2_Prerequisite, '[A-Z]{4}\d{4}','g')));
        FOREACH Course IN ARRAY Temporary LOOP 
            SELECT Id INTO Course_Id FROM subjects WHERE code = Course ;
            IF Course_Id IS NOT NULL AND Course_Id != subject2 THEN 
                Prerequisite_list:= array_append(Prerequisite_list, Course_Id);
            END IF;
        END loop;
    END IF;

    -- Loop to find indirect prerequisites by checking the prerequisites of each subject in the Prerequisite_list
    WHILE Status LOOP
        Status := FALSE;
        FOREACH  Course_Id IN ARRAY Prerequisite_list LOOP 
            SELECT _prereq INTO Course_Id_Temporary FROM subjects WHERE Id = Course_Id ;
            IF Course_Id_Temporary IS NOT NULL THEN 
                Temporary := array(SELECT unnest (regexp_matches(Course_Id_Temporary, '[A-Z]{4}\d{4}','g')));
                FOREACH Course_Id_Temporary_code IN ARRAY Temporary LOOP
                    SELECT Id INTO Course_Id_Temporary_Id FROM subjects WHERE code = Course_Id_Temporary_code ;
                    IF NOT Course_Id_Temporary_Id = ANY(Prerequisite_list) AND Course_Id_Temporary_Id != subject2 THEN 
                        Prerequisite_list:= array_append(Prerequisite_list, Course_Id_Temporary_Id);
                        Status := TRUE;
                    END IF;
                END LOOP;
            END IF;
        END LOOP;
    END LOOP;

    -- Check if subject1 is a prerequisite of subject2
    IF subject1 = ANY (Prerequisite_list) THEN 
        RETURN $1 || ' is a prerequisite of ' || $2 || '.' ;
    ELSE 
        RETURN $1 || ' is not a prerequisite of ' || $2 || '.';
    END IF;
    
END

$$ language plpgsql;

