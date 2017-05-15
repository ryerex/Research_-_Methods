/* table of students */
create table student
( sno integer,
sname varchar(10),
age integer
);


/* table of courses */
create table courses
( cno varchar(5),
title varchar(10),
credits integer
);

/* table of professors */
create table professor
( lname varchar(10),
fname varchar(10),
dept varchar(10),
salary integer,
age integer
);

/* table of students and the courses they take */
create table take
( sno integer,
cno varchar(5)
);

/* table of professors and the courses they teach */
create table teach
( lname varchar(10),
fname varchar(10),
cno varchar(5)
);

insert into student values (1,'AARON',20);
insert into student values (2,'CHUCK',21);
insert into student values (3,'DOUG',20);
insert into student values (4,'MAGGIE',19);
insert into student values (5,'STEVE',22);
insert into student values (6,'JING',18);
insert into student values (7,'BRIAN',21);
insert into student values (8,'KAY',20);
insert into student values (9,'GILLIAN',20);
insert into student values (10,'CHAD',21);
insert into courses values ('CS112','PHYSICS',4);
insert into courses values ('CS113','CALCULUS',4);
insert into courses values ('CS114','HISTORY',4);
insert into professor values ('CHOI','FRANK','SCIENCE',400,45);
insert into professor values ('GUNN','JACK','HISTORY',300,60);
insert into professor values ('MAYER','BETTY','MATH',400,55);
insert into professor values ('POMEL','GERTRUDE','SCIENCE',500,65);
insert into professor values ('FEUER','ER','MATH',400,40);
insert into take values (1,'CS112');
insert into take values (1,'CS113');
insert into take values (1,'CS114');
insert into take values (2,'CS112');
insert into take values (3,'CS112');
insert into take values (3,'CS114');
insert into take values (4,'CS112');
insert into take values (4,'CS113');
insert into take values (5,'CS113');
insert into take values (6,'CS113');
insert into take values (6,'CS114');
insert into teach values ('CHOI','FRANK','CS112');
insert into teach values ('CHOI','FRANK','CS113');
insert into teach values ('CHOI','FRANK','CS114');
insert into teach values ('POMEL','GERTRUDE','CS113');
insert into teach values ('MAYER','BETTY','CS112');
insert into teach values ('MAYER','BETTY','CS114');