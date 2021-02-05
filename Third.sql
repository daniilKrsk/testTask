--- какие предметы читает заданный преподаватель заданной группе
select s.name
from test.teacher t
join test.stgroup_subject_teacher gst on t.idteacher = gst.idteacher 
join test.stgroup_subject gs on gst.idstgroup_subject = gs.idstgroup_subject 
join test.stgroup g on gs.idstgroup = g.idstgroup 
join test.subject s on s.idsubject = gs.idsubject 
where t.name = 'Иванов' and g.name = 'Группа 2'

--какие преподаватели читают заданный предмет заданной группе,
select t.name
from test.teacher t
join test.stgroup_subject_teacher gst on t.idteacher = gst.idteacher 
join test.stgroup_subject gs on gst.idstgroup_subject = gs.idstgroup_subject 
join test.stgroup g on gs.idstgroup = g.idstgroup 
join test.subject s on s.idsubject = gs.idsubject 
where s.name = 'Предмет 1' and g.name = 'Группа 1'

--каким группам преподает заданный преподаватель
select g.name
from test.teacher t
join test.stgroup_subject_teacher gst on t.idteacher = gst.idteacher 
join test.stgroup_subject gs on gst.idstgroup_subject = gs.idstgroup_subject 
join test.stgroup g on gs.idstgroup = g.idstgroup 
join test.subject s on s.idsubject = gs.idsubject 
where t.name = 'Иванов'