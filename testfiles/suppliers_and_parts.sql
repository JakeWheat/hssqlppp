create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);

create table p (
       p_po int primary key,
       pname text not null,
       color text not null,
       weight int not null,
       city text not null
);

create table sp (
       s_no int not null references s,
       p_no int not null references p,
       qty int not null,
       primary key (s_no,p_no)
);

create view good_supplier
  as selet s_no,status,city from s where status > 15;



/*

select * from s;

delete from sp where p_no =2

update s
set status =2 * status
,city = rome
where city = paris;

94-95
select s#,status from good_supplier where city = london

insert into p p#pname weight values ?pno ?pname ?pwt
delete from sp where ?city = (select city from s where s.sno= sp.snp);

update s 
set status = status + :raise where city = 'london';

*/