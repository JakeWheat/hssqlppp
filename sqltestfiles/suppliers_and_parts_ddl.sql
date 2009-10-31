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
