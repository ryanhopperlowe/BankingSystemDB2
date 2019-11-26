-- Project 2 DDLs: drop.clp
--
connect to cs157a;
--
-- drop previous definition first
drop specific function p2.encrypt;
drop specific function p2.decrypt;
drop table p2.account;
drop table p2.customer;
--
-- 
commit;
terminate;