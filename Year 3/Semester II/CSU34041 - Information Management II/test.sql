-- CSU34041 2022 paper, Q1 iii

-- create tables
CREATE TABLE DogTable (
    DogID int NOT NULL,
    DogName varchar(255),
    DogBreed varchar(255),
    DogDesc varchar(255),
    PRIMARY KEY (DogID)
);
CREATE TABLE StaffTable (
    StaffID int NOT NULL,
    StaffName varchar(255),
    StaffRole varchar(255),
    PRIMARY KEY (StaffID)
);
CREATE TABLE OwnerTable (
    OwnerID int NOT NULL,
    OwnerName varchar(255),
    PNumber varchar(255),
    OwnerAddress varchar(255),
    PRIMARY KEY (OwnerID)
);
CREATE TABLE TreatmentTable (
    TreatmentID int NOT NULL,
    DogID int,
    StaffID int,
    DateStart Date,
    DateEnd Date,
    TreatmentDesc varchar(255),
    PRIMARY KEY (TreatmentID),
    FOREIGN KEY (StaffID) REFERENCES StaffTable(StaffID),
    FOREIGN KEY (DogID) REFERENCES DogTable(DogID)
);
CREATE TABLE AdoptionTable (
    DogID int,
    OwnerID int,
    AdoptionDate Date,
    FOREIGN KEY (OwnerID) REFERENCES OwnerTable(OwnerID),
    FOREIGN KEY (DogID) REFERENCES DogTable(DogID)
);

-- insert info into tables
insert into DogTable 
values (00454, "spot", "black", "cool");
insert into DogTable 
values (00132, "race", "white", "big");
insert into DogTable 
values (00111, "axel", "mixed", "uwu");
insert into StaffTable
values(4, "maria", "vet");
insert into StaffTable
values(1, "adam", "vet");
insert into StaffTable
values(10, "bea", "vet");
insert into TreatmentTable
values(141, 00111, 10, '2008-11-11', '2008-11-12', 'Vaccinated for Parvovirus');
insert into TreatmentTable
values(41, 00132, 10, '2008-11-11', '2008-11-12', 'Vaccinated for Parvovirus');
insert into TreatmentTable
values(10, 00454, 10, '2008-11-11', '2008-11-12', 'long');
insert into TreatmentTable
values(1, 00454, 1, '2008-11-11', '2008-11-12', 'long');
insert into TreatmentTable
values(4, 00454, 4, '2008-11-11', '2008-11-12', 'long');
insert into OwnerTable
values(900, "iris", "0831234567", 'whoville');
insert into AdoptionTable
values(00454, 900, '2008-11-22');

-- questions
-- a
select * from DogTable, TreatmentTable
where TreatmentTable.TreatmentDesc = "Vaccinated for Parvovirus"
and TreatmentTable.DogID = DogTable.DogID;
-- b
select count(distinct StaffID) from TreatmentTable where TreatmentTable.DogID = 00454;
-- c
select * from AdoptionTable; -- before
delete from AdoptionTable where AdoptionTable.DogID = 00454;
select * from AdoptionTable; -- after - note how it doesn't print anything
-- d
-- reinsert adopted dogs
insert into AdoptionTable
values(00454, 900, '2008-11-22');
insert into AdoptionTable
values(00132, 900, '2008-11-22');
insert into AdoptionTable
values(00111, 900, '2008-11-22');
-- select * from OwnerTable; -- before
-- step 1
alter table OwnerTable add DogsOwned int;
-- select * from OwnerTable; -- after

-- step 2
update OwnerTable 
set OwnerTable.DogsOwned = (select count(distinct DogID) from AdoptionTable 
where AdoptionTable.OwnerID = OwnerTable.OwnerID);

-- select * from OwnerTable; -- after adoptions



