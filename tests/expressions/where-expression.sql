SELECT * FROM "tests/sources/departements.csv" AS depts
WHERE (depts.idd - 1) = 2 * (depts.idd - 1);

[normal]
idd, nom, directeur
1, Direction, 1
