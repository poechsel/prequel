(SELECT * FROM "departements.csv" e WHERE e.idd < 2)
UNION
(SELECT * FROM "departements.csv" e WHERE e.idd > 5);

[normal]
idd, nom, directeur
1, Direction, 1
6, Achats, 29
7, Comptabilité, 48
8, RH, 54
9, Exportations, 14
10, Administration, 21
