(SELECT * FROM "departements.csv" e WHERE e.idd > 2)
MINUS
(SELECT * FROM "departements.csv" e WHERE e.idd > 5);

[normal]
idd, nom, directeur
3, Développement, 8
4, Marketing, 9
5, Production, 70
