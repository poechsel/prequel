SELECT proj.responsable, COUNT(proj.idp) AS m
FROM "tests/sources/projets.csv" proj GROUP BY proj.responsable
HAVING m > 1;

[normal]
responsable, m
5, 2
44, 2
75, 3
