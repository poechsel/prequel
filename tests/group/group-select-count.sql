SELECT proj.responsable, COUNT(proj.idp) AS m
FROM "projets.csv" proj GROUP BY proj.responsable;

[normal]
responsable, m
4, 1
5, 2
8, 1
15, 1
26, 1
29, 1
38, 1
40, 1
44, 2
48, 1
68, 1
75, 3
79, 1
94, 1
96, 1
99, 1
