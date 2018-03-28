SELECT proj.responsable, MAX(proj.idp) AS m
FROM "tests/sources/projets.csv" proj GROUP BY proj.responsable;

[normal]
responsable, m
4, 13
5, 9
8, 19
15, 18
26, 14
29, 3
38, 12
40, 17
44, 16
48, 7
68, 1
75, 20
79, 5
94, 4
96, 6
99, 8