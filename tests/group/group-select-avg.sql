SELECT proj.responsable, AVG(proj.idp) AS m
FROM "projets.csv" proj GROUP BY proj.responsable;

[normal]
responsable, m
4, 13.
5, 5.5
8, 19.
15, 18.
26, 14.
29, 3.
38, 12.
40, 17.
44, 13.
48, 7.
68, 1.
75, 15.3333333333
79, 5.
94, 4.
96, 6.
99, 8.
