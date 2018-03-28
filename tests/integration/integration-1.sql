SELECT depts.dir1 + depts.dir2 FROM (
	SELECT
		depts1.directeur AS dir1,
		depts2.directeur AS dir2
	FROM
		"tests/sources/departements.csv" AS depts1,
		"tests/sources/departements.csv" AS depts2
	WHERE depts2.idd - depts1.idd = 1)
AS depts;

[normal]
1
19
26
17
79
99
77
102
68
35
