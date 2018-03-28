SELECT e.dpt, e.nom
FROM "tests/sources/employes.csv" e
WHERE e.dpt IN (
	SELECT s.dpt
	FROM
		"tests/sources/employes.csv" s,
		"tests/sources/departements.csv" ds
	WHERE
		ds.directeur = s.ide AND
		e.dpt = ds.idd
);

[normal]
dpt, nom
1,Hamish Fulton
1,Gemma Calhoun
1,Devin Bolton
1,McKenzie Jensen
1,Kieran Weaver
10,Tatyana Becker
2,Kenyon Hood
10,Hoyt Alston
2,Oliver Lowe
10,Ray Tran
1,Tana Thomas
1,Vivian Gregory
10,Melinda Lott
1,Eliana Santos
2,Diana Peterson
2,Vernon Nieves
1,Austin Mueller
2,Forrest Harrison
10,Alexander Sullivan
1,Fredericka Alexander
1,Clark Henson
1,Kameko Short
1,Iris Mcbride
1,Thor Bush
2,Ora Grimes
2,Lillian Berg
