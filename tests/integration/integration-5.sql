SELECT p.titre, e.nom
FROM "employes.csv" e, "projets.csv" p, "membres.csv" m
WHERE
	e.ide = m.ide AND
	m.idp = p.idp AND
	e.dpt NOT IN (
		SELECT r.dpt
		FROM "employes.csv" r
		WHERE r.ide = p.responsable
	);

[normal]
titre, nom
Gemini,Coby Dennis
Lyra,Coby Dennis
Aries,Coby Dennis
Gemini,Paki Stanley
Perseus,Ray Atkins
Scorpius,Ray Atkins
Cancer,Otto Gray
Lyra,Otto Gray
Orion,Otto Gray
Capricornus,Nell Hurst
Aquarius,Nell Hurst
Aquarius,Devin Bolton
Lyra,McKenzie Jensen
Aquarius,McKenzie Jensen
Aquila,Yoshio Short
Scorpius,Yoshio Short
Lupus,Tana Dillard
Lupus,Tana Dillard
Perseus,Tana Dillard
Ursa,Rose Hancock
Perseus,Rose Hancock
Aquarius,Rose Hancock
Lupus,Guinevere Byers
Aquarius,Guinevere Byers
Andromeda ,Philip Whitney
Argo Navis,Philip Whitney
Aquila,Philip Whitney
Orion,Philip Whitney
Sagitta,Tatyana Becker
Capricornus,Tatyana Becker
Aquarius,Tatyana Becker
Scorpius,Tatyana Becker
Ophiuchus,Kenyon Hood
Ursa,Brandon Travis
Cancer,Brandon Travis
Virgo,Brandon Travis
Orion,Brandon Travis
Ursa,Giacomo Case
Scorpius,Giacomo Case
Scorpius,Giacomo Case
Pegasus,Hoyt Alston
Pisces,Oliver Lowe
Sagitta,Carly Casey
Aries,Garth Chen
Pegasus,Garth Chen
Libra,Ray Tran
Capricornus,Ray Tran
Scorpius,Ray Tran
Libra,Hyacinth Harris
Scorpius,Hyacinth Harris
Lupus,Tana Thomas
Virgo,Olympia Gay
Pisces,Olympia Gay
Argo Navis,Clarke Christian
Virgo,Clarke Christian
Virgo,Ivy Carroll
Scorpius,Ivy Carroll
Pisces,Joelle Meyer
Gemini,Martin Cabrera
Virgo,Diana Levy
Aquarius,Diana Levy
Cancer,Zenaida Ashley
Argo Navis,Bruno Shepherd
Cancer,Gemma Wells
Orion,Gemma Wells
Scorpius,Gemma Wells
Sagitta,Amir Wall
Aries,Amir Wall
Orion,Amir Wall
Lupus,Maxwell Trujillo
Aries,Mollie Poole
Scorpius,Mollie Poole
Ophiuchus,Uta Lindsey
Cancer,Elmo Harrington
Aquarius,Elmo Harrington
Pisces,Elmo Harrington
Ophiuchus,Iona Rasmussen
Ophiuchus,Aidan Livingston
Orion,Aidan Livingston
Aries,Vivian Gregory
Ophiuchus,Vivian Gregory
Perseus,Dante Pena
Ophiuchus,Dante Pena
Pegasus,Odysseus Joyce
Perseus,Clark Sanford
Perseus,Melinda Lott
Ophiuchus,Melinda Lott
Ursa,Eliana Santos
Aquila,Eliana Santos
Ursa,Diana Peterson
Perseus,Diana Peterson
Aries,Diana Peterson
Aquarius,Diana Peterson
Andromeda ,Yen Perez
Virgo,Yen Perez
Pegasus,Yen Perez
Scorpius,Yen Perez
Argo Navis,Vernon Nieves
Lyra,Honorato Bowman
Aries,Honorato Bowman
Scorpius,Honorato Bowman
Cancer,Kerry Walton
Virgo,Kerry Walton
Virgo,Kerry Walton
Argo Navis,Keelie Rosales
Gemini,Keelie Rosales
Virgo,Gregory Hunter
Ursa,Destiny Love
Sagitta,Destiny Love
Aries,Destiny Love
Orion,Destiny Love
Orion,Destiny Love
Orion,Peter Steele
Orion,Peter Steele
Lyra,Austin Mueller
Sagitta,Austin Mueller
Aries,Austin Mueller
Ursa,Forrest Harrison
Perseus,Forrest Harrison
Pegasus,Forrest Harrison
Ophiuchus,Forrest Harrison
Andromeda ,Nichole Baldwin
Pegasus,Nichole Baldwin
Aquarius,Nichole Baldwin
Perseus,Marshall Nguyen
Scorpius,Raphael Diaz
Ursa,Alexander Sullivan
Aquarius,Alexander Sullivan
Perseus,Hedwig Macias
Pisces,Hedwig Macias
Andromeda ,Fredericka Alexander
Lupus,Fredericka Alexander
Perseus,Fredericka Alexander
Scorpius,Fredericka Alexander
Cancer,Aristotle Lloyd
Libra,Aristotle Lloyd
Pegasus,Blaze Cooper
Lupus,Rebecca Carson
Libra,Rebecca Carson
Aries,Rebecca Carson
Ophiuchus,Rebecca Carson
Perseus,Kameko Short
Perseus,Kameko Short
Aries,Kameko Short
Argo Navis,Patrick Carey
Virgo,Patrick Carey
Orion,Patrick Carey
Pisces,Clark Hensley
Ursa,Deborah Noel
Pegasus,Deborah Noel
Pisces,Deborah Noel
Scorpius,Deborah Noel
Scorpius,Deborah Noel
Argo Navis,Iris Mcbride
Gemini,Iris Mcbride
Lupus,Iris Mcbride
Aquila,Iris Mcbride
Capricornus,Iris Mcbride
Aquarius,Hop White
Aries,Kenneth Estrada
Ophiuchus,Kenneth Estrada
Pegasus,Jillian Watson
Pegasus,Jillian Watson
Orion,Arden Lancaster
Andromeda ,Thor Bush
Aries,Heidi Vinson
Ophiuchus,Heidi Vinson
Pisces,Heidi Vinson
Aquarius,Yuli Pate
Ophiuchus,Yuli Pate
Pisces,Yuli Pate
Lupus,Kenyon Molina
Perseus,Kenyon Molina
Pegasus,Kenyon Molina
Scorpius,Lillian Berg
Cancer,Evan Lynch
Lyra,Evan Lynch
Perseus,Evan Lynch
Ophiuchus,Evan Lynch
Lupus,Norman Murphy
Perseus,Norman Murphy
Ophiuchus,Norman Murphy
