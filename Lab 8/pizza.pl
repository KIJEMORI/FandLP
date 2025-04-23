
salami(salami,1).
salami(bavarian,1).
salami(margarita,0).
salami(pesto,0).

tomato(salami,0).
tomato(bavarian,0).
tomato(margarita,1).
tomato(pesto,1).

chicken(salami,0).
chicken(bavarian,0).
chicken(margarita,0).
chicken(pesto,1).

olives(salami,0).
olives(bavarian,1).
olives(margarita,0).
olives(pesto,0).

question1(X1):-	write("Is pizza has salami?"),nl,
				write("1. Yes"),nl,
				write("0. NO"),nl,
				read(X1).

question2(X2):-	write("Is pizza has tomato?"),nl,
				write("1. Yes"),nl,
				write("0. NO"),nl,
				read(X2).

question3(X3):-	write("Is pizza has chicken?"),nl,
				write("1. Yes"),nl,
				write("0. NO"),nl,
				read(X3).

question4(X4):-	write("Is pizza has olives?"),nl,
				write("1. Yes"),nl,
				write("0. NO"),nl,
				read(X4).

pr:-	question1(X1),question2(X2),question3(X3),question4(X4),
		salami(X,X1),tomato(X,X2),chicken(X,X3),olives(X,X4),
		write(X).