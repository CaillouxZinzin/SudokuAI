% FAITS

% Exemple grille partiellement remplie taille 9 [[8,2,1,5,9,7,6,4,3],[0,7,3,0,2,4,8,5,1],[4,6,5,1,8,3,0,2,0],[0,3,6,2,4,1,9,8,7],[2,0,8,0,3,6,4,0,5],[1,4,7,8,5,0,3,6,2],[0,5,0,9,0,8,1,7,4],[7,8,9,4,1,2,0,3,0],[6,0,4,3,7,0,2,9,8]]

% Exemple grille vide taille 16 [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

% Exemple grille partiellement remplie taille 16 [[14,6,0,13,0,0,9,0,12,0,0,0,0,16,0,3],[15,7,2,0,13,0,0,0,14,16,0,0,0,0,9,0],[0,16,4,8,15,0,0,0,13,9,1,5,12,7,0,11],[0,0,0,10,0,11,8,16,3,0,0,2,14,15,0,0],[0,14,0,0,0,9,0,11,0,0,5,7,13,1,0,0],[1,5,3,2,14,0,13,0,0,0,9,4,0,10,0,0],[0,8,13,9,0,0,1,0,0,0,0,0,0,11,4,0],[0,0,0,0,10,2,4,5,1,0,0,0,9,14,3,8],[2,15,0,0,0,0,0,9,5,0,0,6,3,0,0,0],[4,0,0,0,3,0,15,0,9,11,7,0,2,0,0,16],[0,0,9,0,4,14,0,0,10,0,0,15,7,0,0,0],[13,12,16,6,5,1,7,0,4,0,14,0,0,9,10,0],[9,0,10,3,11,0,0,0,0,12,2,0,4,8,0,13],[8,0,14,15,16,3,0,0,0,5,13,0,0,12,11,0],[0,13,0,0,8,0,0,1,0,14,3,9,0,2,7,6],[6,0,7,12,9,0,10,13,0,0,11,0,0,0,5,14]]

% Exemple grille remplie taille 16 [[14,6,11,13,2,7,9,4,12,10,15,8,5,16,1,3],[15,7,2,1,13,12,5,3,14,16,4,11,8,6,9,10],[3,16,4,8,15,6,14,10,13,9,1,5,12,7,2,11],[5,9,12,10,1,11,8,16,3,7,6,2,14,15,13,4],[10,14,15,4,6,9,3,11,16,8,5,7,13,1,12,2],[1,5,3,2,14,8,13,12,11,6,9,4,15,10,16,7],[12,8,13,9,7,16,1,15,2,3,10,14,6,11,4,5],[7,11,6,16,10,2,4,5,1,15,12,13,9,14,3,8],[2,15,8,7,12,10,11,9,5,13,16,6,3,4,14,1],[4,10,1,14,3,13,15,6,9,11,7,12,2,5,8,16],[11,3,9,5,4,14,16,2,10,1,8,15,7,13,6,12],[13,12,16,6,5,1,7,8,4,2,14,3,11,9,10,15],[9,1,10,3,11,5,6,14,7,12,2,16,4,8,15,13],[8,4,14,15,16,3,2,7,6,5,13,10,1,12,11,9],[16,13,5,11,8,4,12,1,15,14,3,9,10,2,7,6],[6,2,7,12,9,15,10,13,8,4,11,1,16,3,5,14]]



nombre(1).
nombre(2).
nombre(3).
nombre(4).
nombre(5).
nombre(6).
nombre(7).
nombre(8).
nombre(9).
nombre(10).
nombre(11).
nombre(12).
nombre(13).
nombre(14).
nombre(15).
nombre(16).


taille(9).
taille(16).

% REGLES

valide(X,S) :- nombre(X), X =< S, X > 0.

ligne(S) :-  S = 9, write('------------------------------------').
ligne(S) :-  S = 16, write('----------------------------------------------------------------').

afficher_ligne([]) :- write('|').
afficher_ligne([T|Q]) :- T < 10, write('|  '), write(T), write('  '), afficher_ligne(Q).
afficher_ligne([T|Q]) :- write('| '), write(T), write(' '), afficher_ligne(Q).

afficher_sudoku([],S) :- ligne(S), nl, nl.
afficher_sudoku([T|Q],S) :- ligne(S), nl, afficher_ligne(T), nl, afficher_sudoku(Q,S).

% Crée une nouvelle liste vide de taille S

creer_liste([],0).
creer_liste([0|L],S) :- NS is S-1, creer_liste(L,NS).

% Crée une nouvelle grille de sudoku vide de taille S

creer_grille([],0,_).
creer_grille([NL|L],T,S) :- NT is T-1, creer_grille(L,NT,S), creer_liste(NL,S).

% Retourne une ligne de la grille par son indice

extraire_ligne([T|_],1,T).
extraire_ligne([_|Q],N,R) :- NN is N-1, extraire_ligne(Q,NN,R).

% Trouver un élément d'une liste par son indice

trouve_element_ligne([T|_],1,T) :- !.
trouve_element_ligne([_|Q],N,R) :- P is N-1, trouve_element_ligne(Q,P,R).

% Trouver un élément dans une grille par ses coordonnées

trouve_element_grille(G,X,Y,R) :- extraire_ligne(G,Y,L), trouve_element_ligne(L,X,R).

% Savoir si un élément existe dans une liste

element(X,[X|_]) :- !.
element(X,[_|Q]) :- element(X,Q).

% Savoir si un élément existe dans une grille

element_grille(G,E,S) :- valide(Y,S), extraire_ligne(G,Y,R), element(E,R).

% Ajouter un élément à une liste

concat_element(E,L,[E|L]).

% Concaténer deux listes

concat_liste([],L,L).
concat_liste([T|Q],L,[T|R]) :- concat_liste(Q,L,R).

% Tester si les éléments de la liste sont tous différents ou égaux à 0

tous_differents([]).
tous_differents([0|Q]) :- tous_differents(Q).
tous_differents([T|Q]) :- \+ element(T,Q), tous_differents(Q).

% Tester si une ligne de sudoku est valide

valide_ligne(L) :- tous_differents(L).

% Tester si les lignes de la grille de sudoku sont valides

valide_lignes([]).
valide_lignes([T|Q]) :- valide_ligne(T), valide_lignes(Q).

% Extraire une colone sous forme de liste

creer_colone([],_,[]).
creer_colone([T|Q],N,[X|C]) :- trouve_element_ligne(T,N,X), creer_colone(Q,N,C).

% Tester si les colones de la grille de sudoku sont valides

valide_colones(_,0) :- !.
valide_colones(G,S) :- creer_colone(G,S,C), valide_ligne(C), NS is S-1, valide_colones(G,NS).

% Extraire une partie d'une ligne

extraire_partie_ligne([T|_],1,1,[T]).
extraire_partie_ligne([T|Q],1,F,[T|R]) :- NF is F-1, extraire_partie_ligne(Q,1,NF,R).
extraire_partie_ligne([_|Q],D,F,R) :- ND is D-1, NF is F-1, extraire_partie_ligne(Q,ND,NF,R).

% Extraire une partie d'une ligne au sein de la grille

extraire_partie_ligne_grille(G,N,D,F,NR) :- extraire_ligne(G,N,R), extraire_partie_ligne(R,D,F,NR).

% Extraire un bloc de la grille en position N

creer_bloc(G,S,N,B) :- T is round(sqrt(S)), creer_bloc(G,S,N,T,B).
creer_bloc(_,_,_,0,[]).
creer_bloc(G,S,N,T,NB) :- D is ((N-1)*round(sqrt(S)))+1, F is (((N-1)*round(sqrt(S)))+round(sqrt(S))), extraire_partie_ligne_grille(G,T,D,F,R), NT is T-1, creer_bloc(G,S,N,NT,B), concat_liste(B,R,NB).

% Tester si les blocs de la grille de sudoku sont valides

valide_blocs(G,S) :- T is round(sqrt(S)), valide_blocs(G,S,T).

valide_blocs(_,_,0).
valide_blocs(G,S,T) :- creer_bloc(G,S,T,B), valide_ligne(B), NT is T-1, valide_blocs(G,S,NT).

% Tester si une grille est valide

valide_grille(G,S) :- valide_lignes(G), !, valide_colones(G,S), !, valide_blocs(G,S), !.

% Modifie un élément d'unep liste égal à 0

modifier_ligne([],_,_,[]) :- !.
modifier_ligne([0|Q],1,X,[X|R]) :- P is 0, modifier_ligne(Q,P,X,R), !.
modifier_ligne([_|_],1,_,_) :- !, fail.
modifier_ligne([T|Q],N,X,[T|R]) :- P is N-1, modifier_ligne(Q,P,X,R).

% Modifie un élément égal à 0 d'une grille

modifier_grille([],_,_,_,[]) :- !.
modifier_grille([T|Q],N,X,1,[NR|R]) :- !, modifier_ligne(T,X,N,NR), NY is 0, modifier_grille(Q,N,X,NY,R), !.
modifier_grille([T|Q],N,X,Y,[T|R]) :- NY is Y-1, modifier_grille(Q,N,X,NY,R).

% Vide une case dans une liste

vider_case([],_,[]) :- !.
vider_case([_|Q],1,[0|R]) :- P is 0, vider_case(Q,P,R), !.
vider_case([0|_],1,_) :- !, fail.
vider_case([T|Q],N,[T|R]) :- P is N-1, vider_case(Q,P,R).

% Vérifie si la case est vide au sein de la liste

vide(L,X) :- trouve_element_ligne(L,X,R), R = 0.

% Vérifie si la case est vide au sein de la grille

vide_grille(G,X,Y) :- extraire_ligne(G,Y,R), vide(R,X).

% Vide une case dans une grille

vider_grille([],_,_,[]).
vider_grille([T|Q],X,1,[NR|R]) :- !, vider_case(T,X,NR), NY is 0, vider_grille(Q,X,NY,R), !.
vider_grille([T|Q],X,Y,[T|R]) :- NY is Y-1, vider_grille(Q,X,NY,R).

% Vérifier si une grille est pleine

grille_pleine(G,S) :- \+ element_grille(G,0,S).

% Vérifier si un changement est valide

changement_valide(G,S,X,Y,N) :- valide(X,S), valide(Y,S), valide(N,S), modifier_grille(G,N,X,Y,NG), valide_grille(NG,S).

% Détermine combien de nombres seront générés aléatoirement

niveau_generation(S,7) :- S = 9, !.
niveau_generation(S,20) :- S = 16.

% Lancement jeu de difficulté D (avec D chiffres préremplis) et de taille S

sudoku(D,S) :-
  taille(S),
  creer_grille(G,S,S),
  niveau_generation(S,W),
  L is S*S,
  random(1,L,R),
  genere(G,R,W,D,S).


% On teste si la grille est resolvable avant de lancer le jeu, sinon on en regénère une

test(G,D,S) :- resolution(G,_,_,_,1,D,S).
test(_,D,S) :-
  creer_grille(G,S,S),
  niveau_generation(S,W),
  L is S*S,
  random(1,L,R),
  genere(G,R,W,D,S).

% Validation grille de départ et choix du mode

play(G,S) :-
afficher_sudoku(G,S), nl, nl,
write("Entrez '1.' pour jouer, '2.' pour resoudre ou '3.' pour quitter."), nl,
read(C),
choix(C,G,S).

% Répartition des choix

choix(1,G,S) :- nl, write("Bienvenue dans le jeu du Sudoku : Quand il vous est demande d'interagir avec le programme, veuillez entrer votre reponse suivie d un point avant de valider avec la touche entree. Pour sortir du jeu, entrez 0 à toutes les coordonnees. Bon jeu !"), nl, nl, write("Quel coup voulez vous realiser ?"), nl, nl, write("X ="), read(X), write("Y ="), read(Y), write("Nombre ="), read(N), jouer(G,X,Y,N,S).
choix(2,G,S) :- resolution(G,_,_,_,2,_,S), nl, nl, write("Voici la solution!").
choix(3,_,_) :- write("A bientot !").

% Sortir du jeu

jouer(_,0,0,0,_).

% Tour de jeu final

jouer(G,X,Y,N,S) :- valide(X,S), valide(Y,S), valide(N,S), modifier_grille(G,N,X,Y,NG), valide_grille(NG,S), nl, grille_pleine(NG,S), afficher_sudoku(NG,S), nl, nl, write("Le sudoku est fini, felicitations"), !.

% Tour de jeu : remplissage d'une case vide

jouer(G,X,Y,N,S) :- valide(X,S), valide(Y,S), valide(N,S), modifier_grille(G,N,X,Y,NG), valide_grille(NG,S), nl, afficher_sudoku(NG,S), nl, write("Quel coup voulez vous realiser ?"), nl, nl, write("X ="), read(A), write("Y ="), read(B), write("Nombre ="), read(M), jouer(NG,A,B,M,S).
jouer(G,X,Y,N,S) :- valide(X,S), valide(Y,S), valide(N,S), modifier_grille(G,N,X,Y,NG), \+ valide_grille(NG,S), nl, write("Coup impossible... Quel coup voulez vous realiser ?"), nl, nl, write("X ="), read(A), write("Y ="), read(B), write("Nombre ="), read(M), jouer(G,A,B,M,S).

% Tour de jeu : Essai de remplissage d'une case occupée

jouer(G,X,Y,N,S) :- valide(X,S), valide(Y,S), valide(N,S), \+ modifier_grille(G,N,X,Y,_), nl, write("Coup impossible... Quel coup voulez vous realiser ?"), nl, write("X ="), read(A), nl, write("Y ="), read(B), nl, write("Nombre ="), read(M), jouer(G,A,B,M,S).

% Résolution du sudoku

resolution(G,_,_,_,1,D,S) :- grille_pleine(G,S), L is S*S, ND is L-D, random(1,L,R), preparer(G,ND,R,S).
resolution(G,_,_,_,2,_,S) :- grille_pleine(G,S), afficher_sudoku(G,S).
resolution(G,X,Y,N,M,D,S) :- valide(X,S), valide(Y,S), vide_grille(G,X,Y), !, valide(N,S), modifier_grille(G,N,X,Y,NG), valide_grille(NG,S), resolution(NG,_,_,_,M,D,S).


% Génère une grile valide remplie avec D chiffres

  % Si on a effectivement inséré le nombre de chiffres voulu dans la grille et qu'elle est valable, on teste si elle est resolvable

genere(G,_,0,I,S) :- test(G,I,S).

  % Sinon on cherche à insérer le premier chiffre correct à la position aléatoire donnée (si échec car déjà remplie, on recommence avec une autre position aléatoire).

genere(G,R,D,I,S) :- X is ((R-1)mod(S))+1, Y is round((R-1)/S)+1, valide(N,S), modifier_grille(G,N,X,Y,NG), valide_grille(NG,S), ND is D-1, L is S*S, random(1,L,P), genere(NG,P,ND,I,S).
genere(G,R,D,I,S) :- X is ((R-1)mod(S))+1, Y is round((R-1)/S)+1, valide(N,S), modifier_grille(G,N,X,Y,NG), \+ valide_grille(NG,S), L is S*S, random(1,L,P), genere(G,P,D,I,S).
genere(G,R,D,I,S) :- X is ((R-1)mod(S))+1, Y is round((R-1)/S)+1, valide(N,S), \+ modifier_grille(G,N,X,Y,_), L is S*S, random(1,L,P), genere(G,P,D,I,S).

% Vider aléatoirement N cases d'une grille pleine

preparer(G,0,_,S) :- play(G,S).

preparer(G,N,R,S) :- X is ((R-1)mod(S))+1, Y is round((R-1)/S)+1, \+ vide_grille(G,X,Y), vider_grille(G,X,Y,NG), NN is N-1, L is S*S, random(1,L,P), preparer(NG,NN,P,S).
preparer(G,N,R,S) :- X is ((R-1)mod(S))+1, Y is round((R-1)/S)+1, vide_grille(G,X,Y), L is S*S, random(1,L,P), preparer(G,N,P,S).
