# IN213 : Projet langage logo

## Réflexions initiales sur les attendus du langage

Créer un mini-langage type Logo qui contrôle une « tortue graphique » :

- La tortue se déplace sur l'écran et dessine.
- L'utilisateur écrit un petit programme dans le langage.
- Ton interpréteur l'exécute et affiche le résultat (via Graphics).


On va retrouver les commandes simples qui permettent de faire des tracés simples : un peu comme Scratch.\
forward 100  --> on avance dans une direction en précisant le nombre de pixels\
right 90     --> on procède à une rotation en précisant le nombre de degrés\ 
left 45\
penup        --> on dessine ou non en levant le stylo --> à voir si c'est simple facilement avec Graphics\ 
pendown\
repeat 4 [   --> implémentation de la notion de boucle\
  forward 100\
  right 90\
]

Set color ?  --> changer la couleur du stylo
Go to        --> se téléporter sur un point donné de la grille
Clear? --> nettoyer la toile (à voir si vraiment utile car on peut juste recommencer un programme)


Pour aller plus loin : 
 - tracer des formes particulières grâce à un simple appel de fonction 
     - Carré (20) --> carré de 20 de côté 
 - Tracer de fractales ?
 - Grâce au module gaphics on peut récupérer la position de la souris, on pourrait tracer comme ça avec la tortue qui se déplace avec une certaine vitesse vers la souris. 


## Démarche à suivre 

main.ml

ast.ml (définition des types du langage)

parser.ml (lecture du fichier source en command list)

exec.ml (interprétation des commandes)

Compile avec :

ocamlc graphics.cma ast.ml parser.ml exec.ml main.ml -o logo