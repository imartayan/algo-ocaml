# Quelques algorithmes classiques en OCaml

Ce dépôt contient quelques algorithmes classiques que j'ai étudiés en classe préparatoire.
Je me contente d'utiliser la bibliothèque standard du langage OCaml, quitte à redéfinir les structures de données dont j'ai besoin.
Chaque programme est accompagné d'un petit exemple pour le tester.
L'implémentation de ces programmes peut certainement être rendue plus claire et plus efficace, donc n'hésitez pas à me suggérer des modifications.
Le code est licencié selon les termes de la licence GPLv3, vous pouvez le réutiliser librement à condition de garder la même licence.

## Détails d'implémentation

### Algorithme de Dijkstra

Pour l'algorithme de Dijkstra, j'utilise un tas min dont les éléments peuvent être modifié sans avoir à les réinsérer. Afin de pouvoir utiliser des distances infinies, je définis également un type `poids` dont les valeurs sont des entiers ou l'infini.

### Algorithme de Prim

Mon implémentation de l'algorithme de Prim ressemble beaucoup à celle de l'algorithme de Dijkstra et réutilise une bonne partie des fonctions.

### Algorithme de Kruskal

Pour l'algorithme de Kruskal, j'utilise une structure union-find avec compression des chemins. Le tri utlisé pour les arêtes est un tri fusion.

### Algorithme de Knuth-Morris-Pratt

Les deux méthodes proposées pour l'algorithme de Knuth-Morris calculent le tableau des bords du motif.
