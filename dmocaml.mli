type couleur = Blanc | Noir
type quadtree =
    Feuille of couleur
  | Noeud of quadtree * quadtree * quadtree * quadtree
val quadtree_full : int -> quadtree
val quadtree_empty : int -> quadtree
val inverse : quadtree -> quadtree
val rotate : quadtree -> quadtree
val union : quadtree -> quadtree -> quadtree
val intersection : quadtree -> quadtree -> quadtree
val liste_max : 'a list -> 'a
val compteur : quadtree -> int
val color : int -> int -> quadtree -> couleur option
val optimise : quadtree -> quadtree
type bit = Zero | Un
val quadtree_to_list : quadtree -> bit list
val quadtree_0 : quadtree
val quadtree_1 : quadtree
val quadtree_2 : quadtree
val quadtree_3 : quadtree
val quadtree_4 : quadtree
val quadtree_5 : quadtree
val quadtree_6 : quadtree
val quadtree_7 : quadtree
val quadtree_8 : quadtree
val code : bit list
