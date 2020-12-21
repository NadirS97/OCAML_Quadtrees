type couleur =  Blanc | Noir;;
type quadtree = Feuille of couleur |Noeud of quadtree * quadtree * quadtree * quadtree;;
(*1-------------------------------------------------*)
(*La fonction quadtree_full prend en parametre un nombre n (correspondant a la taille du quadtree a construire) et nous retourne un quadtree de taille n, on gere les erreures dans un premier temps le cas ou le n est nul ou negatif on retourne une erreure et dans le cas ou n est superieur ou egale a 1, on rentre dans la sous-fonction aux qui prend en parametre  une taille t (correspondant a la taille du quadtree-1 "n-1") et un quadtree a initialement correspondant a Feuille Noir, dans un premier cas lorsque la taille t=0 (n-1 =0 qui signifie qu'on veut construire un quadtree de taille 1) on retourne a (Feuille Noir correspondant au parametre a), dans le deuxieme cas, on appelle recursivement la fonction aux avec comme parametre la taille t/2 ainsi que le Noeud(a,a,a,a) c'est a dire le Noeud avec "Feuille Noir * Feuille Noir * Feuille Noir * Feuille Noir" autant de fois que necessaire*)


(*La fonction quadtree_empty prend en parametre un nombre n (correspondant a la taille du quadtree a construire) et nous retourne un quadtree de taille n, on gere les erreures dans un premier temps le cas ou le n est nul ou negatif on retourne une erreure et dans le cas ou n est superieur ou egale a 1, on rentre dans la sous-fonction aux qui prend en parametre  une taille t (correspondant a la taille du quadtree-1 "n-1") et un quadtree a initialement correspondant a Feuille Blanc, dans un premier cas lorsque la taille t=0 (n-1 =0 qui signifie qu'on veut construire un quadtree de taille 1) on retourne a (Feuille Blanc correspondant au parametre a), dans le deuxieme cas, on appelle recursivement la fonction aux avec comme parametre la taille t/2 ainsi que le Noeud(a,a,a,a) c'est a dire le Noeud avec "Feuille Blanc * Feuille Blanc * Feuille Blanc * Feuille Blanc" autant de fois que necessaire*)

(*val quadtree_full : int -> quadtree = <fun>*)
let quadtree_full = fun n ->
  match n with
  | n when n=0 -> failwith "Erreur impossible de creer un quadtree de taille 0"
  | n when n<0 -> failwith "Erreur impossible de creer un quadtree de taille negatif"
  | n -> let rec aux = fun t a ->
           let t'=t/2
           in
           match t with
           | t when t=0 -> a
           | t when t>=1 -> aux t' (Noeud(a,a,a,a))
           | _ -> failwith "Erreur"
         in
         aux (n-1) (Feuille Noir) ;;


(*val quadtree_empty : int -> quadtree = <fun>*)
let quadtree_empty = fun n ->
  match n with
  | n when n=0 -> failwith "Erreur impossible de creer un quadtree de taille 0"
  | n when n<0 -> failwith "Erreur impossible de creer un quadtree de taille negatif"
  | n -> let rec aux = fun t a ->
           let t'=t/2
           in
           match t with
           | t when t=0 -> a
           | t when t>=1 -> aux (t') (Noeud(a,a,a,a))
           | _ -> failwith "Erreur"
         in
         aux (n-1) (Feuille Blanc);;



(*2-------------------------------------------------*)
(*La fonction inverse prend en parametre un quadtree et inverse la couleur des feuilles, et on l'appelle en recursif toutes les fois ou notre arbre possede un noeud et en reexecute la fonction pour chaque quart de notre noeud*)

(*val inverse : quadtree -> quadtree = <fun>*)
let rec inverse = function
  |Feuille Noir -> Feuille Blanc
  |Feuille Blanc -> Feuille Noir
  |Noeud(q1,q2,q3,q4) -> Noeud(inverse q1, inverse q2, inverse q3, inverse q4);;

(*3-------------------------------------------------*)
(* La fonction rotate prend en parametre un quadtree et effectue une rotation d'un cran vers la gauche et on l'appelle en recursif toutes les fois ou notre arbre possede un noeud et en reexecute la fonction pour chaque quart de notre noeud pour avoir une rotation juste*)

(*val rotate : quadtree -> quadtree = <fun>*)
let rec rotate = function
  |Feuille Noir -> Feuille Noir
  |Feuille Blanc -> Feuille Blanc
  |Noeud (q1,q2,q3,q4) -> Noeud (rotate q2, rotate q3, rotate q4, rotate q1);;

(*4-------------------------------------------------*)
(* La fonction union prend deux parametres a et b (a correspondant au premier quadtree) (b corresepondant au deuxieme quadtree) et cette fonction nous permet d'effectuer une union entre les deux arbres (selon la couleur de la feuille) et on l'appelle en recursif pour nous permettre d'appliquer la fonction sur toutes les feuilles de nos Noeuds de nos quadtree*)

(*val union : quadtree -> quadtree -> quadtree = <fun>*)
let rec union = fun a b ->
  match (a,b) with
  | (Feuille Blanc,x) | ( x, Feuille Blanc)  ->  x
  | (Feuille Noir, Feuille Noir) -> Feuille Noir
  | (Feuille Noir, Noeud (q1,q2,q3,q4)) | (Noeud (q1,q2,q3,q4), Feuille Noir) -> Noeud( union (Feuille Noir) q1, union (Feuille Noir) q2, union (Feuille Noir) q3, union (Feuille Noir) q4)
  | (Noeud(q1,q2,q3,q4),Noeud(q1',q2',q3',q4')) ->  Noeud( union q1 q1', union q2 q2', union q3 q3', union q4 q4');;

(*5------------------------------------------------------------------*)
(* La fonction intersection prend deux parametres a et b (a premier quadtree) (b deuxieme quadtree) et cette fonction permet d'effectuer une intersection entre les deux quadtree selon la couleur de la feuille selon les "regles" imposés*)

(*val intersection : quadtree -> quadtree -> quadtree = <fun>*)
let rec intersection = fun a b ->
  match (a,b) with
  | (Feuille Blanc, x) | ( x, Feuille Blanc)  -> Feuille Blanc
  | (Feuille Noir, Feuille Noir) -> Feuille Noir
  | (Feuille Noir, Noeud(q1,q2,q3,q4)) | (Noeud (q1,q2,q3,q4), Feuille Noir) ->
     Noeud( intersection (Feuille Noir) q1, intersection (Feuille Noir) q2, intersection (Feuille Noir) q3, intersection (Feuille Noir) q4)
  | (Noeud(q1,q2,q3,q4),Noeud(q1',q2',q3',q4')) ->
     Noeud( intersection q1 q1', intersection q2 q2', intersection q3 q3', intersection q4 q4');;


(*6-------------------------------------------------------------------*)
(* Question 6 NON Fini: j'epere que vous allez prendre en compte (l'essai) je vous remercie d'avance
une fonction list_max qui me permet de retourner le maximum d'une liste l, 
une fonction compteur qui permet prend en parametre un quadtree et qui me retourne (int) le nombre maximum de noeuds "interne" qui se situe a l'interieur des quarts (ceci nous permetra d'avoir la taille de notre image)
une fonction color qui prend en parametre des coordonées x y et a un quadtree, la variable taille_image correspond a la taille totale de notre image (la longueur du coté total) la sous-fonction aux prend trois parametres cx cy et un arbre quadtree
(la fonction color semble correct mais j'ai une erreur au niveau de mes conditions a l'interieur de aux, elle ne me permet donc pas d'afficher la bonne couleur, j'ai essayé de refaire plusieurs fois mais je n'ai pas pu regler le soucis*)


(*val liste_max : 'a list -> 'a = <fun>*)
let liste_max l =
  if l = [] then failwith "Empty List !"
  else let rec aux l acc =
         match l with
         |[e] -> max e acc
         |e::f -> aux f (max e acc)
         |[] -> failwith "Empty List !"
       in aux (List.tl l) (List.hd l);;

(*val compteur : quadtree -> int = <fun>*)
let rec compteur = function
  |Feuille x -> 0
  |Noeud(q1,q2,q3,q4) -> liste_max [(1+compteur q1);(1+compteur q2);(1+compteur q3);(1+compteur q4)] ;;

(*val color : int -> int -> quadtree -> couleur option = <fun>*)
let  color= fun x y a ->
     let taille_image=int_of_float(2. ** float_of_int(compteur a))
     in
     if x>taille_image then None
       else
         if
           y>taille_image then None
         else
           let rec aux = fun cx cy q ->
                     match q with
                     |Feuille c -> Some c
                     |Noeud(q1,q2,q3,q4)->
                       let t'=int_of_float(2. ** float_of_int(compteur q))
                       in
                       if (cx)>=(t'/2) && (cy)<(t'/2)
                       then
                         aux (cx-2) (cy) q1
                       else
                         if (cx)>=(t'/2) && (cy)>=(t'/2)
                         then
                           aux (cx-2) (cy-2) q2
                         else
                           if(cx)<(t'/2) && (cy)>=(t'/2)
                           then
                             aux (cx) (cy-2) q3
                           else
                             if(cx)<(t'/2) && (cy)>=(t'/2)
                             then
                               aux (cx) (cy) q4
                             else
                               failwith "Erreur"
           in
           aux x y a;;



(*8---------------------------------------------------------*)
(*la fonction optimise prend en parametre un quadtree et nous permet de retourner ce meme quadtree optimisé c'est a dire de retourner une feuille de couleur x si notre quadtree possede un Noeud qui lui meme possede quatre feuille de couleur x, et bien entendu on "reoptimise" notre quadtree a la fin grace a la recursivité pour nous permettre de repasser dessus dans le cas ou apres la premiere optimisation on se retrouve avec un Noeud possedant 4 feuilles de meme couleur x*)

(*val optimise : quadtree -> quadtree = <fun>*)
let rec optimise = function
  |Feuille x -> Feuille x
  |Noeud(q1, q2, q3, q4) -> 
    match (q1,q2,q3,q4) with
    |(Feuille a', Feuille b', Feuille c', Feuille d') when a'=b' && b'=c' && c'=d'  -> Feuille a'
    |(q1,q2,q3,q4) when (optimise q1)=(optimise q2) && (optimise q2)=(optimise q3) && (optimise q3)=(optimise q4) -> optimise q1
    |(_,_,_,_) -> Noeud (optimise q1, optimise q2, optimise q3, optimise q4);;
(*9---------------------------------------------------------*)
type bit= Zero | Un;;
(*la fonction quadtree_to_list prend en parametre un quadtree et nous retourne une liste de type bit list, pour cela j'ai cree une sous-fonction code qui prend en parametre un quadtree ansi qu'une liste, cette sous-fonction nous permet de "coder" la feuille blanc -> [Zero;Zero], la feuille Noir -> [Zero;Un] et le noeud ->[Un;...], le t dans ma sous-fonction represente le tail de notre liste(qui au depart est initialise a liste vide) et par recursivité est appellé une premiere fois (code q4 t) avec t vide et remonte au fur et a mesure, et a chaque fois qu'on execute code on met le resultat dans la liste qu'on va retouner*)

(*val quadtree_to_list : quadtree -> bit list = <fun>*)
let quadtree_to_list =fun q ->
  let tail = []
  in
  let rec code =fun a t ->
     match a with       
    | Feuille Blanc -> Zero::Zero::t
    | Feuille Noir  -> Zero::Un::t
    | Noeud (q1,q2,q3,q4) -> Un::code q1 (code q2 (code q3 (code q4 t)))
  in code q tail;;
(*10--------------------------------------------------------*) 
(*Question 10 NON Fini: j'espere que vous allez prendre en compte (l'essai), je vous remercie d'avance
La fonction list_to_quadtree prend en parametre une liste de type bit list et retourne un quadtree: on procede dans un premier aux tests, c'est a dire, a si la liste est vide -> on retourne une erreur, si notre liste commence par [Zero;Zero;...] ceci signifie que les deux premiers elements correspondent au code d'une Feuille Blanc, si elle commence par [Zero;Un;...] les deux premiers elements corresenpondent au code d'une Feuille Noir, et si celle ci commence par [Un;...] il s'agit d'un debut de Noeud. Je retourne dans le deuxieme et troisieme cas un couple (Feuille Blanc ou Noir, suite) suite etant la suite de notre liste de type bit list mis a part les deux premiers elements (un tail) (au depart j'ai essayé de mettre Feuille Blanc et la suite de la liste type bit list sauf que ce n'est pas possible vu que Feuille Blanc est de type quadtree et ma suite est de type bit list du coup j'ai essayé de le retourner sous forme de couple) pour le quatrieme cas, le cas corresepondant au noeud je n'ai pas su comment finir ma fonction, je voulais reapliquer en recursif ma fonction sur la suite(le tail du noeud) et le faire quatre fois pour (q1,q2,q3,q4) (je ne sais pas si mon raisonement est correct vu que je n'ai pas pu resoudre mon soucis) *)

(*
let rec list_to_quadtree = fun l ->
    match l with
    | [] -> failwith "Erreur la liste est vide"
    | Zero::Zero::suite -> Feuille Blanc,suite
    | Zero::Un::suite -> Feuille Noir,suite
    | Un::suite-> list_to_quadtree suite

                    Noeud(q1,q2,q3,q4);;
*)





(*test*)
(*1*)
quadtree_full (-1);;
quadtree_full 0;;
quadtree_full 1;;
quadtree_full 4 ;;

quadtree_empty (-1);;
quadtree_empty 0;;
quadtree_empty 1;;
quadtree_empty 4 ;;

(*2*)
let quadtree_0=Feuille Noir;;
let quadtree_1 =Noeud (Feuille Noir, Feuille Noir, Feuille Blanc, Feuille Blanc);;
let quadtree_2 =Noeud (Feuille Noir, Feuille Blanc, Feuille Noir, Noeud(Feuille Blanc, Feuille Blanc, Feuille Noir, Feuille Noir));;

inverse quadtree_0;;
inverse quadtree_1;;
inverse quadtree_2;;

(*3*)
rotate quadtree_0;;
rotate quadtree_1;;
rotate quadtree_2;;

(*4*)
let quadtree_3= Noeud (Feuille Blanc, Feuille Noir, Feuille Blanc, Feuille Noir);;

union quadtree_3 quadtree_1;;
union quadtree_2 quadtree_3;;

(*5*)
intersection quadtree_0 quadtree_1;;
intersection quadtree_2 quadtree_3;;

(*6*)
let quadtree_4 =
  Noeud (Noeud
           (Noeud(Feuille Blanc,
                  Feuille Blanc,
                  Feuille Noir,
                  Feuille Noir
                  ),
             Feuille Blanc,
             Feuille Noir,
             Feuille Noir
           ),
         Feuille Blanc,
         Feuille Noir,
         Noeud(Feuille Blanc,
               Feuille Blanc,
               Feuille Noir, 
               Feuille Noir
              )
    );;
compteur quadtree_4;;
color 3 4 quadtree_4;;(*ne marche pas*)

(*8*)
let quadtree_5 = Noeud(Feuille Noir, Feuille Noir,Feuille Noir,Feuille Noir);;
let quadtree_6 = Noeud(Noeud(Feuille Noir, Feuille Blanc,Feuille Blanc,Feuille Blanc), Feuille Noir,Feuille Noir,Feuille Noir);;
let quadtree_7 = Noeud(Noeud(Feuille Blanc, Feuille Blanc,Feuille Blanc,Feuille Blanc), Feuille Noir,Feuille Noir,Feuille Noir);;
let quadtree_8 = Noeud(Noeud(Feuille Noir, Feuille Noir, Feuille Noir, Feuille Noir), Feuille Noir, Feuille Noir,Feuille Noir);;

optimise quadtree_5;;
optimise quadtree_6;;
optimise quadtree_7;;
optimise quadtree_8 ;;

(*9*)

quadtree_to_list quadtree_0;;
quadtree_to_list quadtree_1;;
quadtree_to_list quadtree_2;;

(*10*)
let code = [Un; Un; Zero; Un; Zero; Zero; Zero; Zero; Zero; Zero; Zero; Un; Zero; Un; Zero; Un];;
