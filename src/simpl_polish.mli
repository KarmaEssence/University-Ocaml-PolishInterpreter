open Types_pf5

(*prend un operateur, deux expressions et renvoie une expression simplifie*)
val make_simpl_expr : op -> expr -> expr -> expr

(*prend une condition et renvoie un boolean*)
val can_simpl_block : cond -> bool

(*prend une condition et renvoie un boolean*)
val choose_simpl_block : cond -> bool

(*prend un program et renvoie un program simplifie*)
val simpl_polish : program -> program