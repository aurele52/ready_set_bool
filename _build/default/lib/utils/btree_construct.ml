

  let remove_one_stack stack = 
    match stack with
    | a :: rest -> (a,rest)
    | _ -> failwith "Stack vide"

  let remove_two_stack stack = 
    match stack with
    | a :: b :: rest -> (a,b,rest)
    | _ -> failwith "Stack vide"

let newNode value l r: char Btree.btree = Node (value, l, r)

let btree_construct (a: string): char Btree.btree =
  let rec btree_construct_rec (a: string) (i: int) (stack: char Btree.btree  list): char Btree.btree  =
    if (i < String.length a)
    then (
      if (Is_maj.is_maj a.[i])
      then btree_construct_rec a (i + 1) ((newNode a.[i] Empty Empty) :: stack)
      else if a.[i] = '!'
        then let (first, rest) = remove_one_stack stack in btree_construct_rec a (i + 1) ((newNode a.[i] first Empty) :: rest)
        else let (first, second, rest) = remove_two_stack stack in btree_construct_rec a (i + 1) ((newNode a.[i] first second) :: rest)
          )
      else List.hd stack
          in btree_construct_rec a 0 []
