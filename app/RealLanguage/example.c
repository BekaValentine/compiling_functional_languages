// data Nat;
// con Zero : () ~> Nat;
// con Suc : (Nat) ~> Nat;

typedef enum {
    o_conname_Nat_Zero,
    o_conname_Nat_Suc
} o_conname_Nat;

typedef struct {
    o_conname_Nat constructor;
    void* arg0;
} o_typename_Nat;

o_typename_Nat* Zero() {
    o_typename_Nat* z = (o_typename_Nat*)malloc(sizeof o_typename_Nat);
    z->constructor = o_conname_Nat_Zero;
    return z;
}

o_typename_Nat* Suc(o_typename_Nat* n) {
    o_typename_Nat* s = (o_typename_Nat*)malloc(sizeof o_typename_Nat);
    s->constructor = o_conname_Nat_Suc;
    s->arg0 = n;
    return s;
}

// term $plus : Nat -> (Nat -> Nat);
// term $plus = \x -> \y ->
//   case x of
//     | Zero() -> y
//     | Suc(x') -> Suc($plus x' y)
//     ;

void* o_term_plus;

int main() {
    ...
    o_term_plus = m_make_application(&o_function_plus, 2, 0, ...);
    ...
}

o_typename_Nat* o_function_plus(o_typename_Nat* xy) {
    o_typename_Nat* x = xy[0];
    o_typename_Nat* y = xy[1];
    if (x->constructor == o_conname_Nat_Zero) {
        return y;
    } else if (x->constructor == o_conname_Nat_Suc) {
        return Suc(m_apply(m_apply(o_function_plus, x->arg0), y));
    }
}

void* m_apply(m_apply_struct* application, void* x) {
    add_arg(application, x);
    if (application->current_args == application->total_args) {
        return (*application->actual_function)(application->args);
    } else {
        return application;
    }
}

typedef enum {
    m_function_plus
} m_function_name;

typedef struct {
    m_function_name name;
    void* actual_function;
    int total_args;
    int current_args;
    void* args;
} m_apply_struct;

// data NatList;
// con Nil : () ~> NatList;
// con Cons : (Nat, NatList) ~> NatList;

// term $lengthNatList : NatList -> Nat;
// term $mapNatList : (Nat -> Nat) -> (NatList -> NatList);

// term $lengthNatList = \xs ->
//   case xs of
//     | Nil() -> Zero()
//     | Cons(x,xs') -> Suc($lengthNatList(xs'))
//     ;

// term $mapNatList = \f -> \xs ->
//   case xs of
//     | Nil() -> Nil()
//     | Cons(x,xs') -> Cons(f x, $mapNatList f xs')
//     ;

// term $zip
//     : List(a)
//     -> List(b)
//     -> List(Prod(a,b))
//     ;

// term $zip
//  = \xs -> \ys ->
//     case xs | ys
//     | Cons(x,xs') | Cons(y,xs') -> Cons(Pair(x,y), #$zip xs' xs')
//     | _           | _           -> Nil()
//     ;