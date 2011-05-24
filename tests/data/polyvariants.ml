

TYPE_CONV_PATH "Polyvariants" 

type 'a t = int with odn

type poly_t = [`Toto] t with odn

type poly_arg_t = [ `Foo of string
                  | `Bar of string * int] with odn
