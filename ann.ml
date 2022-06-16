
(* relu *)
let relu x = max(0,x);;

(* sigmoid *)
let sigmoid x = 1.0 /. (1.0 +. 2.71828 ** x) ;;

(* sigmoid derivative *)
let sigmoid_derivative x =
    sigmoid x *. (1.0 -. sigmoid x) ;;

(* auxiliar function to multiply elements of the tuples *)
let untuple (n, m) = n *. m ;;

(* neuron function *)
let neuron activation_function bias inputs =
    activation_function (bias +. List.fold_left (+.) 0.0 (List.map untuple inputs));;

(* delta rule *)
(*  n -> learning rate
    t -> target output
    y -> obtained output
    x -> input*)
let delta_rule (weight, value) n t y x =
 (weight +. n *. (t -. y) *. x, value) ;;

(* weight modification *)
let learn inputs expected_result =
    List.map delta_rule inputs ;;
