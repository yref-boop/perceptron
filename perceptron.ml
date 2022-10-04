(* auxiliar absolute value *)
let absolute_float x =
    if (x < 0.0) then -1.0 *. x else x ;;

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
let delta_rule n error x (weight, value) =
    (weight +. n *. (error) *. x, value) ;;

(* weight modification *)
let learn inputs error =
    List.map (delta_rule 0.1 error 0.1) inputs ;;

(* training function *)
let rec train inputs expected_result error_threshold =
    let error = expected_result -. (neuron sigmoid 1.0 inputs) in
    if (absolute_float error > error_threshold) 
        then train (learn inputs error) expected_result error_threshold
    else inputs
;;
