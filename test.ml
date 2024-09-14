#load "graphics.cma";;
open Graphics;;
open_graph "400x400";;
set_window_title "Demineur";;

let rec menu ()=
    clear_graph();
    set_color black;
    moveto 10 10;
    draw_rect 10 10 120 30;
    draw_string("Jouer seul");

    set_color black;
    moveto 10 40;
    draw_rect 10 10 120 30;
    draw_string("Jouer à deux ");

    set_color black;
    moveto 10 70;
    draw_rect 10 10 120 30;
    draw_string("Options");


and test_menu() = set_color black;

let valeur = wait_next_event[Button_up; Key_pressed] in 
if valeur.key_pressed && valeur.key == '\027' then exit 0 else
test_menu();;

menu();;
test_menu();;


(*
(* Vérifier si un clic est dans la zone d'un bouton *)
let is_inside_button button x y =
  x >= button.x && x <= (button.x + button.width) &&
  y >= button.y && y <= (button.y + button.height)

(* Afficher un message dans une nouvelle fenêtre *)
let display_message () =
  (* Ouvrir une nouvelle fenêtre pour afficher le message et la grille *)
  Graphics.open_graph " 300x300";
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 300 300;
  (* Dessiner une grille dans la nouvelle fenêtre *)
  let cell_size = 30 in
  draw_grid cell_size 300 300;
  (* Afficher un message *)
  Graphics.moveto 50 280;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Grille du démineur !"


(* Gérer les clics en fonction des boutons *)
let handle_click buttons x y =
  (* Parcourir la liste des boutons *)
  List.iter (fun button ->
    if is_inside_button button x y then (
      Printf.printf "Bouton '%s' cliqué !\n" button.label;
      (* Afficher un message si le Bouton 1 est cliqué *)
      if button.label = "Bouton 1" then
        display_message ()
    )
  ) buttons


let () =
  (* Initialiser la bibliothèque graphics *)
  Graphics.open_graph " 600x400";
  set_window_title "Demineur";;

  (* Boucle pour attendre et gérer les clics *)
  let rec wait_for_click () =
    let event = Graphics.wait_next_event [Graphics.Button_down] in
    let x = event.mouse_x in
    let y = event.mouse_y in
    handle_click buttons x y;
    wait_for_click ()
  in

  (* Lancer la boucle d'attente de clics *)
  wait_for_click ()
*)