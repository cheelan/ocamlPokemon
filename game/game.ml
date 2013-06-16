open Definitions
open Util
open Constants
open Netgraphics

(* You have to implement this. Change it from int to yout own state type*)
(* Game state needs:
 * wrapped game_status_data
 * Current Game Step (Draft, Inventory, Battle Phase)
 * 
 *)
 

 

type game =  State.state


let createAttack lst =
  match lst with
    | [a;b;c;d;e;f;g;h] -> {
      name = a;
      element = type_of_string b;
      max_pp = int_of_string c;
      pp_remaining = int_of_string c;
      power = int_of_string d;
      accuracy = int_of_string e;
      crit_chance = int_of_string f;
      effect = (effect_of_num (int_of_string g), int_of_string h) }  
    | _ -> failwith "Invalid string read from attack.txt"
    
 let parseAttacks file = 
   let strlst = List.tl (read_lines file) in
     List.fold_left (fun a e -> (createAttack (wordify e))::a) [] strlst

(* Looks up the name of an attack in the attack list *)
let attack_of_string a atks = 
  List.find (fun x -> x.name = a) atks (*try with? *)

(* Given string input, create a steammon record *)  
let createMon lst atks =
  match lst with
    | [a;b;c;d;e;f;g;h;i;j;k;l;m] -> {
      species = a;
      curr_hp = int_of_string b;
      max_hp = int_of_string b;
      first_type = Some (type_of_string c);
      second_type = if d = "Nothing" then None else (Some (type_of_string d));
      first_attack = attack_of_string e atks;
      second_attack = attack_of_string f atks;
      third_attack = attack_of_string g atks;
      fourth_attack = attack_of_string h atks;
      attack = int_of_string i;
      spl_attack = int_of_string j;
      defense = int_of_string k; 
      spl_defense = int_of_string l;
      speed = int_of_string m;
      status = [];
      mods = 
        { attack_mod = 0; speed_mod = 0; defense_mod = 0; accuracy_mod = 0; }}
    | _ -> failwith "Invalid string read from steammon.txt"
   
    
let parseSteammons file atks= 
  let strlst = read_lines file in
  List.fold_left (fun a e -> (createMon (wordify e) atks)::a) [] strlst

let attack_pool = parseAttacks "attack.txt" 
let mon_pool = ref (parseSteammons "steammon.txt" attack_pool)
let phase = ref State.Draft

let game_datafication g =
  let (gd,_, _, _) = g in gd

	
let game_from_data game_data = 
	(game_data, !phase, attack_pool, !mon_pool)

(* 
  SelectStarter of string
 | PickSteammon of string
| PickInventory of inventory
| SwitchSteammon of string
| UseItem of item * string
| UseAttack of string *)

let firstMove = ref Red
let lastPick = ref Red
(* Handle phase verification here *)
let handle_step gs ra rb =
  let handleCommand gs action color = 
    match action with
      |  Action(SelectStarter(s)) -> 
           add_update(SetChosenSteammon(s)); State.setActive gs color s   
      |  Action(PickSteammon(s)) -> 
           let cmd = State.draftMon gs color s in
           let mon = State.steammonOfString s (State.getSteamMonList cmd color) in
           add_update(UpdateSteammon(s, mon.curr_hp, mon.max_hp, color)); 
           mon_pool := State.getSteamPool cmd;
           cmd
      |  Action(PickInventory(i)) -> print_endline ("Picked an inventory "^string_of_int (List.length i)); State.addItems gs color i
      |  Action(SwitchSteammon(s)) -> 
           add_update(SetChosenSteammon(s)); 
            State.doPoison (State.setActive gs color s) color
      |  Action(UseItem(i, s)) -> 
           let newState = State.useItem gs color i s in
           let active = State.getActive newState color in 
           add_update(UpdateSteammon(active.species, 
             active.curr_hp, active.max_hp, color)); 
          State.doPoison newState color
      |  Action(UseAttack(a)) -> 
         print_endline "Used attack";
         let newState = State.applyAttack gs color a in
         let attacker = State.getActive newState color in
         (if !State.attacked then 
        add_update(Message(attacker.species ^ " Used " ^ a)) else ());
         let active = State.getActive newState (invert_color color) in
         add_update(UpdateSteammon(active.species, 
           active.curr_hp, active.max_hp, invert_color color)); 
        State.doPoison newState color
      | DoNothing -> gs
      | _ -> failwith "Invalid arg to handlecommand" in
    let finalState =  
      match (ra, rb) with
        | (Action(SelectStarter(s)), _) -> 
          add_update(SetFirstAttacker(Red));
          handleCommand (handleCommand gs ra Red) rb Blue
        | (_, Action(SelectStarter(s))) -> 
          add_update(SetFirstAttacker(Blue));
          handleCommand (handleCommand gs rb Blue) ra Red
        | (Action(SwitchSteammon(s)), _) -> 
          add_update(SetFirstAttacker(Red));
          handleCommand (handleCommand gs ra Red) rb Blue
        | (_, Action(SwitchSteammon(s))) -> 
          add_update(SetFirstAttacker(Blue));
          handleCommand (handleCommand gs rb Blue) ra Red
        | (Action(UseItem(i,s)), _) -> 
          add_update(SetFirstAttacker(Red));
          handleCommand (handleCommand gs ra Red) rb Blue
        | (_, Action(UseItem(i,s))) -> 
          add_update(SetFirstAttacker(Blue));
          handleCommand (handleCommand gs rb Blue) ra Red
        | (Action(UseAttack(a)), Action(UseAttack(b))) -> 
          (match State.getFastest gs with
            | Red -> 
                add_update(SetFirstAttacker(Red));
                handleCommand (handleCommand gs ra Red) rb Blue  
            | Blue -> 
              add_update(SetFirstAttacker(Blue));
              handleCommand (handleCommand gs rb Blue) ra Red)
        | (Action(PickSteammon(s)), DoNothing) -> lastPick := Red;
          add_update(SetFirstAttacker(Red));
          handleCommand gs ra Red
        | (DoNothing, Action(PickSteammon(s))) -> lastPick := Blue;
          add_update(SetFirstAttacker(Blue));
          handleCommand gs rb Blue
        | (Action(PickInventory(x)), Action(PickInventory y)) -> (*is this ok?*)
          add_update(SetFirstAttacker(Red)); 
          handleCommand (handleCommand gs ra Red) rb Blue 
        | (DoNothing, DoNothing) -> failwith "Double doNothings"
        | _ -> failwith "Invalid ra,rb combo in handle_step" in
    let (((lred1, ired), (lblue1, iblue)), p, ats, sp) = finalState in
    let (gsd, _, _, _) = finalState in
    let newPhase = ref (State.getPhase(finalState)) in
    let result = State.getResult gsd in
      let (rr, rb) = 
        match !newPhase with (* Try using !newPhase instead *)
          (* Need to handle initial selection case*)
          | State.Draft -> 
            let redLen = List.length lred1 
            and blueLen = List.length lblue1 in
            (*Pick blue, do nothing red *)
            if redLen > blueLen 
            then (DoNothing, Request(PickRequest(Blue, gsd, ats, sp)))
            else if blueLen > redLen 
            then (Request(PickRequest(Red, gsd, ats, sp)), DoNothing)
            else if (blueLen = 0 && redLen = 0)
            then match !firstMove with
              | Red -> (Request(PickRequest(Red, gsd, ats, sp)), DoNothing)
              | Blue -> (DoNothing, Request(PickRequest(Blue, gsd, ats, sp)))
            else (*blueLen = redLen*) 
            ((if blueLen = cNUM_PICKS 
                then (phase := State.Inventory;
                    (Request(PickInventoryRequest(gsd)), 
                Request(PickInventoryRequest(gsd))))
              else (match !lastPick with
              | Red -> (Request(PickRequest(Red, gsd, ats, sp)), DoNothing)
              | Blue -> (DoNothing, Request(PickRequest(Blue, gsd, ats, sp))))))
             
          | State.Inventory -> phase := State.Battle;
            (Request(StarterRequest(gsd)), Request(StarterRequest(gsd))) 
          | State.Battle -> print_endline "battling";
            let swapRed = State.isLeadFainted gsd Red 
            and swapBlue = State.isLeadFainted gsd Blue in
            if swapRed && swapBlue 
            then (print_endline "bothfainted";
            add_update(Message("Red Steammon and Blue Steammon Fainted"));
            (Request(StarterRequest(gsd)), Request(StarterRequest(gsd))))
            else if swapRed then (print_endline"redfainted";
             add_update(Message("Red Steammon Fainted"));
            (Request(StarterRequest(gsd)), DoNothing))
            else if swapBlue then (print_endline"bluefainted";
             add_update(Message("Blue Steammon Fainted"));
            (DoNothing, Request(StarterRequest(gsd))) )
            else (print_endline"actionrequest";(Request(ActionRequest(gsd)), Request(ActionRequest(gsd)))) in
    match result with
      | Some _ -> phase := State.Draft; 
          mon_pool := (parseSteammons "steammon.txt" attack_pool);
          (result, gsd, None, None);
      | None -> 
        let (rr, rb) = (match (rr, rb) with
          | (DoNothing, DoNothing) -> (None, None)
          | (x, DoNothing) -> (Some x, None)
          | (DoNothing, x) -> (None, Some x)
          | (x, y) -> (Some x, Some y)) in
      (result, gsd, rr, rb)
    
        
(*
 * handle_step outputs game_output (gr, gs, rr, br) where:
 * gr is the result of the game. Output None if no winner was determined.
 * gs is the state of the game after handle_step ended.
 * rr is an option containing the request for the red team
 * br is an option containing the request for the blue team
 * None indicates that the team should respond with DoNothing *)


 
 
(* Zapdos uses Thunder on Zapdos, returns damage done *)  
let testing () = 
  let attack_pool = parseAttacks "attack.txt" in
  let mon_pool = parseSteammons "steammon.txt" attack_pool in
  let a = List.hd mon_pool in
  let b = List.hd mon_pool in
  let thunder = a.third_attack in
  State.calcDamage a b thunder
  
  
(* Read steammon.txt, read attacks.txt
 * Game          First   All attacks   All SteamMon
 * Return game * color * attack list * steammon list
 *)
let init_game () =
  send_update (InitGraphics);
  let firstPick = if Random.bool () then Red else (firstMove := Blue; Blue) in
  (* (((lred, ired), (lblue, iblue)), p, ats, sp) *)
  (((([], []), ([], [])), State.Draft, attack_pool, !mon_pool), 
    firstPick, attack_pool, !mon_pool)
  
	
	

