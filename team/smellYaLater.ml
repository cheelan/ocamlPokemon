open Team
open Definitions
open Constants
open Util

let _ = Random.self_init ()


(* Calculate the damage attacker's a does to defender *)    
(* Things to think about - should I consider atk mods? *)
let calcDamage attacker defender a = 
  let crit = 1. in
  let stab = 
    match attacker.first_type, attacker.second_type with
      | ((Some t), _) when t = a.element  -> cSTAB_BONUS
      | (_, Some t) when t = a.element -> cSTAB_BONUS
      | _ -> 1. in 
  let effective =
    let bonus1 =  
      match defender.first_type with
        | Some s -> weakness a.element s
        | None -> failwith "Defender must have at least 1 type"
    and bonus2 =
      match defender.second_type with
        | Some s -> weakness a.element s
        | None -> 1. in bonus1 *. bonus2 in 
  let atkMod =
    match attacker.mods.attack_mod with
      | -3 -> cATTACK_DOWN3
      | -2 -> cATTACK_DOWN3
      | -1 -> cATTACK_DOWN3
      | 0 -> 1.
      | 1 -> cATTACK_UP1
      | 2 -> cATTACK_UP2
      | 3 -> cATTACK_UP3 
      | _ -> failwith "Invalid attack mod"
  and defMod = 
    match defender.mods.defense_mod with
      | -3 -> cDEFENSE_DOWN3
      | -2 -> cDEFENSE_DOWN2
      | -1 -> cDEFENSE_DOWN1
      | 0 -> 1.
      | 1 -> cDEFENSE_UP1
      | 2 -> cDEFENSE_UP2
      | 3 -> cDEFENSE_UP3 
      | _ -> failwith "Invalid attack mod" in
  let (atk, def) =
    match a.element with
      | Normal | Fighting | Flying | Ground 
      | Rock | Bug | Ghost | Poison | Steel -> 
        ((float_of_int attacker.attack) *. atkMod,
          (float_of_int defender.defense) *. defMod)
      | _ -> (float_of_int attacker.spl_attack, 
              float_of_int defender.spl_defense) in
  int_of_float (
    (((float_of_int a.power) *. atk *. (crit) *. (stab) *. (effective)) 
    /. def))
    
   
let isFaster one two =
  let onep = if List.mem Paralyzed one.status then 
      1./.(float_of_int cPARALYSIS_SLOW) else 1. in
  let twop = if List.mem Paralyzed two.status then 1./.
        (float_of_int cPARALYSIS_SLOW) else 1. in
  let onep = match one.mods.speed_mod with
      -3 -> cSPEED_DOWN3 *. onep
      | -2 -> cSPEED_DOWN2 *. onep
      | -1 -> cSPEED_DOWN1 *. onep
      | 0 -> onep
      | 1 -> cSPEED_UP1 *. onep
      | 2 -> cSPEED_UP2 *. onep
      | 3 -> cSPEED_UP3 *. onep
      | x -> failwith ("invalid speed mod" ^ string_of_int(x)) in
  let twop = match two.mods.speed_mod with
      -3 -> cSPEED_DOWN3 *. twop
      | -2 -> cSPEED_DOWN2 *. twop
      | -1 -> cSPEED_DOWN1 *. twop
      | 0 -> twop
      | 1 -> cSPEED_UP1 *. twop
      | 2 -> cSPEED_UP2 *. twop
      | 3 -> cSPEED_UP3 *. twop
      | x -> failwith ("invalid speed mod" ^ string_of_int(x)) in
  let (onespeed, twospeed) =  int_of_float(float_of_int(one.speed)*.onep), 
       int_of_float(float_of_int(two.speed)*.twop) in
  onespeed > twospeed
  
(* 
Returns a tuple of the move and the expected number of turns it takes to win 
with it
Currently returns the move that KO's opponent with cCONFIDENCE in fewest turns 
Returns the best move (by damage/accuracy) to use 
   LOTS of things to consider, like super-prediction *)
let getBestMove attacker defender =
  let atkList = 
    [attacker.first_attack; attacker.second_attack; attacker.third_attack;
    attacker.fourth_attack] in
  let analyze best atk = 
    let oldA, oldDam = best in 
    let damage = calcDamage attacker defender atk in
    if atk.pp_remaining < 1 then best else
    (* If damage is less, only select if accuracy is more and can kill anyway *)
    if damage < oldDam
    then 
      (if (atk.accuracy > oldA.accuracy && defender.curr_hp < damage)
      then (atk, damage)
      else best)
    else 
      (if (oldA.accuracy > atk.accuracy && defender.curr_hp < oldDam)
      then best
      else (atk, damage)) in
  (List.fold_left analyze (attacker.first_attack, 0) atkList)  
 
(*Returns the most damaging move. Assume opponent uses this *)  
let getMostDamagingMove attacker defender =
  let atkList = 
    [attacker.first_attack; attacker.second_attack; attacker.third_attack;
    attacker.fourth_attack] in
  let analyze best atk = 
    let (_, oldDam) = best in  
    if atk.pp_remaining < 1 then best else
    let damage = calcDamage attacker defender atk in
    (* If damage is less, only select if accuracy is more and can kill anyway *)
    if damage < oldDam
    then best
    else (atk, damage) in
  (List.fold_left analyze (attacker.first_attack, 0) atkList)  
  
(*Returns true iff mon counters opponent *)  
let isCounter mon opponent expectedAttack =
  let damage = calcDamage opponent mon expectedAttack in
  let hpAfterSwitch = mon.curr_hp - damage in
  if hpAfterSwitch < 1 then false else(
  let (myBestMove, myDamage) = getBestMove mon opponent in
  let (opBestMove, opDamage) = getMostDamagingMove opponent mon in
  if hpAfterSwitch < 1 then false
  else if (*I am faster and I kill *)
    (isFaster mon opponent) && (myDamage >= opponent.curr_hp) 
    then  true
  else if (*I am slower. I survive best atk and kill *)
    (not (isFaster mon opponent)) && (myDamage >= opponent.curr_hp) 
      && (hpAfterSwitch - opDamage > 0)
    then true
  else if (opDamage = 0 && myDamage > 0) (*It can't damage me *)
    then true
  else false)
  
let isL1Counter opponent mon =
  let (opBestMove, opDamage) = getMostDamagingMove opponent mon in
  let (myBestMove, myDamage) = getBestMove mon opponent in
  if mon.curr_hp < 1 then false
  else if (*I am faster and I kill *)
    (isFaster mon opponent) && (myDamage >= opponent.curr_hp) 
    then  true
  else if (opDamage = 0 && myDamage > 0) (*It can't damage me*)
    then true
  else false
  
(* Returns true if I am slower, but will survive and can KO, or if opponent 
   cannot do much damage to me *)  
let isL2Counter mon opponent switchD = 
  let (opBestMove, opDamage) = getMostDamagingMove opponent mon in
  let (myBestMove, myDamage) = getBestMove mon opponent in
  if (*I am slower. I survive best atk and kill *)
    (not (isFaster mon opponent)) && (myDamage >= opponent.curr_hp) 
      && (switchD + opDamage < mon.curr_hp)
    then true
  else if opDamage < (mon.curr_hp / 8) && (myDamage > opponent.max_hp / 6)
    then true
  else false

(* Returns true if the mon is just ok against the opponent *)
let isOK mon opponent switchDam =
  let (opBestMove, opDamage) = getMostDamagingMove opponent mon in
  let (myBestMove, myDamage) = getBestMove mon opponent in
  if switchDam > mon.curr_hp then false
  else (not (opDamage > mon.max_hp / 2)) && (myDamage > opponent.curr_hp / 3)
  
let rec notUseless myTeam opponent =
  let helper mon =
    let (myBestMove, myDamage) = getBestMove mon opponent in
    (myDamage > 0 && mon.curr_hp > 0) in
  let rec iter team = 
  match team with
    | h::t -> if helper h then h else iter t 
    | [] ->  
      (print_endline("Desperate pick: ");
      try List.find(fun x -> x.curr_hp > 0) myTeam
      with _ -> (List.hd myTeam)) in iter myTeam

(*Like getCounter, but it must return a mon*)
let getBestMon myTeam opponent expectedAttack =
  let l1counters = List.filter (isL1Counter opponent) myTeam in
  if List.length l1counters > 0 
  then List.hd l1counters
  else(
  let rec helper team acc = 
    match team with
      | h::t -> 
          let switchDam = 
            (match expectedAttack with
               | None -> 0
               | Some a -> calcDamage opponent h a) in
          if switchDam > h.curr_hp then helper t acc
          else if isL2Counter h opponent switchDam then [h]
          else if isOK h opponent switchDam then helper t [h]
          else helper t acc
      | [] -> [notUseless myTeam opponent]
           
  in (List.hd (helper myTeam [])))  
  
let getCounter myTeam opponent expectedAttack = 
  let l1counters = List.filter (isL1Counter opponent) myTeam in
  if List.length l1counters > 0 
  then Some (List.hd l1counters)
  else
  let rec helper team = 
    match team with
      | h::t -> 
        if isCounter h opponent expectedAttack then Some h
        else helper t
      | [] -> None in
  helper (List.tl myTeam)    
  
(* Returns a steammon that is immune to an attack I was going to use, if one
   exists. For example, if I'm tempted to use earthquake, but my opponent has 
   a flying type, he should switch. *)  
let dangerousMove atk opTeam =
  let rec teamHasType opTeam element =
    match opTeam with
      | [] -> None
      | h::t -> 
        if (h.curr_hp > 0 && 
        (h.first_type = Some element || h.second_type = Some element)) 
        then Some h 
        else (teamHasType t element) in 
  match atk.element with
    | Psychic -> teamHasType opTeam Dark
    | Ground -> teamHasType opTeam Flying
    | Normal | Fighting -> teamHasType opTeam Ghost
    | Ghost -> teamHasType opTeam Normal
    | _ -> None
      
  

(* returns true iff mon is in sp, ie it hasn't been picked yet *)
let monAvailable sp mon =
  List.fold_left 
    (fun a e -> if e.species = mon.species then true else a) false sp

let calcPower attacker a = 
  match attacker.first_type, attacker.second_type with
    | ((Some t), _) when t = a.element  -> 
                    int_of_float(cSTAB_BONUS *. (float_of_int a.power))
    | (_, Some t) when t = a.element -> 
                    int_of_float(cSTAB_BONUS *. (float_of_int a.power))
    | _ -> a.power
      
let scoreMon mon = 
  (* returns true if atk matches physical/special atr *)
  let typeBonus atr atk = 
    match atk with
      | Normal | Fighting | Flying | Ground 
      | Rock | Bug | Ghost | Poison | Steel -> atr = 3 || atr = 1 
      | _ -> atr = 3 || atr = 2 in
  (*Mons can be physically oriented, specially oriented, or both *)  
  let attr = 
    if mon.attack > mon.spl_attack && mon.spl_attack < 90 then 1
    else if mon.attack > mon.spl_attack && mon.spl_attack >= 90 then 3
    else if mon.spl_attack > mon.attack && mon.attack < 90 then 2
    else 3 in
  let atkScore = (max mon.attack mon.spl_attack) * 3 in
  let hpScore = mon.max_hp / 3 in
  let speedScore = (mon.speed * 2) in
  let otherScore = mon.defense + mon.spl_defense in
  let hasDecentMoves = 
    calcPower mon mon.first_attack > 90 
    && (typeBonus attr mon.first_attack.element)
  || calcPower mon mon.second_attack > 90 
  && (typeBonus attr mon.second_attack.element)
  || calcPower mon mon.third_attack > 90 
  && (typeBonus attr mon.third_attack.element) 
  || calcPower mon mon.fourth_attack > 90 
  && (typeBonus attr mon.fourth_attack.element)
    in
  let moveScore = if hasDecentMoves then 0 else (-1000) in
  let moveScore2 = 
    let moveHT = Hashtbl.create 8 in
    (if calcPower mon mon.first_attack > 55 
    then Hashtbl.replace moveHT mon.first_attack.element 42 else ()); 
    (if calcPower mon mon.second_attack > 55 
    then Hashtbl.replace moveHT mon.second_attack.element 42 else ()); 
    (if calcPower mon mon.third_attack > 55 
    then Hashtbl.replace moveHT mon.third_attack.element 42 else ());
    (if calcPower mon mon.fourth_attack > 55 
    then Hashtbl.replace moveHT mon.fourth_attack.element 42 else ());
    (Hashtbl.length moveHT) * (Hashtbl.length moveHT) * 20 in
  let mewtwoBonus = 
    if mon.species = "Mewtwo" || mon.spl_attack = 154 then 300 else 0 in
  let typeScore = 
    (* What can I say? They're good types to have *)
    match (mon.first_type, mon.second_type) with
      | (Some Rock, Some Dark) -> 500
      | (Some Dark, Some Rock) -> 500 
      | (Some Psychic, Some Flying) -> 200
      | (Some Flying, Some Psychic) -> 100 (*This is intentional *)
      | (Some Flying, _) -> 10
      | (_, Some Flying) -> 10
      | _ -> 0 in
  atkScore + hpScore + speedScore + otherScore + moveScore + typeScore 
  + moveScore2 + mewtwoBonus
  
(*Returns the mon from the team with the greatest variety of damaging attacks *)  
let pickFirstMon myTeam = 
  let moveScore (best, score) mon = 
    let moveHT = Hashtbl.create 8 in
    (if calcPower mon mon.first_attack > 55 
    then Hashtbl.replace moveHT mon.first_attack.element 42 else ()); 
    (if calcPower mon mon.second_attack > 55 
    then Hashtbl.replace moveHT mon.second_attack.element 42 else ()); 
    (if calcPower mon mon.third_attack > 55 
    then Hashtbl.replace moveHT mon.third_attack.element 42 else ());
    (if calcPower mon mon.fourth_attack > 55 
    then Hashtbl.replace moveHT mon.fourth_attack.element 42 else ());
    if (Hashtbl.length moveHT) > score then (mon, (Hashtbl.length moveHT))
    else (best, score) in
  fst(List.fold_left moveScore ((List.hd myTeam), 0) myTeam)

let compareMon mon1 mon2 =
  let s1 = scoreMon mon1 and s2 = scoreMon mon2 in
  if s1 < s2 then (-1) 
  else if s1 > s2 then 1
  else 0
     
let rankMon (steamPool : steammon list) : steammon list =
  let sorted = List.rev (List.sort compareMon steamPool) in 
  (*let _ = List.fold_left (fun a e -> print_endline (e.species)) () sorted in*)
  sorted
  
let rec numAlive team =
  match team with
    | h::t -> if h.curr_hp > 0 then 1 + (numAlive t) else numAlive t
    | [] -> 0
       
let tryRevive team items = 
  let reviveAvailable = 
    match items with
      | [_;_;revives;_;_;_;_;_] -> revives > 0
      | _ -> false in
  let rec faintedMon team' =
    match team' with 
      | h::t -> h.curr_hp = 0 || faintedMon t
      | [] -> false in
   reviveAvailable && faintedMon team

let tryPotion team items = 
  let potionAvailable = 
    match items with
      | [_;potions;_;_;_;_;_;_] -> potions > 0
      | _ -> false in
  let rec hurtMon team' =
    match team' with
      | h::t -> h.curr_hp > 0 && h.curr_hp < (h.max_hp / 2)
      | [] -> false in
  potionAvailable && hurtMon team
  

let lstRemove lst mem = 
  let (ret, seen) = List.fold_left (fun (a, seen) e -> 
    if e = mem then (a, true) else (e::a, seen)) ([], false) lst in
  if seen then (List.rev ret)
  else failwith "not in list"

let rankedMons = ref []
let superPredictOn = ref 1
let superPrev = ref "empty"
let firstTurn = ref true

let handle_request c r =
  match r with
    | StarterRequest(gs)->
        let (a1, b1) = gs in
        let (myTeam, myItems) = if c = Red then a1 else b1 in
        let opponentTeam = if c = Red then fst(b1) else fst(a1) in
        let opponent = List.hd opponentTeam in
        let pick = if !firstTurn then ((firstTurn := false); 
          pickFirstMon myTeam)
          else (getBestMon myTeam opponent None) in
          print_endline ("Selected starter " ^ pick.species);
          SelectStarter(pick.species)
    | PickRequest(c, gs, _, sp) ->
        let _ = if !rankedMons = [] then rankedMons := (rankMon sp) else () in
        let (a1, b1) = gs in
        let (myTeam, _) = if c = Red then a1 else b1 in
        let rec pickMon lst =
          (match lst with
            | h::t -> 
              if (monAvailable myTeam h) then pickMon t
              else if not (monAvailable sp h) then pickMon t
              else (print_endline ("picking " ^ (h.species));
                              PickSteammon(h.species))
            | [] -> failwith "Well that's a pretty crappy steam pool") in
       pickMon !rankedMons                        
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let (myTeam, myItems) = if c = Red then a1 else b1 in
        let opponentTeam = if c = Red then fst(b1) else fst(a1) in
        let me = List.hd myTeam in
        let opponent = List.hd opponentTeam in
        let iAmFaster = isFaster me opponent in
        (* If superPredict ever fails, turn it off *)
        let _ = 
          if !superPrev <> "empty" && !superPrev <> opponent.species
          then superPredictOn := (!superPredictOn - 1) 
          else if !superPrev <> "empty" then superPredictOn := !superPredictOn+1
          else () in
        let (expectedAttack, damage) = getMostDamagingMove opponent me in
        (* Attempts to find the "best" attack for attacker*)
        let (bestAttack, d) = getBestMove me opponent in
        let superPredict = (dangerousMove bestAttack opponentTeam) in
        let counterOpt = getCounter myTeam opponent expectedAttack in
        if (d < (opponent.curr_hp) && (damage < me.max_hp / 3) 
        && (tryPotion myTeam myItems))
          then let target = List.hd (List.filter (fun x -> x.curr_hp > 0 
          && x.curr_hp < (x.max_hp / 2)) myTeam)
          in (print_endline ("Using max potion on "^ target.species); 
             UseItem(MaxPotion, target.species))
        else  
        let doISwitch = (*More to do here *) 
             ((damage >  (me.max_hp / 2)) 
          || (d < opponent.curr_hp / 3)
          
          )  
          (* Don't switch if you can KO before you switch *)
          && (not (d > opponent.curr_hp && me.speed >= opponent.speed))
          in let counterExists = 
          match counterOpt with
            | Some _ -> true
            | None -> false in
       if doISwitch && counterExists
         then 
           match counterOpt with
             | Some counter -> 
                 SelectStarter(counter.species)
             | None -> failwith "Impossible"
        
       else if (((me.curr_hp < damage) && not iAmFaster) || 
         (doISwitch && (me.curr_hp < (me.max_hp / 5)*2))) 
         && tryRevive myTeam myItems 
         then let target = List.hd (List.filter (fun x -> x.curr_hp = 0) myTeam) 
         in
            (print_endline ("Using revive!"); UseItem(Revive,target.species)) 
       else if (((me.curr_hp < damage) && not iAmFaster) || (doISwitch)) 
               && tryPotion myTeam myItems
         then  let target = List.hd (List.filter (fun x -> x.curr_hp > 0
               && x.curr_hp < (x.max_hp / 2)) myTeam)
          in (print_endline ("Using max potion on "^ target.species); 
          UseItem(MaxPotion, target.species))
       else if 
           d = 0 then 
             if numAlive myTeam > 1 
             then SelectStarter ((getBestMon (List.tl myTeam) opponent 
               (Some expectedAttack)).species)
             else if tryRevive myTeam myItems
             then let target = List.hd (List.filter 
               (fun x -> x.curr_hp = 0) myTeam) in
            (print_endline ("Using revive!"); UseItem(Revive,target.species))
            else (UseAttack(me.first_attack.name))
       else if superPredict <> None && (!superPredictOn > 0)
         then 
           (match superPredict with
             | Some s -> 
                 let superAttack = fst(getBestMove me s) in
                 superPrev := s.species;
                 UseAttack(superAttack.name)
             | None -> failwith "Unreachable")
       else
          UseAttack(bestAttack.name) 
          
	 | PickInventoryRequest (gr) -> 
	    let revives = 3 in
	    let potions = 
	      ((cINITIAL_CASH - (revives * cCOST_REVIVE)) / cCOST_MAXPOTION)  in
	 PickInventory(
	                [0;potions;revives;0;
	 				 0;0;0;0])

let () = run_bot handle_request
