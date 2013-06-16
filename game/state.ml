open Netgraphics
open Definitions
open Util
open Constants
let attacked = ref false
(*module State = 
  struct*)
    type game_phase = Draft | Inventory | Battle
    type state = game_status_data * game_phase * attack_set * steam_pool
    exception OutOfPP
    let getPhase (_, p, _, _) = p
    let setPhase (((lred, ired), (lblue, iblue)), _, a, s) p =
      (((lred, ired), (lblue, iblue)), p, a, s)
    let getStatusData (s, _, _, _) = s
    let getSteamMonList (((lstred, _), (lstblue, _)), _, _, _)  color = 
      match color with
        | Red -> lstred
        | Blue -> lstblue
    let getSteamPool (_, _, _, sp) = sp
    let getInventory (((_, invred), (_, invblue)), _, _, _) color = 
      match color with
        | Red -> invred
        | Blue -> invblue
    let getActive state color = 
      match (getSteamMonList state color) with
        | [] -> failwith "GetActive failed in state.ml"
        | h::t -> h
    let lstRemove lst mem = 
      let (ret, seen) = List.fold_left (fun (a, seen) e -> 
        if e = mem then (a, true) else (e::a, seen)) ([], false) lst in
      if seen then ret
      else ret

    let removeConfused steammon = {species = steammon.species; 
        curr_hp = steammon.curr_hp; max_hp = steammon.max_hp; 
        first_type = steammon.first_type; second_type = steammon.second_type;
        first_attack = steammon.first_attack; 
        second_attack = steammon.second_attack; 
        third_attack = steammon.third_attack; 
        fourth_attack = steammon.fourth_attack;
        attack = steammon.attack; spl_attack = steammon.spl_attack; 
        defense = steammon.defense; spl_defense = steammon.spl_defense;
        speed = steammon.speed; status = lstRemove steammon.status Confused;
        mods = { 
                attack_mod= 0;
                speed_mod=0;
                defense_mod= 0;
                accuracy_mod= 0
              }
        }
      
    let setActive ((((lred, ired), (lblue, iblue)), p, ats, sp) as state) 
      color mon =
      let (newlst,seen, m, _) = 
        List.fold_left 
          (fun (a,seen, x, first) e -> 
            if e.species = mon && e.curr_hp > 0 
            then (a, true, e, false) else if first 
              then (removeConfused e::a, seen, x, false)
              else (e::a, seen, x, false)) 
          ([], false, List.hd (getSteamMonList state color), true) 
            (getSteamMonList state color) in
      if seen 
      then 
        match color with
          | Red -> (((m::newlst,ired),(lblue,iblue)), p, ats, sp)
          | Blue -> (((lred, ired), (m::newlst,iblue)), p, ats ,sp)
      else failwith "Trying to set active mon to invalid mon"
    let steammonOfString str sp =
      match List.fold_left (fun m el -> if el.species = str then Some el else m)
        None sp with
        Some x -> x 
        | None -> failwith "mon doesn't exist"
    let draftMon (((lred, ired), (lblue, iblue)), p, ats, sp) color mon =
      let mon = steammonOfString mon sp in
      match p with
        | Draft -> begin
          match color with
            | Red -> 
              (((mon::lred, ired), (lblue, iblue)), p, ats, lstRemove sp mon)
            | Blue -> 
              (((lred, ired), (mon::lblue, iblue)), p, ats, lstRemove sp mon)
          end
        | _ -> failwith "Can't add steammon unless in Draft phase"
    let exceedAmount lst = 
      match lst with 
        [e; m; f; r; a; d; s; acc] -> e * cCOST_ETHER + m * 
          cCOST_MAXPOTION + f * cCOST_FULLHEAL + r * cCOST_REVIVE + a *
          cCOST_XATTACK + d * cCOST_XDEFEND + s * cCOST_XSPEED + acc * 
          cCOST_XATTACK > cINITIAL_CASH      
        | _ -> failwith "invalid inventory list"
    let addItems (((lred, ired), (lblue, iblue)), p, ats, sp) color lst =
      let lst = if exceedAmount lst then [cNUM_ETHER; cNUM_MAX_POTION; 
        cNUM_REVIVE; cNUM_FULL_HEAL; cNUM_XATTACK; cNUM_XDEFENSE; 
        cNUM_XACCURACY; cNUM_XSPEED] else lst in
      match p with
        | _ (*Inventory*) -> begin
          match color with
            | Red -> (((lred, lst), (lblue, iblue)), p, ats, sp)
            | Blue -> (((lred, ired), (lblue, lst)), p, ats, sp)
          end
        | _ -> failwith "Can't add items unless in inventory phase"
    let modSteammon func lst mon = let (ret, seen) = List.fold_left 
      (fun (a, seen) e -> if mon = e.species then ((func e)::a, true) 
        else (e::a, seen)) ([], false) lst in
      if seen then List.rev ret
      else failwith "Steammon not there"    
    let decrement i lst = 
      List.rev (fst (List.fold_left 
        (fun (a, x) e -> if x = i then ((e - 1)::a, x + 1) else (e::a, x + 1))
        ([], 0) lst))
    let hasItem i lst = 
      fst (List.fold_left (fun (a, x) e -> if x = i then (e > 0, x + 1) 
        else (a, x + 1)) (false, 0) lst)
    let useEther (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      let addFive attack = {name = attack.name; element = attack.element; 
        max_pp = attack.max_pp; pp_remaining = (min attack.max_pp 
        (attack.pp_remaining + 5)); power = attack.power; 
        accuracy = attack.accuracy; crit_chance = attack.crit_chance; 
        effect = attack.effect} in
      let etherOp = fun steammon -> {species = steammon.species; 
        curr_hp = steammon.curr_hp; max_hp = steammon.max_hp; 
        first_type = steammon.first_type; second_type = steammon.second_type;
        first_attack = addFive steammon.first_attack; 
        second_attack = addFive steammon.second_attack; 
        third_attack = addFive steammon.third_attack; 
        fourth_attack = addFive steammon.fourth_attack;
        attack = steammon.attack; spl_attack = steammon.spl_attack; 
        defense = steammon.defense; spl_defense = steammon.spl_defense;
        speed = steammon.speed; status = steammon.status; mods = steammon.mods} 
       in
      match color with
        | Red -> if hasItem 0 ired then (((modSteammon etherOp lred mon, 
                  decrement 0 ired), (lblue, iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 0 iblue then (((lred, ired), (modSteammon etherOp 
                   lblue mon, decrement 0 iblue)), p, ats, sp) 
                  else failwith "Can't use item, cause it's not in inventory"
    (*if x is true we have to make sure it's fainted*)
    let restoreOp x = fun steammon -> if x then if steammon.curr_hp > 0 then  
      failwith "can only use on fainted steammon" else () else (); 
      let newStatus = if x then [] else steammon.status in
      let newHp = if x then steammon.max_hp/2 else steammon.max_hp in
      {species = steammon.species; max_hp = steammon.max_hp;
        curr_hp = newHp; first_type = steammon.first_type; 
        second_type = steammon.second_type; 
        first_attack = steammon.first_attack; 
        second_attack = steammon.second_attack; 
        third_attack = steammon.third_attack;
        fourth_attack = steammon.fourth_attack;
        attack = steammon.attack; spl_attack = steammon.spl_attack; 
        defense = steammon.defense; spl_defense = steammon.spl_defense;
        speed = steammon.speed; status = newStatus; mods = steammon.mods} 
    let useMaxPotion (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      match color with
        | Red -> if hasItem 1 ired then (((modSteammon (restoreOp false) 
                  lred mon, decrement 1 ired), (lblue, iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 1 iblue then (((lred, ired), (modSteammon 
                   (restoreOp false) lblue mon, decrement 1 iblue)), p, ats, sp)
                  else failwith "Can't use item, cause it's not in inventory"
    let useRevive (((lred, ired), (lblue, iblue)), p, ats, sp) color mon =
      match color with
        | Red -> if hasItem 2 ired then (((modSteammon (restoreOp true) lred
                  mon, decrement 2 ired), (lblue, iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 2 iblue then (((lred, ired), (modSteammon
                   (restoreOp true) lblue mon, decrement 2 iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
    let useFullHeal (((lred, ired), (lblue, iblue)), p, ats, sp) color mon =
      let removeStat = fun steammon -> {species = steammon.species; 
        max_hp = steammon.max_hp; curr_hp = steammon.curr_hp; 
        first_type = steammon.first_type; 
        second_type = steammon.second_type; 
        first_attack = steammon.first_attack; 
        second_attack = steammon.second_attack; 
        third_attack = steammon.third_attack;
        fourth_attack = steammon.fourth_attack;
        attack = steammon.attack; spl_attack = steammon.spl_attack; 
        defense = steammon.defense; spl_defense = steammon.spl_defense;
        speed = steammon.speed; status = []; mods = steammon.mods} in
      match color with
        | Red -> if hasItem 3 ired then (((modSteammon removeStat lred mon, 
                  decrement 3 ired), (lblue, iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 3 ired then (((lred, ired), (modSteammon
                   removeStat lblue mon, decrement 3 iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
    let moreAttack x = fun steammon -> {species = steammon.species; 
        max_hp = steammon.max_hp; curr_hp = steammon.curr_hp; 
        first_type = steammon.first_type; 
        second_type = steammon.second_type; 
        first_attack = steammon.first_attack; 
        second_attack = steammon.second_attack; 
        third_attack = steammon.third_attack;
        fourth_attack = steammon.fourth_attack;
        attack = steammon.attack; spl_attack = steammon.spl_attack; 
        defense = steammon.defense; spl_defense = steammon.spl_defense;
        speed = steammon.speed; status = steammon.status; 
        mods = x steammon.mods} 
    let useXattack (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      let changeMod modifier = {attack_mod = min (modifier.attack_mod + 1) 3;
        speed_mod = modifier.speed_mod; defense_mod = modifier.defense_mod;
        accuracy_mod = modifier.accuracy_mod} in
      match color with
        | Red -> if hasItem 4 ired then (((modSteammon (moreAttack changeMod) 
                  lred mon, decrement 4 ired), (lblue, iblue)), p, ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 4 ired then (((lred, ired), (modSteammon
                   (moreAttack changeMod) lblue mon, decrement 4 iblue)), p, 
                   ats, sp)
                 else failwith "Can't use item, cause it's not in inventory"
    let useXdefense (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      let changeMod modifier = {attack_mod = modifier.attack_mod; 
        speed_mod = modifier.speed_mod; defense_mod = min 
        (modifier.defense_mod + 1) 3; accuracy_mod = modifier.accuracy_mod} in
      match color with
        | Red -> if hasItem 5 ired then (((modSteammon (moreAttack changeMod) 
                  lred mon, decrement 5 ired), (lblue, iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 5 ired then (((lred, ired), (modSteammon
                   (moreAttack changeMod) lblue mon, decrement 5 iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
    let useXspeed (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      let changeMod modifier = {attack_mod = modifier.attack_mod; 
        speed_mod = min (modifier.speed_mod + 1) 3;
        defense_mod = modifier.defense_mod; 
        accuracy_mod = modifier.accuracy_mod} in
      match color with
        | Red -> if hasItem 6 ired then (((modSteammon (moreAttack changeMod) 
                  lred mon, decrement 6 ired), (lblue, iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 6 ired then (((lred, ired), (modSteammon
                   (moreAttack changeMod) lblue mon, decrement 6 iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
    let useXaccuracy (((lred, ired), (lblue, iblue)), p, ats, sp) color mon = 
      let changeMod modifier = {attack_mod = modifier.attack_mod; 
        speed_mod = modifier.speed_mod; defense_mod = modifier.defense_mod; 
        accuracy_mod = min (modifier.accuracy_mod + 1) 3} in
      match color with
        | Red -> if hasItem 7 ired then (((modSteammon (moreAttack changeMod) 
                  lred mon, decrement 7 ired), (lblue, iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
        | Blue -> if hasItem 7 ired then (((lred, ired), (modSteammon
                   (moreAttack changeMod) lblue mon, decrement 7 iblue)), p, ats, sp) 
                 else failwith "Can't use item, cause it's not in inventory"
    let useItem ((((lred1, ired1), (lblue1, iblue1)), _, _, _) as state) 
      color item mon = 
      let steammon = match color with
        Red -> steammonOfString mon lred1
        | Blue -> steammonOfString mon lblue1 in
      let prevHp = steammon.curr_hp in
      let (((((lred, ired), (lblue, iblue)), p, ats, sp) as ret), 
        update, msg) = 
      match item with
        | Ether -> (useEther state color mon), false, "Used Ether"
        | MaxPotion -> (useMaxPotion state color mon), true, "Used Max Potion"
        | Revive-> (useRevive state color mon), true, "Used Revive"
        | FullHeal -> (useFullHeal state color mon), true, "Used Full Heal"
        | XAttack -> useXattack state color mon, false, "Used X Attack"
        | XDefense -> useXdefense state color mon, false, "Used X Defense"
        | XSpeed -> useXspeed state color mon, false, "Used X Speed"
        | XAccuracy -> useXaccuracy state color mon, false, "Used X Accuracy" in
      let steammon = match color with
        Red -> steammonOfString mon lred
        | Blue -> steammonOfString mon lblue in
      (if update then
      add_update(UpdateSteammon(mon, steammon.curr_hp, steammon.max_hp, color))
      else ());
      add_update(PositiveEffect(msg, color, steammon.curr_hp - prevHp));
      (if item = FullHeal then 
      add_update(SetStatusEffects(mon, [])) else ());
      ret  

        
        
    let random (odds : int) : bool = 
      Random.int 100 < odds
      
    let isLeadFainted gs color = 
      let ((lred, _), (lblue, _)) = gs in
      let lead = 
        match color with
          | Red -> List.hd lred
          | Blue -> List.hd lblue in
      lead.curr_hp = 0
        
    let getResult gs = 
      let ((lred, _), (lblue, _)) = gs in
      let f a e = 
        if e.curr_hp = 0 then a + 1 else a in
      let redLost = (List.fold_left f 0 lred) = cNUM_PICKS
      and blueLost = (List.fold_left f 0 lblue) = cNUM_PICKS in
      if redLost && blueLost then Some Tie
      else if redLost then Some (Winner(Blue))
      else if blueLost then Some (Winner(Red))
      else None
     
    (* Calculate the damage attacker's a does to defender *)    
    let calcDamage attacker defender a = 
      let crit = 
        if random a.crit_chance 
        then (add_update(Message("Critical hit!")); cCRIT_MULTIPLIER)
        else 1. in
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
          | x -> failwith ("Invalid attack mod"^string_of_int(x));
      and defMod = 
        match defender.mods.defense_mod with
          | -3 -> cDEFENSE_DOWN3
          | -2 -> cDEFENSE_DOWN2
          | -1 -> cDEFENSE_DOWN1
          | 0 -> 1.
          | 1 -> cDEFENSE_UP1
          | 2 -> cDEFENSE_UP2
          | 3 -> cDEFENSE_UP3 
          | x -> failwith ("Invalid attack defense"^string_of_int(x)); in
      let (atk, def) =
        match a.element with
          | Normal | Fighting | Flying | Ground 
          | Rock | Bug | Ghost | Poison | Steel -> 
            ((float_of_int attacker.attack) *. atkMod,
              (float_of_int defender.defense) *. defMod)
          | _ -> (float_of_int attacker.spl_attack, 
                  float_of_int defender.spl_defense) in
      print_endline (string_of_int a.power ^ " * " ^ string_of_float atk ^ " * " ^ string_of_float crit ^ " * " ^ string_of_float stab ^ " * " ^ string_of_float effective ^ " / " ^ string_of_float def);
      int_of_float (
        (((float_of_int a.power) *. atk *. (crit) *. (stab) *. (effective)) 
        /. def))

  (* Returns game state after an attack. 
  Assume attack hits 
  Reduce attacker PP
  Calc Damage
  Apply effects
  Handle fainting 
  
    species: string;
  curr_hp : int;
  max_hp : int;
  first_type: steamtype option;
  second_type: steamtype option;
  first_attack: attack;
  second_attack: attack;
  third_attack: attack;
  fourth_attack: attack;
  attack: int;
	spl_attack : int;
  defense: int;
	spl_defense: int;
  speed: int;
  status: status list;
  mods: modifier*)
  let string_of_status s = 
    match s with
      | Paralyzed -> "Paralyzed"
      | Poisoned -> "Poisoned"
      | Asleep -> "Asleep"
      | Confused -> "Confused"
      | Frozen -> "Frozen"
      
  let status_of_string s = 
    match s with 
      | "Paralyzed" -> Paralyzed
      | "Poisoned" -> Poisoned
      | "Asleep" -> Asleep
      | "Confused" -> Confused
      | "Frozen" -> Frozen
      | _ -> failwith "status of string failure"
      
  (* Value must be a string *)
  let changeHP mon value =
   { 
    species = mon.species; 
    curr_hp = value; (* *) 
    max_hp = mon.max_hp;
    first_type = mon.first_type;
    second_type = mon.second_type;
    first_attack = mon.first_attack;
    second_attack = mon.second_attack;
    third_attack = mon.third_attack;
    fourth_attack = mon.fourth_attack;
    attack = mon.attack;
    spl_attack = mon.spl_attack;
    defense = mon.defense;
    spl_defense = mon.spl_defense;
    speed = mon.speed;
    status = mon.status;
    mods = mon.mods }

    let doPoison  (((lstred, y), (lstblue, x)), p, ats, sp)  color = 
      let lead = match color with
        Red -> List.hd lstred
        | Blue -> List.hd lstblue in
      let newLead = if List.mem Poisoned lead.status then
        let x =
        ((int_of_float(cPOISON_DAMAGE*.
          (float_of_int (lead.max_hp))))) in
        add_update(Message(lead.species ^" lost " ^ string_of_int(x) ^ 
        " health Due to Poison"));
        changeHP lead (max 0 (lead.curr_hp - x))
         
      else lead in
      match color with
        Red -> (((newLead::(List.tl lstred), y), (lstblue, x)), p, ats, sp)
        | Blue -> (((lstred, y), (newLead::(List.tl lstblue), x)), p, ats, sp)

    let addModifier mon field value c =   
      let _ = 
        match value with
          | a when a < 0 -> 
            add_update(
              NegativeEffect((field^" modified "
                ^(string_of_int value)), (invert_color c), 0));
          | a when a > 0 -> 
          add_update(
              PositiveEffect((field^" modified "^(string_of_int value)), c, 0));
          | _ -> failwith "Good style"
              in
    { 
      species = mon.species; 
      curr_hp = mon.curr_hp; (* *) 
      max_hp = mon.max_hp;
      first_type = mon.first_type;
      second_type = mon.second_type;
      first_attack = mon.first_attack;
      second_attack = mon.second_attack;
      third_attack = mon.third_attack;
      fourth_attack = mon.fourth_attack;
      attack = mon.attack;
      spl_attack = mon.spl_attack;
      defense = mon.defense;
      spl_defense = mon.spl_defense;
      speed = mon.speed;
      status = mon.status;
      mods = 
        match field with 
          | "Attack" -> { attack_mod = 
                            min 3 (max (-3) (mon.mods.attack_mod + value));
                          speed_mod = mon.mods.speed_mod;
                          defense_mod = mon.mods.defense_mod;
                          accuracy_mod = mon.mods.accuracy_mod }
          | "Speed" -> { attack_mod = mon.mods.attack_mod;
                          speed_mod = 
                            min 3 (max (-3) (mon.mods.speed_mod + value));
                          defense_mod = mon.mods.defense_mod;
                          accuracy_mod = mon.mods.accuracy_mod }
          | "Defense" -> { attack_mod = mon.mods.attack_mod;
                          speed_mod = mon.mods.speed_mod;
                          defense_mod = 
                            min 3 (max (-3) (mon.mods.defense_mod + value));
                          accuracy_mod = mon.mods.accuracy_mod }
          | "Accuracy" ->  { attack_mod = mon.mods.attack_mod + value;
                          speed_mod = mon.mods.speed_mod;
                          defense_mod = mon.mods.defense_mod;
                          accuracy_mod = 
                            min 3 (max (-3) (mon.mods.accuracy_mod + value)); }
          | _ -> failwith "failure in match field"
      }
      
  (* Given game state, determine which steammon attacks first, returns color *)  
  let getFastest gs =
    let (one, two) = (getActive gs Red, getActive gs Blue) in 
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
    if  onespeed < twospeed then Blue
    else if onespeed > twospeed then Red
    else if Random.bool () then Red else Blue
    
    let selfAttack = { name= "confused";
                                      element= Water;
                                      max_pp= 42;
                                      pp_remaining= 42;
                                      power= cSELF_ATTACK_POWER;
                                      accuracy= 100;
                                      crit_chance= 0;
                                      effect= (Nada, 0)
                                   }
 
  let applyAttack gs color atkStr = 
    let (((lred, ired), (lblue, iblue)), p, ats, sp) = gs in
    let (attacker, defender) = 
      match color with
        | Red -> (List.hd lred, List.hd lblue)
        | Blue -> (List.hd lblue, List.hd lred) in 
    let (atk, atkNum) = 
      match atkStr with 
        | a when a = attacker.first_attack.name -> attacker.first_attack, 1
        | a when a = attacker.second_attack.name -> attacker.second_attack, 2
        | a when a = attacker.third_attack.name -> attacker.third_attack, 3
        | a when a = attacker.fourth_attack.name -> attacker.fourth_attack, 4
        | _ -> failwith "Invalid attack in applyAttack" in 
        
        
     (* Need to consider removing status*)
     let addStatus mon value = 
       match value with
         | Some value -> {
           species = mon.species; 
           curr_hp = mon.curr_hp;
           max_hp = mon.max_hp;
           first_type = mon.first_type;
           second_type = mon.second_type;
           first_attack = mon.first_attack;
           second_attack = mon.second_attack;
           third_attack = mon.third_attack;
           fourth_attack = mon.fourth_attack;
           attack = mon.attack;
           spl_attack = mon.spl_attack;
           defense = mon.defense;
           spl_defense = mon.spl_defense;
           speed = mon.speed;
           status = value::mon.status; (* *)
           mods = mon.mods }
         | None -> mon in
    let newAcc = match attacker.mods.accuracy_mod with
      | -3 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_DOWN3)
      | -2 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_DOWN2)
      | -1 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_DOWN1)
      | 0 -> int_of_float(float_of_int(atk.accuracy)*.1.)
      | 1 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_UP1)
      | 2 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_UP2)
      | 3 -> int_of_float(float_of_int(atk.accuracy)*.cACCURACY_UP3)
      | x -> failwith ("invalid accuracy mod" ^ string_of_int(x)) in
    let damage = 
      if random newAcc then 
        let d = calcDamage attacker defender atk  in
        add_update(NegativeEffect((atk.name^" Hit!"), (invert_color color), d)); d
      else (add_update(NegativeEffect((atk.name^" Missed!"), (invert_color color), 0)); 0) in
    let effect = 
      match atk.effect with
        | (Nada, _) -> false
        | (_, p) -> random p in
    (* If the effect happens and the mon can have another status, return 
       Some S, else None *)
    let newStatus s = 
      match defender.status with
        | [] -> if effect then Some s else None
        | [Confused] -> if effect && s != Confused then Some s else None
        | lst -> None(*if effect && s = Confused  then Some Confused else None*)
        (* Need to match s with confused so you can be paralzed and confused !*)
    in
    let decrePP mon atk =
      let newAtk = 
      {
        name = atk.name;
        element = atk.element;
        max_pp = atk.max_pp;
        pp_remaining = atk.pp_remaining - 1;
        power = atk.power;
        accuracy = atk.accuracy;
        crit_chance = atk.crit_chance;
        effect = atk.effect
      } in 
        (match atkNum with
          | 1 -> {
             species = mon.species; 
             curr_hp = mon.curr_hp;
             max_hp = mon.max_hp;
             first_type = mon.first_type;
             second_type = mon.second_type;
             first_attack = newAtk;
             second_attack = mon.second_attack;
             third_attack = mon.third_attack;
             fourth_attack = mon.fourth_attack;
             attack = mon.attack;
             spl_attack = mon.spl_attack;
             defense = mon.defense;
             spl_defense = mon.spl_defense;
             speed = mon.speed;
             status = mon.status; 
             mods = mon.mods }
          | 2 -> {
             species = mon.species; 
             curr_hp = mon.curr_hp;
             max_hp = mon.max_hp;
             first_type = mon.first_type;
             second_type = mon.second_type;
             first_attack = mon.first_attack;
             second_attack = newAtk;
             third_attack = mon.third_attack;
             fourth_attack = mon.fourth_attack;
             attack = mon.attack;
             spl_attack = mon.spl_attack;
             defense = mon.defense;
             spl_defense = mon.spl_defense;
             speed = mon.speed;
             status = mon.status; 
             mods = mon.mods }
          | 3 -> {
             species = mon.species; 
             curr_hp = mon.curr_hp;
             max_hp = mon.max_hp;
             first_type = mon.first_type;
             second_type = mon.second_type;
             first_attack = mon.first_attack;
             second_attack = mon.second_attack;
             third_attack = newAtk;
             fourth_attack = mon.fourth_attack;
             attack = mon.attack;
             spl_attack = mon.spl_attack;
             defense = mon.defense;
             spl_defense = mon.spl_defense;
             speed = mon.speed;
             status = mon.status; 
             mods = mon.mods }
           | 4 -> {
             species = mon.species; 
             curr_hp = mon.curr_hp;
             max_hp = mon.max_hp;
             first_type = mon.first_type;
             second_type = mon.second_type;
             first_attack = mon.first_attack;
             second_attack = mon.second_attack;
             third_attack = mon.fourth_attack;
             fourth_attack = newAtk;
             attack = mon.attack;
             spl_attack = mon.spl_attack;
             defense = mon.defense;
             spl_defense = mon.spl_defense;
             speed = mon.speed;
             status = mon.status; 
             mods = mon.mods }
          | _ -> failwith "Invalid arg to decrePP") in
    let (attacker, hitMyself) = if List.mem Confused attacker.status then
      if random cSNAP_OUT_OF_CONFUSION then
        (add_update(Message(attacker.species ^ " snapped out of confusion"));
         let (attacker, hitMyself) = ({
             species = attacker.species; 
             curr_hp = attacker.curr_hp;
             max_hp = attacker.max_hp;
             first_type = attacker.first_type;
             second_type = attacker.second_type;
             first_attack = attacker.first_attack;
             second_attack = attacker.second_attack;
             third_attack = attacker.third_attack;
             fourth_attack = attacker.fourth_attack;
             attack = attacker.attack;
             spl_attack = attacker.spl_attack;
             defense = attacker.defense;
             spl_defense = attacker.spl_defense;
             speed = attacker.speed;
             status = lstRemove attacker.status Confused; 
             mods = attacker.mods }, false) in
        add_update(SetStatusEffects(attacker.species, attacker.status));
        (attacker, hitMyself)) 
        else if random cSELF_ATTACK_CHANCE then (attacker, true)
          else (attacker, false)
      else attacker, false in

    let (attacker, attackSleep) =
      if List.mem Asleep attacker.status then 
        if random cWAKE_UP_CHANCE then
        (add_update(Message(attacker.species ^ " woke up"));
        let (attacker, attackSleep) = ({
             species = attacker.species; 
             curr_hp = attacker.curr_hp;
             max_hp = attacker.max_hp;
             first_type = attacker.first_type;
             second_type = attacker.second_type;
             first_attack = attacker.first_attack;
             second_attack = attacker.second_attack;
             third_attack = attacker.third_attack;
             fourth_attack = attacker.fourth_attack;
             attack = attacker.attack;
             spl_attack = attacker.spl_attack;
             defense = attacker.defense;
             spl_defense = attacker.spl_defense;
             speed = attacker.speed;
             status = lstRemove attacker.status Asleep; 
             mods = attacker.mods }, true) in
        add_update(SetStatusEffects(attacker.species, attacker.status));
        (attacker, attackSleep)) 
     else (attacker, false)
      else (attacker, true) in
    let attackParalyzed = if List.mem Paralyzed attacker.status then
      random (100-cPARALYSIS_CHANCE) else true in
    let (attacker, attackFrozen) =
     if List.mem Frozen attacker.status then 
        if random cDEFROST_CHANCE then(
        add_update(Message(attacker.species ^ " defrosted!"));
        let (attacker, attackFrozen) = 
          ({
             species = attacker.species; 
             curr_hp = attacker.curr_hp;
             max_hp = attacker.max_hp;
             first_type = attacker.first_type;
             second_type = attacker.second_type;
             first_attack = attacker.first_attack;
             second_attack = attacker.second_attack;
             third_attack = attacker.third_attack;
             fourth_attack = attacker.fourth_attack;
             attack = attacker.attack;
             spl_attack = attacker.spl_attack;
             defense = attacker.defense;
             spl_defense = attacker.spl_defense;
             speed = attacker.speed;
             status = lstRemove attacker.status Frozen; 
             mods = attacker.mods }, false) in
        add_update(SetStatusEffects(attacker.species, attacker.status));
        (attacker, attackFrozen) )
        else (attacker, false)
      else (attacker, true) in 
    let (newDefender, newAttacker) =
      (if not attackSleep then
        add_update(Message(attacker.species ^ " is fast asleep")) else ());
      (if not attackParalyzed then
        add_update(Message(attacker.species ^ " is fully paralyzed")) else ());
      (if not attackFrozen then
        add_update(Message(attacker.species ^ " is completely frozen")) 
        else ());
      if atk.pp_remaining > 0 && attackSleep && attackParalyzed && attackFrozen
      then 
      if hitMyself then (attacked := true;
      (defender, (changeHP attacker ((calcDamage attacker attacker selfAttack)))))
      else 
      let newAttacker = decrePP attacker atk in
      attacked := true;
      let inter =    
      if damage < defender.curr_hp 
      then 
        changeHP defender (defender.curr_hp - damage)       
      else changeHP defender 0 in
      
      match fst(atk.effect) with
        | Nada -> inter, newAttacker
        | Poisons -> 
          let withStatus = addStatus inter (newStatus Poisoned) in
          add_update(SetStatusEffects(defender.species, withStatus.status));
          add_update(NegativeEffect("Poisoned!", (invert_color color), 0));
          withStatus, newAttacker 
        | Confuses -> 
          let withStatus = addStatus inter (newStatus Confused) in
          add_update(SetStatusEffects(defender.species, withStatus.status));
          add_update(NegativeEffect("Confused!", (invert_color color), 0));
          withStatus, newAttacker  
        | Sleeps -> 
          let withStatus = addStatus inter (newStatus Asleep) in
          add_update(SetStatusEffects(defender.species, withStatus.status));
          add_update(NegativeEffect("Asleep!", (invert_color color), 0));
          withStatus, newAttacker  
        | Paralyzes -> 
          let withStatus = addStatus inter (newStatus Paralyzed) in
          add_update(SetStatusEffects(defender.species, withStatus.status));
          add_update(NegativeEffect("Paralyzed!", (invert_color color), 0));
          withStatus, newAttacker  
        | Freezes -> 
          let withStatus = addStatus inter (newStatus Frozen) in
          add_update(SetStatusEffects(defender.species, withStatus.status));
          add_update(NegativeEffect("Frozen!", (invert_color color), 0));
          withStatus, newAttacker  
        | SelfAttackUp1 -> 
          if effect then inter, (addModifier newAttacker "Attack" 1 color) 
          else inter, newAttacker
        | SelfDefenseUp1 ->
          if effect then inter, (addModifier newAttacker "Defense" 1 color)
          else inter, newAttacker
        | SelfSpeedUp1 ->
          if effect then inter, (addModifier newAttacker "Speed" 1 color)
          else inter, newAttacker
        | SelfAccuracyUp1 ->
          if effect then inter, (addModifier newAttacker "Accuracy" 1 color)
          else inter, newAttacker
        | OpponentAttackDown1 ->
          if effect then (addModifier inter "Attack" (-1) color), newAttacker
          else inter, newAttacker
        | OpponentDefenseDown1 ->
          if effect then (addModifier inter "Defense" (-1) color), newAttacker
          else inter, newAttacker
        | OpponentSpeedDown1 ->
          if effect then (addModifier inter "Speed" (-1) color), newAttacker
          else inter, newAttacker
        | OpponentAccuracyDown1 ->
          if effect then (addModifier inter "Accuracy" (-1) color), newAttacker
          else inter, newAttacker 
      else (attacked := false; (defender, attacker)) in
      (*Update the state with new attacker and defender *)
      (* (((lred, ired), (lblue, iblue)), p, ats, sp) *)
      if attacker.curr_hp > 0 then
      match color with
        | Red -> (((newAttacker::(List.tl lred), ired), 
          ((newDefender::(List.tl lblue)), iblue)), p, ats, sp)
        | Blue -> ((((newDefender::(List.tl lred)), ired), 
          ((newAttacker::(List.tl lblue)), iblue)), p, ats, sp)
      else(attacked := false;
        (((lred, ired), (lblue, iblue)), p, ats, sp))
    
     
(*  
  end;;
 
module State :
  sig
    type game_phase = Draft | Inventory | Battle
    type state = game_status_data * game_phase * attack_set * steam_pool
    val getPhase : state -> game_phase
    val getStatusData : state -> game_status_data
    val getSteammonList : state -> color -> steammon list
    val getInventory : state -> color -> inventory
    val getActive : state -> color -> steammon
    val setActive : state -> color -> string -> state
    (* Add a steammon to party in draft phase *)  
    val draftMon : state -> color -> string -> state
    (* Add a list of items to the inventory *)
    val addItems : state -> color -> inventory -> state
    (* Use one item on a target steammon *)
    val useItem : state -> color -> item -> steammon -> state
    (* Active mon of color attacks other color's active mon *)
    val applyAttack : state -> color -> attack -> state 
  end
*)
