type votesType = (string, float) Hashtbl.t
                                  
type coalitionType = string list

let keys tbl =
  let l = Hashtbl.fold (fun a b c -> a::c) tbl []
  in List.sort (fun a b -> compare (Hashtbl.find tbl b)
                                   (Hashtbl.find tbl a)) l
            
let list_all_coalitions votes =
  let rec aux acc cur rest = function
      _ when rest < 0. -> cur::acc
    | []               -> acc
    | h::t             -> let acc = aux acc (h::cur)
                                      (rest -. Hashtbl.find votes h) t in
                          aux acc cur rest t
  in
  let parties = keys votes in
  let majority = ((Hashtbl.fold (fun a b c -> b +. c) votes 0.) /. 2.) in
  aux [] [] majority parties
                                      
let list_coalitions votes cofilter =
  List.filter cofilter (list_all_coalitions votes)

let german_filter co =
  not (List.mem "AfD" co)
  && not (List.mem "Sonstige" co)
  && not (List.mem "Linke" co && ( (List.mem "CDU" co) || (List.mem "FDP" co) || (List.mem "CSU" co)))

let tabubruch_filter co =
  not (List.mem "AfD" co && ( (List.mem "SPD" co) || (List.mem "Grüne" co) || (List.mem "Linke" co)))
  && not (List.mem "Sonstige" co)
  && not (List.mem "Linke" co && ( (List.mem "CDU" co) || (List.mem "FDP" co) || (List.mem "CSU" co)))

(*Bayern, Landtagswahlen 14.10.2018*)

let _ =
  let bayern = Hashtbl.create 7 in
  Hashtbl.add bayern "CSU" 85.;
  Hashtbl.add bayern "SPD" 22.;
  Hashtbl.add bayern "Grüne" 38.;
  Hashtbl.add bayern "FDP" 11.;
  Hashtbl.add bayern "Linke" 0.;
  Hashtbl.add bayern "FW" 27.;
  Hashtbl.add bayern "AfD" 22.;
  Hashtbl.add bayern "Sonstige" 0.;
  list_coalitions bayern german_filter

(*Hessen, Umfrage Forschungsgruppe Wahlen 3.10.2018*)
                  
let _ =
  let hessen = Hashtbl.create 7 in
  Hashtbl.add hessen "CDU" 29.;
  Hashtbl.add hessen "SPD" 23.;
  Hashtbl.add hessen "Grüne" 18.;
  Hashtbl.add hessen "FDP" 6.;
  Hashtbl.add hessen "Linke" 8.;
  Hashtbl.add hessen "AfD" 13.;
  Hashtbl.add hessen "Sonstige" 3.;
  list_coalitions hessen tabubruch_filter

(*Sachsen, Umfrage uniQma 7.9.2018*)
                  
let _ =
  let sachsen = Hashtbl.create 7 in
  Hashtbl.add sachsen "CDU" 28.9;
  Hashtbl.add sachsen "SPD" 11.4;
  Hashtbl.add sachsen "Grüne" 6.8;
  Hashtbl.add sachsen "FDP" 5.6;
  Hashtbl.add sachsen "Linke" 18.6;
  Hashtbl.add sachsen "AfD" 23.9;
  Hashtbl.add sachsen "Sonstige" 4.5;
  list_coalitions sachsen german_filter

(*Meckpo, Umfrage Forsa 30.6.2018*)
                  
let _ =
  let meckpo = Hashtbl.create 7 in
  Hashtbl.add meckpo "CDU" 18.;
  Hashtbl.add meckpo "SPD" 25.;
  Hashtbl.add meckpo "Grüne" 8.;
  Hashtbl.add meckpo "FDP" 4.;
  Hashtbl.add meckpo "Linke" 16.;
  Hashtbl.add meckpo "AfD" 22.;
  Hashtbl.add meckpo "Sonstige" 7.;
  list_coalitions meckpo tabubruch_filter


(*Brandenburg, Umfrage Infratest dimap 19.9.2018*)
                  
let _ =
  let brandenburg = Hashtbl.create 7 in
  Hashtbl.add brandenburg "CDU" 21.;
  Hashtbl.add brandenburg "SPD" 23.;
  Hashtbl.add brandenburg "Grüne" 7.;
  Hashtbl.add brandenburg "FDP" 5.;
  Hashtbl.add brandenburg "Linke" 17.;
  Hashtbl.add brandenburg "AfD" 23.;
  Hashtbl.add brandenburg "Sonstige" 4.;
  list_coalitions brandenburg tabubruch_filter

(*Thüringen, Umfrage Infratest dimap 28.8.2018*)
                  
let _ =
  let thueringen = Hashtbl.create 7 in
  Hashtbl.add thueringen "CDU" 30.;
  Hashtbl.add thueringen "SPD" 10.;
  Hashtbl.add thueringen "Grüne" 6.;
  Hashtbl.add thueringen "FDP" 5.;
  Hashtbl.add thueringen "Linke" 22.;
  Hashtbl.add thueringen "AfD" 23.;
  Hashtbl.add thueringen "Sonstige" 4.;
  list_coalitions thueringen tabubruch_filter
    
    
