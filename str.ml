let get_words (s:string) : string list =
  let is_valid c =
    (c <> ' ' &&(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
  in
  let buffer = Buffer.create 0 in 
  let rec extract_words acc first_word = function
    | [] -> if first_word <> "" then List.rev (first_word :: acc) else List.rev acc
    | hd :: tl ->
        if is_valid hd then begin
	        Buffer.add_char buffer hd;
          extract_words acc (Buffer.contents buffer) tl    
        end else begin
          if Buffer.length buffer <> 0 then
          Buffer.clear buffer;
          extract_words (first_word :: acc) "" tl  
      end
  in
  let chars = List.of_seq (String.to_seq s) in
  extract_words [] "" chars;;