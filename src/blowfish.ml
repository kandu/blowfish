open Cryptokit

let blockSize= 16

let split_bytes size ?pad b=
  let len= Bytes.length b in
  let m= len mod size in
  let t= len - m in
  let rec split pos=
    if pos < t then
      Bytes.sub b pos size :: split (pos + size)
    else if m > 0 then
      let block= Bytes.create size in
      (match pad with
      | Some pad-> pad block m (size - m)
      | None-> Bytes.fill block 0 (size - m) '\x00');
      Bytes.blit b pos block 0 m;
      [block]
    else
      []
  in
  let pad= if len = 0 || m = 0 then 0 else size - m in
  (split 0, pad)

let encryptor key b=
  let en= Cipher.blowfish key Cipher.Encrypt |> transform_string in
  let bs, pad= split_bytes blockSize
    ~pad:(fun b pos len-> Bytes.fill b pos len (char_of_int len))
    (Bytes.of_string b)
  in
  let bs= 
    if pad = 0 then
      List.append bs [(Bytes.init blockSize (fun i-> char_of_int 0))]
    else
      bs
  in
  let bs= bs |> List.map Bytes.to_string in
  String.concat "" (List.map en bs)


let decryptor key b=
  let de= Cipher.blowfish key Cipher.Decrypt |> transform_string in
  let bs, _= split_bytes blockSize (Bytes.of_string b) in
  let bs= bs |> List.map Bytes.to_string in
  let data= String.concat "" (List.map de bs) in
  let len= String.length data in
  let pad= int_of_char (String.get data (len - 1)) in
  let pad= if pad = 0 then blockSize else pad in
  String.sub data 0 (len - pad)

