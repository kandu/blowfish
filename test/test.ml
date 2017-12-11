open OUnit2

let round name=
  Random.self_init ();

  let key= Random.float 1e10 |> string_of_float in

  let encryptor= Blowfish.encryptor key
  and decryptor= Blowfish.decryptor key in

  let test data= assert_equal data (data |> encryptor |> decryptor) in

  let cne ctxt= assert_bool "inequation"
      ("hi" |> encryptor |> decryptor <> "there")
  and c00 ctxt= test ""
  and c15 ctxt= test "radio free zerg"
  and c16 ctxt= test "black sheep wall"
  and c18 ctxt= test "power overwhelming"
  and c35 ctxt= test "show me the money show me the money" in

  name >:::
    [
      "cne" >:: cne;
      "c00" >:: c00;
      "c15" >:: c15;
      "c16" >:: c16;
      "c18" >:: c18;
      "c35" >:: c35;
    ]



let suite=
  "blowfish" >:::[
    round "round1";
    round "round2";
    round "round3";
    round "round4";
    round "round5";
    round "round6";
    round "round7";
    round "round8";
    round "round9";
    round "round10";
  ]

let ()= run_test_tt_main suite

