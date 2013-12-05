local open Layout infix ^^ ++ \ & \\ && in
  fun rute Stop = txt "Stop"
    | rute (Frem (n, r)) = txt "Frem (" & int n && txt "," & rute r && txt ")"
    | rute (Drej (n, r)) = txt "Drej (" & int n && txt "," & rute r && txt ")"

  fun trae pp (K (e, ts)) =
    txt "K (" && pp e && txt ", [" &
    (indent 0 o vsep o map (trae pp)) ts && txt "] )"

  fun func _ = txt "<funktion>"
  fun listetabel _ _ = txt "<ListeTabel.tabel>"
  fun funtabel _ _ = txt "<FunTabel.tabel>"
end
