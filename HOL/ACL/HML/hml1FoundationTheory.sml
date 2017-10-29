structure hml1FoundationTheory :> hml1FoundationTheory =
struct
  val _ = if !Globals.print_thy_loads then TextIO.print "Loading hml1FoundationTheory ... " else ()
  open Type Term Thm
  infixr -->

  fun C s t ty = mk_thy_const{Name=s,Thy=t,Ty=ty}
  fun T s t A = mk_thy_type{Tyop=s, Thy=t,Args=A}
  fun V s q = mk_var(s,q)
  val U     = mk_vartype
  (* Parents and ML dependencies *)
  local open fixedPointTheory indexedListsTheory patternMatchesTheory
  in end;
  val _ = Theory.link_parents
          ("hml1Foundation",
          Arbnum.fromString "1503150832",
          Arbnum.fromString "62750")
          [("fixedPoint",
           Arbnum.fromString "1503148814",
           Arbnum.fromString "991596"),
           ("indexedLists",
           Arbnum.fromString "1503148856",
           Arbnum.fromString "404777"),
           ("patternMatches",
           Arbnum.fromString "1503148884",
           Arbnum.fromString "20925")];
  val _ = Theory.incorporate_types "hml1Foundation" [("hmlForm", 2)];

  val idvector = 
    let fun ID(thy,oth) = {Thy = thy, Other = oth}
    in Vector.fromList
  [ID("hml1Foundation", "hmlForm"), ID("min", "fun"), ID("min", "bool"),
   ID("pair", "prod"), ID("num", "num"), ID("ind_type", "recspace"),
   ID("bool", "!"), ID("arithmetic", "+"), ID("pair", ","),
   ID("bool", "/\\"), ID("num", "0"), ID("min", "="), ID("min", "==>"),
   ID("bool", "?"), ID("bool", "ARB"), ID("arithmetic", "BIT1"),
   ID("ind_type", "BOTTOM"), ID("hml1Foundation", "Box"),
   ID("bool", "COND"), ID("ind_type", "CONSTR"), ID("bool", "DATATYPE"),
   ID("hml1Foundation", "Dia"), ID("bool", "F"), ID("ind_type", "FCONS"),
   ID("pred_set", "GSPEC"), ID("hml1Foundation", "HMUpdate"),
   ID("hml1Foundation", "HMfn"), ID("bool", "IN"), ID("pred_set", "INTER"),
   ID("arithmetic", "NUMERAL"), ID("pred_set", "SUBSET"), ID("num", "SUC"),
   ID("bool", "T"), ID("bool", "TYPE_DEFINITION"), ID("pred_set", "UNION"),
   ID("pred_set", "UNIV"), ID("arithmetic", "ZERO"), ID("bool", "\\/"),
   ID("hml1Foundation", "andh"), ID("hml1Foundation", "extends"),
   ID("hml1Foundation", "ff"), ID("fixedPoint", "gfp"),
   ID("hml1Foundation", "hmGFP"), ID("hml1Foundation", "hmLFP"),
   ID("hml1Foundation", "hmlForm_CASE"),
   ID("hml1Foundation", "hmlForm_size"), ID("hml1Foundation", "hmsat"),
   ID("fixedPoint", "lfp"), ID("fixedPoint", "monotone"),
   ID("hml1Foundation", "orh"), ID("hml1Foundation", "proph"),
   ID("hml1Foundation", "satFun"), ID("hml1Foundation", "tt"),
   ID("bool", "~")]
  end;
  local open SharingTables
  in
  val tyvector = build_type_vector idvector
  [TYV "'propvar", TYV "'action", TYOP [0, 1, 0], TYOP [2],
   TYV "'configuration", TYOP [1, 4, 3], TYOP [1, 5, 5], TYOP [1, 2, 6],
   TYOP [1, 0, 5], TYOP [1, 8, 7], TYOP [1, 0, 9], TYOP [1, 4, 5],
   TYOP [1, 1, 11], TYOP [1, 12, 10], TYOP [1, 0, 2], TYOP [1, 2, 2],
   TYOP [1, 2, 15], TYOP [1, 2, 3], TYOP [3, 12, 8], TYOP [3, 4, 18],
   TYOP [1, 19, 17], TYOP [4], TYOP [1, 2, 21], TYOP [1, 0, 21],
   TYOP [1, 23, 22], TYOP [1, 1, 21], TYOP [1, 25, 24], TYV "'a",
   TYOP [1, 2, 27], TYOP [1, 1, 3], TYOP [1, 29, 28], TYOP [1, 30, 27],
   TYOP [1, 30, 31], TYOP [1, 2, 28], TYOP [1, 33, 32], TYOP [1, 33, 34],
   TYOP [1, 0, 27], TYOP [1, 36, 35], TYOP [1, 27, 37], TYOP [1, 27, 38],
   TYOP [1, 2, 39], TYOP [1, 2, 5], TYOP [1, 8, 41], TYOP [1, 0, 42],
   TYOP [1, 12, 43], TYOP [1, 8, 3], TYOP [1, 8, 45], TYOP [1, 8, 5],
   TYOP [1, 5, 47], TYOP [1, 2, 48], TYOP [1, 12, 49], TYOP [1, 5, 8],
   TYOP [1, 8, 51], TYOP [1, 0, 52], TYOP [1, 29, 15], TYOP [3, 0, 29],
   TYOP [5, 55], TYOP [1, 56, 3], TYOP [1, 27, 3], TYV "'b",
   TYOP [1, 59, 3], TYOP [1, 59, 60], TYOP [1, 27, 61], TYOP [1, 27, 58],
   TYOP [1, 59, 63], TYV "'c", TYOP [1, 65, 58], TYOP [1, 65, 60],
   TYOP [1, 27, 27], TYOP [1, 27, 68], TYOP [1, 2, 69], TYOP [1, 2, 70],
   TYOP [1, 2, 68], TYOP [1, 29, 72], TYOP [0, 27, 65], TYOP [0, 59, 65],
   TYOP [1, 54, 3], TYOP [1, 54, 76], TYOP [1, 16, 77], TYOP [1, 16, 78],
   TYOP [1, 14, 79], TYOP [1, 2, 80], TYOP [1, 2, 81], TYOP [1, 2, 56],
   TYOP [1, 58, 3], TYOP [1, 29, 3], TYOP [1, 65, 3], TYOP [1, 86, 3],
   TYOP [1, 5, 3], TYOP [1, 0, 3], TYOP [1, 89, 3], TYOP [1, 62, 3],
   TYOP [1, 91, 3], TYOP [1, 85, 3], TYOP [1, 12, 3], TYOP [1, 94, 3],
   TYOP [1, 25, 3], TYOP [1, 96, 3], TYOP [1, 60, 3], TYOP [1, 98, 3],
   TYOP [1, 67, 3], TYOP [1, 100, 3], TYOP [1, 88, 3], TYOP [1, 36, 3],
   TYOP [1, 103, 3], TYOP [1, 45, 3], TYOP [1, 23, 3], TYOP [1, 106, 3],
   TYOP [1, 30, 3], TYOP [1, 108, 3], TYOP [1, 73, 3], TYOP [1, 110, 3],
   TYOP [1, 17, 3], TYOP [1, 112, 3], TYOP [1, 33, 3], TYOP [1, 114, 3],
   TYOP [1, 71, 3], TYOP [1, 116, 3], TYOP [1, 20, 3], TYOP [1, 118, 3],
   TYOP [1, 57, 3], TYOP [1, 120, 3], TYOP [1, 74, 3], TYOP [1, 122, 3],
   TYOP [1, 21, 21], TYOP [1, 21, 124], TYOP [3, 27, 3], TYOP [1, 3, 126],
   TYOP [1, 27, 127], TYOP [3, 4, 3], TYOP [1, 3, 129], TYOP [1, 4, 130],
   TYOP [1, 18, 19], TYOP [1, 4, 132], TYOP [1, 29, 55], TYOP [1, 0, 134],
   TYOP [1, 8, 18], TYOP [1, 12, 136], TYOP [1, 3, 3], TYOP [1, 3, 138],
   TYOP [1, 0, 89], TYOP [1, 58, 84], TYOP [1, 29, 85], TYOP [1, 60, 98],
   TYOP [1, 5, 88], TYOP [1, 2, 17], TYOP [1, 21, 3], TYOP [1, 21, 146],
   TYOP [1, 56, 57], TYOP [1, 28, 3], TYOP [1, 149, 3], TYOP [1, 83, 3],
   TYOP [1, 151, 3], TYOP [1, 5, 6], TYOP [1, 3, 153], TYOP [1, 21, 56],
   TYOP [1, 155, 56], TYOP [1, 55, 156], TYOP [1, 21, 157],
   TYOP [1, 155, 155], TYOP [1, 56, 159], TYOP [1, 27, 126],
   TYOP [1, 161, 58], TYOP [1, 4, 129], TYOP [1, 163, 5], TYOP [1, 27, 84],
   TYOP [1, 1, 85], TYOP [1, 4, 88], TYOP [1, 58, 58], TYOP [1, 58, 168],
   TYOP [1, 57, 151], TYOP [1, 6, 5], TYOP [1, 74, 60], TYOP [1, 67, 172],
   TYOP [1, 65, 173], TYOP [1, 62, 174], TYOP [1, 168, 3],
   TYOP [1, 60, 60], TYOP [1, 74, 177], TYOP [1, 67, 178],
   TYOP [1, 65, 179], TYOP [1, 62, 180], TYOP [1, 75, 168],
   TYOP [1, 66, 182], TYOP [1, 65, 183], TYOP [1, 64, 184]]
  end
  val _ = Theory.incorporate_consts "hml1Foundation" tyvector
     [("tt", 2), ("satFun", 13), ("proph", 14), ("orh", 16), ("hmsat", 20),
      ("hmlForm_size", 26), ("hmlForm_CASE", 40), ("hmLFP", 44),
      ("hmGFP", 44), ("ff", 2), ("extends", 46), ("andh", 16),
      ("HMfn", 50), ("HMUpdate", 53), ("Dia", 54), ("Box", 54)];

  local open SharingTables
  in
  val tmvector = build_term_vector idvector tyvector
  [TMV("'hmlForm'", 57), TMV("A", 58), TMV("A'", 58), TMV("Actions", 29),
   TMV("B", 58), TMV("B'", 58), TMV("E", 4), TMV("E", 5), TMV("E'", 4),
   TMV("E1", 5), TMV("E2", 5), TMV("F", 5), TMV("M", 2), TMV("M'", 2),
   TMV("P", 58), TMV("P", 17), TMV("P", 20), TMV("Q", 58),
   TMV("Trans", 62), TMV("Trans", 12), TMV("Trans", 64), TMV("V", 66),
   TMV("V", 67), TMV("V", 8), TMV("V'", 8), TMV("X", 60), TMV("Y", 0),
   TMV("Z", 65), TMV("Z", 0), TMV("a", 1), TMV("a", 0), TMV("a'", 0),
   TMV("a0", 29), TMV("a0", 2), TMV("a0", 56), TMV("a0'", 29),
   TMV("a0'", 2), TMV("a0'", 56), TMV("a1", 2), TMV("a1", 56),
   TMV("a1'", 2), TMV("f", 29), TMV("f", 25), TMV("f", 36), TMV("f", 2),
   TMV("f'", 36), TMV("f0", 27), TMV("f1", 27), TMV("f1", 23),
   TMV("f1", 33), TMV("f1", 2), TMV("f1'", 33), TMV("f2", 36),
   TMV("f2", 33), TMV("f2", 2), TMV("f2'", 33), TMV("f3", 30),
   TMV("f3", 71), TMV("f3'", 30), TMV("f4", 30), TMV("f4", 71),
   TMV("f4'", 30), TMV("f5", 73), TMV("f6", 73), TMV("fn", 28),
   TMV("form", 74), TMV("form", 2), TMV("form", 75), TMV("form'", 2),
   TMV("h", 2), TMV("h0", 2), TMV("hh", 2), TMV("hmlForm", 82),
   TMV("n", 21), TMV("p", 0), TMV("rep", 83), TMV("s", 27), TMV("s", 4),
   TMV("v", 27), TMV("v", 4), TMV("v'", 27), TMV("v1", 27), TMV("v1", 12),
   TMV("v1'", 27), TMV("v2", 8), TMV("v3", 2), TMV("x", 27), TMV("x", 4),
   TMC(6, 84), TMC(6, 85), TMC(6, 87), TMC(6, 88), TMC(6, 90), TMC(6, 92),
   TMC(6, 93), TMC(6, 95), TMC(6, 97), TMC(6, 99), TMC(6, 101),
   TMC(6, 102), TMC(6, 104), TMC(6, 105), TMC(6, 107), TMC(6, 109),
   TMC(6, 111), TMC(6, 113), TMC(6, 115), TMC(6, 117), TMC(6, 119),
   TMC(6, 121), TMC(6, 123), TMC(6, 112), TMC(6, 120), TMC(7, 125),
   TMC(8, 128), TMC(8, 131), TMC(8, 133), TMC(8, 135), TMC(8, 137),
   TMC(9, 139), TMC(10, 21), TMC(11, 63), TMC(11, 140), TMC(11, 139),
   TMC(11, 141), TMC(11, 142), TMC(11, 143), TMC(11, 144), TMC(11, 145),
   TMC(11, 147), TMC(11, 148), TMC(12, 139), TMC(13, 85), TMC(13, 88),
   TMC(13, 90), TMC(13, 93), TMC(13, 150), TMC(13, 152), TMC(13, 112),
   TMC(13, 120), TMC(14, 0), TMC(14, 29), TMC(15, 124), TMC(16, 56),
   TMC(17, 54), TMC(18, 154), TMC(19, 158), TMC(20, 138), TMC(21, 54),
   TMC(22, 3), TMC(23, 160), TMC(24, 162), TMC(24, 164), TMC(25, 53),
   TMC(26, 50), TMC(27, 165), TMC(27, 166), TMC(27, 167), TMC(28, 169),
   TMC(28, 153), TMC(29, 124), TMC(30, 141), TMC(30, 143), TMC(30, 144),
   TMC(31, 124), TMC(32, 3), TMC(33, 170), TMC(34, 169), TMC(34, 153),
   TMC(35, 5), TMC(36, 21), TMC(37, 139), TMC(38, 16), TMC(39, 46),
   TMC(40, 2), TMC(41, 171), TMC(42, 175), TMC(42, 44), TMC(43, 175),
   TMC(43, 44), TMC(44, 40), TMC(45, 26), TMC(46, 20), TMC(47, 171),
   TMC(48, 176), TMC(49, 16), TMC(50, 14), TMC(51, 181), TMC(51, 13),
   TMC(51, 185), TMC(52, 2), TMC(53, 138)]
  end
  structure ThmBind = struct
    val DT = Thm.disk_thm
    val read = Term.read_raw tmvector
  end
  fun op hmlForm_TY_DEF x = x
    val op hmlForm_TY_DEF =
    ThmBind.DT(((("hml1Foundation",0),
                [("bool",[14,25,26,52,131,132,137])]),["DISK_THM"]),
               [ThmBind.read"%137%75%166%37%109%0%131%112%37%131%171%130$0@%146%120@%117%140@%141@2%73%143|@3%171%130$0@%146%164%120@2%117%140@%141@2%73%143|@3%171%134%30%130$1@%30%146%164%164%120@3%117$0@%141@2%73%143|@|$0@2|@2%171%139%34%139%39%119%130$2@%34%39%146%164%164%164%120@4%117%140@%141@2%150$1@%150$0@%73%143|@3||$1@$0@3%119$3$1@2$3$0@3|@|@2%171%139%34%139%39%119%130$2@%34%39%146%164%164%164%164%120@5%117%140@%141@2%150$1@%150$0@%73%143|@3||$1@$0@3%119$3$1@2$3$0@3|@|@2%171%135%32%139%39%119%130$2@%32%39%146%164%164%164%164%164%120@6%117%140@$1@2%150$0@%73%143|@2||$1@$0@3$3$0@2|@|@2%135%32%139%39%119%130$2@%32%39%146%164%164%164%164%164%164%120@7%117%140@$1@2%150$0@%73%143|@2||$1@$0@3$3$0@2|@|@8$1$0@2|@2$0$1@2|@|@$0@|@"])
  fun op hmlForm_case_def x = x
    val op hmlForm_case_def =
    ThmBind.DT(((("hml1Foundation",16),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hml1Foundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),
                 ("ind_type",[33,34]),("pair",[8,9])]),["DISK_THM"]),
               [ThmBind.read"%119%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%190@$6@$5@$4@$3@$2@$1@$0@2$6@|@|@|@|@|@|@|@2%119%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%174@$6@$5@$4@$3@$2@$1@$0@2$5@|@|@|@|@|@|@|@2%119%92%30%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%186$7@2$6@$5@$4@$3@$2@$1@$0@2$4$7@2|@|@|@|@|@|@|@|@2%119%111%33%111%38%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%172$8@$7@2$6@$5@$4@$3@$2@$1@$0@2$3$8@$7@2|@|@|@|@|@|@|@|@|@2%119%111%33%111%38%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%185$8@$7@2$6@$5@$4@$3@$2@$1@$0@2$2$8@$7@2|@|@|@|@|@|@|@|@|@2%119%94%32%111%38%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%144$8@$7@2$6@$5@$4@$3@$2@$1@$0@2$1$8@$7@2|@|@|@|@|@|@|@|@|@2%94%32%111%38%88%78%88%81%100%43%106%49%106%53%103%56%103%59%121%180%148$8@$7@2$6@$5@$4@$3@$2@$1@$0@2$0$8@$7@2|@|@|@|@|@|@|@|@|@7"])
  fun op hmlForm_size_def x = x
    val op hmlForm_size_def =
    ThmBind.DT(((("hml1Foundation",17),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hml1Foundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),
                 ("ind_type",[33,34]),("pair",[8,9])]),["DISK_THM"]),
               [ThmBind.read"%119%96%42%102%48%129%181$1@$0@%190@2%120@|@|@2%119%96%42%102%48%129%181$1@$0@%174@2%120@|@|@2%119%96%42%102%48%92%30%129%181$2@$1@%186$0@3%113%160%142%170@3$1$0@3|@|@|@2%119%96%42%102%48%111%33%111%38%129%181$3@$2@%172$1@$0@3%113%160%142%170@3%113%181$3@$2@$1@2%181$3@$2@$0@4|@|@|@|@2%119%96%42%102%48%111%33%111%38%129%181$3@$2@%185$1@$0@3%113%160%142%170@3%113%181$3@$2@$1@2%181$3@$2@$0@4|@|@|@|@2%119%96%42%102%48%94%32%111%38%129%181$3@$2@%144$1@$0@3%113%160%142%170@3%181$3@$2@$0@3|@|@|@|@2%96%42%102%48%94%32%111%38%129%181$3@$2@%148$1@$0@3%113%160%142%170@3%181$3@$2@$0@3|@|@|@|@7"])
  fun op HMfn_def x = x
    val op HMfn_def =
    ThmBind.DT(((("hml1Foundation",29),[]),[]),
               [ThmBind.read"%95%19%111%44%99%7%101%23%127%154$3@$2@$1@$0@2%152%77%115$0@%119%157$0@$2@2%182%116$0@%118$4@$1@3$3@3|@2|@|@|@|@"])
  fun op HMUpdate_def x = x
    val op HMUpdate_def =
    ThmBind.DT(((("hml1Foundation",30),[]),[]),
               [ThmBind.read"%92%28%101%23%99%7%92%26%127%153$3@$2@$1@$0@2%145%122$0@$3@2$1@$2$0@3|@|@|@|@"])
  fun op extends_def x = x
    val op extends_def =
    ThmBind.DT(((("hml1Foundation",31),[]),[]),
               [ThmBind.read"%101%23%101%24%123%173$1@$0@2%92%28%163$2$0@2$1$0@2|@2|@|@"])
  fun op satFun_def x = x
    val op satFun_def =
    ThmBind.DT(((("hml1Foundation",45),[]),[]),
               [ThmBind.read"%95%19%92%28%101%23%111%66%99%7%127%188$4@$3@$2@$1@$0@2%154$4@$1@%169@%153$3@$2@$0@3|@|@|@|@|@"])
  fun op hmLFP_def x = x
    val op hmLFP_def =
    ThmBind.DT(((("hml1Foundation",48),[]),[]),
               [ThmBind.read"%95%19%92%28%101%23%111%66%127%179$3@$2@$1@$0@2%183%188$3@$2@$1@$0@3|@|@|@|@"])
  fun op hmGFP_def x = x
    val op hmGFP_def =
    ThmBind.DT(((("hml1Foundation",51),[]),[]),
               [ThmBind.read"%95%19%92%28%101%23%111%66%127%177$3@$2@$1@$0@2%175%188$3@$2@$1@$0@3|@|@|@|@"])
  fun op datatype_hmlForm x = x
    val op datatype_hmlForm =
    ThmBind.DT(((("hml1Foundation",18),[("bool",[25,170])]),["DISK_THM"]),
               [ThmBind.read"%147%72%190@%174@%186@%172@%185@%144@%148@2"])
  fun op hmlForm_11 x = x
    val op hmlForm_11 =
    ThmBind.DT(((("hml1Foundation",19),
                [("bool",[14,25,26,30,50,52,55,62,131,132,137,180]),
                 ("hml1Foundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),
                 ("ind_type",[33,34]),("pair",[8,9])]),["DISK_THM"]),
               [ThmBind.read"%119%92%30%92%31%123%128%186$1@2%186$0@3%122$1@$0@2|@|@2%119%111%33%111%38%111%36%111%40%123%128%172$3@$2@2%172$1@$0@3%119%128$3@$1@2%128$2@$0@3|@|@|@|@2%119%111%33%111%38%111%36%111%40%123%128%185$3@$2@2%185$1@$0@3%119%128$3@$1@2%128$2@$0@3|@|@|@|@2%119%94%32%111%38%94%35%111%40%123%128%144$3@$2@2%144$1@$0@3%119%125$3@$1@2%128$2@$0@3|@|@|@|@2%94%32%111%38%94%35%111%40%123%128%148$3@$2@2%148$1@$0@3%119%125$3@$1@2%128$2@$0@3|@|@|@|@5"])
  fun op hmlForm_distinct x = x
    val op hmlForm_distinct =
    ThmBind.DT(((("hml1Foundation",20),
                [("bool",
                 [14,25,26,30,35,46,50,52,53,55,62,131,132,137,180]),
                 ("hml1Foundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),
                 ("ind_type",[33,34]),("pair",[8,9])]),["DISK_THM"]),
               [ThmBind.read"%119%191%128%190@%174@3%119%92%30%191%128%190@%186$0@3|@2%119%111%38%111%33%191%128%190@%172$0@$1@3|@|@2%119%111%38%111%33%191%128%190@%185$0@$1@3|@|@2%119%111%38%94%32%191%128%190@%144$0@$1@3|@|@2%119%111%38%94%32%191%128%190@%148$0@$1@3|@|@2%119%92%30%191%128%174@%186$0@3|@2%119%111%38%111%33%191%128%174@%172$0@$1@3|@|@2%119%111%38%111%33%191%128%174@%185$0@$1@3|@|@2%119%111%38%94%32%191%128%174@%144$0@$1@3|@|@2%119%111%38%94%32%191%128%174@%148$0@$1@3|@|@2%119%111%38%111%33%92%30%191%128%186$0@2%172$1@$2@3|@|@|@2%119%111%38%111%33%92%30%191%128%186$0@2%185$1@$2@3|@|@|@2%119%111%38%94%32%92%30%191%128%186$0@2%144$1@$2@3|@|@|@2%119%111%38%94%32%92%30%191%128%186$0@2%148$1@$2@3|@|@|@2%119%111%40%111%38%111%36%111%33%191%128%172$0@$2@2%185$1@$3@3|@|@|@|@2%119%111%40%111%38%94%35%111%33%191%128%172$0@$2@2%144$1@$3@3|@|@|@|@2%119%111%40%111%38%94%35%111%33%191%128%172$0@$2@2%148$1@$3@3|@|@|@|@2%119%111%40%111%38%94%35%111%33%191%128%185$0@$2@2%144$1@$3@3|@|@|@|@2%119%111%40%111%38%94%35%111%33%191%128%185$0@$2@2%148$1@$3@3|@|@|@|@2%111%40%111%38%94%35%94%32%191%128%144$0@$2@2%148$1@$3@3|@|@|@|@21"])
  fun op hmlForm_case_cong x = x
    val op hmlForm_case_cong =
    ThmBind.DT(((("hml1Foundation",21),
                [("bool",[14,25,26,52,131,132,137,180]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])]),["DISK_THM"]),
               [ThmBind.read"%111%12%111%13%88%78%88%81%100%43%106%49%106%53%103%56%103%59%131%119%128$8@$7@2%119%131%128$7@%190@2%121$6@%80@3%119%131%128$7@%174@2%121$5@%83@3%119%92%30%131%128$8@%186$0@3%121$5$0@2%45$0@3|@2%119%111%33%111%38%131%128$9@%172$1@$0@3%121$5$1@$0@2%51$1@$0@3|@|@2%119%111%33%111%38%131%128$9@%185$1@$0@3%121$4$1@$0@2%55$1@$0@3|@|@2%119%94%32%111%38%131%128$9@%144$1@$0@3%121$3$1@$0@2%58$1@$0@3|@|@2%94%32%111%38%131%128$9@%148$1@$0@3%121$2$1@$0@2%61$1@$0@3|@|@9%121%180$8@$6@$5@$4@$3@$2@$1@$0@2%180$7@%80@%83@%45@%51@%55@%58@%61@3|@|@|@|@|@|@|@|@|@"])
  fun op hmlForm_nchotomy x = x
    val op hmlForm_nchotomy =
    ThmBind.DT(((("hml1Foundation",22),
                [("bool",[14,25,26,52,131,132,137,180]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])]),["DISK_THM"]),
               [ThmBind.read"%111%71%171%128$0@%190@2%171%128$0@%174@2%171%134%74%128$1@%186$0@2|@2%171%138%69%138%70%128$2@%172$1@$0@2|@|@2%171%138%69%138%70%128$2@%185$1@$0@2|@|@2%171%135%41%138%69%128$2@%144$1@$0@2|@|@2%135%41%138%69%128$2@%148$1@$0@2|@|@7|@"])
  fun op hmlForm_Axiom x = x
    val op hmlForm_Axiom =
    ThmBind.DT(((("hml1Foundation",23),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hml1Foundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),
                 ("ind_type",[33,34]),("pair",[8,9])]),["DISK_THM"]),
               [ThmBind.read"%88%46%88%47%100%52%107%57%107%60%104%62%104%63%136%64%119%121$0%190@2$7@2%119%121$0%174@2$6@2%119%92%30%121$1%186$0@3$6$0@2|@2%119%111%33%111%38%121$2%172$1@$0@3$6$1@$0@$2$1@2$2$0@3|@|@2%119%111%33%111%38%121$2%185$1@$0@3$5$1@$0@$2$1@2$2$0@3|@|@2%119%94%32%111%38%121$2%144$1@$0@3$4$1@$0@$2$0@3|@|@2%94%32%111%38%121$2%148$1@$0@3$3$1@$0@$2$0@3|@|@7|@|@|@|@|@|@|@|@"])
  fun op hmlForm_induction x = x
    val op hmlForm_induction =
    ThmBind.DT(((("hml1Foundation",24),
                [("bool",[14,25,26,52,131,132,137]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])]),["DISK_THM"]),
               [ThmBind.read"%105%15%131%119$0%190@2%119$0%174@2%119%92%74$1%186$0@2|@2%119%111%69%111%70%131%119$2$1@2$2$0@3$2%172$1@$0@3|@|@2%119%111%69%111%70%131%119$2$1@2$2$0@3$2%185$1@$0@3|@|@2%119%111%69%131$1$0@2%94%41$2%144$0@$1@2|@2|@2%111%69%131$1$0@2%94%41$2%148$0@$1@2|@2|@8%111%69$1$0@|@2|@"])
  fun op hmsat_ind x = x
    val op hmsat_ind =
    ThmBind.DT(((("hml1Foundation",27),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [14,25,26,27,35,50,51,52,53,57,62,92,95,103,104,106,123,
                  131,132,137,180]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17]),
                 ("numeral",[3,7,8]),("pair",[5,7,9,16]),("prim_rec",[42]),
                 ("relation",[107,119,121]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%108%16%131%119%91%6%95%19%101%23$3%116$2@%118$1@$0@3%190@|@|@|@2%119%91%6%95%19%101%23$3%116$2@%118$1@$0@3%174@|@|@|@2%119%91%6%95%19%101%23%92%28$4%116$3@%118$2@$1@3%186$0@2|@|@|@|@2%119%91%6%95%19%101%23%111%50%111%54%131%119$5%116$4@%118$3@$2@3$1@2$5%116$4@%118$3@$2@3$0@3$5%116$4@%118$3@$2@3%172$1@$0@3|@|@|@|@|@2%119%91%6%95%19%101%23%111%50%111%54%131%119$5%116$4@%118$3@$2@3$1@2$5%116$4@%118$3@$2@3$0@3$5%116$4@%118$3@$2@3%185$1@$0@3|@|@|@|@|@2%119%91%6%95%19%101%23%94%3%111%44%131%89%29%91%8%131%119$5$1@$6@$0@2%156$1@$3@3$7%116$0@%118$5@$4@3$2@2|@|@2$5%116$4@%118$3@$2@3%144$1@$0@3|@|@|@|@|@2%91%6%95%19%101%23%94%3%111%44%131%91%8$6%116$0@%118$4@$3@3$1@|@2$5%116$4@%118$3@$2@3%148$1@$0@3|@|@|@|@|@8%91%79%95%82%101%84%111%85$4%116$3@%118$2@$1@3$0@|@|@|@|@2|@"])
  fun op hmsat_def x = x
    val op hmsat_def =
    ThmBind.DT(((("hml1Foundation",28),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,25,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26]),
                 ("numeral",[3,7,8]),("pair",[7,9,16,49]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%119%101%23%95%19%91%6%123%182%116$0@%118$1@$2@3%190@2%165@|@|@|@2%119%101%23%95%19%91%6%123%182%116$0@%118$1@$2@3%174@2%149@|@|@|@2%119%92%28%101%23%95%19%91%6%123%182%116$0@%118$1@$2@3%186$3@3%157$0@$2$3@3|@|@|@|@2%119%111%54%111%50%101%23%95%19%91%6%123%182%116$0@%118$1@$2@3%172$3@$4@3%119%182%116$0@%118$1@$2@3$3@2%182%116$0@%118$1@$2@3$4@3|@|@|@|@|@2%119%111%54%111%50%101%23%95%19%91%6%123%182%116$0@%118$1@$2@3%185$3@$4@3%171%182%116$0@%118$1@$2@3$3@2%182%116$0@%118$1@$2@3$4@3|@|@|@|@|@2%119%111%44%101%23%95%19%91%6%94%3%123%182%116$1@%118$2@$3@3%144$0@$4@3%91%8%89%29%131$4$0@$3@$1@2%131%156$0@$2@2%182%116$1@%118$4@$5@3$6@3|@|@2|@|@|@|@|@2%111%44%101%23%95%19%91%6%94%3%123%182%116$1@%118$2@$3@3%148$0@$4@3%133%8%132%29%119$4$0@$3@$1@2%119%156$0@$2@2%182%116$1@%118$4@$5@3$6@3|@|@2|@|@|@|@|@7"])
  fun op IN_CLAUSES x = x
    val op IN_CLAUSES =
    ThmBind.DT(((("hml1Foundation",32),
                [("bool",[18,25,50,55])]),["DISK_THM"]),
               [ThmBind.read"%119%124%151%76%114$0@%155$0@%86%171%14$0@2%17$0@2|@2|@2%151%76%114$0@%171%155$0@%86%14$0@|@2%155$0@%86%17$0@|@3|@3%124%151%76%114$0@%155$0@%86%119%14$0@2%17$0@2|@2|@2%151%76%114$0@%119%155$0@%86%14$0@|@2%155$0@%86%17$0@|@3|@3"])
  fun op IN_UNION_INTER_CLAUSES x = x
    val op IN_UNION_INTER_CLAUSES =
    ThmBind.DT(((("hml1Foundation",33),
                [("bool",[18,25,50,55,57]),
                 ("pred_set",[47,59])]),["DISK_THM"]),
               [ThmBind.read"%119%124%151%76%114$0@%155$0@%86%119%14$0@2%17$0@2|@2|@2%158%86%14$0@|@%86%17$0@|@3%124%151%76%114$0@%155$0@%86%171%14$0@2%17$0@2|@2|@2%167%86%14$0@|@%86%17$0@|@3"])
  fun op MONOTONE_INTER x = x
    val op MONOTONE_INTER =
    ThmBind.DT(((("hml1Foundation",34),
                [("pred_set",[28,60])]),["DISK_THM"]),
               [ThmBind.read"%131%161%1@%2@2%131%161%4@%5@2%161%158%1@%4@2%158%2@%5@4"])
  fun op MONOTONE_UNION x = x
    val op MONOTONE_UNION =
    ThmBind.DT(((("hml1Foundation",35),
                [("bool",[25,51,62]),("pred_set",[28,48])]),["DISK_THM"]),
               [ThmBind.read"%131%161%1@%2@2%131%161%4@%5@2%161%167%1@%4@2%167%2@%5@4"])
  fun op hmsat_IN_CLAUSES x = x
    val op hmsat_IN_CLAUSES =
    ThmBind.DT(((("hml1Foundation",36),
                [("bool",[18,25,35,50,55])]),["DISK_THM"]),
               [ThmBind.read"%119%91%77%111%66%101%23%95%19%127%152%77%115$0@%182%116$0@%118$1@$2@3$3@2|@2%152%77%115$0@%157$0@%87%182%116$0@%118$2@$3@3$4@|@2|@2|@|@|@|@2%119%91%77%111%50%111%54%101%23%127%152%77%115$0@%119%182%116$0@%118%19@$1@3$3@2%182%116$0@%118%19@$1@3$2@3|@2%152%77%115$0@%119%157$0@%87%182%116$0@%118%19@$2@3$4@|@2%157$0@%87%182%116$0@%118%19@$2@3$3@|@3|@2|@|@|@|@2%91%77%111%50%111%54%101%23%127%152%77%115$0@%171%182%116$0@%118%19@$1@3$3@2%182%116$0@%118%19@$1@3$2@3|@2%152%77%115$0@%171%157$0@%87%182%116$0@%118%19@$2@3$4@|@2%157$0@%87%182%116$0@%118%19@$2@3$3@|@3|@2|@|@|@|@3"])
  fun op HMfn_CLAUSES x = x
    val op HMfn_CLAUSES =
    ThmBind.DT(((("hml1Foundation",37),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,18,25,35,50,51,52,53,55,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26,29]),
                 ("numeral",[3,7,8]),("pair",[7,9,16,49]),
                 ("pred_set",[22,47,59,455]),("prim_rec",[42]),
                 ("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%119%111%50%111%54%101%23%95%19%127%154$0@%172$3@$2@2%169@$1@2%159%154$0@$3@%169@$1@2%154$0@$2@%169@$1@3|@|@|@|@2%111%50%111%54%101%23%95%19%127%154$0@%185$3@$2@2%169@$1@2%168%154$0@$3@%169@$1@2%154$0@$2@%169@$1@3|@|@|@|@2"])
  fun op HMfn_tt_ff_CLAUSES x = x
    val op HMfn_tt_ff_CLAUSES =
    ThmBind.DT(((("hml1Foundation",38),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,25,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26,29]),
                 ("numeral",[3,7,8]),("pair",[7,9,16,49]),
                 ("pred_set",[22,31]),("prim_rec",[42]),
                 ("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%119%95%19%101%23%101%24%163%154$2@%190@%169@$1@2%154$2@%190@%169@$0@2|@|@|@2%95%19%101%23%101%24%163%154$2@%174@%169@$1@2%154$2@%174@%169@$0@2|@|@|@2"])
  fun op HMfn_MONOTONIC_propvar x = x
    val op HMfn_MONOTONIC_propvar =
    ThmBind.DT(((("hml1Foundation",39),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,18,25,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26,29,31]),
                 ("numeral",[3,7,8]),("pair",[7,9,16,49]),
                 ("pred_set",[8,22]),("prim_rec",[42]),
                 ("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%92%28%101%23%101%24%131%173$1@$0@2%163%154%19@%186$2@2%169@$1@2%154%19@%186$2@2%169@$0@3|@|@|@"])
  fun op HMfn_MONOTONIC_andh x = x
    val op HMfn_MONOTONIC_andh =
    ThmBind.DT(((("hml1Foundation",40),
                [("pred_set",[28,60])]),["DISK_THM"]),
               [ThmBind.read"%131%101%23%101%24%131%173$1@$0@2%163%154%19@%66@%169@$1@2%154%19@%66@%169@$0@3|@|@2%131%101%23%101%24%131%173$1@$0@2%163%154%19@%68@%169@$1@2%154%19@%68@%169@$0@3|@|@2%131%173%23@%24@2%163%159%154%19@%66@%169@%23@2%154%19@%68@%169@%23@3%159%154%19@%66@%169@%24@2%154%19@%68@%169@%24@6"])
  fun op HMfn_MONOTONIC_orh x = x
    val op HMfn_MONOTONIC_orh =
    ThmBind.DT(((("hml1Foundation",41),
                [("bool",[25,51,62]),("pred_set",[28,48])]),["DISK_THM"]),
               [ThmBind.read"%131%101%23%101%24%131%173$1@$0@2%163%154%19@%66@%169@$1@2%154%19@%66@%169@$0@3|@|@2%131%101%23%101%24%131%173$1@$0@2%163%154%19@%68@%169@$1@2%154%19@%68@%169@$0@3|@|@2%131%173%23@%24@2%163%168%154%19@%66@%169@%23@2%154%19@%68@%169@%23@3%168%154%19@%66@%169@%24@2%154%19@%68@%169@%24@6"])
  fun op HMfn_MONOTONIC_Box x = x
    val op HMfn_MONOTONIC_Box =
    ThmBind.DT(((("hml1Foundation",42),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,25,26,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26,29]),
                 ("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[6,22,28]),("prim_rec",[42]),
                 ("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%131%101%23%101%24%131%173$1@$0@2%163%154%19@%66@%169@$1@2%154%19@%66@%169@$0@3|@|@2%131%173%23@%24@2%163%154%19@%144%41@%66@2%169@%23@2%154%19@%144%41@%66@2%169@%24@4"])
  fun op HMfn_MONOTONIC_Dia x = x
    val op HMfn_MONOTONIC_Dia =
    ThmBind.DT(((("hml1Foundation",43),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,25,26,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hml1Foundation",[16,17,25,26,29]),
                 ("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[6,22,28]),("prim_rec",[42]),
                 ("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%131%101%23%101%24%131%173$1@$0@2%163%154%19@%66@%169@$1@2%154%19@%66@%169@$0@3|@|@2%131%173%23@%24@2%163%154%19@%148%41@%66@2%169@%23@2%154%19@%148%41@%66@2%169@%24@4"])
  fun op HMfn_MONOTONIC x = x
    val op HMfn_MONOTONIC =
    ThmBind.DT(((("hml1Foundation",44),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [14,15,18,25,26,35,42,46,47,50,51,52,53,55,57,62,70,75,76,
                  92,93,95,103,104,106,123,131,132,137,145]),
                 ("combin",[19]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,25,26,29,31]),
                 ("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[6,8,22,28,31,47,48,59,60,455]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,14,15])]),["DISK_THM"]),
               [ThmBind.read"%111%66%101%23%101%24%131%173$1@$0@2%163%154%19@$2@%169@$1@2%154%19@$2@%169@$0@3|@|@|@"])
  fun op HMUpdate_MONOTONIC x = x
    val op HMUpdate_MONOTONIC =
    ThmBind.DT(((("hml1Foundation",46),
                [("bool",[25,26,27,29,52,62,63]),
                 ("hml1Foundation",[30,31]),
                 ("pred_set",[0,28])]),["DISK_THM"]),
               [ThmBind.read"%101%23%92%28%99%7%99%11%131%163$1@$0@2%173%153$2@$3@$1@2%153$2@$3@$0@3|@|@|@|@"])
  fun op satFun_MONOTONIC x = x
    val op satFun_MONOTONIC =
    ThmBind.DT(((("hml1Foundation",47),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [14,15,18,25,26,27,29,35,42,46,47,50,51,52,53,55,57,62,63,
                  70,75,76,92,93,95,103,104,106,123,131,132,137,145]),
                 ("combin",[19]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,25,26,29,30,31,
                  45]),("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[0,6,8,22,28,31,47,48,59,60,455]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,14,15])]),["DISK_THM"]),
               [ThmBind.read"%101%23%95%19%92%28%111%66%99%9%99%10%131%163$1@$0@2%163%188$4@$3@$5@$2@$1@2%188$4@$3@$5@$2@$0@3|@|@|@|@|@|@"])
  fun op satFun_monotone x = x
    val op satFun_monotone =
    ThmBind.DT(((("hml1Foundation",49),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [14,15,18,25,26,27,29,35,42,46,47,50,51,52,53,55,57,62,63,
                  70,75,76,92,93,95,103,104,106,123,131,132,137,145]),
                 ("combin",[19]),("fixedPoint",[0]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,25,26,29,30,31,
                  45]),("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[0,6,8,22,28,31,47,48,59,60,455]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,14,15])]),["DISK_THM"]),
               [ThmBind.read"%184%189%20@%27@%21@%67@2"])
  fun op hmLFP_fixedpoint x = x
    val op hmLFP_fixedpoint =
    ThmBind.DT(((("hml1Foundation",50),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [13,14,15,18,25,26,27,29,35,42,46,47,50,51,52,53,55,57,62,
                  63,70,72,74,75,76,77,83,92,93,95,103,104,106,123,131,132,
                  137,145]),("combin",[19]),("fixedPoint",[0,7]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,25,26,29,30,31,
                  45,48]),("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[0,6,8,22,28,31,47,48,59,60,455]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",
                 [1,3,5,6,7,11,12,13,14,15,17,18,19,20,
                  23])]),["DISK_THM"]),
               [ThmBind.read"%93%18%90%27%98%22%110%65%119%126%178$3@$2@$1@$0@2%187$3@$2@$1@$0@%178$3@$2@$1@$0@4%97%25%131%126$0@%187$4@$3@$2@$1@$0@3%162%178$4@$3@$2@$1@2$0@2|@2|@|@|@|@"])
  fun op hmGFP_fixedpoint x = x
    val op hmGFP_fixedpoint =
    ThmBind.DT(((("hml1Foundation",52),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [13,14,15,18,25,26,27,29,35,42,46,47,50,51,52,53,55,57,62,
                  63,70,72,74,75,76,77,83,92,93,95,103,104,106,123,131,132,
                  137,145]),("combin",[19]),("fixedPoint",[0,8]),
                 ("hml1Foundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,25,26,29,30,31,
                  45,51]),("numeral",[3,7,8]),("pair",[3,7,9,16,49]),
                 ("pred_set",[0,6,8,22,28,31,47,48,59,60,455]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",
                 [1,3,5,6,7,11,12,13,14,15,17,18,19,20,
                  23])]),["DISK_THM"]),
               [ThmBind.read"%93%18%90%27%98%22%110%65%119%126%176$3@$2@$1@$0@2%187$3@$2@$1@$0@%176$3@$2@$1@$0@4%97%25%131%126$0@%187$4@$3@$2@$1@$0@3%162$0@%176$4@$3@$2@$1@3|@2|@|@|@|@"])

  val _ = DB.bindl "hml1Foundation"
  [("hmlForm_TY_DEF",hmlForm_TY_DEF,DB.Def),
   ("hmlForm_case_def",hmlForm_case_def,DB.Def),
   ("hmlForm_size_def",hmlForm_size_def,DB.Def),
   ("HMfn_def",HMfn_def,DB.Def), ("HMUpdate_def",HMUpdate_def,DB.Def),
   ("extends_def",extends_def,DB.Def), ("satFun_def",satFun_def,DB.Def),
   ("hmLFP_def",hmLFP_def,DB.Def), ("hmGFP_def",hmGFP_def,DB.Def),
   ("datatype_hmlForm",datatype_hmlForm,DB.Thm),
   ("hmlForm_11",hmlForm_11,DB.Thm),
   ("hmlForm_distinct",hmlForm_distinct,DB.Thm),
   ("hmlForm_case_cong",hmlForm_case_cong,DB.Thm),
   ("hmlForm_nchotomy",hmlForm_nchotomy,DB.Thm),
   ("hmlForm_Axiom",hmlForm_Axiom,DB.Thm),
   ("hmlForm_induction",hmlForm_induction,DB.Thm),
   ("hmsat_ind",hmsat_ind,DB.Thm), ("hmsat_def",hmsat_def,DB.Thm),
   ("IN_CLAUSES",IN_CLAUSES,DB.Thm),
   ("IN_UNION_INTER_CLAUSES",IN_UNION_INTER_CLAUSES,DB.Thm),
   ("MONOTONE_INTER",MONOTONE_INTER,DB.Thm),
   ("MONOTONE_UNION",MONOTONE_UNION,DB.Thm),
   ("hmsat_IN_CLAUSES",hmsat_IN_CLAUSES,DB.Thm),
   ("HMfn_CLAUSES",HMfn_CLAUSES,DB.Thm),
   ("HMfn_tt_ff_CLAUSES",HMfn_tt_ff_CLAUSES,DB.Thm),
   ("HMfn_MONOTONIC_propvar",HMfn_MONOTONIC_propvar,DB.Thm),
   ("HMfn_MONOTONIC_andh",HMfn_MONOTONIC_andh,DB.Thm),
   ("HMfn_MONOTONIC_orh",HMfn_MONOTONIC_orh,DB.Thm),
   ("HMfn_MONOTONIC_Box",HMfn_MONOTONIC_Box,DB.Thm),
   ("HMfn_MONOTONIC_Dia",HMfn_MONOTONIC_Dia,DB.Thm),
   ("HMfn_MONOTONIC",HMfn_MONOTONIC,DB.Thm),
   ("HMUpdate_MONOTONIC",HMUpdate_MONOTONIC,DB.Thm),
   ("satFun_MONOTONIC",satFun_MONOTONIC,DB.Thm),
   ("satFun_monotone",satFun_monotone,DB.Thm),
   ("hmLFP_fixedpoint",hmLFP_fixedpoint,DB.Thm),
   ("hmGFP_fixedpoint",hmGFP_fixedpoint,DB.Thm)]

  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hml1Foundation",
    thydataty = "compute",
    read = ThmBind.read,
    data =
        "hml1Foundation.hmsat_def hml1Foundation.satFun_def hml1Foundation.hmGFP_def hml1Foundation.hmLFP_def hml1Foundation.extends_def hml1Foundation.HMUpdate_def hml1Foundation.HMfn_def"
  }
  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hml1Foundation",
    thydataty = "TypeGrammarDeltas",
    read = ThmBind.read,
    data = "NTY14.hml1Foundation,7.hmlForm"
  }
  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hml1Foundation",
    thydataty = "TermGrammarDeltas",
    read = ThmBind.read,
    data =
        "OO2.tt4.%190OO2.ff4.%174OO5.proph4.%186OO4.andh4.%172OO3.orh4.%185OO3.Box4.%144OO3.Dia4.%148OO12.hmlForm_CASE4.%180OO12.hmlForm_size4.%181OO4.case4.%180RMT4.andhG4.andhOCI0.IR580.H1.RK4.andhS1.0.XRMT3.orhG3.orhOCI0.IR570.H1.RK3.orhS1.0.XRMT5.hmsatG5.hmsatOCI0.IR540.H1.RK5.hmsatS1.0.XOO5.hmsat4.%182OO4.HMfn4.%154OO8.HMUpdate4.%153OO7.extends4.%173OO6.satFun4.%188OO5.hmLFP4.%179OO5.hmGFP4.%177"
  }
  local open GrammarSpecials Parse
    fun UTOFF f = Feedback.trace("Parse.unicode_trace_off_complaints",0)f
  in
  val hml1Foundation_grammars = merge_grammars ["fixedPoint",
                                                "indexedLists",
                                                "patternMatches"]
  local
  val (tyUDs, tmUDs) = GrammarDeltas.thy_deltas{thyname="hml1Foundation"}
  val addtmUDs = term_grammar.add_deltas tmUDs
  val addtyUDs = type_grammar.apply_deltas tyUDs
  in
  val hml1Foundation_grammars = 
    Portable.## (addtyUDs,addtmUDs) hml1Foundation_grammars
  val _ = Parse.grammarDB_insert("hml1Foundation",hml1Foundation_grammars)
  val _ = Parse.temp_set_grammars (addtyUDs (Parse.type_grammar()), addtmUDs (Parse.term_grammar()))
  end (* addUDs local *)
  end


  val _ =
    TypeBase.write [
      let
        open TypeBasePure
        val tyinfo0 = mk_datatype_info
          {ax=ORIG hmlForm_Axiom,
           case_def=hmlForm_case_def,
           case_cong=hmlForm_case_cong,
           induction=ORIG hmlForm_induction,
           nchotomy=hmlForm_nchotomy,
           size=SOME(Parse.Term`(hml1Foundation$hmlForm_size) :('action -> num$num) ->
('propvar -> num$num) ->
('action, 'propvar) hml1Foundation$hmlForm -> num$num`,
                     ORIG hmlForm_size_def),
           encode = NONE,
           lift=NONE,
           one_one=SOME hmlForm_11,
           distinct=SOME hmlForm_distinct,
           fields=let fun T t s A = mk_thy_type{Thy=t,Tyop=s,Args=A}
    val U = mk_vartype
in
[] end,
           accessors=[],
           updates=[],
           recognizers=[],
           destructors=[]}
        val tyinfo0 = tyinfo0
        val () = computeLib.write_datatype_info tyinfo0
      in
        tyinfo0
      end
    ];

val _ = if !Globals.print_thy_loads then TextIO.print "done\n" else ()
val _ = Theory.load_complete "hml1Foundation"
end
