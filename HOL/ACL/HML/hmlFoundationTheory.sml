structure hmlFoundationTheory :> hmlFoundationTheory =
struct
  val _ = if !Globals.print_thy_loads then TextIO.print "Loading hmlFoundationTheory ... " else ()
  open Type Term Thm
  infixr -->

  fun C s t ty = mk_thy_const{Name=s,Thy=t,Ty=ty}
  fun T s t A = mk_thy_type{Tyop=s, Thy=t,Args=A}
  fun V s q = mk_var(s,q)
  val U     = mk_vartype
  (* Parents and ML dependencies *)
  local open indexedListsTheory patternMatchesTheory
  in end;
  val _ = Theory.link_parents
          ("hmlFoundation",
          Arbnum.fromString "1503150831",
          Arbnum.fromString "955337")
          [("indexedLists",
           Arbnum.fromString "1503148856",
           Arbnum.fromString "404777"),
           ("patternMatches",
           Arbnum.fromString "1503148884",
           Arbnum.fromString "20925")];
  val _ = Theory.incorporate_types "hmlFoundation" [("hmlForm", 1)];

  val idvector = 
    let fun ID(thy,oth) = {Thy = thy, Other = oth}
    in Vector.fromList
  [ID("hmlFoundation", "hmlForm"), ID("min", "fun"), ID("min", "bool"),
   ID("pair", "prod"), ID("num", "num"), ID("ind_type", "recspace"),
   ID("bool", "!"), ID("arithmetic", "+"), ID("pair", ","),
   ID("bool", "/\\"), ID("num", "0"), ID("min", "="), ID("min", "==>"),
   ID("bool", "?"), ID("bool", "ARB"), ID("arithmetic", "BIT1"),
   ID("ind_type", "BOTTOM"), ID("hmlFoundation", "Box"),
   ID("ind_type", "CONSTR"), ID("bool", "DATATYPE"),
   ID("hmlFoundation", "Dia"), ID("bool", "F"), ID("ind_type", "FCONS"),
   ID("bool", "IN"), ID("arithmetic", "NUMERAL"), ID("num", "SUC"),
   ID("bool", "T"), ID("bool", "TYPE_DEFINITION"),
   ID("arithmetic", "ZERO"), ID("bool", "\\/"),
   ID("hmlFoundation", "andh"), ID("hmlFoundation", "ff"),
   ID("hmlFoundation", "hmlForm_CASE"),
   ID("hmlFoundation", "hmlForm_size"), ID("hmlFoundation", "hmsat"),
   ID("hmlFoundation", "orh"), ID("hmlFoundation", "tt"), ID("bool", "~")]
  end;
  local open SharingTables
  in
  val tyvector = build_type_vector idvector
  [TYV "'action", TYOP [0, 0], TYOP [1, 1, 1], TYOP [1, 1, 2], TYOP [2],
   TYOP [1, 1, 4], TYV "'configuration", TYOP [1, 6, 4], TYOP [1, 6, 7],
   TYOP [1, 0, 8], TYOP [3, 6, 9], TYOP [1, 10, 5], TYOP [4],
   TYOP [1, 1, 12], TYOP [1, 0, 12], TYOP [1, 14, 13], TYV "'a",
   TYOP [1, 1, 16], TYOP [1, 0, 4], TYOP [1, 18, 17], TYOP [1, 19, 16],
   TYOP [1, 19, 20], TYOP [1, 1, 17], TYOP [1, 22, 21], TYOP [1, 22, 23],
   TYOP [1, 16, 24], TYOP [1, 16, 25], TYOP [1, 1, 26], TYOP [1, 18, 2],
   TYOP [5, 18], TYOP [1, 29, 4], TYOP [1, 16, 16], TYOP [1, 16, 31],
   TYOP [1, 1, 32], TYOP [1, 1, 33], TYOP [1, 1, 31], TYOP [1, 18, 35],
   TYOP [1, 28, 4], TYOP [1, 28, 37], TYOP [1, 3, 38], TYOP [1, 3, 39],
   TYOP [1, 1, 40], TYOP [1, 1, 41], TYOP [1, 1, 29], TYOP [1, 16, 4],
   TYOP [1, 44, 4], TYOP [1, 18, 4], TYOP [1, 7, 4], TYOP [1, 46, 4],
   TYOP [1, 9, 4], TYOP [1, 49, 4], TYOP [1, 14, 4], TYOP [1, 51, 4],
   TYOP [1, 19, 4], TYOP [1, 53, 4], TYOP [1, 36, 4], TYOP [1, 55, 4],
   TYOP [1, 5, 4], TYOP [1, 57, 4], TYOP [1, 22, 4], TYOP [1, 59, 4],
   TYOP [1, 34, 4], TYOP [1, 61, 4], TYOP [1, 11, 4], TYOP [1, 63, 4],
   TYOP [1, 30, 4], TYOP [1, 65, 4], TYOP [1, 12, 12], TYOP [1, 12, 67],
   TYOP [1, 9, 10], TYOP [1, 6, 69], TYOP [1, 4, 4], TYOP [1, 4, 71],
   TYOP [1, 16, 44], TYOP [1, 18, 46], TYOP [1, 1, 5], TYOP [1, 12, 4],
   TYOP [1, 12, 76], TYOP [1, 29, 30], TYOP [1, 17, 4], TYOP [1, 79, 4],
   TYOP [1, 43, 4], TYOP [1, 81, 4], TYOP [1, 12, 29], TYOP [1, 83, 29],
   TYOP [1, 18, 84], TYOP [1, 12, 85], TYOP [1, 83, 83], TYOP [1, 29, 87],
   TYOP [1, 0, 46], TYOP [1, 30, 81]]
  end
  val _ = Theory.incorporate_consts "hmlFoundation" tyvector
     [("tt", 1), ("orh", 3), ("hmsat", 11), ("hmlForm_size", 15),
      ("hmlForm_CASE", 27), ("ff", 1), ("andh", 3), ("Dia", 28),
      ("Box", 28)];

  local open SharingTables
  in
  val tmvector = build_term_vector idvector tyvector
  [TMV("'hmlForm'", 30), TMV("Actions", 18), TMV("E", 6), TMV("E'", 6),
   TMV("M", 1), TMV("M'", 1), TMV("P", 5), TMV("P", 11), TMV("Trans", 9),
   TMV("a", 0), TMV("a0", 18), TMV("a0", 1), TMV("a0", 29), TMV("a0'", 18),
   TMV("a0'", 1), TMV("a0'", 29), TMV("a1", 1), TMV("a1", 29),
   TMV("a1'", 1), TMV("f", 18), TMV("f", 14), TMV("f", 22), TMV("f", 1),
   TMV("f'", 22), TMV("f0", 16), TMV("f1", 16), TMV("f1", 22),
   TMV("f1", 1), TMV("f1'", 22), TMV("f2", 19), TMV("f2", 34),
   TMV("f2", 1), TMV("f2'", 19), TMV("f3", 19), TMV("f3", 34),
   TMV("f3'", 19), TMV("f4", 36), TMV("f5", 36), TMV("fn", 17),
   TMV("h", 1), TMV("h0", 1), TMV("hh", 1), TMV("hmlForm", 42),
   TMV("n", 12), TMV("rep", 43), TMV("v", 16), TMV("v", 6), TMV("v'", 16),
   TMV("v1", 16), TMV("v1", 9), TMV("v1'", 16), TMV("v2", 1), TMC(6, 45),
   TMC(6, 46), TMC(6, 47), TMC(6, 48), TMC(6, 50), TMC(6, 52), TMC(6, 54),
   TMC(6, 56), TMC(6, 58), TMC(6, 60), TMC(6, 62), TMC(6, 64), TMC(6, 66),
   TMC(6, 57), TMC(6, 65), TMC(7, 68), TMC(8, 70), TMC(9, 72), TMC(10, 12),
   TMC(11, 73), TMC(11, 72), TMC(11, 74), TMC(11, 75), TMC(11, 77),
   TMC(11, 78), TMC(12, 72), TMC(13, 46), TMC(13, 47), TMC(13, 48),
   TMC(13, 80), TMC(13, 82), TMC(13, 57), TMC(13, 65), TMC(14, 18),
   TMC(15, 67), TMC(16, 29), TMC(17, 28), TMC(18, 86), TMC(19, 71),
   TMC(20, 28), TMC(21, 4), TMC(22, 88), TMC(23, 89), TMC(24, 67),
   TMC(25, 67), TMC(26, 4), TMC(27, 90), TMC(28, 12), TMC(29, 72),
   TMC(30, 3), TMC(31, 1), TMC(32, 27), TMC(33, 15), TMC(34, 11),
   TMC(35, 3), TMC(36, 1), TMC(37, 71)]
  end
  structure ThmBind = struct
    val DT = Thm.disk_thm
    val read = Term.read_raw tmvector
  end
  fun op hmlForm_TY_DEF x = x
    val op hmlForm_TY_DEF =
    ThmBind.DT(((("hmlFoundation",0),
                [("bool",[14,25,26,52,131,132,137])]),["DISK_THM"]),
               [ThmBind.read"%82%44%98%15%64%0%77%66%15%77%100%76$0@%89%70@%85@%43%87|@3%100%76$0@%89%96%70@2%85@%43%87|@3%100%84%12%84%17%69%76$2@%12%17%89%96%96%70@3%85@%93$1@%93$0@%43%87|@3||$1@$0@3%69$3$1@2$3$0@3|@|@2%100%84%12%84%17%69%76$2@%12%17%89%96%96%96%70@4%85@%93$1@%93$0@%43%87|@3||$1@$0@3%69$3$1@2$3$0@3|@|@2%100%80%10%84%17%69%76$2@%10%17%89%96%96%96%96%70@5$1@%93$0@%43%87|@2||$1@$0@3$3$0@2|@|@2%80%10%84%17%69%76$2@%10%17%89%96%96%96%96%96%70@6$1@%93$0@%43%87|@2||$1@$0@3$3$0@2|@|@7$1$0@2|@2$0$1@2|@|@$0@|@"])
  fun op hmlForm_case_def x = x
    val op hmlForm_case_def =
    ThmBind.DT(((("hmlFoundation",14),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13]),
                 ("ind_type",[33,34])]),["DISK_THM"]),
               [ThmBind.read"%69%52%45%52%48%61%21%61%26%58%29%58%33%71%103%107@$5@$4@$3@$2@$1@$0@2$5@|@|@|@|@|@|@2%69%52%45%52%48%61%21%61%26%58%29%58%33%71%103%102@$5@$4@$3@$2@$1@$0@2$4@|@|@|@|@|@|@2%69%65%11%65%16%52%45%52%48%61%21%61%26%58%29%58%33%71%103%101$7@$6@2$5@$4@$3@$2@$1@$0@2$3$7@$6@2|@|@|@|@|@|@|@|@2%69%65%11%65%16%52%45%52%48%61%21%61%26%58%29%58%33%71%103%106$7@$6@2$5@$4@$3@$2@$1@$0@2$2$7@$6@2|@|@|@|@|@|@|@|@2%69%55%10%65%16%52%45%52%48%61%21%61%26%58%29%58%33%71%103%88$7@$6@2$5@$4@$3@$2@$1@$0@2$1$7@$6@2|@|@|@|@|@|@|@|@2%55%10%65%16%52%45%52%48%61%21%61%26%58%29%58%33%71%103%91$7@$6@2$5@$4@$3@$2@$1@$0@2$0$7@$6@2|@|@|@|@|@|@|@|@6"])
  fun op hmlForm_size_def x = x
    val op hmlForm_size_def =
    ThmBind.DT(((("hmlFoundation",15),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13]),
                 ("ind_type",[33,34])]),["DISK_THM"]),
               [ThmBind.read"%69%57%20%75%104$0@%107@2%70@|@2%69%57%20%75%104$0@%102@2%70@|@2%69%57%20%65%11%65%16%75%104$2@%101$1@$0@3%67%95%86%99@3%67%104$2@$1@2%104$2@$0@4|@|@|@2%69%57%20%65%11%65%16%75%104$2@%106$1@$0@3%67%95%86%99@3%67%104$2@$1@2%104$2@$0@4|@|@|@2%69%57%20%55%10%65%16%75%104$2@%88$1@$0@3%67%95%86%99@3%104$2@$0@3|@|@|@2%57%20%55%10%65%16%75%104$2@%91$1@$0@3%67%95%86%99@3%104$2@$0@3|@|@|@6"])
  fun op datatype_hmlForm x = x
    val op datatype_hmlForm =
    ThmBind.DT(((("hmlFoundation",16),[("bool",[25,170])]),["DISK_THM"]),
               [ThmBind.read"%90%42%107@%102@%101@%106@%88@%91@2"])
  fun op hmlForm_11 x = x
    val op hmlForm_11 =
    ThmBind.DT(((("hmlFoundation",17),
                [("bool",[14,25,26,30,50,52,55,62,131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13]),
                 ("ind_type",[33,34])]),["DISK_THM"]),
               [ThmBind.read"%69%65%11%65%16%65%14%65%18%72%74%101$3@$2@2%101$1@$0@3%69%74$3@$1@2%74$2@$0@3|@|@|@|@2%69%65%11%65%16%65%14%65%18%72%74%106$3@$2@2%106$1@$0@3%69%74$3@$1@2%74$2@$0@3|@|@|@|@2%69%55%10%65%16%55%13%65%18%72%74%88$3@$2@2%88$1@$0@3%69%73$3@$1@2%74$2@$0@3|@|@|@|@2%55%10%65%16%55%13%65%18%72%74%91$3@$2@2%91$1@$0@3%69%73$3@$1@2%74$2@$0@3|@|@|@|@4"])
  fun op hmlForm_distinct x = x
    val op hmlForm_distinct =
    ThmBind.DT(((("hmlFoundation",18),
                [("bool",
                 [14,25,26,30,35,46,50,52,53,55,62,131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13]),
                 ("ind_type",[33,34])]),["DISK_THM"]),
               [ThmBind.read"%69%108%74%107@%102@3%69%65%16%65%11%108%74%107@%101$0@$1@3|@|@2%69%65%16%65%11%108%74%107@%106$0@$1@3|@|@2%69%65%16%55%10%108%74%107@%88$0@$1@3|@|@2%69%65%16%55%10%108%74%107@%91$0@$1@3|@|@2%69%65%16%65%11%108%74%102@%101$0@$1@3|@|@2%69%65%16%65%11%108%74%102@%106$0@$1@3|@|@2%69%65%16%55%10%108%74%102@%88$0@$1@3|@|@2%69%65%16%55%10%108%74%102@%91$0@$1@3|@|@2%69%65%18%65%16%65%14%65%11%108%74%101$0@$2@2%106$1@$3@3|@|@|@|@2%69%65%18%65%16%55%13%65%11%108%74%101$0@$2@2%88$1@$3@3|@|@|@|@2%69%65%18%65%16%55%13%65%11%108%74%101$0@$2@2%91$1@$3@3|@|@|@|@2%69%65%18%65%16%55%13%65%11%108%74%106$0@$2@2%88$1@$3@3|@|@|@|@2%69%65%18%65%16%55%13%65%11%108%74%106$0@$2@2%91$1@$3@3|@|@|@|@2%65%18%65%16%55%13%55%10%108%74%88$0@$2@2%91$1@$3@3|@|@|@|@15"])
  fun op hmlForm_case_cong x = x
    val op hmlForm_case_cong =
    ThmBind.DT(((("hmlFoundation",19),
                [("bool",[14,25,26,52,131,132,137,180]),
                 ("hmlFoundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13,14])]),["DISK_THM"]),
               [ThmBind.read"%65%4%65%5%52%45%52%48%61%21%61%26%58%29%58%33%77%69%74$7@$6@2%69%77%74$6@%107@2%71$5@%47@3%69%77%74$6@%102@2%71$4@%50@3%69%65%11%65%16%77%74$8@%101$1@$0@3%71$5$1@$0@2%23$1@$0@3|@|@2%69%65%11%65%16%77%74$8@%106$1@$0@3%71$4$1@$0@2%28$1@$0@3|@|@2%69%55%10%65%16%77%74$8@%88$1@$0@3%71$3$1@$0@2%32$1@$0@3|@|@2%55%10%65%16%77%74$8@%91$1@$0@3%71$2$1@$0@2%35$1@$0@3|@|@8%71%103$7@$5@$4@$3@$2@$1@$0@2%103$6@%47@%50@%23@%28@%32@%35@3|@|@|@|@|@|@|@|@"])
  fun op hmlForm_nchotomy x = x
    val op hmlForm_nchotomy =
    ThmBind.DT(((("hmlFoundation",20),
                [("bool",[14,25,26,52,131,132,137,180]),
                 ("hmlFoundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13])]),["DISK_THM"]),
               [ThmBind.read"%65%41%100%74$0@%107@2%100%74$0@%102@2%100%83%39%83%40%74$2@%101$1@$0@2|@|@2%100%83%39%83%40%74$2@%106$1@$0@2|@|@2%100%80%19%83%39%74$2@%88$1@$0@2|@|@2%80%19%83%39%74$2@%91$1@$0@2|@|@6|@"])
  fun op hmlForm_Axiom x = x
    val op hmlForm_Axiom =
    ThmBind.DT(((("hmlFoundation",21),
                [("bool",[14,25,26,30,52,62,131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13]),
                 ("ind_type",[33,34])]),["DISK_THM"]),
               [ThmBind.read"%52%24%52%25%62%30%62%34%59%36%59%37%81%38%69%71$0%107@2$6@2%69%71$0%102@2$5@2%69%65%11%65%16%71$2%101$1@$0@3$6$1@$0@$2$1@2$2$0@3|@|@2%69%65%11%65%16%71$2%106$1@$0@3$5$1@$0@$2$1@2$2$0@3|@|@2%69%55%10%65%16%71$2%88$1@$0@3$4$1@$0@$2$0@3|@|@2%55%10%65%16%71$2%91$1@$0@3$3$1@$0@$2$0@3|@|@6|@|@|@|@|@|@|@"])
  fun op hmlForm_induction x = x
    val op hmlForm_induction =
    ThmBind.DT(((("hmlFoundation",22),
                [("bool",[14,25,26,52,131,132,137]),
                 ("hmlFoundation",
                 [1,2,3,4,5,6,7,8,9,10,11,12,13])]),["DISK_THM"]),
               [ThmBind.read"%60%6%77%69$0%107@2%69$0%102@2%69%65%39%65%40%77%69$2$1@2$2$0@3$2%101$1@$0@3|@|@2%69%65%39%65%40%77%69$2$1@2$2$0@3$2%106$1@$0@3|@|@2%69%65%39%77$1$0@2%55%19$2%88$0@$1@2|@2|@2%65%39%77$1$0@2%55%19$2%91$0@$1@2|@2|@7%65%39$1$0@|@2|@"])
  fun op hmsat_ind x = x
    val op hmsat_ind =
    ThmBind.DT(((("hmlFoundation",25),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [14,25,26,27,35,50,51,52,53,57,62,92,95,103,104,106,123,
                  131,132,137,180]),
                 ("hmlFoundation",[1,2,3,4,5,6,7,8,9,10,11,12,13,15]),
                 ("numeral",[3,7,8]),("pair",[5,7,9,16]),("prim_rec",[42]),
                 ("relation",[107,119,121]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%63%7%77%69%54%2%56%8$2%68$1@$0@2%107@|@|@2%69%54%2%56%8$2%68$1@$0@2%102@|@|@2%69%54%2%56%8%65%27%65%31%77%69$4%68$3@$2@2$1@2$4%68$3@$2@2$0@3$4%68$3@$2@2%101$1@$0@3|@|@|@|@2%69%54%2%56%8%65%27%65%31%77%69$4%68$3@$2@2$1@2$4%68$3@$2@2$0@3$4%68$3@$2@2%106$1@$0@3|@|@|@|@2%69%54%2%56%8%55%1%65%22%77%53%9%54%3%77%69$4$1@$5@$0@2%94$1@$3@3$6%68$0@$4@2$2@2|@|@2$4%68$3@$2@2%88$1@$0@3|@|@|@|@2%54%2%56%8%55%1%65%22%77%54%3$5%68$0@$3@2$1@|@2$4%68$3@$2@2%91$1@$0@3|@|@|@|@7%54%46%56%49%65%51$3%68$2@$1@2$0@|@|@|@2|@"])
  fun op hmsat_def x = x
    val op hmsat_def =
    ThmBind.DT(((("hmlFoundation",26),
                [("arithmetic",[24,25,26,27,41,46,59,73,95,179,186]),
                 ("bool",
                 [15,25,35,50,51,52,53,57,62,92,95,103,104,106,123]),
                 ("combin",[19]),("hmlFoundation",[14,15,23,24]),
                 ("numeral",[3,7,8]),("pair",[7,9,16,49]),
                 ("prim_rec",[42]),("relation",[119,121,127,132]),
                 ("sat",[1,3,5,6,7,11,12,13,15])]),["DISK_THM"]),
               [ThmBind.read"%69%56%8%54%2%72%105%68$0@$1@2%107@2%97@|@|@2%69%56%8%54%2%72%105%68$0@$1@2%102@2%92@|@|@2%69%65%31%65%27%56%8%54%2%72%105%68$0@$1@2%101$2@$3@3%69%105%68$0@$1@2$2@2%105%68$0@$1@2$3@3|@|@|@|@2%69%65%31%65%27%56%8%54%2%72%105%68$0@$1@2%106$2@$3@3%100%105%68$0@$1@2$2@2%105%68$0@$1@2$3@3|@|@|@|@2%69%65%22%56%8%54%2%55%1%72%105%68$1@$2@2%88$0@$3@3%54%3%53%9%77$4$0@$3@$1@2%77%94$0@$2@2%105%68$1@$4@2$5@3|@|@2|@|@|@|@2%65%22%56%8%54%2%55%1%72%105%68$1@$2@2%91$0@$3@3%79%3%78%9%69$4$0@$3@$1@2%69%94$0@$2@2%105%68$1@$4@2$5@3|@|@2|@|@|@|@6"])

  val _ = DB.bindl "hmlFoundation"
  [("hmlForm_TY_DEF",hmlForm_TY_DEF,DB.Def),
   ("hmlForm_case_def",hmlForm_case_def,DB.Def),
   ("hmlForm_size_def",hmlForm_size_def,DB.Def),
   ("datatype_hmlForm",datatype_hmlForm,DB.Thm),
   ("hmlForm_11",hmlForm_11,DB.Thm),
   ("hmlForm_distinct",hmlForm_distinct,DB.Thm),
   ("hmlForm_case_cong",hmlForm_case_cong,DB.Thm),
   ("hmlForm_nchotomy",hmlForm_nchotomy,DB.Thm),
   ("hmlForm_Axiom",hmlForm_Axiom,DB.Thm),
   ("hmlForm_induction",hmlForm_induction,DB.Thm),
   ("hmsat_ind",hmsat_ind,DB.Thm), ("hmsat_def",hmsat_def,DB.Thm)]

  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hmlFoundation",
    thydataty = "compute",
    read = ThmBind.read,
    data = "hmlFoundation.hmsat_def"
  }
  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hmlFoundation",
    thydataty = "TypeGrammarDeltas",
    read = ThmBind.read,
    data = "NTY13.hmlFoundation,7.hmlForm"
  }
  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "hmlFoundation",
    thydataty = "TermGrammarDeltas",
    read = ThmBind.read,
    data =
        "OO2.tt4.%107OO2.ff4.%102OO4.andh4.%101OO3.orh4.%106OO3.Box3.%88OO3.Dia3.%91OO12.hmlForm_CASE4.%103OO12.hmlForm_size4.%104OO4.case4.%103RMT4.andhG4.andhOCI0.IR580.H1.RK4.andhS1.0.XRMT3.orhG3.orhOCI0.IR570.H1.RK3.orhS1.0.XRMT5.hmsatG5.hmsatOCI0.IR540.H1.RK5.hmsatS1.0.XOO5.hmsat4.%105"
  }
  local open GrammarSpecials Parse
    fun UTOFF f = Feedback.trace("Parse.unicode_trace_off_complaints",0)f
  in
  val hmlFoundation_grammars = merge_grammars ["indexedLists",
                                               "patternMatches"]
  local
  val (tyUDs, tmUDs) = GrammarDeltas.thy_deltas{thyname="hmlFoundation"}
  val addtmUDs = term_grammar.add_deltas tmUDs
  val addtyUDs = type_grammar.apply_deltas tyUDs
  in
  val hmlFoundation_grammars = 
    Portable.## (addtyUDs,addtmUDs) hmlFoundation_grammars
  val _ = Parse.grammarDB_insert("hmlFoundation",hmlFoundation_grammars)
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
           size=SOME(Parse.Term`(hmlFoundation$hmlForm_size) :('action -> num$num) -> 'action hmlFoundation$hmlForm -> num$num`,
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
val _ = Theory.load_complete "hmlFoundation"
end
