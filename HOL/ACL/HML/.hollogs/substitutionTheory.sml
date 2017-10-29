<<HOL message: Created theory "substitution">>
Saved theorem _____ "COND_NOT"
Saved theorem _____ "COND_NOT_DISJ"
Saved theorem _____ "in_not_in_not_eq"
Saved theorem _____ "INSERT_INSERT_DELETE"
Saved definition __ "fmla_size_def"
Saved definition __ "extend_env_def"
Saved theorem _____ "extend_env_mmUpdate_lemma"
Saved theorem _____ "extend_env_mmUpdate_EQ"
Saved theorem _____ "last_extension_counts"
Saved theorem _____ "last_update_counts"
Saved theorem _____ "uneq_extensions_commute"
Saved theorem _____ "uneq_mmUpdates_commute"
Saved definition __ "setsat_def"
Saved theorem _____ "setsat_is_mmfn_UNIV"
Saved definition __ "frees_def"
Saved theorem _____ "frees_finite"
Saved definition __ "vars_def"
Saved theorem _____ "vars_finite"
Saved theorem _____ "UNION_SUBSET_MONOTONIC"
Saved theorem _____ "frees_SUBSET_vars"
Saved theorem _____ "frees_are_vars"
Saved theorem _____ "variant_EXISTS"
Saved theorem _____ "pair_list_induction"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("fv_tupled")
  invalidated by DelConstant(substitution$fv_tupled)>>
Saved definition __ "fv_def"
Saved induction ___ "fv_ind"
Saved definition __ "rf_def"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("gf_tupled")
  invalidated by DelConstant(substitution$gf_tupled)>>
Saved definition __ "gf_def"
Saved induction ___ "gf_ind"
Saved theorem _____ "gf_im"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("gl_tupled")
  invalidated by DelConstant(substitution$gl_tupled)>>
Saved definition __ "gl_def"
Saved induction ___ "gl_ind"
Saved definition __ "gv_def"
Saved definition __ "l_Sub_def"
Saved theorem _____ "l_Sub_same_size"
Saved theorem _____ "l_Sub_nil"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("Subst_defn_tupled")
  invalidated by DelConstant(substitution$Subst_defn_tupled)>>
Saved theorem _____ "Subst_def"
Saved theorem _____ "Subst_ind"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("ok_r_tupled")
  invalidated by DelConstant(substitution$ok_r_tupled)>>
Saved definition __ "ok_r_def"
Saved induction ___ "ok_r_ind"
Saved theorem _____ "fv_IN_gf"
Saved theorem _____ "fv_inj"
Saved theorem _____ "fv_1_1"
Saved theorem _____ "fv_BIJ"
Saved theorem _____ "gf_empty"
Saved theorem _____ "fv_LEM"
Saved theorem _____ "gf_union"
Saved theorem _____ "gf_insert"
Saved theorem _____ "gf_monotone"
Saved theorem _____ "gf_finite"
Saved theorem _____ "ok_r_subset"
Saved theorem _____ "fv_not_in"
Saved theorem _____ "gf_delete"
Saved theorem _____ "variant_not_in"
Saved theorem _____ "not_in_gf"
Saved theorem _____ "ok_r_gl_insert"
Saved theorem _____ "half_gl_ok"
Saved theorem _____ "muvar_not_free"
Saved theorem _____ "nuvar_not_free"
Saved theorem _____ "simple_ok_r_gl_nu"
Saved theorem _____ "simple_ok_r_gl_mu"
Saved theorem _____ "simple_ok_r_gl"
Saved theorem _____ "frees_LEM"
Saved theorem _____ "fv_append"
Saved theorem _____ "gf_append"
Saved theorem _____ "gl_append"
Saved theorem _____ "l_Sub_append"
Saved theorem _____ "fmla_size_ind"
Saved theorem _____ "fmla_size_induction"
Saved theorem _____ "l_Sub_ID"
Saved theorem _____ "l_Sub_ID_CONS"
Saved theorem _____ "Subst_l_Sub"
Saved theorem _____ "Subst"
Saved theorem _____ "Subst_same_size"
Saved theorem _____ "Subst_not_free"
Saved theorem _____ "alpha_frees"
Saved theorem _____ "alpha_remove"
Saved theorem _____ "setsat_lemma"
Saved theorem _____ "EQ_SUBSET_SUBSET"
Saved theorem _____ "silly_extend"
Saved theorem _____ "alpha_LEM"
Saved theorem _____ "Subst_LEM"
Saved theorem _____ "lfp_monotone"
Saved theorem _____ "gfp_monotone"
Saved theorem _____ "setsat_EQ_satFun"
Saved theorem _____ "setsat_monotone"
Saved theorem _____ "ok_to_unroll_mu"
Saved theorem _____ "ok_to_unroll_nu"
Saved theorem _____ "mmsat_setsat"
Saved theorem _____ "unfold_nu_LEM"
Saved theorem _____ "unfold_mu_LEM"
Theory: substitution

Parents:
    mmFoundation
    res_quan

Term constants:
    Subst        :(α, β) mmForm -> β -> (α, β) mmForm -> (α, β) mmForm
    extend_env   :α -> β -> (α -> β) -> α -> β
    fmla_size    :(α, β) mmForm -> num
    frees        :(β, α) mmForm -> α -> bool
    fv           :('propvar # 'propvar) list -> 'propvar -> 'propvar
    gf           :('propvar # 'propvar) list ->
                  ('propvar -> bool) -> 'propvar -> bool
    gl           :(α # α) list -> α -> (α -> bool) -> (α # α) list
    gv           :(α # α) list -> α -> (α -> bool) -> α
    l_Sub        :(β # β) list -> (α, β) mmForm -> (α, β) mmForm
    ok_r         :(α # α) list -> (α -> bool) -> bool
    rf           :α -> α -> (α -> bool) -> α -> bool
    setsat       :(β -> α -> α -> bool) ->
                  (β, γ) mmForm -> (γ -> α -> bool) -> α -> bool
    variant      :('variable -> bool) -> 'variable -> 'variable
    vars         :(β, α) mmForm -> α -> bool

Definitions:
    Subst_defn_curried_def
      |- ∀x x1 x2.
           Subst x x1 x2 =
           substitution$old3->Subst_defn_tupled<-old (x,x1,x2)
    Subst_defn_tupled_primitive_def
      |- substitution$old3->Subst_defn_tupled<-old =
         WFREC
           (@R.
              WF R ∧ (∀P Q X N. R (N,X,Q) (N,X,P andmm Q)) ∧
              (∀Q P X N. R (N,X,P) (N,X,P andmm Q)) ∧
              (∀P Q X N. R (N,X,Q) (N,X,P ormm Q)) ∧
              (∀Q P X N. R (N,X,P) (N,X,P ormm Q)) ∧
              (∀Actions P X N. R (N,X,P) (N,X,Box Actions P)) ∧
              (∀Actions P X N. R (N,X,P) (N,X,Dia Actions P)) ∧
              (∀N Z X P fs.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∉ frees N ⇒
                 R (N,X,P) (N,X,nu Z P)) ∧
              (∀N Z X P fs W.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∈ frees N ∧
                 (W = variant (frees N ∪ fs) Z) ⇒
                 R (N,X,l_Sub [(Z,W)] P) (N,X,nu Z P)) ∧
              (∀N Z X P fs W.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∈ frees N ∧
                 (W = variant (frees N ∪ fs) Z) ⇒
                 R (N,X,l_Sub [(Z,W)] P) (N,X,mu Z P)) ∧
              ∀N Z X P fs.
                (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∉ frees N ⇒
                R (N,X,P) (N,X,mu Z P))
           (λSubst_defn_tupled a.
              case a of
                (N,X,tt) => I tt
              | (N,X,ff) => I ff
              | (N,X,propmm Y) => I (if Y = X then N else propmm Y)
              | (N,X,P andmm Q) =>
                  I
                    (Subst_defn_tupled (N,X,P) andmm
                     Subst_defn_tupled (N,X,Q))
              | (N,X,P' ormm Q') =>
                  I
                    (Subst_defn_tupled (N,X,P') ormm
                     Subst_defn_tupled (N,X,Q'))
              | (N,X,Box Actions P'') =>
                  I (Box Actions (Subst_defn_tupled (N,X,P'')))
              | (N,X,Dia Actions' P''') =>
                  I (Dia Actions' (Subst_defn_tupled (N,X,P''')))
              | (N,X,nu Z P'''') =>
                  I
                    (let
                       fs = frees P''''
                     in
                       if X ∉ fs ∨ (X = Z) then nu Z P''''
                       else if Z ∈ frees N then
                         (let
                            W = variant (frees N ∪ fs) Z
                          in
                            nu W
                              (Subst_defn_tupled
                                 (N,X,l_Sub [(Z,W)] P'''')))
                       else nu Z (Subst_defn_tupled (N,X,P'''')))
              | (N,X,mu Z' P''''') =>
                  I
                    (let
                       fs = frees P'''''
                     in
                       if X ∉ fs ∨ (X = Z') then mu Z' P'''''
                       else if Z' ∈ frees N then
                         (let
                            W = variant (frees N ∪ fs) Z'
                          in
                            mu W
                              (Subst_defn_tupled
                                 (N,X,l_Sub [(Z',W)] P''''')))
                       else mu Z' (Subst_defn_tupled (N,X,P'''''))))
    extend_env_def
      |- ∀x v f. extend_env x v f = (λy. if y = x then v else f y)
    fmla_size_def
      |- (fmla_size tt = 0) ∧ (fmla_size ff = 0) ∧
         (∀Z. fmla_size (propmm Z) = 1) ∧
         (∀f1 f2.
            fmla_size (f1 andmm f2) = 1 + fmla_size f1 + fmla_size f2) ∧
         (∀f1 f2.
            fmla_size (f1 ormm f2) = 1 + fmla_size f1 + fmla_size f2) ∧
         (∀Actions f. fmla_size (Box Actions f) = 1 + fmla_size f) ∧
         (∀Actions f. fmla_size (Dia Actions f) = 1 + fmla_size f) ∧
         (∀Z f. fmla_size (nu Z f) = 1 + fmla_size f) ∧
         ∀Z f. fmla_size (mu Z f) = 1 + fmla_size f
    frees_def
      |- (frees tt = ∅) ∧ (frees ff = ∅) ∧
         (∀Z. frees (propmm Z) = {Z}) ∧
         (∀f1 f2. frees (f1 andmm f2) = frees f1 ∪ frees f2) ∧
         (∀f1 f2. frees (f1 ormm f2) = frees f1 ∪ frees f2) ∧
         (∀Actions f. frees (Box Actions f) = frees f) ∧
         (∀Actions f. frees (Dia Actions f) = frees f) ∧
         (∀Z f. frees (nu Z f) = frees f DELETE Z) ∧
         ∀Z f. frees (mu Z f) = frees f DELETE Z
    fv_curried_def
      |- ∀x x1. fv x x1 = substitution$old0->fv_tupled<-old (x,x1)
    fv_tupled_primitive_def
      |- substitution$old0->fv_tupled<-old =
         WFREC (@R. WF R ∧ ∀Z Y X l. R (l,X) ((Y,Z)::l,X))
           (λfv_tupled a.
              case a of
                ([],X) => I X
              | ((Y,Z)::l,X) =>
                  I
                    (let
                       X' = fv_tupled (l,X)
                     in
                       if X' = Y then Z else X'))
    gf_curried_def
      |- ∀x x1. gf x x1 = substitution$old1->gf_tupled<-old (x,x1)
    gf_tupled_primitive_def
      |- substitution$old1->gf_tupled<-old =
         WFREC (@R. WF R ∧ ∀Y X fs l. R (l,fs) ((X,Y)::l,fs))
           (λgf_tupled a.
              case a of
                ([],fs) => I fs
              | ((X,Y)::l,fs) => I (rf Y X (gf_tupled (l,fs))))
    gl_curried_def
      |- ∀x x1 x2.
           gl x x1 x2 = substitution$old2->gl_tupled<-old (x,x1,x2)
    gl_tupled_primitive_def
      |- substitution$old2->gl_tupled<-old =
         WFREC (@R. WF R ∧ ∀Y X fs Z l. R (l,Z,fs) ((X,Y)::l,Z,fs))
           (λgl_tupled a.
              case a of
                ([],v1) => I []
              | ((X,Y)::l,Z,fs) =>
                  I
                    (let
                       l' = gl_tupled (l,Z,fs) ;
                       (fs',Z') = (gf l' fs,fv l' Z)
                     in
                       if X ∉ fs' ∨ (X = Z') then l'
                       else if Y = Z' then
                         (X,Y)::(Z',variant (Y INSERT fs') Z')::l'
                       else (X,Y)::l'))
    gv_def
      |- ∀l Z fs. gv l Z fs = fv (gl l Z fs) Z
    l_Sub_def
      |- (∀l. l_Sub l tt = tt) ∧ (∀l. l_Sub l ff = ff) ∧
         (∀l Y. l_Sub l (propmm Y) = propmm (fv l Y)) ∧
         (∀l P Q. l_Sub l (P andmm Q) = l_Sub l P andmm l_Sub l Q) ∧
         (∀l P Q. l_Sub l (P ormm Q) = l_Sub l P ormm l_Sub l Q) ∧
         (∀l Actions P.
            l_Sub l (Box Actions P) = Box Actions (l_Sub l P)) ∧
         (∀l Actions P.
            l_Sub l (Dia Actions P) = Dia Actions (l_Sub l P)) ∧
         (∀l Z P.
            l_Sub l (nu Z P) =
            (let
               fs = frees P ;
               (Z',l') = (gv l Z fs,gl l Z fs)
             in
               nu Z' (l_Sub l' P))) ∧
         ∀l Z P.
           l_Sub l (mu Z P) =
           (let
              fs = frees P ;
              (Z',l') = (gv l Z fs,gl l Z fs)
            in
              mu Z' (l_Sub l' P))
    ok_r_curried_def
      |- ∀x x1. ok_r x x1 ⇔ substitution$old4->ok_r_tupled<-old (x,x1)
    ok_r_tupled_primitive_def
      |- substitution$old4->ok_r_tupled<-old =
         WFREC (@R. WF R ∧ ∀Y X fs l. R (l,fs) ((X,Y)::l,fs))
           (λok_r_tupled a.
              case a of
                ([],fs) => I T
              | ((X,Y)::l,fs) =>
                  I (ok_r_tupled (l,fs) ∧ (X ∈ gf l fs ⇒ Y ∉ gf l fs)))
    rf_def
      |- ∀Y X fs.
           rf Y X fs = if X ∈ fs then Y INSERT fs DELETE X else fs
    setsat_def
      |- ∀Trans f V. setsat Trans f V = {E | (E,Trans,V) mmsat f}
    variant_spec
      |- ∀exclvars.
           INFINITE 𝕌(:'variable) ⇒
           FINITE exclvars ⇒
           ∀v. variant exclvars v ∉ exclvars
    vars_def
      |- (vars tt = ∅) ∧ (vars ff = ∅) ∧ (∀Z. vars (propmm Z) = {Z}) ∧
         (∀f1 f2. vars (f1 andmm f2) = vars f1 ∪ vars f2) ∧
         (∀f1 f2. vars (f1 ormm f2) = vars f1 ∪ vars f2) ∧
         (∀Actions f. vars (Box Actions f) = vars f) ∧
         (∀Actions f. vars (Dia Actions f) = vars f) ∧
         (∀Z f. vars (nu Z f) = vars f ∪ {Z}) ∧
         ∀Z f. vars (mu Z f) = vars f ∪ {Z}

Theorems:
    COND_NOT
      |- ∀P A B. (if ¬P then A else B) = if P then B else A
    COND_NOT_DISJ
      |- ∀P Q A B.
           (if ¬Q ∨ P then A else B) =
           if P then A else if Q then B else A
    EQ_SUBSET_SUBSET
      |- ∀s1 s2. (s1 = s2) ⇔ s1 ⊆ s2 ∧ s2 ⊆ s1
    INSERT_INSERT_DELETE
      |- ∀a t. a INSERT t DELETE a = a INSERT t
    Subst
      |- INFINITE 𝕌(:β) ⇒
         (Subst p X tt = tt) ∧ (Subst p X ff = ff) ∧
         (Subst p X (propmm Z) = if Z = X then p else propmm Z) ∧
         (Subst p X (Fm1 andmm Fm2) =
          Subst p X Fm1 andmm Subst p X Fm2) ∧
         (Subst p X (Fm1 ormm Fm2) = Subst p X Fm1 ormm Subst p X Fm2) ∧
         (Subst p X (Box Actions Fm) = Box Actions (Subst p X Fm)) ∧
         (Subst p X (Dia Actions Fm) = Dia Actions (Subst p X Fm)) ∧
         (Subst p X (nu Z Fm) =
          (let
             fs = frees Fm
           in
             if X ∉ frees (nu Z Fm) then nu Z Fm
             else if Z ∈ frees p then
               (let
                  Z' = variant (frees p ∪ fs) Z
                in
                  nu Z' (Subst p X (Subst (propmm Z') Z Fm)))
             else nu Z (Subst p X Fm))) ∧
         (Subst p X (mu Z Fm) =
          (let
             fs = frees Fm
           in
             if X ∉ frees (mu Z Fm) then mu Z Fm
             else if Z ∈ frees p then
               (let
                  Z' = variant (frees p ∪ fs) Z
                in
                  mu Z' (Subst p X (Subst (propmm Z') Z Fm)))
             else mu Z (Subst p X Fm)))
    Subst_LEM
      |- ∀Trans Fm p Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst p Z Fm) V =
            setsat Trans Fm (extend_env Z (setsat Trans p V) V))
    Subst_def
      |- (Subst N X tt = tt) ∧ (Subst N X ff = ff) ∧
         (Subst N X (propmm Y) = if Y = X then N else propmm Y) ∧
         (Subst N X (P andmm Q) = Subst N X P andmm Subst N X Q) ∧
         (Subst N X (P ormm Q) = Subst N X P ormm Subst N X Q) ∧
         (Subst N X (Box Actions P) = Box Actions (Subst N X P)) ∧
         (Subst N X (Dia Actions P) = Dia Actions (Subst N X P)) ∧
         (Subst N X (nu Z P) =
          (let
             fs = frees P
           in
             if X ∉ fs ∨ (X = Z) then nu Z P
             else if Z ∈ frees N then
               (let
                  W = variant (frees N ∪ fs) Z
                in
                  nu W (Subst N X (l_Sub [(Z,W)] P)))
             else nu Z (Subst N X P))) ∧
         (Subst N X (mu Z P) =
          (let
             fs = frees P
           in
             if X ∉ fs ∨ (X = Z) then mu Z P
             else if Z ∈ frees N then
               (let
                  W = variant (frees N ∪ fs) Z
                in
                  mu W (Subst N X (l_Sub [(Z,W)] P)))
             else mu Z (Subst N X P)))
    Subst_ind
      |- ∀P'.
           (∀N X. P' N X tt) ∧ (∀N X. P' N X ff) ∧
           (∀N X Y. P' N X (propmm Y)) ∧
           (∀N X P Q. P' N X P ∧ P' N X Q ⇒ P' N X (P andmm Q)) ∧
           (∀N X P Q. P' N X P ∧ P' N X Q ⇒ P' N X (P ormm Q)) ∧
           (∀N X Actions P. P' N X P ⇒ P' N X (Box Actions P)) ∧
           (∀N X Actions P. P' N X P ⇒ P' N X (Dia Actions P)) ∧
           (∀N X Z P.
              (∀fs W.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∈ frees N ∧
                 (W = variant (frees N ∪ fs) Z) ⇒
                 P' N X (l_Sub [(Z,W)] P)) ∧
              (∀fs.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∉ frees N ⇒
                 P' N X P) ⇒
              P' N X (nu Z P)) ∧
           (∀N X Z P.
              (∀fs W.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∈ frees N ∧
                 (W = variant (frees N ∪ fs) Z) ⇒
                 P' N X (l_Sub [(Z,W)] P)) ∧
              (∀fs.
                 (fs = frees P) ∧ ¬(X ∉ fs ∨ (X = Z)) ∧ Z ∉ frees N ⇒
                 P' N X P) ⇒
              P' N X (mu Z P)) ⇒
           ∀v v1 v2. P' v v1 v2
    Subst_l_Sub
      |- ∀f X Y.
           INFINITE 𝕌(:β) ⇒ (Subst (propmm Y) X f = l_Sub [(X,Y)] f)
    Subst_not_free
      |- ∀N X Fm. INFINITE 𝕌(:β) ⇒ X ∉ frees Fm ⇒ (Subst N X Fm = Fm)
    Subst_same_size
      |- ∀Fm X Z.
           INFINITE 𝕌(:β) ⇒
           (fmla_size (Subst (propmm X) Z Fm) = fmla_size Fm)
    UNION_SUBSET_MONOTONIC
      |- x1 ⊆ y1 ⇒ x2 ⊆ y2 ⇒ x1 ∪ x2 ⊆ y1 ∪ y2
    alpha_LEM
      |- ∀Trans Fm V Q X X'.
           INFINITE 𝕌(:β) ⇒
           X' ∉ frees Fm ⇒
           (setsat Trans (Subst (propmm X') X Fm) (extend_env X' Q V) =
            setsat Trans Fm (extend_env X Q V))
    alpha_frees
      |- ∀Y X Fm.
           INFINITE 𝕌(:β) ⇒
           Y ∉ frees Fm ⇒
           (frees (Subst (propmm Y) X Fm) = rf Y X (frees Fm))
    alpha_remove
      |- ∀Y X Fm.
           INFINITE 𝕌(:β) ⇒
           Y ∉ frees Fm ∧ Y ≠ X ⇒
           X ∉ frees (Subst (propmm Y) X Fm)
    extend_env_mmUpdate_EQ
      |- extend_env Z E V = mmUpdate Z V E
    extend_env_mmUpdate_lemma
      |- extend_env Z E V Y = mmUpdate Z V E Y
    fmla_size_ind
      |- ∀P.
           (∀f. (∀g. fmla_size g < fmla_size f ⇒ P g) ⇒ P f) ⇒
           ∀n f. (fmla_size f = n) ⇒ P f
    fmla_size_induction
      |- ∀P.
           P tt ∧ P ff ∧ (∀s. P (propmm s)) ∧
           (∀f g. P f ∧ P g ⇒ P (f andmm g)) ∧
           (∀f g. P f ∧ P g ⇒ P (f ormm g)) ∧
           (∀Actions f. P f ⇒ P (Box Actions f)) ∧
           (∀Actions f. P f ⇒ P (Dia Actions f)) ∧
           (∀f.
              (∀g. (fmla_size g = fmla_size f) ⇒ P g) ⇒
              ∀s. P (nu s f)) ∧
           (∀f.
              (∀g. (fmla_size g = fmla_size f) ⇒ P g) ⇒
              ∀s. P (mu s f)) ⇒
           ∀f. P f
    frees_LEM
      |- ∀Fm l.
           INFINITE 𝕌(:β) ⇒
           ok_r l (frees Fm) ⇒
           (frees (l_Sub l Fm) = gf l (frees Fm))
    frees_SUBSET_vars
      |- ∀f. frees f ⊆ vars f
    frees_are_vars
      |- ∀f x. x ∈ frees f ⇒ x ∈ vars f
    frees_finite
      |- ∀f. FINITE (frees f)
    fv_1_1
      |- ∀l fs.
           ok_r l fs ⇒
           ∀A B. A ∈ fs ∧ B ∈ fs ⇒ ((fv l A = fv l B) ⇔ (A = B))
    fv_BIJ
      |- ∀l fs. ok_r l fs ⇒ BIJ (fv l) fs (gf l fs)
    fv_IN_gf
      |- ∀l fs (A::fs). fv l A ∈ gf l fs
    fv_LEM
      |- ∀l s. gf l {s} = {fv l s}
    fv_append
      |- ∀l m. fv (l ++ m) = fv l ∘ fv m
    fv_def
      |- (∀X. fv [] X = X) ∧
         ∀l Z Y X.
           fv ((Y,Z)::l) X =
           (let X' = fv l X in if X' = Y then Z else X')
    fv_ind
      |- ∀P.
           (∀X. P [] X) ∧ (∀Y Z l X. P l X ⇒ P ((Y,Z)::l) X) ⇒
           ∀v v1. P v v1
    fv_inj
      |- ∀l fs.
           ok_r l fs ⇒
           ∀A B. A ∈ fs ∧ B ∈ fs ⇒ (fv l A = fv l B) ⇒ (A = B)
    fv_not_in
      |- ∀fs gs Z l.
           ok_r l fs ∧ gs ⊆ fs ∧ Z ∈ fs ∧ Z ∉ gs ⇒ fv l Z ∉ gf l gs
    gf_append
      |- ∀l m fs. gf (l ++ m) fs = gf l (gf m fs)
    gf_def
      |- (∀fs. gf [] fs = fs) ∧
         ∀l fs Y X. gf ((X,Y)::l) fs = rf Y X (gf l fs)
    gf_delete
      |- ∀l fs Z.
           ok_r l (Z INSERT fs) ⇒
           (gf l (fs DELETE Z) = gf l fs DELETE fv l Z)
    gf_empty
      |- ∀l. gf l ∅ = ∅
    gf_finite
      |- ∀fs. FINITE fs ⇒ ∀l. FINITE (gf l fs)
    gf_im
      |- ∀l. gf l = IMAGE (fv l)
    gf_ind
      |- ∀P.
           (∀fs. P [] fs) ∧ (∀X Y l fs. P l fs ⇒ P ((X,Y)::l) fs) ⇒
           ∀v v1. P v v1
    gf_insert
      |- ∀l fs Z. gf l (Z INSERT fs) = fv l Z INSERT gf l fs
    gf_monotone
      |- ∀l big sma. sma ⊆ big ⇒ gf l sma ⊆ gf l big
    gf_union
      |- ∀l fs fs'. gf l (fs ∪ fs') = gf l fs ∪ gf l fs'
    gfp_monotone
      |- ∀G H. (∀s. G s ⊆ H s) ⇒ gfp G ⊆ gfp H
    gl_append
      |- ∀Z fs m l.
           gl (l ++ m) Z fs =
           gl l (fv (gl m Z fs) Z) (gf (gl m Z fs) fs) ++ gl m Z fs
    gl_def
      |- (∀fs Z. gl [] Z fs = []) ∧
         ∀l fs Z Y X.
           gl ((X,Y)::l) Z fs =
           (let
              l' = gl l Z fs ;
              (fs',Z') = (gf l' fs,fv l' Z)
            in
              if X ∉ fs' ∨ (X = Z') then l'
              else if Y = Z' then
                (X,Y)::(Z',variant (Y INSERT fs') Z')::l'
              else (X,Y)::l')
    gl_ind
      |- ∀P.
           (∀Z fs. P [] Z fs) ∧
           (∀X Y l Z fs. P l Z fs ⇒ P ((X,Y)::l) Z fs) ⇒
           ∀v v1 v2. P v v1 v2
    half_gl_ok
      |- ∀l Z fs.
           INFINITE 𝕌(:α) ∧ FINITE fs ∧ ok_r l (fs DELETE Z) ⇒
           ok_r (gl l Z fs) (Z INSERT fs)
    in_not_in_not_eq
      |- ∀X Y s. X ∈ s ∧ Y ∉ s ⇒ X ≠ Y
    l_Sub_ID
      |- ∀Fm. l_Sub [(X,X)] Fm = Fm
    l_Sub_ID_CONS
      |- ∀f l. l_Sub ((X,X)::l) f = l_Sub l f
    l_Sub_append
      |- ∀P l m.
           INFINITE 𝕌(:β) ⇒
           ok_r m (frees P) ⇒
           (l_Sub (l ++ m) P = l_Sub l (l_Sub m P))
    l_Sub_nil
      |- ∀Fm. l_Sub [] Fm = Fm
    l_Sub_same_size
      |- ∀Fm l. fmla_size (l_Sub l Fm) = fmla_size Fm
    last_extension_counts
      |- ∀x v v' f.
           extend_env x v (extend_env x v' f) = extend_env x v f
    last_update_counts
      |- ∀x v v' f. mmUpdate x (mmUpdate x f v') v = mmUpdate x f v
    lfp_monotone
      |- ∀G H. (∀s. G s ⊆ H s) ⇒ lfp G ⊆ lfp H
    mmsat_setsat
      |- (E,Trans,V) mmsat f ⇔ E ∈ setsat Trans f V
    muvar_not_free
      |- ∀s Fm. s ∉ frees (mu s Fm)
    not_in_gf
      |- ∀A excl l fs Q.
           INFINITE 𝕌(:α) ⇒
           FINITE excl ⇒
           A ∉ gf ((A,variant (A INSERT excl) Q)::l) fs
    nuvar_not_free
      |- ∀s Fm. s ∉ frees (nu s Fm)
    ok_r_def
      |- (∀fs. ok_r [] fs ⇔ T) ∧
         ∀l fs Y X.
           ok_r ((X,Y)::l) fs ⇔ ok_r l fs ∧ (X ∈ gf l fs ⇒ Y ∉ gf l fs)
    ok_r_gl_insert
      |- ∀l Z fs.
           INFINITE 𝕌(:α) ∧ FINITE fs ∧ ok_r l (fs DELETE Z) ⇒
           ok_r (gl l Z fs) (Z INSERT fs) ∧
           ∀X::fs DELETE Z. fv (gl l Z fs) X = fv l X
    ok_r_ind
      |- ∀P.
           (∀fs. P [] fs) ∧ (∀X Y l fs. P l fs ⇒ P ((X,Y)::l) fs) ⇒
           ∀v v1. P v v1
    ok_r_subset
      |- ∀l big sma. sma ⊆ big ⇒ ok_r l big ⇒ ok_r l sma
    ok_to_unroll_mu
      |- ∀Trans Fm Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst (mu Z Fm) Z Fm) V =
            setsat Trans (mu Z Fm) V)
    ok_to_unroll_nu
      |- ∀Trans Fm Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst (nu Z Fm) Z Fm) V =
            setsat Trans (nu Z Fm) V)
    pair_list_induction
      |- ∀P. P [] ∧ (∀l. P l ⇒ ∀X Y. P ((X,Y)::l)) ⇒ ∀l. P l
    setsat_EQ_satFun
      |- ∀Trans Fm Z E V.
           setsat Trans Fm (extend_env Z E V) = satFun Trans Z V Fm E
    setsat_is_mmfn_UNIV
      |- setsat Trans f V = mmfn Trans f 𝕌(:α) V
    setsat_lemma
      |- (∀V. setsat Trans tt V = 𝕌(:'configuration)) ∧
         (∀V. setsat Trans ff V = ∅) ∧
         (∀Z V. setsat Trans (propmm Z) V = V Z) ∧
         (∀Fm1 Fm2 V.
            setsat Trans (Fm1 andmm Fm2) V =
            setsat Trans Fm1 V ∩ setsat Trans Fm2 V) ∧
         (∀Fm1 Fm2 V.
            setsat Trans (Fm1 ormm Fm2) V =
            setsat Trans Fm1 V ∪ setsat Trans Fm2 V) ∧
         (∀Z Fm V.
            setsat Trans (Box Actions Fm) V =
            {E |
             ∀E' a.
               Trans a E E' ⇒ a ∈ Actions ⇒ (E',Trans,V) mmsat Fm}) ∧
         (∀Z Fm V.
            setsat Trans (Dia Actions Fm) V =
            {E |
             ∃E' a.
               Trans a E E' ∧ a ∈ Actions ∧ (E',Trans,V) mmsat Fm}) ∧
         (∀Z Fm V.
            setsat Trans (nu Z Fm) V =
            gfp (λQ. setsat Trans Fm (extend_env Z Q V))) ∧
         ∀Z Fm V.
           setsat Trans (mu Z Fm) V =
           lfp (λQ. setsat Trans Fm (extend_env Z Q V))
    setsat_monotone
      |- ∀Trans Fm Z V.
           monotone (λQ. setsat Trans Fm (extend_env Z Q V))
    silly_extend
      |- ∀Trans Z Fm a V.
           Z ∉ frees Fm ⇒
           (setsat Trans Fm (extend_env Z a V) = setsat Trans Fm V)
    simple_ok_r_gl
      |- (∀l s Fm.
            INFINITE 𝕌(:β) ∧ ok_r l (frees (nu s Fm)) ⇒
            ok_r (gl l s (frees Fm)) (frees Fm)) ∧
         ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (mu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)
    simple_ok_r_gl_mu
      |- ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (mu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)
    simple_ok_r_gl_nu
      |- ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (nu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)
    uneq_extensions_commute
      |- ∀v w x y f.
           y ≠ x ⇒
           (extend_env y w (extend_env x v f) =
            extend_env x v (extend_env y w f))
    uneq_mmUpdates_commute
      |- ∀v w x y f.
           y ≠ x ⇒
           (mmUpdate y (mmUpdate x f v) w =
            mmUpdate x (mmUpdate y f w) v)
    unfold_mu_LEM
      |- ∀Trans E V Z f.
           INFINITE 𝕌(:β) ⇒
           ((E,Trans,V) mmsat mu Z f ⇔
            (E,Trans,V) mmsat Subst (mu Z f) Z f)
    unfold_nu_LEM
      |- ∀Trans E V Z f.
           INFINITE 𝕌(:β) ⇒
           ((E,Trans,V) mmsat nu Z f ⇔
            (E,Trans,V) mmsat Subst (nu Z f) Z f)
    variant_EXISTS
      |- ∃variant.
           ∀exclvars.
             INFINITE 𝕌(:'variable) ⇒
             FINITE exclvars ⇒
             ∀v. variant exclvars v ∉ exclvars
    variant_not_in
      |- ∀s excl. INFINITE 𝕌(:α) ⇒ FINITE excl ⇒ variant excl s ∉ excl
    vars_finite
      |- ∀f. FINITE (vars f)
Exporting theory "substitution" ... done.
Theory "substitution" took 1.3s to build
Completed load of substitutionScript
