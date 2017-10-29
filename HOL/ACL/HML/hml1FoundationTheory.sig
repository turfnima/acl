signature hml1FoundationTheory =
sig
  type thm = Thm.thm

  (*  Definitions  *)
    val HMUpdate_def : thm
    val HMfn_def : thm
    val extends_def : thm
    val hmGFP_def : thm
    val hmLFP_def : thm
    val hmlForm_TY_DEF : thm
    val hmlForm_case_def : thm
    val hmlForm_size_def : thm
    val satFun_def : thm

  (*  Theorems  *)
    val HMUpdate_MONOTONIC : thm
    val HMfn_CLAUSES : thm
    val HMfn_MONOTONIC : thm
    val HMfn_MONOTONIC_Box : thm
    val HMfn_MONOTONIC_Dia : thm
    val HMfn_MONOTONIC_andh : thm
    val HMfn_MONOTONIC_orh : thm
    val HMfn_MONOTONIC_propvar : thm
    val HMfn_tt_ff_CLAUSES : thm
    val IN_CLAUSES : thm
    val IN_UNION_INTER_CLAUSES : thm
    val MONOTONE_INTER : thm
    val MONOTONE_UNION : thm
    val datatype_hmlForm : thm
    val hmGFP_fixedpoint : thm
    val hmLFP_fixedpoint : thm
    val hmlForm_11 : thm
    val hmlForm_Axiom : thm
    val hmlForm_case_cong : thm
    val hmlForm_distinct : thm
    val hmlForm_induction : thm
    val hmlForm_nchotomy : thm
    val hmsat_IN_CLAUSES : thm
    val hmsat_def : thm
    val hmsat_ind : thm
    val satFun_MONOTONIC : thm
    val satFun_monotone : thm

  val hml1Foundation_grammars : type_grammar.grammar * term_grammar.grammar
(*
   [fixedPoint] Parent theory of "hml1Foundation"

   [indexedLists] Parent theory of "hml1Foundation"

   [patternMatches] Parent theory of "hml1Foundation"

   [HMUpdate_def]  Definition

      |- ∀Z V E Y. HMUpdate Z V E Y = if Y = Z then E else V Y

   [HMfn_def]  Definition

      |- ∀Trans f E V. HMfn Trans f E V = {s | s ∈ E ∧ (s,Trans,V) hmsat f}

   [extends_def]  Definition

      |- ∀V V'. extends V V' ⇔ ∀Z. V Z ⊆ V' Z

   [hmGFP_def]  Definition

      |- ∀Trans Z V form.
           hmGFP Trans Z V form = gfp (satFun Trans Z V form)

   [hmLFP_def]  Definition

      |- ∀Trans Z V form.
           hmLFP Trans Z V form = lfp (satFun Trans Z V form)

   [hmlForm_TY_DEF]  Definition

      |- ∃rep.
           TYPE_DEFINITION
             (λa0'.
                ∀'hmlForm' .
                  (∀a0'.
                     (a0' =
                      ind_type$CONSTR 0 (ARB,ARB) (λn. ind_type$BOTTOM)) ∨
                     (a0' =
                      ind_type$CONSTR (SUC 0) (ARB,ARB)
                        (λn. ind_type$BOTTOM)) ∨
                     (∃a.
                        a0' =
                        (λa.
                           ind_type$CONSTR (SUC (SUC 0)) (a,ARB)
                             (λn. ind_type$BOTTOM)) a) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC 0))) (ARB,ARB)
                              (ind_type$FCONS a0
                                 (ind_type$FCONS a1
                                    (λn. ind_type$BOTTOM)))) a0 a1) ∧
                        'hmlForm' a0 ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC (SUC 0))))
                              (ARB,ARB)
                              (ind_type$FCONS a0
                                 (ind_type$FCONS a1
                                    (λn. ind_type$BOTTOM)))) a0 a1) ∧
                        'hmlForm' a0 ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC (SUC (SUC 0)))))
                              (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM))) a0
                           a1) ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC (SUC 0))))))
                              (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM))) a0
                           a1) ∧ 'hmlForm' a1) ⇒
                     'hmlForm' a0') ⇒
                  'hmlForm' a0') rep

   [hmlForm_case_def]  Definition

      |- (∀v v1 f f1 f2 f3 f4. hmlForm_CASE tt v v1 f f1 f2 f3 f4 = v) ∧
         (∀v v1 f f1 f2 f3 f4. hmlForm_CASE ff v v1 f f1 f2 f3 f4 = v1) ∧
         (∀a v v1 f f1 f2 f3 f4.
            hmlForm_CASE (proph a) v v1 f f1 f2 f3 f4 = f a) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4.
            hmlForm_CASE (a0 andh a1) v v1 f f1 f2 f3 f4 = f1 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4.
            hmlForm_CASE (a0 orh a1) v v1 f f1 f2 f3 f4 = f2 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4.
            hmlForm_CASE (Box a0 a1) v v1 f f1 f2 f3 f4 = f3 a0 a1) ∧
         ∀a0 a1 v v1 f f1 f2 f3 f4.
           hmlForm_CASE (Dia a0 a1) v v1 f f1 f2 f3 f4 = f4 a0 a1

   [hmlForm_size_def]  Definition

      |- (∀f f1. hmlForm_size f f1 tt = 0) ∧
         (∀f f1. hmlForm_size f f1 ff = 0) ∧
         (∀f f1 a. hmlForm_size f f1 (proph a) = 1 + f1 a) ∧
         (∀f f1 a0 a1.
            hmlForm_size f f1 (a0 andh a1) =
            1 + (hmlForm_size f f1 a0 + hmlForm_size f f1 a1)) ∧
         (∀f f1 a0 a1.
            hmlForm_size f f1 (a0 orh a1) =
            1 + (hmlForm_size f f1 a0 + hmlForm_size f f1 a1)) ∧
         (∀f f1 a0 a1.
            hmlForm_size f f1 (Box a0 a1) = 1 + hmlForm_size f f1 a1) ∧
         ∀f f1 a0 a1.
           hmlForm_size f f1 (Dia a0 a1) = 1 + hmlForm_size f f1 a1

   [satFun_def]  Definition

      |- ∀Trans Z V form E.
           satFun Trans Z V form E =
           HMfn Trans form 𝕌(:'configuration) (HMUpdate Z V E)

   [HMUpdate_MONOTONIC]  Theorem

      |- ∀V Z E F. E ⊆ F ⇒ extends (HMUpdate Z V E) (HMUpdate Z V F)

   [HMfn_CLAUSES]  Theorem

      |- (∀f1 f2 V Trans.
            HMfn Trans (f1 andh f2) 𝕌(:'configuration) V =
            HMfn Trans f1 𝕌(:'configuration) V ∩
            HMfn Trans f2 𝕌(:'configuration) V) ∧
         ∀f1 f2 V Trans.
           HMfn Trans (f1 orh f2) 𝕌(:'configuration) V =
           HMfn Trans f1 𝕌(:'configuration) V ∪
           HMfn Trans f2 𝕌(:'configuration) V

   [HMfn_MONOTONIC]  Theorem

      |- ∀form V V'.
           extends V V' ⇒
           HMfn Trans form 𝕌(:'configuration) V ⊆
           HMfn Trans form 𝕌(:'configuration) V'

   [HMfn_MONOTONIC_Box]  Theorem

      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans (Box f form) 𝕌(:'configuration) V ⊆
         HMfn Trans (Box f form) 𝕌(:'configuration) V'

   [HMfn_MONOTONIC_Dia]  Theorem

      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans (Dia f form) 𝕌(:'configuration) V ⊆
         HMfn Trans (Dia f form) 𝕌(:'configuration) V'

   [HMfn_MONOTONIC_andh]  Theorem

      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         (∀V V'.
            extends V V' ⇒
            HMfn Trans form' 𝕌(:'configuration) V ⊆
            HMfn Trans form' 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans form 𝕌(:'configuration) V ∩
         HMfn Trans form' 𝕌(:'configuration) V ⊆
         HMfn Trans form 𝕌(:'configuration) V' ∩
         HMfn Trans form' 𝕌(:'configuration) V'

   [HMfn_MONOTONIC_orh]  Theorem

      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         (∀V V'.
            extends V V' ⇒
            HMfn Trans form' 𝕌(:'configuration) V ⊆
            HMfn Trans form' 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans form 𝕌(:'configuration) V ∪
         HMfn Trans form' 𝕌(:'configuration) V ⊆
         HMfn Trans form 𝕌(:'configuration) V' ∪
         HMfn Trans form' 𝕌(:'configuration) V'

   [HMfn_MONOTONIC_propvar]  Theorem

      |- ∀Z V V'.
           extends V V' ⇒
           HMfn Trans (proph Z) 𝕌(:'configuration) V ⊆
           HMfn Trans (proph Z) 𝕌(:'configuration) V'

   [HMfn_tt_ff_CLAUSES]  Theorem

      |- (∀Trans V V'.
            HMfn Trans tt 𝕌(:'configuration) V ⊆
            HMfn Trans tt 𝕌(:'configuration) V') ∧
         ∀Trans V V'.
           HMfn Trans ff 𝕌(:'configuration) V ⊆
           HMfn Trans ff 𝕌(:'configuration) V'

   [IN_CLAUSES]  Theorem

      |- ({s | s ∈ (λx. P x ∨ Q x)} =
          {s | s ∈ (λx. P x) ∨ s ∈ (λx. Q x)}) ∧
         ({s | s ∈ (λx. P x ∧ Q x)} = {s | s ∈ (λx. P x) ∧ s ∈ (λx. Q x)})

   [IN_UNION_INTER_CLAUSES]  Theorem

      |- ({s | s ∈ (λx. P x ∧ Q x)} = (λx. P x) ∩ (λx. Q x)) ∧
         ({s | s ∈ (λx. P x ∨ Q x)} = (λx. P x) ∪ (λx. Q x))

   [MONOTONE_INTER]  Theorem

      |- A ⊆ A' ⇒ B ⊆ B' ⇒ A ∩ B ⊆ A' ∩ B'

   [MONOTONE_UNION]  Theorem

      |- A ⊆ A' ⇒ B ⊆ B' ⇒ A ∪ B ⊆ A' ∪ B'

   [datatype_hmlForm]  Theorem

      |- DATATYPE (hmlForm tt ff proph $andh $orh Box Dia)

   [hmGFP_fixedpoint]  Theorem

      |- ∀Trans Z V form.
           (hmGFP Trans Z V form =
            satFun Trans Z V form (hmGFP Trans Z V form)) ∧
           ∀X. (X = satFun Trans Z V form X) ⇒ X ⊆ hmGFP Trans Z V form

   [hmLFP_fixedpoint]  Theorem

      |- ∀Trans Z V form.
           (hmLFP Trans Z V form =
            satFun Trans Z V form (hmLFP Trans Z V form)) ∧
           ∀X. (X = satFun Trans Z V form X) ⇒ hmLFP Trans Z V form ⊆ X

   [hmlForm_11]  Theorem

      |- (∀a a'. (proph a = proph a') ⇔ (a = a')) ∧
         (∀a0 a1 a0' a1'.
            (a0 andh a1 = a0' andh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (a0 orh a1 = a0' orh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (Box a0 a1 = Box a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         ∀a0 a1 a0' a1'.
           (Dia a0 a1 = Dia a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')

   [hmlForm_Axiom]  Theorem

      |- ∀f0 f1 f2 f3 f4 f5 f6.
           ∃fn.
             (fn tt = f0) ∧ (fn ff = f1) ∧ (∀a. fn (proph a) = f2 a) ∧
             (∀a0 a1. fn (a0 andh a1) = f3 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (a0 orh a1) = f4 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (Box a0 a1) = f5 a0 a1 (fn a1)) ∧
             ∀a0 a1. fn (Dia a0 a1) = f6 a0 a1 (fn a1)

   [hmlForm_case_cong]  Theorem

      |- ∀M M' v v1 f f1 f2 f3 f4.
           (M = M') ∧ ((M' = tt) ⇒ (v = v')) ∧ ((M' = ff) ⇒ (v1 = v1')) ∧
           (∀a. (M' = proph a) ⇒ (f a = f' a)) ∧
           (∀a0 a1. (M' = a0 andh a1) ⇒ (f1 a0 a1 = f1' a0 a1)) ∧
           (∀a0 a1. (M' = a0 orh a1) ⇒ (f2 a0 a1 = f2' a0 a1)) ∧
           (∀a0 a1. (M' = Box a0 a1) ⇒ (f3 a0 a1 = f3' a0 a1)) ∧
           (∀a0 a1. (M' = Dia a0 a1) ⇒ (f4 a0 a1 = f4' a0 a1)) ⇒
           (hmlForm_CASE M v v1 f f1 f2 f3 f4 =
            hmlForm_CASE M' v' v1' f' f1' f2' f3' f4')

   [hmlForm_distinct]  Theorem

      |- tt ≠ ff ∧ (∀a. tt ≠ proph a) ∧ (∀a1 a0. tt ≠ a0 andh a1) ∧
         (∀a1 a0. tt ≠ a0 orh a1) ∧ (∀a1 a0. tt ≠ Box a0 a1) ∧
         (∀a1 a0. tt ≠ Dia a0 a1) ∧ (∀a. ff ≠ proph a) ∧
         (∀a1 a0. ff ≠ a0 andh a1) ∧ (∀a1 a0. ff ≠ a0 orh a1) ∧
         (∀a1 a0. ff ≠ Box a0 a1) ∧ (∀a1 a0. ff ≠ Dia a0 a1) ∧
         (∀a1 a0 a. proph a ≠ a0 andh a1) ∧
         (∀a1 a0 a. proph a ≠ a0 orh a1) ∧
         (∀a1 a0 a. proph a ≠ Box a0 a1) ∧
         (∀a1 a0 a. proph a ≠ Dia a0 a1) ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ a0' orh a1') ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ Dia a0' a1') ∧
         (∀a1' a1 a0' a0. a0 orh a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 orh a1 ≠ Dia a0' a1') ∧
         ∀a1' a1 a0' a0. Box a0 a1 ≠ Dia a0' a1'

   [hmlForm_induction]  Theorem

      |- ∀P.
           P tt ∧ P ff ∧ (∀p. P (proph p)) ∧
           (∀h h0. P h ∧ P h0 ⇒ P (h andh h0)) ∧
           (∀h h0. P h ∧ P h0 ⇒ P (h orh h0)) ∧
           (∀h. P h ⇒ ∀f. P (Box f h)) ∧ (∀h. P h ⇒ ∀f. P (Dia f h)) ⇒
           ∀h. P h

   [hmlForm_nchotomy]  Theorem

      |- ∀hh.
           (hh = tt) ∨ (hh = ff) ∨ (∃p. hh = proph p) ∨
           (∃h h0. hh = h andh h0) ∨ (∃h h0. hh = h orh h0) ∨
           (∃f h. hh = Box f h) ∨ ∃f h. hh = Dia f h

   [hmsat_IN_CLAUSES]  Theorem

      |- (∀s form V Trans.
            {s | (s,Trans,V) hmsat form} =
            {s | s ∈ (λx. (x,Trans,V) hmsat form)}) ∧
         (∀s f1 f2 V.
            {s | (s,Trans,V) hmsat f1 ∧ (s,Trans,V) hmsat f2} =
            {s |
             s ∈ (λx. (x,Trans,V) hmsat f1) ∧
             s ∈ (λx. (x,Trans,V) hmsat f2)}) ∧
         ∀s f1 f2 V.
           {s | (s,Trans,V) hmsat f1 ∨ (s,Trans,V) hmsat f2} =
           {s |
            s ∈ (λx. (x,Trans,V) hmsat f1) ∨
            s ∈ (λx. (x,Trans,V) hmsat f2)}

   [hmsat_def]  Theorem

      |- (∀V Trans E. (E,Trans,V) hmsat tt ⇔ T) ∧
         (∀V Trans E. (E,Trans,V) hmsat ff ⇔ F) ∧
         (∀Z V Trans E. (E,Trans,V) hmsat proph Z ⇔ E ∈ V Z) ∧
         (∀f2 f1 V Trans E.
            (E,Trans,V) hmsat f1 andh f2 ⇔
            (E,Trans,V) hmsat f1 ∧ (E,Trans,V) hmsat f2) ∧
         (∀f2 f1 V Trans E.
            (E,Trans,V) hmsat f1 orh f2 ⇔
            (E,Trans,V) hmsat f1 ∨ (E,Trans,V) hmsat f2) ∧
         (∀f V Trans E Actions.
            (E,Trans,V) hmsat Box Actions f ⇔
            ∀E' a. Trans a E E' ⇒ a ∈ Actions ⇒ (E',Trans,V) hmsat f) ∧
         ∀f V Trans E Actions.
           (E,Trans,V) hmsat Dia Actions f ⇔
           ∃E' a. Trans a E E' ∧ a ∈ Actions ∧ (E',Trans,V) hmsat f

   [hmsat_ind]  Theorem

      |- ∀P.
           (∀E Trans V. P (E,Trans,V) tt) ∧
           (∀E Trans V. P (E,Trans,V) ff) ∧
           (∀E Trans V Z. P (E,Trans,V) (proph Z)) ∧
           (∀E Trans V f1 f2.
              P (E,Trans,V) f1 ∧ P (E,Trans,V) f2 ⇒
              P (E,Trans,V) (f1 andh f2)) ∧
           (∀E Trans V f1 f2.
              P (E,Trans,V) f1 ∧ P (E,Trans,V) f2 ⇒
              P (E,Trans,V) (f1 orh f2)) ∧
           (∀E Trans V Actions f.
              (∀a E'. Trans a E E' ∧ a ∈ Actions ⇒ P (E',Trans,V) f) ⇒
              P (E,Trans,V) (Box Actions f)) ∧
           (∀E Trans V Actions f.
              (∀E'. P (E',Trans,V) f) ⇒ P (E,Trans,V) (Dia Actions f)) ⇒
           ∀v v1 v2 v3. P (v,v1,v2) v3

   [satFun_MONOTONIC]  Theorem

      |- ∀V Trans Z form E1 E2.
           E1 ⊆ E2 ⇒ satFun Trans Z V form E1 ⊆ satFun Trans Z V form E2

   [satFun_monotone]  Theorem

      |- monotone (satFun Trans Z V form)


*)
end
