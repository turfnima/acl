<<HOL message: Created theory "hml1Foundation">>
Saved theorem _____ "datatype_hmlForm"
Saved theorem _____ "hmlForm_11"
Saved theorem _____ "hmlForm_distinct"
Saved theorem _____ "hmlForm_case_cong"
Saved theorem _____ "hmlForm_nchotomy"
Saved theorem _____ "hmlForm_Axiom"
Saved theorem _____ "hmlForm_induction"
<<HOL message: Defined type: "hmlForm">>
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("hmsat_tupled")
  invalidated by DelConstant(hml1Foundation$hmsat_tupled)>>
Saved definition __ "hmsat_def"
Saved induction ___ "hmsat_ind"
Saved definition __ "HMfn_def"
Saved definition __ "HMUpdate_def"
Saved definition __ "extends_def"
Saved theorem _____ "IN_CLAUSES"
Saved theorem _____ "IN_UNION_INTER_CLAUSES"
Saved theorem _____ "MONOTONE_INTER"
Saved theorem _____ "MONOTONE_UNION"
Saved theorem _____ "hmsat_IN_CLAUSES"
Saved theorem _____ "HMfn_CLAUSES"
Saved theorem _____ "HMfn_tt_ff_CLAUSES"
Saved theorem _____ "HMfn_MONOTONIC_propvar"
Saved theorem _____ "HMfn_MONOTONIC_andh"
Saved theorem _____ "HMfn_MONOTONIC_orh"
Saved theorem _____ "HMfn_MONOTONIC_Box"
Saved theorem _____ "HMfn_MONOTONIC_Dia"
Saved theorem _____ "HMfn_MONOTONIC"
Saved definition __ "satFun_def"
Saved theorem _____ "HMUpdate_MONOTONIC"
Saved theorem _____ "satFun_MONOTONIC"
Saved definition __ "hmLFP_def"
Saved theorem _____ "satFun_monotone"
Saved theorem _____ "hmLFP_fixedpoint"
Saved definition __ "hmGFP_def"
Saved theorem _____ "hmGFP_fixedpoint"
Theory: hml1Foundation

Parents:
    fixedPoint
    indexedLists
    patternMatches

Type constants:
    hmlForm 2

Term constants:
    Box            :('action -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm
    Dia            :('action -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm
    HMUpdate       :'propvar ->
                    ('propvar -> 'configuration -> bool) ->
                    ('configuration -> bool) ->
                    'propvar -> 'configuration -> bool
    HMfn           :('action ->
                     'configuration -> 'configuration -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    ('configuration -> bool) ->
                    ('propvar -> 'configuration -> bool) ->
                    'configuration -> bool
    andh           :('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm
    extends        :('propvar -> 'configuration -> bool) ->
                    ('propvar -> 'configuration -> bool) -> bool
    ff             :('action, 'propvar) hmlForm
    hmGFP          :('action ->
                     'configuration -> 'configuration -> bool) ->
                    'propvar ->
                    ('propvar -> 'configuration -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    'configuration -> bool
    hmLFP          :('action ->
                     'configuration -> 'configuration -> bool) ->
                    'propvar ->
                    ('propvar -> 'configuration -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    'configuration -> bool
    hmlForm_CASE   :('action, 'propvar) hmlForm ->
                    α ->
                    α ->
                    ('propvar -> α) ->
                    (('action, 'propvar) hmlForm ->
                     ('action, 'propvar) hmlForm -> α) ->
                    (('action, 'propvar) hmlForm ->
                     ('action, 'propvar) hmlForm -> α) ->
                    (('action -> bool) ->
                     ('action, 'propvar) hmlForm -> α) ->
                    (('action -> bool) ->
                     ('action, 'propvar) hmlForm -> α) -> α
    hmlForm_size   :('action -> num) ->
                    ('propvar -> num) ->
                    ('action, 'propvar) hmlForm -> num
    hmsat          :'configuration #
                    ('action ->
                     'configuration -> 'configuration -> bool) #
                    ('propvar -> 'configuration -> bool) ->
                    ('action, 'propvar) hmlForm -> bool
    orh            :('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm ->
                    ('action, 'propvar) hmlForm
    proph          :'propvar -> ('action, 'propvar) hmlForm
    satFun         :('action ->
                     'configuration -> 'configuration -> bool) ->
                    'propvar ->
                    ('propvar -> 'configuration -> bool) ->
                    ('action, 'propvar) hmlForm ->
                    ('configuration -> bool) -> 'configuration -> bool
    tt             :('action, 'propvar) hmlForm

Definitions:
    HMUpdate_def
      |- ∀Z V E Y. HMUpdate Z V E Y = if Y = Z then E else V Y
    HMfn_def
      |- ∀Trans f E V.
           HMfn Trans f E V = {s | s ∈ E ∧ (s,Trans,V) hmsat f}
    extends_def
      |- ∀V V'. extends V V' ⇔ ∀Z. V Z ⊆ V' Z
    hmGFP_def
      |- ∀Trans Z V form.
           hmGFP Trans Z V form = gfp (satFun Trans Z V form)
    hmLFP_def
      |- ∀Trans Z V form.
           hmLFP Trans Z V form = lfp (satFun Trans Z V form)
    hmlForm_TY_DEF
      |- ∃rep.
           TYPE_DEFINITION
             (λa0'.
                ∀'hmlForm' .
                  (∀a0'.
                     (a0' =
                      ind_type$CONSTR 0 (ARB,ARB)
                        (λn. ind_type$BOTTOM)) ∨
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
                            ind_type$CONSTR (SUC (SUC (SUC 0)))
                              (ARB,ARB)
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
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC 0))))) (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC (SUC 0))))))
                              (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'hmlForm' a1) ⇒
                     'hmlForm' a0') ⇒
                  'hmlForm' a0') rep
    hmlForm_case_def
      |- (∀v v1 f f1 f2 f3 f4. hmlForm_CASE tt v v1 f f1 f2 f3 f4 = v) ∧
         (∀v v1 f f1 f2 f3 f4.
            hmlForm_CASE ff v v1 f f1 f2 f3 f4 = v1) ∧
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
    hmlForm_size_def
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
    hmsat_curried_def
      |- ∀x x1.
           x hmsat x1 ⇔ hml1Foundation$old9->hmsat_tupled<-old (x,x1)
    hmsat_tupled_primitive_def
      |- hml1Foundation$old9->hmsat_tupled<-old =
         WFREC
           (@R.
              WF R ∧
              (∀f1 f2 V Trans E.
                 R ((E,Trans,V),f2) ((E,Trans,V),f1 andh f2)) ∧
              (∀f2 f1 V Trans E.
                 R ((E,Trans,V),f1) ((E,Trans,V),f1 andh f2)) ∧
              (∀f1 f2 V Trans E.
                 R ((E,Trans,V),f2) ((E,Trans,V),f1 orh f2)) ∧
              (∀f2 f1 V Trans E.
                 R ((E,Trans,V),f1) ((E,Trans,V),f1 orh f2)) ∧
              (∀f V Actions E Trans a E'.
                 Trans a E E' ∧ a ∈ Actions ⇒
                 R ((E',Trans,V),f) ((E,Trans,V),Box Actions f)) ∧
              ∀Actions E f V Trans E'.
                R ((E',Trans,V),f) ((E,Trans,V),Dia Actions f))
           (λhmsat_tupled a'.
              case a' of
                ((E,Trans,V),tt) => I T
              | ((E,Trans,V),ff) => I F
              | ((E,Trans,V),proph Z) => I (E ∈ V Z)
              | ((E,Trans,V),f1 andh f2) =>
                  I
                    (hmsat_tupled ((E,Trans,V),f1) ∧
                     hmsat_tupled ((E,Trans,V),f2))
              | ((E,Trans,V),f1' orh f2') =>
                  I
                    (hmsat_tupled ((E,Trans,V),f1') ∨
                     hmsat_tupled ((E,Trans,V),f2'))
              | ((E,Trans,V),Box Actions f) =>
                  I
                    (∀E' a.
                       Trans a E E' ⇒
                       a ∈ Actions ⇒
                       hmsat_tupled ((E',Trans,V),f))
              | ((E,Trans,V),Dia Actions' f') =>
                  I
                    (∃E' a.
                       Trans a E E' ∧ a ∈ Actions' ∧
                       hmsat_tupled ((E',Trans,V),f')))
    satFun_def
      |- ∀Trans Z V form E.
           satFun Trans Z V form E =
           HMfn Trans form 𝕌(:'configuration) (HMUpdate Z V E)

Theorems:
    HMUpdate_MONOTONIC
      |- ∀V Z E F. E ⊆ F ⇒ extends (HMUpdate Z V E) (HMUpdate Z V F)
    HMfn_CLAUSES
      |- (∀f1 f2 V Trans.
            HMfn Trans (f1 andh f2) 𝕌(:'configuration) V =
            HMfn Trans f1 𝕌(:'configuration) V ∩
            HMfn Trans f2 𝕌(:'configuration) V) ∧
         ∀f1 f2 V Trans.
           HMfn Trans (f1 orh f2) 𝕌(:'configuration) V =
           HMfn Trans f1 𝕌(:'configuration) V ∪
           HMfn Trans f2 𝕌(:'configuration) V
    HMfn_MONOTONIC
      |- ∀form V V'.
           extends V V' ⇒
           HMfn Trans form 𝕌(:'configuration) V ⊆
           HMfn Trans form 𝕌(:'configuration) V'
    HMfn_MONOTONIC_Box
      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans (Box f form) 𝕌(:'configuration) V ⊆
         HMfn Trans (Box f form) 𝕌(:'configuration) V'
    HMfn_MONOTONIC_Dia
      |- (∀V V'.
            extends V V' ⇒
            HMfn Trans form 𝕌(:'configuration) V ⊆
            HMfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         HMfn Trans (Dia f form) 𝕌(:'configuration) V ⊆
         HMfn Trans (Dia f form) 𝕌(:'configuration) V'
    HMfn_MONOTONIC_andh
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
    HMfn_MONOTONIC_orh
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
    HMfn_MONOTONIC_propvar
      |- ∀Z V V'.
           extends V V' ⇒
           HMfn Trans (proph Z) 𝕌(:'configuration) V ⊆
           HMfn Trans (proph Z) 𝕌(:'configuration) V'
    HMfn_tt_ff_CLAUSES
      |- (∀Trans V V'.
            HMfn Trans tt 𝕌(:'configuration) V ⊆
            HMfn Trans tt 𝕌(:'configuration) V') ∧
         ∀Trans V V'.
           HMfn Trans ff 𝕌(:'configuration) V ⊆
           HMfn Trans ff 𝕌(:'configuration) V'
    IN_CLAUSES
      |- ({s | s ∈ (λx. P x ∨ Q x)} =
          {s | s ∈ (λx. P x) ∨ s ∈ (λx. Q x)}) ∧
         ({s | s ∈ (λx. P x ∧ Q x)} =
          {s | s ∈ (λx. P x) ∧ s ∈ (λx. Q x)})
    IN_UNION_INTER_CLAUSES
      |- ({s | s ∈ (λx. P x ∧ Q x)} = (λx. P x) ∩ (λx. Q x)) ∧
         ({s | s ∈ (λx. P x ∨ Q x)} = (λx. P x) ∪ (λx. Q x))
    MONOTONE_INTER
      |- A ⊆ A' ⇒ B ⊆ B' ⇒ A ∩ B ⊆ A' ∩ B'
    MONOTONE_UNION
      |- A ⊆ A' ⇒ B ⊆ B' ⇒ A ∪ B ⊆ A' ∪ B'
    datatype_hmlForm
      |- DATATYPE (hmlForm tt ff proph $andh $orh Box Dia)
    hmGFP_fixedpoint
      |- ∀Trans Z V form.
           (hmGFP Trans Z V form =
            satFun Trans Z V form (hmGFP Trans Z V form)) ∧
           ∀X. (X = satFun Trans Z V form X) ⇒ X ⊆ hmGFP Trans Z V form
    hmLFP_fixedpoint
      |- ∀Trans Z V form.
           (hmLFP Trans Z V form =
            satFun Trans Z V form (hmLFP Trans Z V form)) ∧
           ∀X. (X = satFun Trans Z V form X) ⇒ hmLFP Trans Z V form ⊆ X
    hmlForm_11
      |- (∀a a'. (proph a = proph a') ⇔ (a = a')) ∧
         (∀a0 a1 a0' a1'.
            (a0 andh a1 = a0' andh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (a0 orh a1 = a0' orh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (Box a0 a1 = Box a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         ∀a0 a1 a0' a1'.
           (Dia a0 a1 = Dia a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')
    hmlForm_Axiom
      |- ∀f0 f1 f2 f3 f4 f5 f6.
           ∃fn.
             (fn tt = f0) ∧ (fn ff = f1) ∧ (∀a. fn (proph a) = f2 a) ∧
             (∀a0 a1. fn (a0 andh a1) = f3 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (a0 orh a1) = f4 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (Box a0 a1) = f5 a0 a1 (fn a1)) ∧
             ∀a0 a1. fn (Dia a0 a1) = f6 a0 a1 (fn a1)
    hmlForm_case_cong
      |- ∀M M' v v1 f f1 f2 f3 f4.
           (M = M') ∧ ((M' = tt) ⇒ (v = v')) ∧
           ((M' = ff) ⇒ (v1 = v1')) ∧
           (∀a. (M' = proph a) ⇒ (f a = f' a)) ∧
           (∀a0 a1. (M' = a0 andh a1) ⇒ (f1 a0 a1 = f1' a0 a1)) ∧
           (∀a0 a1. (M' = a0 orh a1) ⇒ (f2 a0 a1 = f2' a0 a1)) ∧
           (∀a0 a1. (M' = Box a0 a1) ⇒ (f3 a0 a1 = f3' a0 a1)) ∧
           (∀a0 a1. (M' = Dia a0 a1) ⇒ (f4 a0 a1 = f4' a0 a1)) ⇒
           (hmlForm_CASE M v v1 f f1 f2 f3 f4 =
            hmlForm_CASE M' v' v1' f' f1' f2' f3' f4')
    hmlForm_distinct
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
    hmlForm_induction
      |- ∀P.
           P tt ∧ P ff ∧ (∀p. P (proph p)) ∧
           (∀h h0. P h ∧ P h0 ⇒ P (h andh h0)) ∧
           (∀h h0. P h ∧ P h0 ⇒ P (h orh h0)) ∧
           (∀h. P h ⇒ ∀f. P (Box f h)) ∧ (∀h. P h ⇒ ∀f. P (Dia f h)) ⇒
           ∀h. P h
    hmlForm_nchotomy
      |- ∀hh.
           (hh = tt) ∨ (hh = ff) ∨ (∃p. hh = proph p) ∨
           (∃h h0. hh = h andh h0) ∨ (∃h h0. hh = h orh h0) ∨
           (∃f h. hh = Box f h) ∨ ∃f h. hh = Dia f h
    hmsat_IN_CLAUSES
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
    hmsat_def
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
    hmsat_ind
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
    satFun_MONOTONIC
      |- ∀V Trans Z form E1 E2.
           E1 ⊆ E2 ⇒ satFun Trans Z V form E1 ⊆ satFun Trans Z V form E2
    satFun_monotone
      |- monotone (satFun Trans Z V form)
Exporting theory "hml1Foundation" ... done.
Theory "hml1Foundation" took 0.60800s to build
Completed load of hml1FoundationScript
