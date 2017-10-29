<<HOL message: Created theory "mmFoundation">>
Saved theorem _____ "datatype_mmForm"
Saved theorem _____ "mmForm_11"
Saved theorem _____ "mmForm_distinct"
Saved theorem _____ "mmForm_case_cong"
Saved theorem _____ "mmForm_nchotomy"
Saved theorem _____ "mmForm_Axiom"
Saved theorem _____ "mmForm_induction"
<<HOL message: Defined type: "mmForm">>
Saved definition __ "mmUpdate_def"
<<HOL warning: GrammarDeltas.revise_data: 
  Grammar-deltas:
    overload_on("mmsat_tupled")
  invalidated by DelConstant(mmFoundation$mmsat_tupled)>>
Saved definition __ "mmsat_def"
Saved induction ___ "mmsat_ind"
Saved definition __ "mmfn_def"
Saved definition __ "extends_def"
Saved theorem _____ "IN_CLAUSES"
Saved theorem _____ "IN_UNION_INTER_CLAUSES"
Saved theorem _____ "MONOTONE_INTER"
Saved theorem _____ "MONOTONE_UNION"
Saved theorem _____ "mmsat_IN_CLAUSES"
Saved theorem _____ "mmfn_CLAUSES"
Saved theorem _____ "mmfn_tt_ff_CLAUSES"
Saved theorem _____ "mmfn_MONOTONIC_propvar"
Saved theorem _____ "mmfn_MONOTONIC_andmm"
Saved theorem _____ "mmfn_MONOTONIC_ormm"
Saved theorem _____ "mmfn_MONOTONIC_Box"
Saved theorem _____ "mmfn_MONOTONIC_Dia"
Saved theorem _____ "mmUpdate_MONOTONIC"
Saved theorem _____ "mmfn_MONOTONIC_nu"
Saved theorem _____ "mmfn_MONOTONIC_mu"
Saved theorem _____ "mmfn_MONOTONIC"
Saved definition __ "satFun_def"
Saved theorem _____ "satFun_MONOTONIC"
Saved theorem _____ "satFun_gfp_thm"
Saved theorem _____ "mmsat_nu_gfp"
Saved theorem _____ "satFun_lfp_thm"
Saved theorem _____ "mmsat_mu_lfp"
Theory: mmFoundation

Parents:
    fixedPoint
    indexedLists
    patternMatches

Type constants:
    mmForm 2

Term constants:
    Box           :('action -> bool) ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    Dia           :('action -> bool) ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    andmm         :('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    extends       :('propvar -> 'configuration -> bool) ->
                   ('propvar -> 'configuration -> bool) -> bool
    ff            :('action, 'propvar) mmForm
    mmForm_CASE   :('action, 'propvar) mmForm ->
                   α ->
                   α ->
                   ('propvar -> α) ->
                   (('action, 'propvar) mmForm ->
                    ('action, 'propvar) mmForm -> α) ->
                   (('action, 'propvar) mmForm ->
                    ('action, 'propvar) mmForm -> α) ->
                   (('action -> bool) ->
                    ('action, 'propvar) mmForm -> α) ->
                   (('action -> bool) ->
                    ('action, 'propvar) mmForm -> α) ->
                   ('propvar -> ('action, 'propvar) mmForm -> α) ->
                   ('propvar -> ('action, 'propvar) mmForm -> α) -> α
    mmForm_size   :('action -> num) ->
                   ('propvar -> num) ->
                   ('action, 'propvar) mmForm -> num
    mmUpdate      :'propvar ->
                   ('propvar -> 'configuration -> bool) ->
                   ('configuration -> bool) ->
                   'propvar -> 'configuration -> bool
    mmfn          :('action -> 'configuration -> 'configuration -> bool)
                   ->
                   ('action, 'propvar) mmForm ->
                   ('configuration -> bool) ->
                   ('propvar -> 'configuration -> bool) ->
                   'configuration -> bool
    mmsat         :'configuration #
                   ('action -> 'configuration -> 'configuration -> bool)
                   # ('propvar -> 'configuration -> bool) ->
                   ('action, 'propvar) mmForm -> bool
    mu            :'propvar ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    nu            :'propvar ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    ormm          :('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm ->
                   ('action, 'propvar) mmForm
    propmm        :'propvar -> ('action, 'propvar) mmForm
    satFun        :('action -> 'configuration -> 'configuration -> bool)
                   ->
                   'propvar ->
                   ('propvar -> 'configuration -> bool) ->
                   ('action, 'propvar) mmForm ->
                   ('configuration -> bool) -> 'configuration -> bool
    tt            :('action, 'propvar) mmForm

Definitions:
    extends_def
      |- ∀V V'. extends V V' ⇔ ∀Z. V Z ⊆ V' Z
    mmForm_TY_DEF
      |- ∃rep.
           TYPE_DEFINITION
             (λa0'.
                ∀'mmForm' .
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
                        'mmForm' a0 ∧ 'mmForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC (SUC 0))))
                              (ARB,ARB)
                              (ind_type$FCONS a0
                                 (ind_type$FCONS a1
                                    (λn. ind_type$BOTTOM)))) a0 a1) ∧
                        'mmForm' a0 ∧ 'mmForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC 0))))) (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'mmForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC (SUC 0))))))
                              (ARB,a0)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'mmForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC
                                 (SUC (SUC (SUC (SUC (SUC (SUC 0)))))))
                              (a0,ARB)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'mmForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC
                                 (SUC
                                    (SUC
                                       (SUC
                                          (SUC (SUC (SUC (SUC 0))))))))
                              (a0,ARB)
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'mmForm' a1) ⇒
                     'mmForm' a0') ⇒
                  'mmForm' a0') rep
    mmForm_case_def
      |- (∀v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE tt v v1 f f1 f2 f3 f4 f5 f6 = v) ∧
         (∀v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE ff v v1 f f1 f2 f3 f4 f5 f6 = v1) ∧
         (∀a v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (propmm a) v v1 f f1 f2 f3 f4 f5 f6 = f a) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (a0 andmm a1) v v1 f f1 f2 f3 f4 f5 f6 =
            f1 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (a0 ormm a1) v v1 f f1 f2 f3 f4 f5 f6 =
            f2 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (Box a0 a1) v v1 f f1 f2 f3 f4 f5 f6 =
            f3 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (Dia a0 a1) v v1 f f1 f2 f3 f4 f5 f6 =
            f4 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
            mmForm_CASE (nu a0 a1) v v1 f f1 f2 f3 f4 f5 f6 =
            f5 a0 a1) ∧
         ∀a0 a1 v v1 f f1 f2 f3 f4 f5 f6.
           mmForm_CASE (mu a0 a1) v v1 f f1 f2 f3 f4 f5 f6 = f6 a0 a1
    mmForm_size_def
      |- (∀f f1. mmForm_size f f1 tt = 0) ∧
         (∀f f1. mmForm_size f f1 ff = 0) ∧
         (∀f f1 a. mmForm_size f f1 (propmm a) = 1 + f1 a) ∧
         (∀f f1 a0 a1.
            mmForm_size f f1 (a0 andmm a1) =
            1 + (mmForm_size f f1 a0 + mmForm_size f f1 a1)) ∧
         (∀f f1 a0 a1.
            mmForm_size f f1 (a0 ormm a1) =
            1 + (mmForm_size f f1 a0 + mmForm_size f f1 a1)) ∧
         (∀f f1 a0 a1.
            mmForm_size f f1 (Box a0 a1) = 1 + mmForm_size f f1 a1) ∧
         (∀f f1 a0 a1.
            mmForm_size f f1 (Dia a0 a1) = 1 + mmForm_size f f1 a1) ∧
         (∀f f1 a0 a1.
            mmForm_size f f1 (nu a0 a1) =
            1 + (f1 a0 + mmForm_size f f1 a1)) ∧
         ∀f f1 a0 a1.
           mmForm_size f f1 (mu a0 a1) =
           1 + (f1 a0 + mmForm_size f f1 a1)
    mmUpdate_def
      |- ∀Z V E Y. mmUpdate Z V E Y = if Y = Z then E else V Y
    mmfn_def
      |- ∀Trans f E V.
           mmfn Trans f E V = {s | s ∈ E ∧ (s,Trans,V) mmsat f}
    mmsat_curried_def
      |- ∀x x1.
           x mmsat x1 ⇔ mmFoundation$old11->mmsat_tupled<-old (x,x1)
    mmsat_tupled_primitive_def
      |- mmFoundation$old11->mmsat_tupled<-old =
         WFREC
           (@R.
              WF R ∧
              (∀f1 f2 V Trans E.
                 R ((E,Trans,V),f2) ((E,Trans,V),f1 andmm f2)) ∧
              (∀f2 f1 V Trans E.
                 R ((E,Trans,V),f1) ((E,Trans,V),f1 andmm f2)) ∧
              (∀f1 f2 V Trans E.
                 R ((E,Trans,V),f2) ((E,Trans,V),f1 ormm f2)) ∧
              (∀f2 f1 V Trans E.
                 R ((E,Trans,V),f1) ((E,Trans,V),f1 ormm f2)) ∧
              (∀f V Actions E Trans a E'.
                 Trans a E E' ∧ a ∈ Actions ⇒
                 R ((E',Trans,V),f) ((E,Trans,V),Box Actions f)) ∧
              (∀Actions E f V Trans E'.
                 R ((E',Trans,V),f) ((E,Trans,V),Dia Actions f)) ∧
              (∀E f V Z Trans E' setE.
                 E' ∈ setE ⇒
                 R ((E',Trans,mmUpdate Z V setE),f)
                   ((E,Trans,V),nu Z f)) ∧
              ∀f V Z Trans E setE E'.
                E ∉ setE ⇒
                R ((E',Trans,mmUpdate Z V setE),f) ((E,Trans,V),mu Z f))
           (λmmsat_tupled a'.
              case a' of
                ((E,Trans,V),tt) => I T
              | ((E,Trans,V),ff) => I F
              | ((E,Trans,V),propmm Z) => I (E ∈ V Z)
              | ((E,Trans,V),f1 andmm f2) =>
                  I
                    (mmsat_tupled ((E,Trans,V),f1) ∧
                     mmsat_tupled ((E,Trans,V),f2))
              | ((E,Trans,V),f1' ormm f2') =>
                  I
                    (mmsat_tupled ((E,Trans,V),f1') ∨
                     mmsat_tupled ((E,Trans,V),f2'))
              | ((E,Trans,V),Box Actions f) =>
                  I
                    (∀E' a.
                       Trans a E E' ⇒
                       a ∈ Actions ⇒
                       mmsat_tupled ((E',Trans,V),f))
              | ((E,Trans,V),Dia Actions' f') =>
                  I
                    (∃E' a.
                       Trans a E E' ∧ a ∈ Actions' ∧
                       mmsat_tupled ((E',Trans,V),f'))
              | ((E,Trans,V),nu Z' f'') =>
                  I
                    (∃setE.
                       E ∈ setE ∧
                       ∀E'.
                         E' ∈ setE ⇒
                         mmsat_tupled
                           ((E',Trans,mmUpdate Z' V setE),f''))
              | ((E,Trans,V),mu Z'' f''') =>
                  I
                    (∀setE.
                       E ∉ setE ⇒
                       ∃E'.
                         mmsat_tupled
                           ((E',Trans,mmUpdate Z'' V setE),f''') ∧
                         E' ∉ setE))
    satFun_def
      |- ∀Trans Z V form E.
           satFun Trans Z V form E =
           mmfn Trans form 𝕌(:'configuration) (mmUpdate Z V E)

Theorems:
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
    datatype_mmForm
      |- DATATYPE (mmForm tt ff propmm $andmm $ormm Box Dia nu mu)
    mmForm_11
      |- (∀a a'. (propmm a = propmm a') ⇔ (a = a')) ∧
         (∀a0 a1 a0' a1'.
            (a0 andmm a1 = a0' andmm a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (a0 ormm a1 = a0' ormm a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (Box a0 a1 = Box a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (Dia a0 a1 = Dia a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (nu a0 a1 = nu a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         ∀a0 a1 a0' a1'.
           (mu a0 a1 = mu a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')
    mmForm_Axiom
      |- ∀f0 f1 f2 f3 f4 f5 f6 f7 f8.
           ∃fn.
             (fn tt = f0) ∧ (fn ff = f1) ∧ (∀a. fn (propmm a) = f2 a) ∧
             (∀a0 a1. fn (a0 andmm a1) = f3 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (a0 ormm a1) = f4 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (Box a0 a1) = f5 a0 a1 (fn a1)) ∧
             (∀a0 a1. fn (Dia a0 a1) = f6 a0 a1 (fn a1)) ∧
             (∀a0 a1. fn (nu a0 a1) = f7 a0 a1 (fn a1)) ∧
             ∀a0 a1. fn (mu a0 a1) = f8 a0 a1 (fn a1)
    mmForm_case_cong
      |- ∀M M' v v1 f f1 f2 f3 f4 f5 f6.
           (M = M') ∧ ((M' = tt) ⇒ (v = v')) ∧
           ((M' = ff) ⇒ (v1 = v1')) ∧
           (∀a. (M' = propmm a) ⇒ (f a = f' a)) ∧
           (∀a0 a1. (M' = a0 andmm a1) ⇒ (f1 a0 a1 = f1' a0 a1)) ∧
           (∀a0 a1. (M' = a0 ormm a1) ⇒ (f2 a0 a1 = f2' a0 a1)) ∧
           (∀a0 a1. (M' = Box a0 a1) ⇒ (f3 a0 a1 = f3' a0 a1)) ∧
           (∀a0 a1. (M' = Dia a0 a1) ⇒ (f4 a0 a1 = f4' a0 a1)) ∧
           (∀a0 a1. (M' = nu a0 a1) ⇒ (f5 a0 a1 = f5' a0 a1)) ∧
           (∀a0 a1. (M' = mu a0 a1) ⇒ (f6 a0 a1 = f6' a0 a1)) ⇒
           (mmForm_CASE M v v1 f f1 f2 f3 f4 f5 f6 =
            mmForm_CASE M' v' v1' f' f1' f2' f3' f4' f5' f6')
    mmForm_distinct
      |- tt ≠ ff ∧ (∀a. tt ≠ propmm a) ∧ (∀a1 a0. tt ≠ a0 andmm a1) ∧
         (∀a1 a0. tt ≠ a0 ormm a1) ∧ (∀a1 a0. tt ≠ Box a0 a1) ∧
         (∀a1 a0. tt ≠ Dia a0 a1) ∧ (∀a1 a0. tt ≠ nu a0 a1) ∧
         (∀a1 a0. tt ≠ mu a0 a1) ∧ (∀a. ff ≠ propmm a) ∧
         (∀a1 a0. ff ≠ a0 andmm a1) ∧ (∀a1 a0. ff ≠ a0 ormm a1) ∧
         (∀a1 a0. ff ≠ Box a0 a1) ∧ (∀a1 a0. ff ≠ Dia a0 a1) ∧
         (∀a1 a0. ff ≠ nu a0 a1) ∧ (∀a1 a0. ff ≠ mu a0 a1) ∧
         (∀a1 a0 a. propmm a ≠ a0 andmm a1) ∧
         (∀a1 a0 a. propmm a ≠ a0 ormm a1) ∧
         (∀a1 a0 a. propmm a ≠ Box a0 a1) ∧
         (∀a1 a0 a. propmm a ≠ Dia a0 a1) ∧
         (∀a1 a0 a. propmm a ≠ nu a0 a1) ∧
         (∀a1 a0 a. propmm a ≠ mu a0 a1) ∧
         (∀a1' a1 a0' a0. a0 andmm a1 ≠ a0' ormm a1') ∧
         (∀a1' a1 a0' a0. a0 andmm a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 andmm a1 ≠ Dia a0' a1') ∧
         (∀a1' a1 a0' a0. a0 andmm a1 ≠ nu a0' a1') ∧
         (∀a1' a1 a0' a0. a0 andmm a1 ≠ mu a0' a1') ∧
         (∀a1' a1 a0' a0. a0 ormm a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 ormm a1 ≠ Dia a0' a1') ∧
         (∀a1' a1 a0' a0. a0 ormm a1 ≠ nu a0' a1') ∧
         (∀a1' a1 a0' a0. a0 ormm a1 ≠ mu a0' a1') ∧
         (∀a1' a1 a0' a0. Box a0 a1 ≠ Dia a0' a1') ∧
         (∀a1' a1 a0' a0. Box a0 a1 ≠ nu a0' a1') ∧
         (∀a1' a1 a0' a0. Box a0 a1 ≠ mu a0' a1') ∧
         (∀a1' a1 a0' a0. Dia a0 a1 ≠ nu a0' a1') ∧
         (∀a1' a1 a0' a0. Dia a0 a1 ≠ mu a0' a1') ∧
         ∀a1' a1 a0' a0. nu a0 a1 ≠ mu a0' a1'
    mmForm_induction
      |- ∀P.
           P tt ∧ P ff ∧ (∀p. P (propmm p)) ∧
           (∀m m0. P m ∧ P m0 ⇒ P (m andmm m0)) ∧
           (∀m m0. P m ∧ P m0 ⇒ P (m ormm m0)) ∧
           (∀m. P m ⇒ ∀f. P (Box f m)) ∧ (∀m. P m ⇒ ∀f. P (Dia f m)) ∧
           (∀m. P m ⇒ ∀p. P (nu p m)) ∧ (∀m. P m ⇒ ∀p. P (mu p m)) ⇒
           ∀m. P m
    mmForm_nchotomy
      |- ∀mm.
           (mm = tt) ∨ (mm = ff) ∨ (∃p. mm = propmm p) ∨
           (∃m m0. mm = m andmm m0) ∨ (∃m m0. mm = m ormm m0) ∨
           (∃f m. mm = Box f m) ∨ (∃f m. mm = Dia f m) ∨
           (∃p m. mm = nu p m) ∨ ∃p m. mm = mu p m
    mmUpdate_MONOTONIC
      |- (∀V Z E F. E ⊆ F ⇒ extends (mmUpdate Z V E) (mmUpdate Z V F)) ∧
         ∀V V' Z E.
           extends V V' ⇒ extends (mmUpdate Z V E) (mmUpdate Z V' E)
    mmfn_CLAUSES
      |- (∀f1 f2 V Trans.
            mmfn Trans (f1 andmm f2) 𝕌(:'configuration) V =
            mmfn Trans f1 𝕌(:'configuration) V ∩
            mmfn Trans f2 𝕌(:'configuration) V) ∧
         ∀f1 f2 V Trans.
           mmfn Trans (f1 ormm f2) 𝕌(:'configuration) V =
           mmfn Trans f1 𝕌(:'configuration) V ∪
           mmfn Trans f2 𝕌(:'configuration) V
    mmfn_MONOTONIC
      |- ∀form V V'.
           extends V V' ⇒
           mmfn Trans form 𝕌(:'configuration) V ⊆
           mmfn Trans form 𝕌(:'configuration) V'
    mmfn_MONOTONIC_Box
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans (Box f form) 𝕌(:'configuration) V ⊆
         mmfn Trans (Box f form) 𝕌(:'configuration) V'
    mmfn_MONOTONIC_Dia
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans (Dia f form) 𝕌(:'configuration) V ⊆
         mmfn Trans (Dia f form) 𝕌(:'configuration) V'
    mmfn_MONOTONIC_andmm
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         (∀V V'.
            extends V V' ⇒
            mmfn Trans form' 𝕌(:'configuration) V ⊆
            mmfn Trans form' 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans form 𝕌(:'configuration) V ∩
         mmfn Trans form' 𝕌(:'configuration) V ⊆
         mmfn Trans form 𝕌(:'configuration) V' ∩
         mmfn Trans form' 𝕌(:'configuration) V'
    mmfn_MONOTONIC_mu
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans (mu p form) 𝕌(:'configuration) V ⊆
         mmfn Trans (mu p form) 𝕌(:'configuration) V'
    mmfn_MONOTONIC_nu
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans (nu p form) 𝕌(:'configuration) V ⊆
         mmfn Trans (nu p form) 𝕌(:'configuration) V'
    mmfn_MONOTONIC_ormm
      |- (∀V V'.
            extends V V' ⇒
            mmfn Trans form 𝕌(:'configuration) V ⊆
            mmfn Trans form 𝕌(:'configuration) V') ⇒
         (∀V V'.
            extends V V' ⇒
            mmfn Trans form' 𝕌(:'configuration) V ⊆
            mmfn Trans form' 𝕌(:'configuration) V') ⇒
         extends V V' ⇒
         mmfn Trans form 𝕌(:'configuration) V ∪
         mmfn Trans form' 𝕌(:'configuration) V ⊆
         mmfn Trans form 𝕌(:'configuration) V' ∪
         mmfn Trans form' 𝕌(:'configuration) V'
    mmfn_MONOTONIC_propvar
      |- ∀Z V V'.
           extends V V' ⇒
           mmfn Trans (propmm Z) 𝕌(:'configuration) V ⊆
           mmfn Trans (propmm Z) 𝕌(:'configuration) V'
    mmfn_tt_ff_CLAUSES
      |- (∀Trans V V'.
            mmfn Trans tt 𝕌(:'configuration) V ⊆
            mmfn Trans tt 𝕌(:'configuration) V') ∧
         ∀Trans V V'.
           mmfn Trans ff 𝕌(:'configuration) V ⊆
           mmfn Trans ff 𝕌(:'configuration) V'
    mmsat_IN_CLAUSES
      |- (∀s form V Trans.
            {s | (s,Trans,V) mmsat form} =
            {s | s ∈ (λx. (x,Trans,V) mmsat form)}) ∧
         (∀s f1 f2 V.
            {s | (s,Trans,V) mmsat f1 ∧ (s,Trans,V) mmsat f2} =
            {s |
             s ∈ (λx. (x,Trans,V) mmsat f1) ∧
             s ∈ (λx. (x,Trans,V) mmsat f2)}) ∧
         ∀s f1 f2 V.
           {s | (s,Trans,V) mmsat f1 ∨ (s,Trans,V) mmsat f2} =
           {s |
            s ∈ (λx. (x,Trans,V) mmsat f1) ∨
            s ∈ (λx. (x,Trans,V) mmsat f2)}
    mmsat_def
      |- (∀V Trans E. (E,Trans,V) mmsat tt ⇔ T) ∧
         (∀V Trans E. (E,Trans,V) mmsat ff ⇔ F) ∧
         (∀Z V Trans E. (E,Trans,V) mmsat propmm Z ⇔ E ∈ V Z) ∧
         (∀f2 f1 V Trans E.
            (E,Trans,V) mmsat f1 andmm f2 ⇔
            (E,Trans,V) mmsat f1 ∧ (E,Trans,V) mmsat f2) ∧
         (∀f2 f1 V Trans E.
            (E,Trans,V) mmsat f1 ormm f2 ⇔
            (E,Trans,V) mmsat f1 ∨ (E,Trans,V) mmsat f2) ∧
         (∀f V Trans E Actions.
            (E,Trans,V) mmsat Box Actions f ⇔
            ∀E' a. Trans a E E' ⇒ a ∈ Actions ⇒ (E',Trans,V) mmsat f) ∧
         (∀f V Trans E Actions.
            (E,Trans,V) mmsat Dia Actions f ⇔
            ∃E' a. Trans a E E' ∧ a ∈ Actions ∧ (E',Trans,V) mmsat f) ∧
         (∀f Z V Trans E.
            (E,Trans,V) mmsat nu Z f ⇔
            ∃setE.
              E ∈ setE ∧
              ∀E'. E' ∈ setE ⇒ (E',Trans,mmUpdate Z V setE) mmsat f) ∧
         ∀f Z V Trans E.
           (E,Trans,V) mmsat mu Z f ⇔
           ∀setE.
             E ∉ setE ⇒
             ∃E'. (E',Trans,mmUpdate Z V setE) mmsat f ∧ E' ∉ setE
    mmsat_ind
      |- ∀P.
           (∀E Trans V. P (E,Trans,V) tt) ∧
           (∀E Trans V. P (E,Trans,V) ff) ∧
           (∀E Trans V Z. P (E,Trans,V) (propmm Z)) ∧
           (∀E Trans V f1 f2.
              P (E,Trans,V) f1 ∧ P (E,Trans,V) f2 ⇒
              P (E,Trans,V) (f1 andmm f2)) ∧
           (∀E Trans V f1 f2.
              P (E,Trans,V) f1 ∧ P (E,Trans,V) f2 ⇒
              P (E,Trans,V) (f1 ormm f2)) ∧
           (∀E Trans V Actions f.
              (∀a E'. Trans a E E' ∧ a ∈ Actions ⇒ P (E',Trans,V) f) ⇒
              P (E,Trans,V) (Box Actions f)) ∧
           (∀E Trans V Actions f.
              (∀E'. P (E',Trans,V) f) ⇒ P (E,Trans,V) (Dia Actions f)) ∧
           (∀E Trans V Z f.
              (∀E' setE. E' ∈ setE ⇒ P (E',Trans,mmUpdate Z V setE) f) ⇒
              P (E,Trans,V) (nu Z f)) ∧
           (∀E Trans V Z f.
              (∀setE E'. E ∉ setE ⇒ P (E',Trans,mmUpdate Z V setE) f) ⇒
              P (E,Trans,V) (mu Z f)) ⇒
           ∀v v1 v2 v3. P (v,v1,v2) v3
    mmsat_mu_lfp
      |- ∀f Z V Trans E.
           (E,Trans,V) mmsat mu Z f ⇔ E ∈ lfp (satFun Trans Z V f)
    mmsat_nu_gfp
      |- ∀f Z V Trans E.
           (E,Trans,V) mmsat nu Z f ⇔ E ∈ gfp (satFun Trans Z V f)
    satFun_MONOTONIC
      |- ∀V Trans Z form E1 E2.
           E1 ⊆ E2 ⇒ satFun Trans Z V form E1 ⊆ satFun Trans Z V form E2
    satFun_gfp_thm
      |- gfp (satFun Trans Z V f) =
         BIGUNION {setE | setE ⊆ satFun Trans Z V f setE}
    satFun_lfp_thm
      |- lfp (satFun Trans Z V f) =
         BIGINTER {setE | satFun Trans Z V f setE ⊆ setE}
Exporting theory "mmFoundation" ... done.
Theory "mmFoundation" took 0.59200s to build
Completed load of mmFoundationScript
