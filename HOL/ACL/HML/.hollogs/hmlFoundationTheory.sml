<<HOL message: Created theory "hmlFoundation">>
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
  invalidated by DelConstant(hmlFoundation$hmsat_tupled)>>
Saved definition __ "hmsat_def"
Saved induction ___ "hmsat_ind"
Theory: hmlFoundation

Parents:
    indexedLists
    patternMatches

Type constants:
    hmlForm 1

Term constants:
    Box            :('action -> bool) ->
                    'action hmlForm -> 'action hmlForm
    Dia            :('action -> bool) ->
                    'action hmlForm -> 'action hmlForm
    andh           :'action hmlForm ->
                    'action hmlForm -> 'action hmlForm
    ff             :'action hmlForm
    hmlForm_CASE   :'action hmlForm ->
                    α ->
                    α ->
                    ('action hmlForm -> 'action hmlForm -> α) ->
                    ('action hmlForm -> 'action hmlForm -> α) ->
                    (('action -> bool) -> 'action hmlForm -> α) ->
                    (('action -> bool) -> 'action hmlForm -> α) -> α
    hmlForm_size   :('action -> num) -> 'action hmlForm -> num
    hmsat          :'configuration #
                    ('action ->
                     'configuration -> 'configuration -> bool) ->
                    'action hmlForm -> bool
    orh            :'action hmlForm ->
                    'action hmlForm -> 'action hmlForm
    tt             :'action hmlForm

Definitions:
    hmlForm_TY_DEF
      |- ∃rep.
           TYPE_DEFINITION
             (λa0'.
                ∀'hmlForm' .
                  (∀a0'.
                     (a0' =
                      ind_type$CONSTR 0 ARB (λn. ind_type$BOTTOM)) ∨
                     (a0' =
                      ind_type$CONSTR (SUC 0) ARB
                        (λn. ind_type$BOTTOM)) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC 0)) ARB
                              (ind_type$FCONS a0
                                 (ind_type$FCONS a1
                                    (λn. ind_type$BOTTOM)))) a0 a1) ∧
                        'hmlForm' a0 ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC 0))) ARB
                              (ind_type$FCONS a0
                                 (ind_type$FCONS a1
                                    (λn. ind_type$BOTTOM)))) a0 a1) ∧
                        'hmlForm' a0 ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR (SUC (SUC (SUC (SUC 0)))) a0
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'hmlForm' a1) ∨
                     (∃a0 a1.
                        (a0' =
                         (λa0 a1.
                            ind_type$CONSTR
                              (SUC (SUC (SUC (SUC (SUC 0))))) a0
                              (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1) ∧ 'hmlForm' a1) ⇒
                     'hmlForm' a0') ⇒
                  'hmlForm' a0') rep
    hmlForm_case_def
      |- (∀v v1 f f1 f2 f3. hmlForm_CASE tt v v1 f f1 f2 f3 = v) ∧
         (∀v v1 f f1 f2 f3. hmlForm_CASE ff v v1 f f1 f2 f3 = v1) ∧
         (∀a0 a1 v v1 f f1 f2 f3.
            hmlForm_CASE (a0 andh a1) v v1 f f1 f2 f3 = f a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3.
            hmlForm_CASE (a0 orh a1) v v1 f f1 f2 f3 = f1 a0 a1) ∧
         (∀a0 a1 v v1 f f1 f2 f3.
            hmlForm_CASE (Box a0 a1) v v1 f f1 f2 f3 = f2 a0 a1) ∧
         ∀a0 a1 v v1 f f1 f2 f3.
           hmlForm_CASE (Dia a0 a1) v v1 f f1 f2 f3 = f3 a0 a1
    hmlForm_size_def
      |- (∀f. hmlForm_size f tt = 0) ∧ (∀f. hmlForm_size f ff = 0) ∧
         (∀f a0 a1.
            hmlForm_size f (a0 andh a1) =
            1 + (hmlForm_size f a0 + hmlForm_size f a1)) ∧
         (∀f a0 a1.
            hmlForm_size f (a0 orh a1) =
            1 + (hmlForm_size f a0 + hmlForm_size f a1)) ∧
         (∀f a0 a1.
            hmlForm_size f (Box a0 a1) = 1 + hmlForm_size f a1) ∧
         ∀f a0 a1. hmlForm_size f (Dia a0 a1) = 1 + hmlForm_size f a1
    hmsat_curried_def
      |- ∀x x1.
           x hmsat x1 ⇔ hmlFoundation$old8->hmsat_tupled<-old (x,x1)
    hmsat_tupled_primitive_def
      |- hmlFoundation$old8->hmsat_tupled<-old =
         WFREC
           (@R.
              WF R ∧
              (∀f1 f2 Trans E.
                 R ((E,Trans),f2) ((E,Trans),f1 andh f2)) ∧
              (∀f2 f1 Trans E.
                 R ((E,Trans),f1) ((E,Trans),f1 andh f2)) ∧
              (∀f1 f2 Trans E. R ((E,Trans),f2) ((E,Trans),f1 orh f2)) ∧
              (∀f2 f1 Trans E. R ((E,Trans),f1) ((E,Trans),f1 orh f2)) ∧
              (∀f Actions E Trans a E'.
                 Trans a E E' ∧ a ∈ Actions ⇒
                 R ((E',Trans),f) ((E,Trans),Box Actions f)) ∧
              ∀Actions E f Trans E'.
                R ((E',Trans),f) ((E,Trans),Dia Actions f))
           (λhmsat_tupled a'.
              case a' of
                ((E,Trans),tt) => I T
              | ((E,Trans),ff) => I F
              | ((E,Trans),f1 andh f2) =>
                  I
                    (hmsat_tupled ((E,Trans),f1) ∧
                     hmsat_tupled ((E,Trans),f2))
              | ((E,Trans),f1' orh f2') =>
                  I
                    (hmsat_tupled ((E,Trans),f1') ∨
                     hmsat_tupled ((E,Trans),f2'))
              | ((E,Trans),Box Actions f) =>
                  I
                    (∀E' a.
                       Trans a E E' ⇒
                       a ∈ Actions ⇒
                       hmsat_tupled ((E',Trans),f))
              | ((E,Trans),Dia Actions' f') =>
                  I
                    (∃E' a.
                       Trans a E E' ∧ a ∈ Actions' ∧
                       hmsat_tupled ((E',Trans),f')))

Theorems:
    datatype_hmlForm
      |- DATATYPE (hmlForm tt ff $andh $orh Box Dia)
    hmlForm_11
      |- (∀a0 a1 a0' a1'.
            (a0 andh a1 = a0' andh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (a0 orh a1 = a0' orh a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         (∀a0 a1 a0' a1'.
            (Box a0 a1 = Box a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')) ∧
         ∀a0 a1 a0' a1'.
           (Dia a0 a1 = Dia a0' a1') ⇔ (a0 = a0') ∧ (a1 = a1')
    hmlForm_Axiom
      |- ∀f0 f1 f2 f3 f4 f5.
           ∃fn.
             (fn tt = f0) ∧ (fn ff = f1) ∧
             (∀a0 a1. fn (a0 andh a1) = f2 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (a0 orh a1) = f3 a0 a1 (fn a0) (fn a1)) ∧
             (∀a0 a1. fn (Box a0 a1) = f4 a0 a1 (fn a1)) ∧
             ∀a0 a1. fn (Dia a0 a1) = f5 a0 a1 (fn a1)
    hmlForm_case_cong
      |- ∀M M' v v1 f f1 f2 f3.
           (M = M') ∧ ((M' = tt) ⇒ (v = v')) ∧
           ((M' = ff) ⇒ (v1 = v1')) ∧
           (∀a0 a1. (M' = a0 andh a1) ⇒ (f a0 a1 = f' a0 a1)) ∧
           (∀a0 a1. (M' = a0 orh a1) ⇒ (f1 a0 a1 = f1' a0 a1)) ∧
           (∀a0 a1. (M' = Box a0 a1) ⇒ (f2 a0 a1 = f2' a0 a1)) ∧
           (∀a0 a1. (M' = Dia a0 a1) ⇒ (f3 a0 a1 = f3' a0 a1)) ⇒
           (hmlForm_CASE M v v1 f f1 f2 f3 =
            hmlForm_CASE M' v' v1' f' f1' f2' f3')
    hmlForm_distinct
      |- tt ≠ ff ∧ (∀a1 a0. tt ≠ a0 andh a1) ∧
         (∀a1 a0. tt ≠ a0 orh a1) ∧ (∀a1 a0. tt ≠ Box a0 a1) ∧
         (∀a1 a0. tt ≠ Dia a0 a1) ∧ (∀a1 a0. ff ≠ a0 andh a1) ∧
         (∀a1 a0. ff ≠ a0 orh a1) ∧ (∀a1 a0. ff ≠ Box a0 a1) ∧
         (∀a1 a0. ff ≠ Dia a0 a1) ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ a0' orh a1') ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 andh a1 ≠ Dia a0' a1') ∧
         (∀a1' a1 a0' a0. a0 orh a1 ≠ Box a0' a1') ∧
         (∀a1' a1 a0' a0. a0 orh a1 ≠ Dia a0' a1') ∧
         ∀a1' a1 a0' a0. Box a0 a1 ≠ Dia a0' a1'
    hmlForm_induction
      |- ∀P.
           P tt ∧ P ff ∧ (∀h h0. P h ∧ P h0 ⇒ P (h andh h0)) ∧
           (∀h h0. P h ∧ P h0 ⇒ P (h orh h0)) ∧
           (∀h. P h ⇒ ∀f. P (Box f h)) ∧ (∀h. P h ⇒ ∀f. P (Dia f h)) ⇒
           ∀h. P h
    hmlForm_nchotomy
      |- ∀hh.
           (hh = tt) ∨ (hh = ff) ∨ (∃h h0. hh = h andh h0) ∨
           (∃h h0. hh = h orh h0) ∨ (∃f h. hh = Box f h) ∨
           ∃f h. hh = Dia f h
    hmsat_def
      |- (∀Trans E. (E,Trans) hmsat tt ⇔ T) ∧
         (∀Trans E. (E,Trans) hmsat ff ⇔ F) ∧
         (∀f2 f1 Trans E.
            (E,Trans) hmsat f1 andh f2 ⇔
            (E,Trans) hmsat f1 ∧ (E,Trans) hmsat f2) ∧
         (∀f2 f1 Trans E.
            (E,Trans) hmsat f1 orh f2 ⇔
            (E,Trans) hmsat f1 ∨ (E,Trans) hmsat f2) ∧
         (∀f Trans E Actions.
            (E,Trans) hmsat Box Actions f ⇔
            ∀E' a. Trans a E E' ⇒ a ∈ Actions ⇒ (E',Trans) hmsat f) ∧
         ∀f Trans E Actions.
           (E,Trans) hmsat Dia Actions f ⇔
           ∃E' a. Trans a E E' ∧ a ∈ Actions ∧ (E',Trans) hmsat f
    hmsat_ind
      |- ∀P.
           (∀E Trans. P (E,Trans) tt) ∧ (∀E Trans. P (E,Trans) ff) ∧
           (∀E Trans f1 f2.
              P (E,Trans) f1 ∧ P (E,Trans) f2 ⇒
              P (E,Trans) (f1 andh f2)) ∧
           (∀E Trans f1 f2.
              P (E,Trans) f1 ∧ P (E,Trans) f2 ⇒
              P (E,Trans) (f1 orh f2)) ∧
           (∀E Trans Actions f.
              (∀a E'. Trans a E E' ∧ a ∈ Actions ⇒ P (E',Trans) f) ⇒
              P (E,Trans) (Box Actions f)) ∧
           (∀E Trans Actions f.
              (∀E'. P (E',Trans) f) ⇒ P (E,Trans) (Dia Actions f)) ⇒
           ∀v v1 v2. P (v,v1) v2
Exporting theory "hmlFoundation" ... done.
Theory "hmlFoundation" took 0.15600s to build
Completed load of hmlFoundationScript
