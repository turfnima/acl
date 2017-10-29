signature substitutionTheory =
sig
  type thm = Thm.thm

  (*  Definitions  *)
    val extend_env_def : thm
    val fmla_size_def : thm
    val frees_def : thm
    val gv_def : thm
    val l_Sub_def : thm
    val rf_def : thm
    val setsat_def : thm
    val variant_spec : thm
    val vars_def : thm

  (*  Theorems  *)
    val COND_NOT : thm
    val COND_NOT_DISJ : thm
    val EQ_SUBSET_SUBSET : thm
    val INSERT_INSERT_DELETE : thm
    val Subst : thm
    val Subst_LEM : thm
    val Subst_def : thm
    val Subst_ind : thm
    val Subst_l_Sub : thm
    val Subst_not_free : thm
    val Subst_same_size : thm
    val UNION_SUBSET_MONOTONIC : thm
    val alpha_LEM : thm
    val alpha_frees : thm
    val alpha_remove : thm
    val extend_env_mmUpdate_EQ : thm
    val extend_env_mmUpdate_lemma : thm
    val fmla_size_ind : thm
    val fmla_size_induction : thm
    val frees_LEM : thm
    val frees_SUBSET_vars : thm
    val frees_are_vars : thm
    val frees_finite : thm
    val fv_1_1 : thm
    val fv_BIJ : thm
    val fv_IN_gf : thm
    val fv_LEM : thm
    val fv_append : thm
    val fv_def : thm
    val fv_ind : thm
    val fv_inj : thm
    val fv_not_in : thm
    val gf_append : thm
    val gf_def : thm
    val gf_delete : thm
    val gf_empty : thm
    val gf_finite : thm
    val gf_im : thm
    val gf_ind : thm
    val gf_insert : thm
    val gf_monotone : thm
    val gf_union : thm
    val gfp_monotone : thm
    val gl_append : thm
    val gl_def : thm
    val gl_ind : thm
    val half_gl_ok : thm
    val in_not_in_not_eq : thm
    val l_Sub_ID : thm
    val l_Sub_ID_CONS : thm
    val l_Sub_append : thm
    val l_Sub_nil : thm
    val l_Sub_same_size : thm
    val last_extension_counts : thm
    val last_update_counts : thm
    val lfp_monotone : thm
    val mmsat_setsat : thm
    val muvar_not_free : thm
    val not_in_gf : thm
    val nuvar_not_free : thm
    val ok_r_def : thm
    val ok_r_gl_insert : thm
    val ok_r_ind : thm
    val ok_r_subset : thm
    val ok_to_unroll_mu : thm
    val ok_to_unroll_nu : thm
    val pair_list_induction : thm
    val setsat_EQ_satFun : thm
    val setsat_is_mmfn_UNIV : thm
    val setsat_lemma : thm
    val setsat_monotone : thm
    val silly_extend : thm
    val simple_ok_r_gl : thm
    val simple_ok_r_gl_mu : thm
    val simple_ok_r_gl_nu : thm
    val uneq_extensions_commute : thm
    val uneq_mmUpdates_commute : thm
    val unfold_mu_LEM : thm
    val unfold_nu_LEM : thm
    val variant_EXISTS : thm
    val variant_not_in : thm
    val vars_finite : thm

  val substitution_grammars : type_grammar.grammar * term_grammar.grammar
(*
   [mmFoundation] Parent theory of "substitution"

   [res_quan] Parent theory of "substitution"

   [extend_env_def]  Definition

      |- ∀x v f. extend_env x v f = (λy. if y = x then v else f y)

   [fmla_size_def]  Definition

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

   [frees_def]  Definition

      |- (frees tt = ∅) ∧ (frees ff = ∅) ∧ (∀Z. frees (propmm Z) = {Z}) ∧
         (∀f1 f2. frees (f1 andmm f2) = frees f1 ∪ frees f2) ∧
         (∀f1 f2. frees (f1 ormm f2) = frees f1 ∪ frees f2) ∧
         (∀Actions f. frees (Box Actions f) = frees f) ∧
         (∀Actions f. frees (Dia Actions f) = frees f) ∧
         (∀Z f. frees (nu Z f) = frees f DELETE Z) ∧
         ∀Z f. frees (mu Z f) = frees f DELETE Z

   [gv_def]  Definition

      |- ∀l Z fs. gv l Z fs = fv (gl l Z fs) Z

   [l_Sub_def]  Definition

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

   [rf_def]  Definition

      |- ∀Y X fs. rf Y X fs = if X ∈ fs then Y INSERT fs DELETE X else fs

   [setsat_def]  Definition

      |- ∀Trans f V. setsat Trans f V = {E | (E,Trans,V) mmsat f}

   [variant_spec]  Definition

      |- ∀exclvars.
           INFINITE 𝕌(:'variable) ⇒
           FINITE exclvars ⇒
           ∀v. variant exclvars v ∉ exclvars

   [vars_def]  Definition

      |- (vars tt = ∅) ∧ (vars ff = ∅) ∧ (∀Z. vars (propmm Z) = {Z}) ∧
         (∀f1 f2. vars (f1 andmm f2) = vars f1 ∪ vars f2) ∧
         (∀f1 f2. vars (f1 ormm f2) = vars f1 ∪ vars f2) ∧
         (∀Actions f. vars (Box Actions f) = vars f) ∧
         (∀Actions f. vars (Dia Actions f) = vars f) ∧
         (∀Z f. vars (nu Z f) = vars f ∪ {Z}) ∧
         ∀Z f. vars (mu Z f) = vars f ∪ {Z}

   [COND_NOT]  Theorem

      |- ∀P A B. (if ¬P then A else B) = if P then B else A

   [COND_NOT_DISJ]  Theorem

      |- ∀P Q A B.
           (if ¬Q ∨ P then A else B) = if P then A else if Q then B else A

   [EQ_SUBSET_SUBSET]  Theorem

      |- ∀s1 s2. (s1 = s2) ⇔ s1 ⊆ s2 ∧ s2 ⊆ s1

   [INSERT_INSERT_DELETE]  Theorem

      |- ∀a t. a INSERT t DELETE a = a INSERT t

   [Subst]  Theorem

      |- INFINITE 𝕌(:β) ⇒
         (Subst p X tt = tt) ∧ (Subst p X ff = ff) ∧
         (Subst p X (propmm Z) = if Z = X then p else propmm Z) ∧
         (Subst p X (Fm1 andmm Fm2) = Subst p X Fm1 andmm Subst p X Fm2) ∧
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

   [Subst_LEM]  Theorem

      |- ∀Trans Fm p Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst p Z Fm) V =
            setsat Trans Fm (extend_env Z (setsat Trans p V) V))

   [Subst_def]  Theorem

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

   [Subst_ind]  Theorem

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

   [Subst_l_Sub]  Theorem

      |- ∀f X Y. INFINITE 𝕌(:β) ⇒ (Subst (propmm Y) X f = l_Sub [(X,Y)] f)

   [Subst_not_free]  Theorem

      |- ∀N X Fm. INFINITE 𝕌(:β) ⇒ X ∉ frees Fm ⇒ (Subst N X Fm = Fm)

   [Subst_same_size]  Theorem

      |- ∀Fm X Z.
           INFINITE 𝕌(:β) ⇒
           (fmla_size (Subst (propmm X) Z Fm) = fmla_size Fm)

   [UNION_SUBSET_MONOTONIC]  Theorem

      |- x1 ⊆ y1 ⇒ x2 ⊆ y2 ⇒ x1 ∪ x2 ⊆ y1 ∪ y2

   [alpha_LEM]  Theorem

      |- ∀Trans Fm V Q X X'.
           INFINITE 𝕌(:β) ⇒
           X' ∉ frees Fm ⇒
           (setsat Trans (Subst (propmm X') X Fm) (extend_env X' Q V) =
            setsat Trans Fm (extend_env X Q V))

   [alpha_frees]  Theorem

      |- ∀Y X Fm.
           INFINITE 𝕌(:β) ⇒
           Y ∉ frees Fm ⇒
           (frees (Subst (propmm Y) X Fm) = rf Y X (frees Fm))

   [alpha_remove]  Theorem

      |- ∀Y X Fm.
           INFINITE 𝕌(:β) ⇒
           Y ∉ frees Fm ∧ Y ≠ X ⇒
           X ∉ frees (Subst (propmm Y) X Fm)

   [extend_env_mmUpdate_EQ]  Theorem

      |- extend_env Z E V = mmUpdate Z V E

   [extend_env_mmUpdate_lemma]  Theorem

      |- extend_env Z E V Y = mmUpdate Z V E Y

   [fmla_size_ind]  Theorem

      |- ∀P.
           (∀f. (∀g. fmla_size g < fmla_size f ⇒ P g) ⇒ P f) ⇒
           ∀n f. (fmla_size f = n) ⇒ P f

   [fmla_size_induction]  Theorem

      |- ∀P.
           P tt ∧ P ff ∧ (∀s. P (propmm s)) ∧
           (∀f g. P f ∧ P g ⇒ P (f andmm g)) ∧
           (∀f g. P f ∧ P g ⇒ P (f ormm g)) ∧
           (∀Actions f. P f ⇒ P (Box Actions f)) ∧
           (∀Actions f. P f ⇒ P (Dia Actions f)) ∧
           (∀f. (∀g. (fmla_size g = fmla_size f) ⇒ P g) ⇒ ∀s. P (nu s f)) ∧
           (∀f. (∀g. (fmla_size g = fmla_size f) ⇒ P g) ⇒ ∀s. P (mu s f)) ⇒
           ∀f. P f

   [frees_LEM]  Theorem

      |- ∀Fm l.
           INFINITE 𝕌(:β) ⇒
           ok_r l (frees Fm) ⇒
           (frees (l_Sub l Fm) = gf l (frees Fm))

   [frees_SUBSET_vars]  Theorem

      |- ∀f. frees f ⊆ vars f

   [frees_are_vars]  Theorem

      |- ∀f x. x ∈ frees f ⇒ x ∈ vars f

   [frees_finite]  Theorem

      |- ∀f. FINITE (frees f)

   [fv_1_1]  Theorem

      |- ∀l fs.
           ok_r l fs ⇒
           ∀A B. A ∈ fs ∧ B ∈ fs ⇒ ((fv l A = fv l B) ⇔ (A = B))

   [fv_BIJ]  Theorem

      |- ∀l fs. ok_r l fs ⇒ BIJ (fv l) fs (gf l fs)

   [fv_IN_gf]  Theorem

      |- ∀l fs (A::fs). fv l A ∈ gf l fs

   [fv_LEM]  Theorem

      |- ∀l s. gf l {s} = {fv l s}

   [fv_append]  Theorem

      |- ∀l m. fv (l ++ m) = fv l ∘ fv m

   [fv_def]  Theorem

      |- (∀X. fv [] X = X) ∧
         ∀l Z Y X.
           fv ((Y,Z)::l) X = (let X' = fv l X in if X' = Y then Z else X')

   [fv_ind]  Theorem

      |- ∀P.
           (∀X. P [] X) ∧ (∀Y Z l X. P l X ⇒ P ((Y,Z)::l) X) ⇒
           ∀v v1. P v v1

   [fv_inj]  Theorem

      |- ∀l fs.
           ok_r l fs ⇒ ∀A B. A ∈ fs ∧ B ∈ fs ⇒ (fv l A = fv l B) ⇒ (A = B)

   [fv_not_in]  Theorem

      |- ∀fs gs Z l.
           ok_r l fs ∧ gs ⊆ fs ∧ Z ∈ fs ∧ Z ∉ gs ⇒ fv l Z ∉ gf l gs

   [gf_append]  Theorem

      |- ∀l m fs. gf (l ++ m) fs = gf l (gf m fs)

   [gf_def]  Theorem

      |- (∀fs. gf [] fs = fs) ∧
         ∀l fs Y X. gf ((X,Y)::l) fs = rf Y X (gf l fs)

   [gf_delete]  Theorem

      |- ∀l fs Z.
           ok_r l (Z INSERT fs) ⇒
           (gf l (fs DELETE Z) = gf l fs DELETE fv l Z)

   [gf_empty]  Theorem

      |- ∀l. gf l ∅ = ∅

   [gf_finite]  Theorem

      |- ∀fs. FINITE fs ⇒ ∀l. FINITE (gf l fs)

   [gf_im]  Theorem

      |- ∀l. gf l = IMAGE (fv l)

   [gf_ind]  Theorem

      |- ∀P.
           (∀fs. P [] fs) ∧ (∀X Y l fs. P l fs ⇒ P ((X,Y)::l) fs) ⇒
           ∀v v1. P v v1

   [gf_insert]  Theorem

      |- ∀l fs Z. gf l (Z INSERT fs) = fv l Z INSERT gf l fs

   [gf_monotone]  Theorem

      |- ∀l big sma. sma ⊆ big ⇒ gf l sma ⊆ gf l big

   [gf_union]  Theorem

      |- ∀l fs fs'. gf l (fs ∪ fs') = gf l fs ∪ gf l fs'

   [gfp_monotone]  Theorem

      |- ∀G H. (∀s. G s ⊆ H s) ⇒ gfp G ⊆ gfp H

   [gl_append]  Theorem

      |- ∀Z fs m l.
           gl (l ++ m) Z fs =
           gl l (fv (gl m Z fs) Z) (gf (gl m Z fs) fs) ++ gl m Z fs

   [gl_def]  Theorem

      |- (∀fs Z. gl [] Z fs = []) ∧
         ∀l fs Z Y X.
           gl ((X,Y)::l) Z fs =
           (let
              l' = gl l Z fs ;
              (fs',Z') = (gf l' fs,fv l' Z)
            in
              if X ∉ fs' ∨ (X = Z') then l'
              else if Y = Z' then (X,Y)::(Z',variant (Y INSERT fs') Z')::l'
              else (X,Y)::l')

   [gl_ind]  Theorem

      |- ∀P.
           (∀Z fs. P [] Z fs) ∧
           (∀X Y l Z fs. P l Z fs ⇒ P ((X,Y)::l) Z fs) ⇒
           ∀v v1 v2. P v v1 v2

   [half_gl_ok]  Theorem

      |- ∀l Z fs.
           INFINITE 𝕌(:α) ∧ FINITE fs ∧ ok_r l (fs DELETE Z) ⇒
           ok_r (gl l Z fs) (Z INSERT fs)

   [in_not_in_not_eq]  Theorem

      |- ∀X Y s. X ∈ s ∧ Y ∉ s ⇒ X ≠ Y

   [l_Sub_ID]  Theorem

      |- ∀Fm. l_Sub [(X,X)] Fm = Fm

   [l_Sub_ID_CONS]  Theorem

      |- ∀f l. l_Sub ((X,X)::l) f = l_Sub l f

   [l_Sub_append]  Theorem

      |- ∀P l m.
           INFINITE 𝕌(:β) ⇒
           ok_r m (frees P) ⇒
           (l_Sub (l ++ m) P = l_Sub l (l_Sub m P))

   [l_Sub_nil]  Theorem

      |- ∀Fm. l_Sub [] Fm = Fm

   [l_Sub_same_size]  Theorem

      |- ∀Fm l. fmla_size (l_Sub l Fm) = fmla_size Fm

   [last_extension_counts]  Theorem

      |- ∀x v v' f. extend_env x v (extend_env x v' f) = extend_env x v f

   [last_update_counts]  Theorem

      |- ∀x v v' f. mmUpdate x (mmUpdate x f v') v = mmUpdate x f v

   [lfp_monotone]  Theorem

      |- ∀G H. (∀s. G s ⊆ H s) ⇒ lfp G ⊆ lfp H

   [mmsat_setsat]  Theorem

      |- (E,Trans,V) mmsat f ⇔ E ∈ setsat Trans f V

   [muvar_not_free]  Theorem

      |- ∀s Fm. s ∉ frees (mu s Fm)

   [not_in_gf]  Theorem

      |- ∀A excl l fs Q.
           INFINITE 𝕌(:α) ⇒
           FINITE excl ⇒
           A ∉ gf ((A,variant (A INSERT excl) Q)::l) fs

   [nuvar_not_free]  Theorem

      |- ∀s Fm. s ∉ frees (nu s Fm)

   [ok_r_def]  Theorem

      |- (∀fs. ok_r [] fs ⇔ T) ∧
         ∀l fs Y X.
           ok_r ((X,Y)::l) fs ⇔ ok_r l fs ∧ (X ∈ gf l fs ⇒ Y ∉ gf l fs)

   [ok_r_gl_insert]  Theorem

      |- ∀l Z fs.
           INFINITE 𝕌(:α) ∧ FINITE fs ∧ ok_r l (fs DELETE Z) ⇒
           ok_r (gl l Z fs) (Z INSERT fs) ∧
           ∀X::fs DELETE Z. fv (gl l Z fs) X = fv l X

   [ok_r_ind]  Theorem

      |- ∀P.
           (∀fs. P [] fs) ∧ (∀X Y l fs. P l fs ⇒ P ((X,Y)::l) fs) ⇒
           ∀v v1. P v v1

   [ok_r_subset]  Theorem

      |- ∀l big sma. sma ⊆ big ⇒ ok_r l big ⇒ ok_r l sma

   [ok_to_unroll_mu]  Theorem

      |- ∀Trans Fm Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst (mu Z Fm) Z Fm) V =
            setsat Trans (mu Z Fm) V)

   [ok_to_unroll_nu]  Theorem

      |- ∀Trans Fm Z V.
           INFINITE 𝕌(:β) ⇒
           (setsat Trans (Subst (nu Z Fm) Z Fm) V =
            setsat Trans (nu Z Fm) V)

   [pair_list_induction]  Theorem

      |- ∀P. P [] ∧ (∀l. P l ⇒ ∀X Y. P ((X,Y)::l)) ⇒ ∀l. P l

   [setsat_EQ_satFun]  Theorem

      |- ∀Trans Fm Z E V.
           setsat Trans Fm (extend_env Z E V) = satFun Trans Z V Fm E

   [setsat_is_mmfn_UNIV]  Theorem

      |- setsat Trans f V = mmfn Trans f 𝕌(:α) V

   [setsat_lemma]  Theorem

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
             ∀E' a. Trans a E E' ⇒ a ∈ Actions ⇒ (E',Trans,V) mmsat Fm}) ∧
         (∀Z Fm V.
            setsat Trans (Dia Actions Fm) V =
            {E |
             ∃E' a. Trans a E E' ∧ a ∈ Actions ∧ (E',Trans,V) mmsat Fm}) ∧
         (∀Z Fm V.
            setsat Trans (nu Z Fm) V =
            gfp (λQ. setsat Trans Fm (extend_env Z Q V))) ∧
         ∀Z Fm V.
           setsat Trans (mu Z Fm) V =
           lfp (λQ. setsat Trans Fm (extend_env Z Q V))

   [setsat_monotone]  Theorem

      |- ∀Trans Fm Z V. monotone (λQ. setsat Trans Fm (extend_env Z Q V))

   [silly_extend]  Theorem

      |- ∀Trans Z Fm a V.
           Z ∉ frees Fm ⇒
           (setsat Trans Fm (extend_env Z a V) = setsat Trans Fm V)

   [simple_ok_r_gl]  Theorem

      |- (∀l s Fm.
            INFINITE 𝕌(:β) ∧ ok_r l (frees (nu s Fm)) ⇒
            ok_r (gl l s (frees Fm)) (frees Fm)) ∧
         ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (mu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)

   [simple_ok_r_gl_mu]  Theorem

      |- ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (mu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)

   [simple_ok_r_gl_nu]  Theorem

      |- ∀l s Fm.
           INFINITE 𝕌(:β) ∧ ok_r l (frees (nu s Fm)) ⇒
           ok_r (gl l s (frees Fm)) (frees Fm)

   [uneq_extensions_commute]  Theorem

      |- ∀v w x y f.
           y ≠ x ⇒
           (extend_env y w (extend_env x v f) =
            extend_env x v (extend_env y w f))

   [uneq_mmUpdates_commute]  Theorem

      |- ∀v w x y f.
           y ≠ x ⇒
           (mmUpdate y (mmUpdate x f v) w = mmUpdate x (mmUpdate y f w) v)

   [unfold_mu_LEM]  Theorem

      |- ∀Trans E V Z f.
           INFINITE 𝕌(:β) ⇒
           ((E,Trans,V) mmsat mu Z f ⇔
            (E,Trans,V) mmsat Subst (mu Z f) Z f)

   [unfold_nu_LEM]  Theorem

      |- ∀Trans E V Z f.
           INFINITE 𝕌(:β) ⇒
           ((E,Trans,V) mmsat nu Z f ⇔
            (E,Trans,V) mmsat Subst (nu Z f) Z f)

   [variant_EXISTS]  Theorem

      |- ∃variant.
           ∀exclvars.
             INFINITE 𝕌(:'variable) ⇒
             FINITE exclvars ⇒
             ∀v. variant exclvars v ∉ exclvars

   [variant_not_in]  Theorem

      |- ∀s excl. INFINITE 𝕌(:α) ⇒ FINITE excl ⇒ variant excl s ∉ excl

   [vars_finite]  Theorem

      |- ∀f. FINITE (vars f)


*)
end
