                                        ; --- representation ---

;; regle = ( (liste_premisse) fait)
;; pour un SE d'ordre 0+, le fait est associe a une valeur
;; donc on represent un fait par (attribut valeur)
;; et une premisse par (fait operateur valuer) 
;; exemple :
;; soit une regle : si A>7, alors B=5
;; => ( ((A > 7)) (B 5) )

;; --- base de regle ---

(defparameter *uvs*
  '((FQ01 TM 6)
    (GE37 TM 6)
    (GE38 TM 6)
    (GE39 TM 6)
    (GE40 TM 6)
    (IA01 CS 6)
    (IA03 TM 6)
    (LO12 TM 6)
    (LO17 TM 6)
    (LO23 TM 6)
    (MB11 CS 6)
    (MI01 TM 6)
    (MP03 TM 6)
    (MT09 CS 6)
    (MT12 CS 6)
    (NF16 CS 6)
    (MT10 CS 6)
    (SY02 CS 6)
    (LO21 TM 6)
    (NF17 TM 6)
    (SR01 TM 6)
    (SR06 TM 6)
    (NF29 TM 6)
    (PR TM 5)
    (RO05 CS 6)
    (RO06 TM 6)
    (RV01 TM 6)
    (SR04 CS 6)
    (SR05 CS 6)
    (SY08 CS 6)
    (SY19 TM 6)
    (SY27 TM 6)
    (SY31 TM 6)
    (TX TM 5)
    (BI01 CS 6)
    (IA02 CS 6)
    (IA04 TM 6)
    (LO18 TM 6)
    (LO22 TM 6)
    (MI03 TM 6)
    (MP02 TM 6)
    (NF11 CS 6)
    (NF26 TM 6)
    (NF28 TM 6)
    (NF33 TM 6)
    (R003 CS 6)
    (RO04 CS 6)
    (SR02 CS 6)
    (SR03 TM 6)
    (SY06 CS 6)
    (SY09 CS 6)
    (SY14 CS 6)
    (SY15 CS 6)
    (SY23 TM 6)
    (SY26 TM 6)
    ))

(defun uv-affichable (nom)
  (let* ((uv (assoc nom *uvs*)))
    (when uv
      (format nil "~a : ~a crédits ~a" nom (caddr uv) (cadr uv))
      )
    )
  )

(defmacro afficher-liste (body)
  `(format t "~{~a~%~}" ,body)
  )

(defun afficher-liste-uvs (uvs)
  (afficher-liste (loop for uv in uvs collect (uv-affichable uv)))
  )

(defparameter *br* 
  '(; règles d'aggrégat
    (((semestre <= 3)) (set profil pre_stage)) ; R1
    (((semestre > 3)) (set profil post_stage)) ; R2
                                        ; règles de demande d'informations
    (((semestre = 1)) (ask cursus "Venez vous de DUT ou de TC ?"))
    (((semestre >= 4)) (ask filiere "Quelle est votre filière ?"))
                                        ; règles de détermination
    (((semestre >= 2)) (set GE37 faisable)); R3
    (((semestre >= 1)) (set FQ01 faisable)); R3_
    (((profil = pre_stage) (saison = p)) 
     (set IA02 faisable) (set MT10 faisable) (set SY06 faisable)) ; R4
    (((profil = pre_stage) (saison = a))
     (set IA01 faisable) (set MI01 faisable) (set MT09 faisable)
     (set SR01 faisable) (set SY31 faisable) (set SY08 faisable)) ; R5
    (((profil = pre_stage)) (set NF16 obligatoire)) ;r47
    (((profil = pre_stage) (cursus = DUT)) (set MB11 obligatoire)) ; R6
    (((profil = pre_stage) (cursus = TC)) (set MB11 impossible)) ;R7
    (((profil = pre_stage) (saison = p))
     (set LO22 faisable) (set RO03 faisable) (set RO04 faisable)
     (set SR02 faisable) (set SY14 faisable)) ; r9
    (((profil = pre_stage)) (set MT12 faisable) (set SY02 faisable)) ;r10
    (((profil = post_stage)) (set GE38 faisable)) ;r11
    (((filiere = ICSI) (saison = a)) 
     (set IA03 obligatoire) (set NF29 obligatoire)) ; r13, r19
    (((filiere = ICSI)) (LO17 obligatoire)) ; r16
    (((filiere = ADEL) (saison = a))
     (set MP03 obligatoire) (set RO05 obligatoire)) ; r18,r22
    (((filiere = SRI) (saison = a))
     (set SR04 obligatoire) (set SR05 obligatoire) (set SR06 obligatoire)) ;r25, r26, r29'
    (((filiere = FDD) (saison = a))
     (set SY19 obligatoire)) ;r28
    (((filiere = STRIE) (saison = a))
     (set SY27 obligatoire)) ;r29
    (((filiere = ISCI) (saison = p))
     (set IA04 obligatoire) (set NF28 obligatoire));r32, r39
    (((filiere = STRIE) (saison = p))
     (set MI03 obligatoire) (set NF33 obligatoire)) ;r34, r40
    (((filiere = ADEL) (saison = p))
     (set MP02 obligatoire)) ;r35
    (((filiere = FDD) (saison = p) (NF17 = fait))
     (set NF26 obligatoire)) ;r38
    (((filiere = SRI) (saison = p))
     (set SR03 obligatoire)) ;r42
    (((filiere = FDD) (saison = p))
     (set SY09 obligatoire)) ;r44
    (((profil = pre_stage) (saison = p)) (set LO21 obligatoire)) ;r48
    (((profil = post_stage) (saison = p) (NF16 = fait))
     (set nf17  obligatoire));r49
    (((profil = pre_stage) (saison = p) (NF16 = fait))
     (set NF17 conseillee)) ;r50
    (((profil = post_stage) (GE37 = fait)) (set GE40 faisable)) ;r51
    (((semestre >= 5)) (set GE39 faisable))  ;r52
    (((profil = post_stage) (saison = a) (filiere != ICSI))
     (set IA03 faisable) (set LO17 faisable)) ;r12, r15
    (((profil = post_stage) (saison = a))
     (set LO12 faisable) (set RV01 faisable)) ;r14, r23
    (((profil = post_stage) (saison = a) (LO21 = fait))
     (set LO23 faisable)) ;r17
    (((profil = post_stage) (saison = a) (filiere != ADEL))
     (set RO05 faisable)) ;r21
    (((profil = post_stage) (saison = a) (filiere != SRI))
     (set SR04 faisable) (set SR05 faisable) (set SR06 faisable)) ;r24, r27, r29'_
    (((profil = post_stage) (saison = a) (filiere != FDD))
     (set SY19 faisable)) ;r28',
    (((profil = post_stage) (saison = a) (filiere != STRIE))
     (set SY27 faisable)) ;r29_
    (((profil = post_stage) (saison = p))
     (set BI04 faisable) (set LO18 faisable) (set SY23 faisable) (set SY26 faisable)); r30, r33, r45, r46
    (((profil = post_stage) (saison = p) (filiere != SRI))
     (set IA04 faisable) (set SR03 faisable)); r31, r41
    (((profil = post_stage) (saison = p) (filiere != STRIE))
     (set MI03 faisable)) ;r34_
    (((profil = post_stage) (saison = p) (filiere != ADEL))
     (set MP02 faisable)) ;r35_
    (((profil = post_stage) (saison = p) (NF16 = fait))
     (set nf11 faisable)) ;r36
    (((profil = post_stage) (saison = p) (filiere != FDD))
     (set NF26 faisable) (set SY09 faisable)) ;r37_, r43
    ))

;; --- base de regle ---
;; *br* = liste de fait = liste de (attribut valeur)

;; --- fonction de service ---

;; verif_premisse_simple (premisse fait)
;; verifier une simple premisse a partir d'un fait
;; retourner le fait si vrai, sinon nil
(defun verif_premisse_simple (premisse fait)
  (if (not (equal (car premisse) (car fait)))
      (return-from verif_premisse_simple nil))
  (let ((v_p (caddr premisse)) 
        (v_f (cadr fait))
        (op (cadr premisse)))
    (cond 
      ((and (equal op '>) (> v_f v_p))
       fait)
      ((and (equal op '>=) (>= v_f v_p))
       fait)
      ((and (equal op '=) (equal v_f v_p))
       fait)
      ((and (equal op '<) (< v_f v_p))
       fait)
      ((and (equal op '<=) (<= v_f v_p))
       fait)
      ((and (equal op '!=) (not (equal v_f v_p)))
       fait)
      (t nil))))

;; verif_premisse (premisses faits)
;; verifier une liste de premisses a partir d'une liste de faits
;; retourner t si vrai, sinon nil
(defun verif_premisse (premisses faits)
  (when (< (length faits) (length premisses))
    (return-from verif_premisse nil))
  (if (member 
       nil
       (mapcar #'(lambda (p) 
                   (remove
                    nil
                    (mapcar #'(lambda (f) 
                                (verif_premisse_simple 
                                 p f)) 
                            faits)))
               premisses))
      nil
      t))

;; verif_regle (regle bf)
;; verifier une regle a partir de la base de faits
;; retourne la conclusion (une liste de faits) si la regle est verifiee, sinon nil
(defun verif_regle (regle bf)
  (if (verif_premisse (car regle) bf)
      (cdr regle)
      nil))

;; chercher_regle (br bf)
;; chercher la premier regle applicable
;; retourne la regle si existe, sinon nil
(defun chercher_regle (br bf)
  (do* ((regles br (cdr regles))
        (regle (car regles) (car regles)))
       ((or (null regle) (verif_regle regle bf))
        regle)))

;; verifier_avant (but BF BR) 
;; fonction verifier en utilisant le chainage avant
;; si verifier, retourner but et sa valeur
;; sinon nil
(defun verifier_avant (but BF BR)
  (if (assoc but bf)
      (return-from verifier_avant (assoc but bf)))
   ; la construction de la boucle do* est assez particulière
   ; (do* (declarations) (test_de_fin retour))
   ; avec une declaration de la forme (nom_de_variable valeur_initiale valeur_recalculee_a_chaque_iteration)
  (do* ((regles br (remove regle regles))
        (faits bf (process-conclusions conclusions faits))
        (regle (chercher_regle regles faits)
               (chercher_regle regles faits))
        (conclusions (cdr regle) (cdr regle))
        (but_atteint (assoc but faits) (assoc but faits)))
       ((null regle) but_atteint)
    )
  )

(defun process-conclusions (conclusions BF)
  (dolist (conclusion conclusions BF)
    (unless (assoc (cadr conclusion) BF) ; si le fait est déjà dans la base on ne le modifie pas
      (let ((res (process-one-conclusion conclusion)))
        (when res (push res BF))
        )
      )
    )
  )

; renvoie le couple (fait valeur) à ajouter à la base
; ou nil si il ne faut rien ajouter
(defun process-one-conclusion (conclusion)
  (case (verbe-action conclusion)
    ('set (fait-conclusion conclusion))
    ('ask (list (fait-cible conclusion) (lire (question conclusion))))
    )
  )

(defmacro verbe-action (conclusion)
  `(car ,conclusion))

(defmacro fait-conclusion (conclusion)
  `(cdr ,conclusion))

(defmacro fait-cible (conclusion)
  `(cadr ,conclusion))

(defmacro question (conclusion)
  `(caddr ,conclusion))

;; verifier_avant_ (but BF BR)
;; meme fonction aue verifier_avant
;; mais retourner une couple (but bf)
(defun verifier_avant_ (but BF BR)
  (do* ((regles br (remove regle regles))
        (faits bf (process-conclusions conclusions faits))
        (regle (chercher_regle regles faits)
               (chercher_regle regles faits))
        (conclusions (cdr regle) (cdr regle))
        (fait_atteint (assoc but faits) (assoc but faits)))
       ((null regle) (list fait_atteint faits))))

;; resultat_complet
;; retourner une liste sous forme (but, uv_obligatoire, uv_conseillee, uv_faisable)
;; argument res: couple (but faits)  => resultat de verifier_avant_
(defun resultat_complet (res)
  (let ((l1 (list)) (l2 (list)) (l3 (list)))
    (dolist (f (cadr res))
      (cond 
        ((equal (cadr f) 'obligatoire)
         (setq l1 (append l1 (list (car f)))))
        ((equal (cadr f) 'conseillee)
         (setq l2 (append l2 (list (car f)))))
        ((equal (cadr f) 'faisable)
         (setq l3 (append l3 (list (car f)))))))
    (if (car res)
        (format t "* UV choisie : ~a~%" (car res))
        (format t "* UV choisie impossible à faire~%")
        )
    (when l1
      (format t "* UVs obligatoires:~%")
      (afficher-liste-uvs l1))
    (when l2
      (format t "* UVs conseillées:~%")
      (afficher-liste-uvs l2))
    (when l3
      (format t "* UVs faisables:~%")
      (afficher-liste-uvs l3)
      )
    (list (car res) l1 l2 l3)
    )
  )

;; load_profil
;; lire les informations initials a partir d'un fichier texte
(defun load_profil (f)
  (let ((bf (list)))
    (with-open-file (file f)
      (do ((stream (read-line file) (read-line file)))
          ((string= stream "end"))
        (cond
          ((string= stream "semestre")
           (setq bf
                 (append
                  bf
                  (list (list 'semestre 
                              (read-from-string
                               (read-line file)))))))
          ((string= stream "profil")
           (setq bf
                 (append 
                  bf
                  (list (list 'profil
                              (read-from-string
                               (read-line file)))))))
          ((string= stream "cursus")
           (setq bf
                 (append 
                  bf
                  (list (list 'cursus
                              (read-from-string
                               (read-line file)))))))
          ((string= stream "saison")
           (setq bf
                 (append
                  bf
                  (list (list 'saison
                              (read-from-string
                               (read-line file)))))))
          ((string= stream "filiere")
           (setq bf
                 (append
                  bf
                  (list (list 'filiere
                              (read-from-string
                               (read-line file)))))))
          ((string= stream "uv_acquise")
           (do ((s (read-line file) (read-line file)))
               ((string= s "end_uv"))
             (setq bf
                   (append 
                    bf
                    (list (list (read-from-string s)
                                'fait))))))))) 
    bf))

;;===  test
;; CL-USER> (verifier_avant_ (nf16 (load_profil "p1") *br*))
;; ; Evaluation aborted on #<UNDEFINED-FUNCTION @ #x20df8a7a>.
;; CL-USER> (verifier_avant_ 'nf16 (load_profil "p1") *br*)
;; ((NF16 OBLIGATOIRE)
;;  ((NF16 OBLIGATOIRE) (SY02 FAISABLE) (MT12 FAISABLE) (FQ01 FAISABLE)
;;   (PROFIL PRE_STAGE) (SEMESTRE 1)))
;; CL-USER> (resultat_complet (verifier_avant_ 'nf16 (load_profil "p1") *br*))
;; ((NF16 OBLIGATOIRE) (NF16) NIL (SY02 MT12 FQ01))

(defun lire (message)
  (format *query-io* "> ~a " message)
  (force-output *query-io*)
  (read *query-io*))

(defun ask_profil (&aux bf)
  (push `(semestre ,(lire "Quel est votre semestre ?")) bf)
  (push `(saison ,(lire "Somme nous au Printemps (P) ou en Automne (A) ?")) bf)
  bf
  )

; est-ce qu'un certain fait est défini dans la base de fait ?
(defun fait-set? (fait bf)
  (assoc fait bf)
  )

; est-ce qu'une règle défini un certain fait ?
; peut importe le type de conclusion le fait défini est toujours en deuxième position dans la liste ( cadr ), juste après le verbe d'action
(defun regle-define (fait regle &aux (conclusions (cdr regle)))
  (loop for conclusion in conclusions
     if (equal (cadr conclusion) fait) return conclusion)
  )

; renvoie toute les règles qui définissent la valeur de fait dans leur conclusion
(defun regles-possibles (fait br)
  (loop for regle in br
     if (regle-define fait regle)
     collect regle
       )
  )

(defun verifier_arriere (uv bf br)
  (let ((done (fait-set? uv bf)))
    (if done done
        (dolist (regle (regles-possibles uv br) nil)
          (when (premisses-verifiees? regle bf br)
            (return (assoc uv (process-conclusions (cdr regle) bf)))
            )
          )
        )
    )
  )

(defun premisses (regle)
  (car regle))

(defun premisses-verifiees? (regle bf br)
  (dolist (premisse (premisses regle) t)
    (unless (verif_premisse_simple premisse (assoc (car premisse) bf))
      (unless (verifier-premisse-arriere premisse bf br)
        (return-from premisses-verifiees? nil)))
    )
  )

(defun verifier-premisse-arriere (premisse bf br &aux (fait (car premisse)))
  (let ((couple_fait (fait-set? fait bf)))
    (when couple_fait
      (return-from verifier-premisse-arriere (verif_premisse_simple premisse couple_fait))
      )
    )
  (dolist (regle br nil)
    (let ((conclusion (regle-define fait regle)))
      (when conclusion
        (when (premisses-verifiees? regle bf br) ; on regarde d'abord si cette regle peut etre executee
; on a trouvé une conclusion qui définie ce fait maintenant il faut voir si la valeur est en adéquation
          (let ((couple_fait (process-one-conclusion conclusion))) ; puis la valeur resultante
            (when (verif_premisse_simple premisse couple_fait)
              (push couple_fait bf)
              (return-from verifier-premisse-arriere couple_fait)
              )
            )
          )
        )
      )
    )
  )
