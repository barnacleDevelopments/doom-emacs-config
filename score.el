;;; Elfeed score file                                     -*- lisp -*-
;;; How much I give a shit points
((version 10)
 ("title")
 ("content")
 ("title-or-content"
  ;; High score
  (:text "advisory" :title-value 2000 :content-value 2000 :type W)
  (:text "judas release" :title-value 2000 :content-value 2000 :type W)
  (:text "bioshock release" :title-value 2000 :content-value 2000 :type W)
  (:text "cosmic desktop" :title-value 2000 :content-value 2000 :type W)
  (:text "judas" :title-value 1000 :content-value 1000 :type W)
  (:text "ghost story" :title-value 1000 :content-value 1000 :type W)
  (:text "fallout" :title-value 500 :content-value 500 :type W)
  (:text "call of duty" :title-value 500 :content-value 500 :type W)
  (:text "doom" :title-value 500 :content-value 500 :type W)
  (:text "elder scrolls" :title-value 500 :content-value 500 :type W)
  (:text "media molecule" :title-value 500 :content-value 500 :type W)
  (:text "magit release" :title-value 400 :content-value 400 :type W)

  ;; MID SCORE
  (:text "halifax election" :title-value 400 :content-value 400 :type W)
  (:text "starship" :title-value 400 :content-value 400 :type W)
  (:text "mars" :title-value 400 :content-value 400 :type W)
  (:text "nova scotia election" :title-value 400 :content-value 400 :type W)
  (:text "education" :title-value 300 :content-value 300 :type W)
  (:text "housing" :title-value 200 :content-value 200 :type W)
  (:text "healthcare" :title-value 200 :content-value 200 :type W)
  (:text "climate change" :title-value 100 :content-value 100 :type W)
  (:text "emissions targets" :title-value 50 :content-value 50 :type W)
  (:text "child care" :title-value 100 :content-value 100 :type W)

  ;; LOW SCORE
  (:text "halifax" :title-value 99 :content-value 99 :type s)
  (:text "nova scotia" :title-value 99 :content-value 99 :type s)
  (:text "bioshock" :title-value 99 :content-value 99 :type s)

  ;; Blockchain + AI
  (:text "interoperability" :title-value 400 :content-value 400 :type W)
  (:text "zero-Knowledge proofs" :title-value 400 :content-value 400  :type W)
  (:text "decentralized identity" :title-value 400 :content-value 400  :type W)
  (:text "decentralized identity" :title-value 400 :content-value 400  :type W)
  (:text "decentralized governance" :title-value 400 :content-value 400  :type W)
  (:text "artificial general inteligence" :title-value 400 :content-value 400  :type W)
  (:text "milestones" :title-value 400 :content-value 400  :type W)

  ;; Stuff I don't need to hear about
  (:text "woke culture" :title-value -5000 :content-value -5000  :type W)
  (:text "cancel culture" :title-value -5000 :content-value -5000  :type W)
  (:text "virtue signaling" :title-value -5000 :content-value -5000  :type W)
  (:text "culture wars" :title-value -5000 :content-value -5000 :type W)
  (:text "tokenism" :title-value -5000 :content-value -5000 :type W)
  (:text "trans" :title-value -5000 :content-value -5000 :type W)

   ;; Bonus Points
  (:text "positive" :title-value 50 :content-value 50 :type W)
  (:text "motivational" :title-value 50 :content-value 50 :type W)
  (:text "inspiring" :title-value 50 :content-value 50 :type W)
  (:text "hopeful" :title-value 50 :content-value 50 :type W)
  (:text "purpose" :title-value 50 :content-value 50 :type W)
  (:text "resilient" :title-value 50 :content-value 50 :type W)
  )
 ("tag")
 ("authors"
  (:text "Ken Levine" :value 2000 :type s))
 ("feed"
  (:text "Hyperledger" :value 500 :type S :attr t))
 ("adjust-tags"))
