;;; Elfeed score file                                     -*- lisp -*-
((version 10)
 ("title")
 ("content")
 ("title-or-content"
  ;; Judas and Bioshock at the absolute top
  (:text "Judas" :title-value 1000 :content-value 1000 :type s)
  (:text "Bioshock" :title-value 999 :content-value 999 :type s)
  (:text "Fallout" :title-value 500 :content-value 500 :type s)
  (:text "Call of Duty" :title-value 500 :content-value 500 :type s)
  (:text "Elder Scrolls" :title-value 500 :content-value 500 :type s)
  ;; News with a much lower score
  (:text "Halifax" :title-value 400 :content-value 400 :type s))
 ("tag")
 ("authors"
  (:text "Ken Levine" :value 2000 :type s))
 ("feed"
  (:text "Hacker News" :value 150 :type S :attr t)
  (:text "Hyperledger" :value 498 :type S :attr t))
 (mark -2500)
 ("adjust-tags"))
