#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         file/glob
         json
         "which.rkt")
(module+ main
  (require racket/cmdline))
(module+ test
  (require rackunit))

;; TODO: replace these with something more sensible and configurable
(define-runtime-path mscore
  "/Applications/MuseScore 4.app/Contents/MacOS/mscore")

(module+ main
  (define recursive-mode (make-parameter #f))
  (define watch-mode (make-parameter #f))
  (define pdf-paths
    (command-line
     #:program "musescore-guitar-open-lines"
     #:once-each
     [("-r" "--recursive")
      "Convert recursively in sub-directories"
      (recursive-mode #t)]
     [("-w" "--watch")
      "Continuously watch for updates"
      (watch-mode #t)]
     #:args paths
     (apply main #:r (recursive-mode) #:w (watch-mode) paths)))
  (unless (empty? pdf-paths)
    (displayln "output pdf files:")
    (for ([p (in-list pdf-paths)])
      (displayln (find-relative-path (current-directory) p)))))

;; #:r Boolean PathString ... -> (Listof FilePath)
(define (main #:r [r? #f] #:w [w? #f] . paths)
  (cond
    [(empty? paths) (paths-openlines #:r r? #:w w? (list (current-directory)))]
    [else (paths-openlines #:r r? #:w w? paths)]))

;; #:r Boolean (Listof PathString) -> (Listof FilePath)
(define (paths-openlines #:r [r? #f] #:w [w? #f] paths)
  (files-openlines #:w w? (paths->file-paths #:r r? paths)))

;; (Listof PathString) -> (Listof FilePath)
(define (files-openlines #:w [w? #f] paths)
  (cond
    [w? (watch-openlines paths)]
    [else (append-map file-openlines paths)]))

;; (Listof PathString) -> (Listof FilePath)
(define (watch-openlines paths)
  (define before (current-seconds))
  (define r (map file-openlines paths))
  (let loop ([before before] [r r])
    (cond
      [(input-done?) (append* r)]
      [else
       (define before2 (current-seconds))
       (define r2
         (for/fold ([r r])
                   ([path (in-list paths)] [i (in-naturals)])
           (cond
             [(modified-since? path before)
              (list-set r i (file-openlines path))]
             [else r])))
       (cond
         [(eq? r2 r) (loop before r)]
         [else (loop before2 r2)])])))

;; PathString -> (Listof FilePath)
(define (file-openlines path)
  (cond
    [(or (path-has-extension? path #".mscz")
         (path-has-extension? path #".mscx"))
     (musescore-file-openlines path)]
    [(path-has-extension? path #".musicxml")
     (musicxml-file-openlines path)]
    [else '()]))

;; PathString -> Boolean
(define (can-convert? p)
  (or (path-has-extension? p #".mscz")
      (path-has-extension? p #".mscx")
      (path-has-extension? p #".musicxml")))

;; #:r Boolean -> [Path -> Boolean]
(define ((use-dir? #:r [r? #f]) p)
  (and r? (not (string-contains? (path->string p) "conversion"))))

;; #:r Boolean (Listof PathString) -> (Listof PathString)
(define (paths->file-paths #:r [r? #f] paths)
  (append-map (path->file-paths #:r r?) paths))

;; #:r Boolean -> [PathString -> (Listof PathString)]
(define ((path->file-paths #:r [r? #f]) path)
  (cond
    [(can-convert? path) (list path)]
    [(directory-exists? path)
     (for/list ([p (in-directory path (use-dir? #:r r?))] #:when (can-convert? p))
       p)]
    [else '()]))

(define (input-done? [in (current-input-port)])
  (or (port-closed? in)
      (and (sync/timeout 0 in) (eof-object? (read-byte in)))))

(define (modified-since? path before)
  (define modified (file-or-directory-modify-seconds path #f (λ () #f)))
  (cond
    [modified (< before modified)]
    [else #false]))

;; ---------------------------------------------------------

;; PathString -> (Listof FilePath)
(define (musescore-file-openlines path*)
  (unless (or (path-has-extension? path* #".mscz")
              (path-has-extension? path* #".mscx"))
    (error "expected mscz or mscx"))
  (setup-conversion-dir path* musescore->openlines-pdf-parts))

;; PathString -> (Listof FilePath)
(define (musicxml-file-openlines path*)
  (unless (path-has-extension? path* #".musicxml")
    (error "expected musicxml"))
  (setup-conversion-dir path* musicxml->openlines-pdf))

;; PathString [FilePath DirPath String -> (Listof FilePath)] -> (Listof FilePath)
(define (setup-conversion-dir path* convert)
  (define path (normalize-path path*))
  (define name (file-name-from-path path))
  (define name-string (path->string (path-replace-extension name #"")))
  (define dir (path-only path))
  (define conversion-dir* (build-path dir "conversion/"))
  (make-directory* conversion-dir*)
  (define conversion-dir (normalize-path conversion-dir*))
  (map
   normalize-path
   (parameterize ([current-directory conversion-dir])
     (convert path conversion-dir name-string))))

;; FilePath DirPath String -> (Listof FilePath)
(define (musescore->openlines-pdf-parts path conversion-dir name-string)
  ;; conversion to musicxml
  (define musicxml-parts
    (musescore->musicxml-parts path conversion-dir name-string))
  ;; conversion to pdf
  (musicxml-parts->pdf-parts name-string musicxml-parts))

;; FilePath DirPath String -> (Listof FilePath)
(define (musicxml->openlines-pdf path conversion-dir name-string)
  ;; copy musicxml into conversion-dir
  (define conversion-path
    (build-path conversion-dir (string-append name-string ".musicxml")))
  (copy-file path conversion-path #:exists-ok? #t)
  ;; conversion to pdf
  (musicxml-parts->pdf-parts name-string (list conversion-path)))

;; String (Listof FilePath) -> (Listof FilePath)
(define (musicxml-parts->pdf-parts name-string musicxml-parts)
  ;; conversion to lilypond
  (define raw-lilypond-parts
    (musicxml-parts->raw-lilypond-parts name-string musicxml-parts))
  ;; openlines lilypond conversion and cleanup
  (define openlines-lilypond-parts
    (lilypond-parts->openlines name-string raw-lilypond-parts))
  ;; conversion to pdf
  (lilypond-parts->pdf-parts name-string openlines-lilypond-parts))

;; ---------------------------------------------------------

;; FilePath DirPath String -> (Listof FilePath)
(define (musescore->musicxml-parts path conversion-dir name-string)
  (define conversion-job-path
    (build-path conversion-dir "musescore-conversion-job.json"))
  (define path/conversion-dir (find-relative-path conversion-dir path))
  (define conversion-job-json
    (list
     (hasheq 'in (path->string path/conversion-dir)
             'out (string-append name-string ".musicxml"))
     (hasheq 'in (path->string path/conversion-dir)
             'out (list (list (string-append name-string "-") ".musicxml")))))
  (call-with-output-file*
   conversion-job-path
   (λ (out)
     (write-json conversion-job-json out)
     (newline out))
   #:exists 'replace)
  (system* mscore "-j" conversion-job-path)

  (glob (string-append (glob-quote name-string) "*.musicxml")))

;; String (Listof FilePath) -> (Listof FilePath)
(define (musicxml-parts->raw-lilypond-parts name-string musicxml-parts)
  (define musicxml2ly (which "musicxml2ly"))
  (for/list ([mx (in-list musicxml-parts)])
    (system* musicxml2ly mx)
    (path-replace-extension mx #".ly")))

;; String (Listof FilePath) -> (Listof FilePath)
(define (lilypond-parts->openlines name-string lilypond-raw-parts)
  (define sed (which "sed"))
  (for/list ([ly (in-list lilypond-raw-parts)])
    (define openlines-ly (path-replace-extension ly #"-openlines.ly"))
    (with-output-to-file openlines-ly
      (λ () (write-string (lilypond-string->openlines (file->string ly))))
      #:exists 'replace)
    openlines-ly))

;; String (Listof FilePath) -> (Listof FilePath)
(define (lilypond-parts->pdf-parts name-string lilypond-parts)
  (define lilypond (which "lilypond"))
  (for/list ([ly (in-list lilypond-parts)])
    (system* lilypond ly)
    (path-replace-extension ly #".pdf")))

;; ---------------------------------------------------------

;; lilypond-string->openlines : String -> String
(define (lilypond-string->openlines ly)
  (regexp-replaces
   ly
   '([#rx":m5 "
      ":m "]
     [#rx"( *)\\\\set Staff.shortInstrumentName = \"Guit.\""
      #<<```
&
\1\\override Staff.StaffSymbol.line-positions = #'(-11 -8 -5 -2 0 3)
\1\\override Staff.StaffSymbol.ledger-positions = #'(-9 -6 -3 2 5 7 9)

```
      ])))

(module+ test
  (check-equal? (lilypond-string->openlines
                 #<<```
﻿            \set Staff.instrumentName = "Guitar"
            \set Staff.shortInstrumentName = "Guit."
            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPTwoVoiceOne" {  \voiceOne \PartPTwoVoiceOne }
                \context Voice = "PartPTwoVoiceTwo" {  \voiceTwo \PartPTwoVoiceTwo }
                >>
```
                 )
                #<<```
﻿            \set Staff.instrumentName = "Guitar"
            \set Staff.shortInstrumentName = "Guit."
            \override Staff.StaffSymbol.line-positions = #'(-11 -8 -5 -2 0 3)
            \override Staff.StaffSymbol.ledger-positions = #'(-9 -6 -3 2 5 7 9)

            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPTwoVoiceOne" {  \voiceOne \PartPTwoVoiceOne }
                \context Voice = "PartPTwoVoiceTwo" {  \voiceTwo \PartPTwoVoiceTwo }
                >>
```
                ))

