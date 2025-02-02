;; -*- lexical-binding: t; -*-

;; Take the current buffer (the entire text of Moby Dick and a Man)
;; Extract 5,000 (or N) words that are 5 characters (M) or longer.
;; Return a list of strings.

(let ((lorem '(
"Lorem ipsum dolor sit amet, consectetur adipiscing elit. In
 facilisis dolor non bibendum bibendum. Proin gravida magna risus,
 ac interdum odio commodo sed. Suspendisse tempus lorem sit amet
 fermentum placerat. Vivamus nec sapien augue. Sed blandit lorem
 non ex ultricies, sed dapibus enim mollis. Quisque lectus lorem,
 placerat et sagittis maximus, gravida ut augue. Mauris ac quam
 orci. Integer finibus vel erat id semper. In ut maximus libero.
 Etiam laoreet sit amet mi ut efficitur."

"Maecenas tortor tortor, elementum sit amet eros sit amet,
 ultricies tincidunt neque. Cras quis mauris nec urna facilisis
 placerat. Mauris id dapibus leo, in euismod nunc. Integer et
 tortor vehicula, rhoncus sem vitae, condimentum arcu. Nam
 hendrerit porta pretium. Aliquam ut venenatis odio, non iaculis
 odio. Nullam nec lorem in odio commodo elementum."

"Cras sem justo, consectetur eget eros ultricies, congue ornare
 nunc. Phasellus viverra dui a sapien feugiat, at blandit sem
 pulvinar. Morbi vehicula enim metus, ac euismod ante maximus
 vitae. Maecenas dignissim lacus quam, ac vestibulum dui lobortis
 in. Cras quis velit id quam gravida suscipit. Nunc dignissim
 eros ligula, et rhoncus velit commodo sit amet. Aenean a
 ultricies mauris. In efficitur non eros sed vulputate. Morbi
 tortor arcu, cursus a risus a, porttitor dictum turpis. Nunc
 vestibulum hendrerit enim, eu ultrices sapien. Aenean quis
 turpis non ligula viverra tincidunt."

"Pellentesque fermentum euismod egestas. Pellentesque vitae
 imperdiet libero, ac laoreet mi. Fusce eros est, scelerisque nec
 dignissim cursus, euismod sit amet metus. Morbi et quam nec
 augue imperdiet tristique at vel felis. In iaculis consectetur
 facilisis. Donec nec dolor non ante rutrum rutrum. Phasellus in
 porta erat. Vestibulum tortor massa, euismod non euismod vel,
 vehicula ut felis."

"Fusce felis purus, semper nec interdum non, placerat at neque.
 Quisque viverra nisl metus, quis iaculis nibh gravida eget.
 Maecenas ipsum libero, pellentesque at risus quis, aliquam
 ullamcorper augue. Vivamus vel sem vitae tellus condimentum
 posuere. Sed vitae diam eget tortor lobortis dignissim vitae eu
 arcu. Fusce luctus et est non posuere. Integer facilisis mauris
 vel neque molestie dignissim. Quisque non enim sit amet erat
 pulvinar maximus. Class aptent taciti sociosqu ad litora
 torquent per conubia nostra, per inceptos himenaeos. Cras
 posuere, ligula nec maximus elementum, massa nunc ultricies
 magna, eget sollicitudin eros libero ac neque. Morbi orci est,
 vulputate in leo sit amet, viverra mollis felis. Vestibulum vel
 mi orci."

"Donec rutrum laoreet suscipit. Nulla a enim sed dui iaculis
 elementum. Integer a enim vitae arcu molestie vehicula. Aenean
 mollis magna quis sapien pellentesque gravida. Curabitur commodo
 odio ornare euismod mattis. Nunc sed lobortis turpis, eu dictum
 magna. Proin tristique ligula ac metus lacinia molestie. Donec
 convallis tortor sit amet vestibulum porta. Duis urna neque,
 aliquet et elementum vel, congue quis nunc. Nullam sed neque
 velit. Sed malesuada feugiat placerat."

"Cras sollicitudin bibendum rutrum. Phasellus pellentesque orci et
 suscipit volutpat. Donec rutrum laoreet est, vel ultricies
 tortor scelerisque ac. Aliquam fringilla sollicitudin sem, ut
 ultricies lorem egestas vitae. Mauris tristique suscipit dolor,
 eu dictum metus vulputate a. Integer at nibh ac ex aliquet
 pretium vitae nec mauris. Orci varius natoque penatibus et
 magnis dis parturient montes, nascetur ridiculus mus."

"Proin porta facilisis interdum. Mauris pharetra leo sit amet
 lectus consectetur, vel fermentum ante ultrices. Fusce in quam
 ac neque lobortis dapibus eu eu ipsum. Suspendisse elit purus,
 posuere et nunc eu, rhoncus tempor tortor. Aliquam nec libero
 lorem. Nullam id risus sapien. Morbi ullamcorper, massa at
 posuere posuere, eros arcu tempor sapien, vel rutrum turpis
 ipsum nec urna. Maecenas id mauris congue, fringilla ligula ut,
 pulvinar augue."

"Quisque posuere mauris ac placerat consequat. Maecenas dictum,
 justo nec suscipit lobortis, ligula quam scelerisque ex, vel
 auctor nisl lectus ut eros. Aenean lectus tellus, tempus nec
 rhoncus eu, vehicula ut dui. Maecenas vehicula quis nunc eget
 accumsan. Nulla nibh tellus, tempus a sem faucibus, dignissim
 vestibulum felis. Duis et nibh metus. Nunc congue risus et
 lacinia dapibus. Integer accumsan laoreet arcu. Pellentesque
 commodo orci vel odio maximus tempor ut vel arcu."

"Ut elit metus, feugiat ac urna a, rutrum vehicula erat. Aliquam
 ut diam nisi. In vestibulum, ipsum eu pulvinar scelerisque, erat
 tellus consequat risus, ac mattis enim quam mattis lorem.
 Aliquam erat volutpat. Pellentesque a nulla accumsan, fermentum
 ante quis, gravida lectus. Nam porttitor tellus ac dictum
 consectetur. Vestibulum sollicitudin nisl eget nunc semper
 pulvinar non quis mauris. Sed eget imperdiet diam. Suspendisse
 tortor nisi, porta sit amet imperdiet in, faucibus ut augue.
 Class aptent taciti sociosqu ad litora torquent per conubia
 nostra, per inceptos himenaeos. Sed facilisis orci quis risus
 ultrices, vel tristique metus luctus. Nam fermentum congue metus
 ut maximus. Maecenas iaculis turpis quis aliquet mollis.")))

  (with-temp-buffer
    (insert-file-contents "20250131_moby dick.txt")

    (let ((goal-words-count 5000)
          (min-char-length 9)
          (words '())
          (total-word-count 0))
      (setq total-word-count (count-words 1 (point-max)))
      (while (and (> goal-words-count (length words)) (not (eobp)))
        (let ((word (downcase (word-at-point :no-props))))
          (and word
               (>=  (length word) min-char-length)
               (not (member word words))
               (push word words))
          (forward-word)))
      (with-temp-file "moby-dick-glossary.org"
        (erase-buffer)
        (insert (format "Total Words in File: %d\n" total-word-count))
        (insert (format "Number of Words Extracted: %d\n\n" (length words)))
        (insert "* Words\n\n")
        (dolist (word (sort words #'string<))
          (insert (concat "** " word "\n"))
          (insert (concat "    <<" word ">>\n"))
          (insert (concat "\s") (nth (random 9) lorem))
          (insert "\n\n"))
        (delay-mode-hooks
          (let ((org-inhibit-startup t)
                (org-agenda-files nil))
            (org-mode)
            (org-indent-region 1 (point-max))))))))
