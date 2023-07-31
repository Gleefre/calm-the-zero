(in-package #:calm)

;; buggy GETF in JSCL
(defun mgetf (list label)
  (loop for (a b) on list by #'cddr
        when (eql a label) return b))

(defun (setf mgetf) (value list label)
  (loop for REF on list by #'cddr
        for (a b) = REF
        when (eql a label) do (setf (cadr REF) value) (return))
  value)

(setf *calm-window-width* 600)
(setf *calm-window-height* 600)
(setf *calm-window-title* "Calm the zero")

(defparameter *level* nil)
(defparameter *screen* :menu)

(defun draw-level-board (level r g b)
  (c:move-to 0 0)
  (c:set-source-rgb r g b)
  (loop for y from 80 by 150
        for line in (mgetf *level* :field)
        do (loop for x from 80 by 150
                 for cell in line
                 do (c:rrectangle x y 140 140 :radius 8)))
  (c:fill-path)
  (loop for y from 90 by 150
        for line in (mgetf *level* :field)
        do (loop for x from 90 by 150
                 for cell in line
                 do (c:rrectangle x y 120 120 :radius 8)))
  (c:set-source-rgb 1 1 1)
  (c:fill-path)
  (c:set-source-rgb r g b)
  (c:select-font-family "PromptFont" :normal :bold)
  (c:set-font-size 65)
  (loop for y from 80 by 150
        for |y| from 0
        for line in (mgetf *level* :field)
        do (loop for x from 80 by 150
                 for |x| from 0
                 for cell in line
                 do (when (and (= |x| (mgetf level :x))
                               (= |y| (mgetf level :y)))
                      (c:set-source-rgb r g b)
                      (c:move-to (+ 50 x) (+ 100 y))
                      (c:show-text (format nil "~A" (mgetf level :zero))))
                    (when cell
                      (c:set-source-rgb (/ 3 10) (/ 7 10) (/ 6 10))
                      (c:move-to (+ 30 x) (+ 100 y))
                      (c:show-text (format nil "~A~A" (car cell) (cadr cell)))))))

(defun draw-level (level)
  (draw-level-board level 0 0 0)
  (c:set-source-rgb 0 0 0)
  (c:select-font-family "PromptFont" :normal :bold)
  (c:set-font-size 30)
  (c:move-to 10 50)
  (c:show-text "Level 0")
  (c:move-to 70 550)
  (c:show-text "Collect all modifiers...")
  (c:move-to 100 575)
  (c:show-text "...and remain being the 0")
  (c:set-source-rgb (/ 4 10) (/ 4 10) (/ 4 10))
  (c:select-font-family "PromptFont" :normal :bold)
  (c:move-to 250 25)
  (c:show-text "move: WASD")
  (c:move-to 270 50)
  (c:show-text "reset: R")
  (c:move-to 290 75)
  (c:show-text "menu: M"))

(defun draw-menu ()
  (c:set-source-rgb 0 0 0)
  (c:select-font-family "PromptFont" :normal :bold)
  (c:set-font-size 45)
  (c:move-to 20 300)
  (c:show-text "Press SPACE to start"))

(defun draw-win ()
  (draw-level-board *level* (/ 8 9) (/ 8 9) (/ 8 9))
  (c:set-source-rgb 0 0 0)
  (c:select-font-family "PromptFont" :italic :bold)
  (c:set-font-size 50)
  (c:move-to 10 300)
  (c:show-text "You won!")
  (c:move-to 110 350)
  (c:set-font-size 40)
  (c:set-source-rgb (/ 6 10) (/ 6 10) (/ 6 10))
  (c:select-font-family "PromptFont" :italic :normal)
  (c:show-text "Press M to go")
  (c:move-to 210 390)
  (c:show-text "to the menu"))

(defun draw-forever ()
  (c:set-source-rgb 1 1 1)
  (c:paint)
  (case *screen*
    (:level
     (draw-level *level*))
    (:menu
     (draw-menu))
    (:win
     (draw-win))))

(defun restart-level ()
  (setf *level* (list :x 0
                      :y 0
                      :zero 0
                      :field (list (list nil nil (list '+ 1))
                                   (list nil (list '- 1) (list '* 2))
                                   (list nil nil nil))
                      :mods 3)))

(defun move (dx dy)
  (let ((x (mgetf *level* :x))
        (y (mgetf *level* :y)))
    (loop while (<= 0 (+ dy y) 2)
          while (<= 0 (+ dx x) 2)
          until (nth x (nth y (mgetf *level* :field)))
          do (incf x dx)
             (incf y dy))
    (setf (mgetf *level* :x) x
          (mgetf *level* :y) y)
    (when (nth x (nth y (mgetf *level* :field)))
      (decf (mgetf *level* :mods))
      (setf (mgetf *level* :zero)
            (destructuring-bind (op arg)
                (nth x (nth y (mgetf *level* :field)))
              (funcall op (mgetf *level* :zero) arg)))
      (setf (nth x (nth y (mgetf *level* :field))) nil)
      (when (and (= (mgetf *level* :mods) 0)
                 (= (mgetf *level* :zero) 0))
        (setf *screen* :win)))))

(defun up () (move 0 -1))
(defun down () (move 0 1))
(defun right () (move 1 0))
(defun left () (move -1 0))

(defun on-keydown (key)
  (case *screen*
    (:menu (cond ((c:keq key :scancode-space)
                  (setf *level* (restart-level)
                        *screen* :level))))
    (:level (cond ((c:keq key :scancode-r)
                   (setf *level* (restart-level)))
                  ((c:keq key :scancode-m)
                   (setf *screen* :menu))
                  ((c:keq key :scancode-a :scancode-left)
                   (left))
                  ((c:keq key :scancode-d :scancode-right)
                   (right))
                  ((c:keq key :scancode-w :scancode-up)
                   (up))
                  ((c:keq key :scancode-s :scancode-down)
                   (down))))
    (:win (cond ((c:keq key :scancode-m)
                 (setf *screen* :menu))
                ((c:keq key :scancode-a :scancode-left)
                 (left))
                ((c:keq key :scancode-d :scancode-right)
                 (right))
                ((c:keq key :scancode-w :scancode-up)
                 (up))
                ((c:keq key :scancode-s :scancode-down)
                 (down))))))
